use std::{
    collections::HashMap,
    fmt::{self, Debug, Formatter},
    io,
    sync::{Arc, Condvar, Mutex},
    thread,
    time::Duration,
};

use half::f16;
use image::{Rgba32FImage, RgbaImage};
use itertools::Itertools;
use plumber_vmt::{
    MaterialInfo, ParameterError, ParameterType, Shader, ShaderResolveError, TexturePath, Vmt,
};
use thiserror::Error;
use vtflib::{BoundVtfFile, VtfFile, VtfGuard, VtfLib};
use zerocopy::LayoutVerified;

use plumber_fs::{GamePathBuf, OpenFileSystem, Path, PathBuf};
use plumber_uncased::AsUncased;
use plumber_vdf as vdf;

use crate::Handler;

const DIMENSION_REFERENCE_TEXTURES: &[&str] = &["$basetexture", "$normalmap"];
const NODRAW_MATERIALS: &[&str] = &[
    "materials/tools/toolsareaportal",
    "materials/tools/toolsoccluder",
];
const NODRAW_PARAMS: &[&str] = &[
    "%compilenodraw",
    "%compileinvisible",
    "%compilehint",
    "%compileskip",
    "%compilesky",
    "%compile2dsky",
    "%compiletrigger",
    "%compileorigin",
    "%compilefog",
    "%compilenpcclip",
    "%compileplayerclip",
    "%compiledroneclip",
    "%compilegrenadeclip",
    "%compileclip",
    "$no_draw",
];
const CUBEMAP_SUFFIXES: [&str; 6] = ["lf", "rt", "up", "dn", "ft", "bk"];

#[derive(Debug, Error, Clone, Hash, PartialEq, Eq)]
pub enum MaterialLoadError {
    #[error("io error reading `{path}`: {error}")]
    Io { path: String, error: String },
    #[error("error loading patch material: {0}")]
    Patch(#[from] ShaderResolveError),
    #[error("error deserializing material: {0}")]
    Deserialization(#[from] vdf::Error),
    #[error("error loading texture: {0}")]
    Texture(#[from] TextureLoadError),
    #[error("error reading parameters: {0}")]
    Parameter(#[from] ParameterError),
    #[error("{0}")]
    Custom(&'static str),
}

impl MaterialLoadError {
    fn from_io(err: &io::Error, path: &impl ToString) -> Self {
        Self::Io {
            path: path.to_string(),
            error: err.to_string(),
        }
    }
}

#[derive(Debug, Error, Clone, Hash, PartialEq, Eq)]
pub enum TextureLoadError {
    #[error("io error reading `{path}`: {error}")]
    Io { path: String, error: String },
    #[error("error loading vtf file: {0}")]
    Vtf(#[from] vtflib::Error),
}

impl TextureLoadError {
    fn from_io(err: &io::Error, path: &impl ToString) -> Self {
        Self::Io {
            path: path.to_string(),
            error: err.to_string(),
        }
    }
}

fn is_nodraw(material_path: &PathBuf, shader: &Shader) -> bool {
    let no_draw = NODRAW_MATERIALS.iter().any(|m| material_path == *m)
        || NODRAW_PARAMS.iter().any(|p| {
            shader
                .parameters
                .get(p.as_uncased())
                .map_or(false, |v| v == "1")
        });
    no_draw
}

fn get_dimension_reference(shader: &Shader) -> Option<GamePathBuf> {
    DIMENSION_REFERENCE_TEXTURES.iter().find_map(|parameter| {
        let path: TexturePath = shader.try_extract_param(parameter).ok().flatten()?;
        Some(path.absolute_path())
    })
}

#[derive(Clone)]
pub struct LoadedMaterial<D: Send + 'static> {
    pub name: GamePathBuf,
    pub info: MaterialInfo,
    pub data: D,
}

impl<D: Send + Sync + 'static> Debug for LoadedMaterial<D> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.debug_struct("LoadedMaterial")
            .field("name", &self.name)
            .field("info", &self.info)
            .finish_non_exhaustive()
    }
}

struct LoadedMaterialAndTextures<D: Send + 'static> {
    loaded_material: LoadedMaterial<D>,
    loaded_textures: Vec<LoadedTexture>,
}

#[derive(Debug)]
enum MaterialStatus {
    Loading,
    Done(Result<MaterialInfo, MaterialLoadError>),
}

#[derive(Debug)]
enum MaterialJob {
    Material(PathBuf),
    SkyBox(PathBuf),
    Texture(PathBuf),
}

#[derive(Clone, Debug)]
pub struct SkyBox {
    pub name: GamePathBuf,
    pub data: SkyBoxData,
}

#[derive(Clone)]
pub enum SkyBoxData {
    Sdr([RgbaImage; 6]),
    Hdr([Rgba32FImage; 6]),
}

impl Debug for SkyBoxData {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Sdr(_) => f.debug_struct("Sdr").finish_non_exhaustive(),
            Self::Hdr(_) => f.debug_struct("Hdr").finish_non_exhaustive(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Loader {
    material_job_sender: crossbeam_channel::Sender<MaterialJob>,
    worker_state: Arc<WorkerState>,
}

impl Loader {
    #[must_use]
    pub fn new() -> Self {
        let (material_job_sender, material_job_receiver) = crossbeam_channel::unbounded();
        let worker_state = Arc::new(WorkerState::new(material_job_receiver));

        Self {
            material_job_sender,
            worker_state,
        }
    }

    /// Starts loading the given material without waiting for it to finish.
    /// Does nothing is the material is already requested.
    /// `material_path` should be absolute, ie. start with `materials/`.
    pub fn load_material(&self, mut material_path: PathBuf) {
        material_path.normalize_extension();

        self.worker_state
            .send_material_job(&material_path, &self.material_job_sender);
    }

    /// Block the thread until the worker thread has loaded a given material, and return the info.
    /// Blocks forever if the material isn't separately loaded using `load_material()`.
    /// `material_path` should be absolute, ie. start with `materials/`.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the material loading fails or a load was already attempted and it failed.
    ///
    /// # Panics
    ///
    /// Panics after 30 seconds if any materials haven't been loaded to prevent deadlocks.
    pub fn block_on_material(
        &self,
        mut material_path: PathBuf,
    ) -> Result<MaterialInfo, MaterialLoadError> {
        material_path.normalize_extension();

        self.worker_state.wait_for_material(&material_path)
    }

    /// Execute the given function and wait until the worker thread has loaded a given material,
    /// and return the info.
    /// Blocks forever if the material isn't separately loaded using `load_material()`.
    /// `material_path` should be absolute, ie. start with `materials/`.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the material loading fails or a load was already attempted and it failed.
    ///
    /// # Panics
    ///
    /// Panics after 30 seconds if any materials haven't been loaded to prevent deadlocks.
    pub fn parallel_block_on_material(
        self,
        mut material_path: PathBuf,
        f: impl FnOnce(),
    ) -> Result<MaterialInfo, MaterialLoadError> {
        // prevent locks waiting for new materials
        drop(self.material_job_sender);

        f();

        material_path.normalize_extension();

        self.worker_state.wait_for_material(&material_path)
    }

    /// Start loading the material, and
    /// block the thread until the worker thread has loaded a given material, and return the info.
    /// If the material is already loaded, instantly returns the result.
    /// `material_path` should be absolute, ie. start with `materials/`.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the material loading fails or a load was already attempted and it failed.
    ///
    /// # Panics
    ///
    /// Panics after 30 seconds if any materials haven't been loaded to prevent deadlocks.
    pub fn load_material_blocking(
        &self,
        mut material_path: PathBuf,
    ) -> Result<MaterialInfo, MaterialLoadError> {
        material_path.normalize_extension();

        self.worker_state
            .send_material_job(&material_path, &self.material_job_sender);

        self.worker_state.wait_for_material(&material_path)
    }

    /// Start loading the given skybox without waiting for it to finish.
    /// `sky_path` should be absolute, ie. start with `materials/`.
    pub fn load_skybox(&self, sky_path: PathBuf) {
        self.material_job_sender
            .send(MaterialJob::SkyBox(sky_path))
            .expect("material job channel shouldn't be disconnected");
    }

    /// Start loading the given texture without waiting for it to finish.
    /// `texture_path` should be absolute, ie. start with `materials/`.
    pub fn load_texture(&self, mut texture_path: PathBuf) {
        texture_path.normalize_extension();

        self.material_job_sender
            .send(MaterialJob::Texture(texture_path))
            .expect("material job channel shouldn't be disconnected");
    }

    pub fn start_worker_thread(&self, file_system: Arc<OpenFileSystem>, handler: impl Handler) {
        let worker_state = self.worker_state.clone();

        thread::Builder::new()
            .name("material loader".into())
            .spawn(move || {
                worker_state.work(&file_system, handler);
            })
            .expect("thread spawning shouldn't fail");
    }
}

impl Default for Loader {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
struct WorkerState {
    material_job_receiver: crossbeam_channel::Receiver<MaterialJob>,
    material_cache: Mutex<HashMap<PathBuf, MaterialStatus>>,
    material_condvar: Condvar,
    texture_cache: Mutex<HashMap<PathBuf, Result<TextureInfo, TextureLoadError>>>,
}

impl WorkerState {
    #[must_use]
    fn new(material_job_receiver: crossbeam_channel::Receiver<MaterialJob>) -> Self {
        Self {
            material_job_receiver,
            material_cache: Mutex::new(HashMap::new()),
            material_condvar: Condvar::new(),
            texture_cache: Mutex::new(HashMap::new()),
        }
    }

    fn send_material_job(&self, path: &PathBuf, sender: &crossbeam_channel::Sender<MaterialJob>) {
        let mut guard = self
            .material_cache
            .lock()
            .expect("the mutex shouldn't be poisoned");

        if guard.get(path).is_some() {
            // material is already being loaded / has been loaded
            return;
        }

        guard.insert(path.clone(), MaterialStatus::Loading);
        sender
            .send(MaterialJob::Material(path.clone()))
            .expect("material job channel shouldn't be disconnected");
    }

    fn wait_for_material(
        &self,
        material_path: &PathBuf,
    ) -> Result<MaterialInfo, MaterialLoadError> {
        let mut guard = self
            .material_cache
            .lock()
            .expect("the mutex shouldn't be poisoned");
        loop {
            match guard.get(material_path) {
                None | Some(MaterialStatus::Loading) => {
                    let (g, res) = self
                        .material_condvar
                        .wait_timeout(guard, Duration::from_secs(30))
                        .expect("the mutex shouldn't be poisoned");

                    assert!(!res.timed_out(), "any materials haven't been loaded in 30 seconds while waiting for a material");

                    guard = g;
                }
                Some(MaterialStatus::Done(result)) => return result.clone(),
            }
        }
    }

    fn work<'a>(&'a self, file_system: &'a OpenFileSystem, mut handler: impl Handler) {
        let mut vtf_lib = VtfLib::initialize().expect("vtflib is already initialized");

        for job in &self.material_job_receiver {
            match job {
                MaterialJob::Material(material_path) => {
                    match self.load_material(material_path, file_system, &mut vtf_lib, |vmt| {
                        handler.build_material(vmt)
                    }) {
                        Ok((_, Some(loaded))) => {
                            for texture in loaded.loaded_textures {
                                handler.handle_texture(texture);
                            }

                            handler.handle_material(loaded.loaded_material);
                        }
                        Ok((_, None)) => continue,
                        Err((path, error)) => {
                            handler.handle_error(crate::Error::Material { path, error });
                        }
                    }
                }
                MaterialJob::SkyBox(sky_path) => {
                    match Self::load_skybox(&sky_path, file_system, &mut vtf_lib) {
                        Ok(loaded) => handler.handle_skybox(loaded),
                        Err(error) => {
                            handler.handle_error(crate::Error::Material {
                                path: sky_path,
                                error,
                            });
                        }
                    }
                }
                MaterialJob::Texture(texture_path) => {
                    match self.load_texture(texture_path.clone(), file_system, &mut vtf_lib) {
                        Ok((_, Some(loaded))) => {
                            handler.handle_texture(loaded);
                        }
                        Ok((_, None)) => continue,
                        Err(error) => handler.handle_error(crate::Error::Texture {
                            path: texture_path,
                            error,
                        }),
                    }
                }
            }
        }
    }

    #[allow(clippy::type_complexity)]
    fn load_material<D>(
        &self,
        material_path: PathBuf,
        file_system: &OpenFileSystem,
        vtf_lib: &mut (VtfLib, VtfGuard),
        build: impl FnMut(LoadedVmt) -> Result<D, MaterialLoadError>,
    ) -> Result<(MaterialInfo, Option<LoadedMaterialAndTextures<D>>), (PathBuf, MaterialLoadError)>
    where
        D: Send + 'static,
    {
        if let Some(MaterialStatus::Done(info_result)) = self
            .material_cache
            .lock()
            .expect("the mutex shouldn't be poisoned")
            .get(&material_path)
        {
            return info_result
                .clone()
                .map_or_else(|e| Err((material_path, e)), |i| Ok((i, None)));
        }

        let result = self
            .load_material_inner(&material_path, file_system, vtf_lib, build)
            .map_err(|e| (material_path.clone(), e));

        let info_result = match &result {
            Ok(LoadedMaterialAndTextures {
                loaded_material: LoadedMaterial { info, .. },
                ..
            }) => Ok(info.clone()),
            Err((_, err)) => Err(err.clone()),
        };

        self.material_cache
            .lock()
            .expect("the mutex shouldn't be poisoned")
            .insert(material_path, MaterialStatus::Done(info_result));
        self.material_condvar.notify_all();

        result.map(|b| (b.loaded_material.info.clone(), Some(b)))
    }

    fn load_material_inner<D>(
        &self,
        material_path: &PathBuf,
        file_system: &OpenFileSystem,
        vtf_lib: &mut (VtfLib, VtfGuard),
        mut build: impl FnMut(LoadedVmt) -> Result<D, MaterialLoadError>,
    ) -> Result<LoadedMaterialAndTextures<D>, MaterialLoadError>
    where
        D: Send + 'static,
    {
        let shader = get_shader(material_path, file_system)?;

        let mut loaded_textures = Vec::new();

        let info = get_material_info(
            material_path,
            &shader,
            self,
            vtf_lib,
            file_system,
            &mut loaded_textures,
        )?;
        let loaded_vmt = LoadedVmt {
            shader,
            loaded_textures: &mut loaded_textures,
            worker_state: self,
            vtf_lib,
            file_system,
            material_path,
            info: info.clone(),
        };
        let data = build(loaded_vmt)?;

        let loaded_material = LoadedMaterial {
            name: match material_path {
                PathBuf::Game(path) => path.clone(),
                PathBuf::Os(path) => path
                    .file_name()
                    .expect("file is already read, file_name should exist")
                    .to_string_lossy()
                    .into_owned()
                    .into(),
            },
            data,
            info,
        };

        Ok(LoadedMaterialAndTextures {
            loaded_material,
            loaded_textures,
        })
    }

    fn load_skybox(
        sky_path: &PathBuf,
        file_system: &OpenFileSystem,
        vtf_lib: &mut (VtfLib, VtfGuard),
    ) -> Result<SkyBox, MaterialLoadError> {
        let skybox_shaders: Vec<_> = CUBEMAP_SUFFIXES
            .iter()
            .map(|suffix| {
                let mut file_name = sky_path
                    .file_name()
                    .ok_or(MaterialLoadError::Custom("invalid sky filename"))?
                    .to_owned();
                file_name.push_str(suffix);

                let mut path = sky_path.clone();
                path.set_file_name(&file_name);
                get_shader(&path, file_system)
            })
            .try_collect()?;

        let parameters = &skybox_shaders[0].parameters;
        let hdr = parameters.contains_key("$hdrbasetexture".as_uncased())
            || parameters.contains_key("$hdrcompressedtexture".as_uncased());

        let texture_paths: Vec<_> = skybox_shaders
            .iter()
            .map(|shader| {
                let current_hdr;
                let texture_path;

                if let Some(value) = shader
                    .extract_param::<TexturePath>("$hdrbasetexture", sky_path)
                    .or_else(|| shader.extract_param("$hdrcompressedtexture", sky_path))
                {
                    current_hdr = true;
                    texture_path = value.absolute_path();
                } else if let Some(value) =
                    shader.extract_param::<TexturePath>("$basetexture", sky_path)
                {
                    current_hdr = false;
                    texture_path = value.absolute_path();
                } else {
                    return Err(MaterialLoadError::Custom(
                        "skybox material has no texture specified",
                    ));
                }

                if current_hdr != hdr {
                    return Err(MaterialLoadError::Custom(
                        "skybox sides have inconsistent hdr/sdr format",
                    ));
                }

                Ok(texture_path)
            })
            .try_collect()?;

        let data = if hdr {
            let textures: Vec<_> = texture_paths
                .into_iter()
                .map(|path| {
                    let texture_path = Path::Game(&path);
                    let vtf = open_texture(vtf_lib, texture_path, file_system)?;
                    let (data, format, width, height) = vtf_data(&vtf)?;

                    let f32_data = match format {
                        vtflib::ImageFormat::Rgba32323232F => LayoutVerified::new_slice(data)
                            .expect("vtflib should return properly aligned images")
                            .into_slice()
                            .to_vec(),
                        vtflib::ImageFormat::Rgba16161616F => f16s_to_f32s(data),
                        vtflib::ImageFormat::Bgra8888 => decompress_hdr(data),
                        _ => {
                            let data =
                                VtfFile::convert_image_to_rgba8888(data, width, height, format)?;
                            data.into_iter().map(|b| f32::from(b) / 255.0).collect()
                        }
                    };

                    Ok(Rgba32FImage::from_raw(width, height, f32_data)
                        .expect("vtflib should return valid images"))
                })
                .collect::<Result<_, TextureLoadError>>()?;

            SkyBoxData::Hdr(textures.try_into().expect("vec should have correct length"))
        } else {
            let textures: Vec<_> = texture_paths
                .into_iter()
                .map(|path| {
                    let texture_path = Path::Game(&path);
                    let vtf = open_texture(vtf_lib, texture_path, file_system)?;
                    let (data, format, width, height) = vtf_data(&vtf)?;

                    let data = VtfFile::convert_image_to_rgba8888(data, width, height, format)?;

                    Ok(RgbaImage::from_raw(width, height, data)
                        .expect("vtflib should return valid images"))
                })
                .collect::<Result<_, TextureLoadError>>()?;

            SkyBoxData::Sdr(textures.try_into().expect("vec should have correct length"))
        };

        Ok(SkyBox {
            name: match sky_path {
                PathBuf::Game(path) => path.clone(),
                PathBuf::Os(path) => path
                    .file_name()
                    .expect("file is already read, file_name should exist")
                    .to_string_lossy()
                    .into_owned()
                    .into(),
            },
            data,
        })
    }

    fn load_texture(
        &self,
        texture_path: PathBuf,
        file_system: &OpenFileSystem,
        vtf_lib: &mut (VtfLib, VtfGuard),
    ) -> Result<(TextureInfo, Option<LoadedTexture>), TextureLoadError> {
        if let Some(info_result) = self
            .texture_cache
            .lock()
            .expect("the mutex shouldn't be poisoned")
            .get(&texture_path)
        {
            return info_result.clone().map(|i| (i, None));
        }
        let loaded_result = LoadedTexture::load(texture_path.clone(), file_system, vtf_lib);
        self.texture_cache
            .lock()
            .expect("the mutex shouldn't be poisoned")
            .insert(texture_path, loaded_result.clone().map(|l| l.info));
        loaded_result.map(|l| (l.info.clone(), Some(l)))
    }
}

fn f16s_to_f32s(data: &[u8]) -> Vec<f32> {
    let floats: &[f16] = LayoutVerified::new_slice(data)
        .expect("vtflib should return properly aligned images")
        .into_slice();
    floats.iter().copied().map(f32::from).collect()
}

fn decompress_hdr(data: &[u8]) -> Vec<f32> {
    data.iter()
        .copied()
        .tuples()
        .flat_map(|(b, g, r, a)| {
            let a = f32::from(a);

            let r = f32::from(r) * a * 16.0 / 262_144.0;
            let g = f32::from(g) * a * 16.0 / 262_144.0;
            let b = f32::from(b) * a * 16.0 / 262_144.0;

            let a = 1.0;

            [r, g, b, a]
        })
        .collect()
}

fn get_shader(
    material_path: &PathBuf,
    file_system: &OpenFileSystem,
) -> Result<Shader, MaterialLoadError> {
    let material_path = material_path.ensure_extension("vmt");
    let material_contents = file_system
        .read(&material_path)
        .map_err(|err| MaterialLoadError::from_io(&err, &material_path))?;
    let material = Vmt::from_bytes(&material_contents)?;
    Ok(material.resolve_shader(file_system)?)
}

fn get_material_info(
    material_path: &PathBuf,
    shader: &Shader,
    worker_state: &WorkerState,
    vtf_lib: &mut (VtfLib, VtfGuard),
    file_system: &OpenFileSystem,
    loaded_textures: &mut Vec<LoadedTexture>,
) -> Result<MaterialInfo, MaterialLoadError> {
    // get material dimensions
    let (width, height) = match get_dimension_reference(shader) {
        Some(texture_path) => {
            let (info, texture) =
                worker_state.load_texture(PathBuf::Game(texture_path), file_system, vtf_lib)?;

            if let Some(texture) = texture {
                loaded_textures.push(texture);
            }

            (info.width, info.height)
        }
        None => (512, 512),
    };
    let no_draw = is_nodraw(material_path, shader);

    Ok(MaterialInfo::new(width as u16, height as u16, no_draw))
}

#[derive(Debug)]
pub struct LoadedVmt<'a> {
    worker_state: &'a WorkerState,
    vtf_lib: &'a mut (VtfLib, VtfGuard),
    file_system: &'a OpenFileSystem,
    loaded_textures: &'a mut Vec<LoadedTexture>,
    material_path: &'a PathBuf,
    shader: Shader,
    info: MaterialInfo,
}

impl<'a> LoadedVmt<'a> {
    /// Loads the given texture and returns the texture details.
    /// The texture itself will be automatically forwarded to the asset handler.
    /// `texture_path` should be absolute, ie. start with `materials/`.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the texture loading fails.
    pub fn load_texture(
        &mut self,
        texture_path: GamePathBuf,
    ) -> Result<TextureInfo, TextureLoadError> {
        let (info, texture) = self.worker_state.load_texture(
            PathBuf::Game(texture_path),
            self.file_system,
            self.vtf_lib,
        )?;
        if let Some(texture) = texture {
            self.loaded_textures.push(texture);
        }
        Ok(info)
    }

    /// Extracts a parameter.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the parameter is not valid.
    pub fn try_extract_param<T: ParameterType>(
        &self,
        parameter: &'static str,
    ) -> Result<Option<T>, ParameterError> {
        self.shader.try_extract_param(parameter)
    }

    /// Extracts a parameter, or returns the default value if the
    /// parameter doesn't exist.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the parameter is not valid.
    pub fn try_extract_param_or_default<T: ParameterType + Default>(
        &self,
        parameter: &'static str,
    ) -> Result<T, ParameterError> {
        self.shader.try_extract_param_or_default(parameter)
    }

    /// Extracts a parameter, or returns None if the
    /// parameter doesn't exist. Returns None and logs a warning if the
    /// parameter is invalid.
    #[must_use]
    pub fn extract_param<T: ParameterType>(&self, parameter: &'static str) -> Option<T> {
        self.shader.extract_param(parameter, self.material_path)
    }

    /// Extracts a parameter, or returns the default value if the
    /// parameter doesn't exist. Returns the default value and logs a warning if the
    /// parameter is invalid.
    #[must_use]
    pub fn extract_param_or_default<T: ParameterType + Default>(
        &self,
        parameter: &'static str,
    ) -> T {
        self.shader
            .extract_param_or_default(parameter, self.material_path)
    }

    #[must_use]
    pub fn material_path(&self) -> &PathBuf {
        self.material_path
    }

    #[must_use]
    pub fn shader(&self) -> &Shader {
        &self.shader
    }

    #[must_use]
    pub fn info(&self) -> &MaterialInfo {
        &self.info
    }
}

#[derive(Debug, Clone)]
pub struct TextureInfo {
    width: u32,
    height: u32,
}

impl TextureInfo {
    #[must_use]
    pub fn width(&self) -> u32 {
        self.width
    }

    #[must_use]
    pub fn height(&self) -> u32 {
        self.height
    }
}

#[derive(Debug, Clone)]
pub struct LoadedTexture {
    pub name: GamePathBuf,
    pub info: TextureInfo,
    pub data: RgbaImage,
}

impl LoadedTexture {
    fn load(
        texture_path: PathBuf,
        file_system: &OpenFileSystem,
        vtf_lib: &mut (VtfLib, VtfGuard),
    ) -> Result<Self, TextureLoadError> {
        let vtf = open_texture(vtf_lib, Path::from(&texture_path), file_system)?;

        let name = match texture_path {
            PathBuf::Game(path) => path,
            PathBuf::Os(path) => path
                .file_name()
                .expect("file is already read, file_name should exist")
                .to_string_lossy()
                .into_owned()
                .into(),
        };

        Self::load_vtf(name, &vtf)
    }

    fn load_vtf(name: GamePathBuf, vtf: &BoundVtfFile) -> Result<Self, TextureLoadError> {
        let (source, format, width, height) = vtf_data(vtf)?;
        let data = VtfFile::convert_image_to_rgba8888(source, width, height, format)?;
        Ok(Self {
            name,
            info: TextureInfo { width, height },
            data: RgbaImage::from_raw(width, height, data)
                .expect("vtflib should return valid images"),
        })
    }

    #[must_use]
    pub fn info(&self) -> &TextureInfo {
        &self.info
    }

    #[must_use]
    pub fn data(&self) -> &RgbaImage {
        &self.data
    }
}

fn open_texture<'a>(
    vtf_lib: &'a mut (VtfLib, VtfGuard),
    texture_path: Path,
    file_system: &OpenFileSystem,
) -> Result<BoundVtfFile<'a, 'a>, TextureLoadError> {
    let (vtf_lib, guard) = vtf_lib;

    let vtf_bytes = file_system
        .read(&texture_path.ensure_extension("vtf"))
        .map_err(|err| TextureLoadError::from_io(&err, &texture_path))?;

    let mut vtf = vtf_lib.new_vtf_file().bind(guard);
    vtf.load(&vtf_bytes)?;

    Ok(vtf)
}

fn vtf_data<'a>(
    vtf: &'a BoundVtfFile,
) -> Result<(&'a [u8], vtflib::ImageFormat, u32, u32), TextureLoadError> {
    let data = vtf.data(0, 0, 0, 0).ok_or(vtflib::Error::ImageNotLoaded)?;
    let format = vtf.format().ok_or(vtflib::Error::InvalidFormat)?;
    let width = vtf.width();
    let height = vtf.height();

    Ok((data, format, width, height))
}
