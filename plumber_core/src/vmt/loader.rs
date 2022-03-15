use std::{
    collections::HashMap,
    fmt::{self, Debug, Formatter},
    io,
    sync::{Arc, Condvar, Mutex},
    thread,
    time::Duration,
};

use image::RgbaImage;
use thiserror::Error;
use uncased::AsUncased;
use vtflib::{BoundVtfFile, VtfFile, VtfGuard, VtfLib};

use crate::{
    fs::{GamePath, GamePathBuf, OpenFileSystem, PathBuf},
    vmt::Vmt,
};

use plumber_vdf as vdf;

use super::{Shader, ShaderResolveError};

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

fn get_dimension_reference(shader: &Shader) -> Option<&String> {
    DIMENSION_REFERENCE_TEXTURES
        .iter()
        .find_map(|parameter| shader.parameters.get(parameter.as_uncased()))
}

#[derive(Clone)]
pub struct LoadedMaterial<D: Send + Sync + 'static> {
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

#[derive(Debug)]
enum MaterialStatus {
    Loading,
    Done(Result<MaterialInfo, MaterialLoadError>),
}

#[derive(Debug, Clone)]
pub struct Loader {
    material_job_sender: crossbeam_channel::Sender<PathBuf>,
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

    pub fn load_material(&self, material_path: &PathBuf) {
        self.worker_state
            .send_material_job(material_path, &self.material_job_sender);
    }

    /// Block the thread until the worker thread has loaded a given material, and return the info.
    /// Blocks forever if the material isn't separately loaded using `load_material()`.
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
        material_path: &PathBuf,
    ) -> Result<MaterialInfo, MaterialLoadError> {
        self.worker_state.wait_for_material(material_path)
    }

    /// Block the thread until the worker thread has loaded a given material, and return the info.
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
        material_path: &PathBuf,
    ) -> Result<MaterialInfo, MaterialLoadError> {
        self.worker_state
            .send_material_job(material_path, &self.material_job_sender);
        self.worker_state.wait_for_material(material_path)
    }

    pub fn start_worker_thread<D>(
        &self,
        file_system: Arc<OpenFileSystem>,
        build: impl FnMut(LoadedVmt) -> Result<D, MaterialLoadError> + Send + 'static,
        mut handle: impl FnMut(Result<LoadedMaterial<D>, (PathBuf, MaterialLoadError)>) + Send + 'static,
    ) where
        D: Send + Sync + 'static,
    {
        let worker_state = self.worker_state.clone();

        thread::spawn(move || {
            for result in worker_state.load_materials(&file_system, build) {
                handle(result);
            }
        });
    }
}

impl Default for Loader {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
struct WorkerState {
    material_job_receiver: crossbeam_channel::Receiver<PathBuf>,
    material_cache: Mutex<HashMap<PathBuf, MaterialStatus>>,
    material_condvar: Condvar,
    texture_cache: Mutex<HashMap<GamePathBuf, Result<TextureInfo, TextureLoadError>>>,
}

impl WorkerState {
    #[must_use]
    fn new(material_job_receiver: crossbeam_channel::Receiver<PathBuf>) -> Self {
        Self {
            material_job_receiver,
            material_cache: Mutex::new(HashMap::new()),
            material_condvar: Condvar::new(),
            texture_cache: Mutex::new(HashMap::new()),
        }
    }

    fn send_material_job(&self, path: &PathBuf, sender: &crossbeam_channel::Sender<PathBuf>) {
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
            .send(path.clone())
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
                    if res.timed_out() {
                        panic!("any materials haven't been loaded in 30 seconds while waiting for a material");
                    }
                    guard = g;
                }
                Some(MaterialStatus::Done(result)) => return result.clone(),
            }
        }
    }

    fn load_materials<'a, D>(
        &'a self,
        file_system: &'a OpenFileSystem,
        mut build: impl FnMut(LoadedVmt) -> Result<D, MaterialLoadError> + 'a,
    ) -> impl Iterator<Item = Result<LoadedMaterial<D>, (PathBuf, MaterialLoadError)>> + 'a
    where
        D: Send + Sync + 'static,
    {
        let mut vtf_lib = VtfLib::initialize().expect("vtflib is already initialized");
        let material_paths = self.material_job_receiver.iter();

        material_paths.filter_map(move |material_path| {
            match self.load_material(material_path, file_system, &mut vtf_lib, &mut build) {
                Ok((_, Some(loaded))) => Some(Ok(loaded)),
                Ok((_, None)) => None,
                Err(err) => Some(Err(err)),
            }
        })
    }

    #[allow(clippy::type_complexity)]
    fn load_material<D>(
        &self,
        material_path: PathBuf,
        file_system: &OpenFileSystem,
        vtf_lib: &mut (VtfLib, VtfGuard),
        build: impl FnMut(LoadedVmt) -> Result<D, MaterialLoadError>,
    ) -> Result<(MaterialInfo, Option<LoadedMaterial<D>>), (PathBuf, MaterialLoadError)>
    where
        D: Send + Sync + 'static,
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
            Ok(LoadedMaterial { info, .. }) => Ok(info.clone()),
            Err((_, err)) => Err(err.clone()),
        };

        self.material_cache
            .lock()
            .expect("the mutex shouldn't be poisoned")
            .insert(material_path, MaterialStatus::Done(info_result));
        self.material_condvar.notify_all();

        result.map(|b| (b.info.clone(), Some(b)))
    }

    fn load_material_inner<D>(
        &self,
        material_path: &PathBuf,
        file_system: &OpenFileSystem,
        vtf_lib: &mut (VtfLib, VtfGuard),
        mut build: impl FnMut(LoadedVmt) -> Result<D, MaterialLoadError>,
    ) -> Result<LoadedMaterial<D>, MaterialLoadError>
    where
        D: Send + Sync + 'static,
    {
        let shader = get_shader(material_path, file_system)?;

        let mut loaded_vmt = LoadedVmt {
            shader,
            textures: Vec::new(),
            worker_state: self,
            vtf_lib,
            file_system,
            material_path,
        };
        let info = loaded_vmt.get_info()?;
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
            info,
            data,
        };

        Ok(loaded_material)
    }

    fn load_texture(
        &self,
        texture_path: GamePathBuf,
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
        let loaded_result = LoadedTexture::load(&texture_path, file_system, vtf_lib);
        self.texture_cache
            .lock()
            .expect("the mutex shouldn't be poisoned")
            .insert(texture_path, loaded_result.clone().map(|l| l.info));
        loaded_result.map(|l| (l.info.clone(), Some(l)))
    }
}

fn get_shader(
    material_path: &PathBuf,
    file_system: &OpenFileSystem,
) -> Result<Shader, MaterialLoadError> {
    let material_path = material_path.with_extension("vmt");
    let material_contents = file_system
        .read(&material_path)
        .map_err(|err| MaterialLoadError::from_io(&err, &material_path))?;
    let material = Vmt::from_bytes(&material_contents)?;
    Ok(material.resolve_shader(file_system)?)
}

#[derive(Debug, Clone)]
pub struct MaterialInfo {
    width: u32,
    height: u32,
    no_draw: bool,
}

impl MaterialInfo {
    #[must_use]
    pub fn new(width: u32, height: u32, no_draw: bool) -> Self {
        Self {
            width,
            height,
            no_draw,
        }
    }

    #[must_use]
    pub fn width(&self) -> u32 {
        self.width
    }

    #[must_use]
    pub fn height(&self) -> u32 {
        self.height
    }

    #[must_use]
    pub fn no_draw(&self) -> bool {
        self.no_draw
    }
}

impl Default for MaterialInfo {
    fn default() -> Self {
        Self {
            width: 512,
            height: 512,
            no_draw: false,
        }
    }
}

#[derive(Debug)]
pub struct LoadedVmt<'a> {
    worker_state: &'a WorkerState,
    vtf_lib: &'a mut (VtfLib, VtfGuard),
    file_system: &'a OpenFileSystem,
    material_path: &'a PathBuf,
    shader: Shader,
    textures: Vec<LoadedTexture>,
}

impl<'a> LoadedVmt<'a> {
    /// # Errors
    ///
    /// Returns `Err` if the texture loading fails.
    pub fn load_texture(
        &mut self,
        texture_path: GamePathBuf,
    ) -> Result<TextureInfo, TextureLoadError> {
        let (info, texture) =
            self.worker_state
                .load_texture(texture_path, self.file_system, self.vtf_lib)?;
        if let Some(texture) = texture {
            self.textures.push(texture);
        }
        Ok(info)
    }

    #[must_use]
    pub fn shader(&self) -> &Shader {
        &self.shader
    }

    #[must_use]
    pub fn textures(&self) -> &[LoadedTexture] {
        &self.textures
    }

    #[must_use]
    pub fn textures_mut(&mut self) -> &mut Vec<LoadedTexture> {
        &mut self.textures
    }

    fn get_info(&mut self) -> Result<MaterialInfo, MaterialLoadError> {
        // get material dimensions
        let (width, height) = match get_dimension_reference(&self.shader) {
            Some(texture_path) => {
                let texture_path = texture_path.clone().into();
                self.load_texture(texture_path)
                    .map(|info| (info.width, info.height))
            }
            None => Ok((512, 512)),
        }?;
        let no_draw = is_nodraw(self.material_path, &self.shader);

        Ok(MaterialInfo {
            width,
            height,
            no_draw,
        })
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
        texture_path: &GamePath,
        file_system: &OpenFileSystem,
        vtf_lib: &mut (VtfLib, VtfGuard),
    ) -> Result<Self, TextureLoadError> {
        let (vtf_lib, guard) = vtf_lib;
        let texture_path = GamePath::try_from_str("materials")
            .unwrap()
            .join(texture_path);
        let vtf_bytes = file_system
            .read(&texture_path.with_extension("vtf"))
            .map_err(|err| TextureLoadError::from_io(&err, &texture_path))?;
        let mut vtf = vtf_lib.new_vtf_file().bind(guard);
        vtf.load(&vtf_bytes)?;
        Self::load_vtf(texture_path, &vtf)
    }

    fn load_vtf(name: GamePathBuf, vtf: &BoundVtfFile) -> Result<Self, TextureLoadError> {
        let source = vtf.data(0, 0, 0, 0).ok_or(vtflib::Error::ImageNotLoaded)?;
        let format = vtf.format().ok_or(vtflib::Error::InvalidFormat)?;
        let width = vtf.width();
        let height = vtf.height();
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
