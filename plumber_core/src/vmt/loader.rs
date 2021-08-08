use std::{
    collections::HashMap,
    fmt::Debug,
    io,
    sync::{Condvar, Mutex},
    time::Duration,
};

use image::RgbaImage;
use thiserror::Error;
use uncased::AsUncased;
use vtflib::{BoundVtfFile, VtfFile, VtfGuard, VtfLib};

use crate::{
    fs::{OpenFileSystem, Path, PathBuf},
    vmt::Vmt,
};

use plumber_vdf as vdf;

use super::{Shader, ShaderResolveError};

const DIMENSION_REFERENCE_TEXTURES: &[&str] = &["$basetexture", "$normalmap"];
const NODRAW_MATERIALS: &[&str] = &["tools/toolsareaportal", "tools/toolsoccluder"];
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

#[derive(Debug, Error, Clone)]
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
    fn from_io(err: &io::Error, path: &Path) -> Self {
        Self::Io {
            path: path.as_str().to_string(),
            error: err.to_string(),
        }
    }
}

#[derive(Debug, Error, Clone)]
pub enum TextureLoadError {
    #[error("io error reading `{path}`: {error}")]
    Io { path: String, error: String },
    #[error("error loading vtf file: {0}")]
    Vtf(#[from] vtflib::Error),
}

impl TextureLoadError {
    fn from_io(err: &io::Error, path: &Path) -> Self {
        Self::Io {
            path: path.as_str().to_string(),
            error: err.to_string(),
        }
    }
}

pub trait MaterialBuilder: Sized + Clone + Send + Sync + 'static {
    type Built: Debug + Send + Sync + 'static;

    /// # Errors
    ///
    /// Returns `Err` if the info getting fails.
    fn info(&self, vmt: &mut LoadedVmt<Self>) -> Result<MaterialInfo, MaterialLoadError> {
        // get material dimensions
        let (width, height) = match get_dimension_reference(&vmt.shader) {
            Some(texture_path) => {
                let texture_path = texture_path.clone().into();
                vmt.load_texture(texture_path)
                    .map(|info| (info.width, info.height))
            }
            None => Ok((512, 512)),
        }?;
        let no_draw = is_nodraw(vmt.material_path, &vmt.shader);

        Ok(MaterialInfo {
            width,
            height,
            no_draw,
        })
    }

    /// # Errors
    ///
    /// Returns `Err` if the building fails.
    fn build(&self, vmt: LoadedVmt<Self>) -> Result<Self::Built, MaterialLoadError>;
}

fn is_nodraw(material_path: &Path, shader: &Shader) -> bool {
    let no_draw = NODRAW_MATERIALS
        .iter()
        .any(|m| material_path.as_str() == *m)
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

/// The default material builder.
/// Does nothing.
#[derive(Debug, Default, Clone)]
pub struct DefaultMaterialBuilder;

impl MaterialBuilder for DefaultMaterialBuilder {
    type Built = ();

    fn build(&self, _vmt: LoadedVmt<Self>) -> Result<Self::Built, MaterialLoadError> {
        Ok(())
    }
}

#[derive(Debug, Default)]
pub struct Loader<L> {
    material_loader: L,
    material_cache: Mutex<HashMap<PathBuf, Result<MaterialInfo, MaterialLoadError>>>,
    material_condvar: Condvar,
    texture_cache: Mutex<HashMap<PathBuf, Result<TextureInfo, TextureLoadError>>>,
}

impl<L> Loader<L>
where
    L: MaterialBuilder,
{
    #[must_use]
    pub fn new(material_loader: L) -> Self {
        Self {
            material_loader,
            material_cache: Mutex::new(HashMap::new()),
            material_condvar: Condvar::new(),
            texture_cache: Mutex::new(HashMap::new()),
        }
    }

    /// Block the thread until another thread has loaded a given material, and return the info.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the material loading fails or a load was already attempted and it failed.
    ///
    /// # Panics
    ///
    /// Panics after 30 seconds if any materials haven't been loaded to prevent deadlocks.
    pub fn wait_for_material(
        &self,
        material_path: impl AsRef<Path>,
    ) -> Result<MaterialInfo, MaterialLoadError> {
        let material_path = material_path.as_ref();
        let mut guard = self
            .material_cache
            .lock()
            .expect("the mutex shouldn't be poisoned");
        loop {
            match guard.get(material_path) {
                None => {
                    let (g, res) = self
                        .material_condvar
                        .wait_timeout(guard, Duration::from_secs(30))
                        .expect("the mutex shouldn't be poisoned");
                    if res.timed_out() {
                        panic!("any materials haven't been loaded in 30 seconds while waiting for a material");
                    }
                    guard = g;
                }
                Some(result) => return result.clone(),
            }
        }
    }

    /// Load materials from an iterator.
    ///
    /// # Panics
    ///
    /// Panics if vtflib is already initialized.
    pub fn load_materials<'a, I, O>(
        &'a self,
        material_paths: I,
        file_system: &'a OpenFileSystem,
    ) -> LoadMaterials<'a, O, L>
    where
        I: IntoIterator<Item = PathBuf, IntoIter = O>,
    {
        LoadMaterials {
            loader: self,
            file_system,
            vtf_lib: VtfLib::initialize().expect("vtflib is already initialized"),
            material_paths: material_paths.into_iter(),
        }
    }

    fn load_material(
        &self,
        material_path: PathBuf,
        file_system: &OpenFileSystem,
        vtf_lib: &mut (VtfLib, VtfGuard),
    ) -> Result<(MaterialInfo, Option<<L as MaterialBuilder>::Built>), MaterialLoadError> {
        if let Some(info_result) = self
            .material_cache
            .lock()
            .expect("the mutex shouldn't be poisoned")
            .get(&material_path)
        {
            return info_result.clone().map(|i| (i, None));
        }

        let result = self.load_material_inner(&material_path, file_system, vtf_lib);

        let info_result = match &result {
            Ok((info, _)) => Ok(info.clone()),
            Err(err) => Err(err.clone()),
        };

        self.material_cache
            .lock()
            .expect("the mutex shouldn't be poisoned")
            .insert(material_path, info_result);
        self.material_condvar.notify_all();

        result.map(|(i, b)| (i, Some(b)))
    }

    fn load_material_inner(
        &self,
        material_path: &PathBuf,
        file_system: &OpenFileSystem,
        vtf_lib: &mut (VtfLib, VtfGuard),
    ) -> Result<(MaterialInfo, <L as MaterialBuilder>::Built), MaterialLoadError> {
        let shader = get_shader(material_path, filesystem)?;

        let mut loaded_vmt = LoadedVmt {
            shader,
            textures: Vec::new(),
            loader: self,
            vtf_lib,
            file_system,
            material_path,
        };
        let info = self.material_loader.info(&mut loaded_vmt)?;
        let loaded_material = self.material_loader.build(loaded_vmt)?;

        Ok((info, loaded_material))
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

#[derive(Debug)]
pub struct LoadMaterials<'a, I, L> {
    loader: &'a Loader<L>,
    file_system: &'a OpenFileSystem,
    vtf_lib: (VtfLib, VtfGuard),
    material_paths: I,
}

impl<'a, I, L> Iterator for LoadMaterials<'a, I, L>
where
    I: Iterator<Item = PathBuf>,
    L: MaterialBuilder,
{
    type Item = Result<<L as MaterialBuilder>::Built, MaterialLoadError>;

    fn next(&mut self) -> Option<Self::Item> {
        for material_path in &mut self.material_paths {
            match self
                .loader
                .load_material(material_path, self.file_system, &mut self.vtf_lib)
            {
                Ok((_, Some(loaded))) => return Some(Ok(loaded)),
                Ok((_, None)) => continue,
                Err(err) => return Some(Err(err)),
            }
        }
        None
    }
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
pub struct LoadedVmt<'a, L> {
    loader: &'a Loader<L>,
    vtf_lib: &'a mut (VtfLib, VtfGuard),
    file_system: &'a OpenFileSystem,
    material_path: &'a Path,
    shader: Shader,
    textures: Vec<LoadedTexture>,
}

impl<'a, L> LoadedVmt<'a, L>
where
    L: MaterialBuilder,
{
    /// # Errors
    ///
    /// Returns `Err` if the texture loading fails.
    pub fn load_texture(&mut self, texture_path: PathBuf) -> Result<TextureInfo, TextureLoadError> {
        let (info, texture) =
            self.loader
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
    info: TextureInfo,
    data: RgbaImage,
}

impl LoadedTexture {
    fn load(
        texture_path: &Path,
        file_system: &OpenFileSystem,
        vtf_lib: &mut (VtfLib, VtfGuard),
    ) -> Result<Self, TextureLoadError> {
        let (vtf_lib, guard) = vtf_lib;
        let mut texture_path = Path::try_from_str("materials").unwrap().join(texture_path);
        texture_path.set_extension("vtf");
        let vtf_bytes = filesystem
            .read(&texture_path)
            .map_err(|err| TextureLoadError::from_io(&err, &texture_path))?;
        let mut vtf = vtf_lib.new_vtf_file().bind(guard);
        vtf.load(&vtf_bytes)?;
        Self::load_vtf(&vtf)
    }

    fn load_vtf(vtf: &BoundVtfFile) -> Result<Self, TextureLoadError> {
        let source = vtf.data(0, 0, 0, 0).ok_or(vtflib::Error::ImageNotLoaded)?;
        let format = vtf.format().ok_or(vtflib::Error::InvalidFormat)?;
        let width = vtf.width();
        let height = vtf.height();
        let data = VtfFile::convert_image_to_rgba8888(source, width, height, format)?;
        Ok(Self {
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
