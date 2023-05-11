use std::io;

use plumber_asset_core::{Cached, CachedAssetConfig, Handler};
use plumber_asset_vtf::VtfErrorInner;
use plumber_fs::{GamePathBuf, OpenFileSystem, Path, PathBuf};
use plumber_uncased::AsUncased;
use plumber_vmt::{
    MaterialInfo, ParameterError, ParameterType, Shader, ShaderResolveError, TexturePath, Vmt,
};

use thiserror::Error;
use vtflib2::VtfFile;

pub mod skybox;

pub trait VmtConfig<H>:
    for<'a> CachedAssetConfig<
    H,
    Id = PathBuf,
    Input<'a> = PathBuf,
    CachedOutput = MaterialInfo,
    Error = VmtError,
>
where
    H: Handler<Cached<Self>>,
{
}

#[derive(Debug, Error, Clone, Hash, PartialEq, Eq)]
#[error("material `{path}`: {error}")]
pub struct VmtError {
    pub path: PathBuf,
    pub error: VmtErrorInner,
}

#[derive(Debug, Error, Clone, Hash, PartialEq, Eq)]
pub enum VmtErrorInner {
    #[error("io error reading `{path}`: {error}")]
    Io { path: String, error: String },
    #[error("error loading patch material: {0}")]
    Patch(#[from] ShaderResolveError),
    #[error("error deserializing material: {0}")]
    Deserialization(#[from] plumber_vdf::Error),
    #[error("error loading texture: {0}")]
    Texture(#[from] VtfErrorInner),
    #[error("error reading parameters: {0}")]
    Parameter(#[from] ParameterError),
    #[error("{0}")]
    Custom(&'static str),
}

impl VmtErrorInner {
    fn from_io(err: &io::Error, path: &impl ToString) -> Self {
        Self::Io {
            path: path.to_string(),
            error: err.to_string(),
        }
    }
}

#[derive(Debug)]
pub struct VmtHelper<'a> {
    material_path: &'a PathBuf,
    shader: Shader,
}

impl<'a> VmtHelper<'a> {
    pub fn new(material_path: &'a PathBuf, fs: &OpenFileSystem) -> Result<Self, VmtError> {
        Ok(Self {
            material_path,
            shader: get_shader(material_path.into(), fs).map_err(|error| VmtError {
                path: material_path.clone(),
                error,
            })?,
        })
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
        self.shader
            .extract_param(parameter, self.material_path.into())
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

    pub fn get_info(&self, fs: &OpenFileSystem) -> Result<MaterialInfo, VmtError> {
        self.get_info_inner(fs).map_err(|error| VmtError {
            path: self.material_path.clone(),
            error,
        })
    }

    fn get_info_inner(&self, fs: &OpenFileSystem) -> Result<MaterialInfo, VmtErrorInner> {
        // get material dimensions
        let (width, height) = match get_dimension_reference(&self.shader) {
            Some(texture_path) => {
                let texture_path = Path::Game(&texture_path);

                let vtf_bytes = fs
                    .read(&texture_path.ensure_extension("vtf"))
                    .map_err(|err| VtfErrorInner::from_io(&err, &texture_path))?;

                let mut vtf = VtfFile::new();
                vtf.load_header(&vtf_bytes).map_err(VtfErrorInner::from)?;

                (vtf.width(), vtf.height())
            }
            None => (512, 512),
        };

        let no_draw = is_nodraw(self.material_path, &self.shader);

        Ok(MaterialInfo::new(width, height, no_draw))
    }
}

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

fn is_nodraw(material_path: &PathBuf, shader: &Shader) -> bool {
    NODRAW_MATERIALS.iter().any(|m| material_path == *m)
        || NODRAW_PARAMS.iter().any(|p| {
            shader
                .parameters
                .get(p.as_uncased())
                .map_or(false, |v| v == "1")
        })
}

const DIMENSION_REFERENCE_TEXTURES: &[&str] = &["$basetexture", "$normalmap"];

fn get_dimension_reference(shader: &Shader) -> Option<GamePathBuf> {
    DIMENSION_REFERENCE_TEXTURES.iter().find_map(|parameter| {
        let path: TexturePath = shader.try_extract_param(parameter).ok().flatten()?;
        Some(path.absolute_path())
    })
}

pub fn get_shader(material_path: Path, fs: &OpenFileSystem) -> Result<Shader, VmtErrorInner> {
    let material_path = material_path.ensure_extension("vmt");

    let material_contents = fs
        .read(&material_path)
        .map_err(|err| VmtErrorInner::from_io(&err, &material_path))?;

    let material = Vmt::from_bytes(&material_contents)?;

    Ok(material.resolve_shader(fs)?)
}
