use std::io;

use image::RgbaImage;
use plumber_asset_core::{Cached, CachedAssetConfig, Context, Handler};
use plumber_fs::PathBuf;
use thiserror::Error;
use vtflib2::VtfFile;

#[derive(Debug, Clone, Copy)]
pub struct VtfConfig;

impl<H> CachedAssetConfig<H> for VtfConfig
where
    H: Handler<Cached<Self>>,
{
    type Input<'a> = PathBuf;
    type Id = PathBuf;
    type Output<'a> = LoadedVtf;
    type CachedOutput = VtfInfo;
    type Error = VtfError;

    fn cache_id(self, input: &Self::Input<'_>) -> Self::Id {
        let mut id = input.clone();

        id.normalize_extension();
        id
    }

    fn process<'a>(
        self,
        mut input: Self::Input<'a>,
        context: &mut Context<H>,
    ) -> Result<(Self::Output<'a>, VtfInfo), VtfError> {
        input.normalize_extension();

        self.process_vtf(input.clone(), context)
            .map_err(|e| VtfError::new(input, e))
    }
}

impl VtfConfig {
    fn process_vtf<H>(
        self,
        input: PathBuf,
        context: &mut Context<H>,
    ) -> Result<(LoadedVtf, VtfInfo), VtfErrorInner>
    where
        H: Handler<Cached<Self>>,
    {
        let vtf_path = input.ensure_extension("vtf");

        let bytes = context
            .fs()
            .read(&vtf_path)
            .map_err(|err| VtfErrorInner::from_io(&err, &vtf_path))?;

        let mut vtf = VtfFile::new();
        vtf.load(&bytes)?;

        let data = vtf.data(0, 0, 0, 0).ok_or(vtflib2::Error::ImageNotLoaded)?;
        let format = vtf.format().ok_or(vtflib2::Error::InvalidFormat)?;
        let width = vtf.width();
        let height = vtf.height();

        let data = VtfFile::convert_image_to_rgba8888(data, width, height, format)?;

        let info = VtfInfo { width, height };
        let loaded = LoadedVtf {
            name: input,
            info: info.clone(),
            data: RgbaImage::from_raw(width, height, data)
                .expect("vtflib should return valid images"),
        };

        Ok((loaded, info))
    }
}

#[derive(Debug, Error, Clone, Hash, PartialEq, Eq)]
#[error("texture `{path}`: {error}")]
pub struct VtfError {
    path: PathBuf,
    error: VtfErrorInner,
}

impl VtfError {
    pub fn new(path: PathBuf, error: impl Into<VtfErrorInner>) -> Self {
        Self {
            path,
            error: error.into(),
        }
    }
}

#[derive(Debug, Error, Clone, Hash, PartialEq, Eq)]
pub enum VtfErrorInner {
    #[error("io error reading `{path}`: {error}")]
    Io { path: String, error: String },
    #[error("error loading vtf: {0}")]
    Vtf(#[from] vtflib2::Error),
}

impl VtfErrorInner {
    pub fn from_io(err: &io::Error, path: &impl ToString) -> Self {
        Self::Io {
            path: path.to_string(),
            error: err.to_string(),
        }
    }
}

#[derive(Debug, Clone)]
#[non_exhaustive]
pub struct VtfInfo {
    pub width: u32,
    pub height: u32,
}

#[derive(Debug, Clone)]
#[non_exhaustive]
pub struct LoadedVtf {
    pub name: PathBuf,
    pub info: VtfInfo,
    pub data: RgbaImage,
}
