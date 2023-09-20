use std::fmt::{self, Debug, Formatter};

use plumber_asset_core::{Asset, AssetConfig, Context, Handler};
use plumber_asset_vtf::VtfErrorInner;
use plumber_fs::{GamePathBuf, OpenFileSystem, Path};
use plumber_uncased::AsUncased;
use plumber_vmt::TexturePath;

use half::f16;
use image::{Rgba32FImage, RgbaImage};
use itertools::Itertools;
use thiserror::Error;
use vtflib2::VtfFile;
use zerocopy::LayoutVerified;

use crate::{get_shader, VmtErrorInner};

#[derive(Debug, Clone, Copy)]
pub struct SkyBoxConfig;

impl<H: Handler<Asset<Self>>> AssetConfig<H> for SkyBoxConfig {
    type Input<'a> = GamePathBuf;
    type Output<'a> = SkyBox;
    type Error<'a> = SkyBoxError;

    fn process<'a>(
        self,
        mut input: GamePathBuf,
        context: &mut Context<H>,
    ) -> Result<Self::Output<'a>, SkyBoxError> {
        input.set_extension("");

        load_skybox(&input, context.fs()).map_err(|e| SkyBoxError::new(input, e))
    }
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

#[derive(Debug, Clone, Error, Hash, PartialEq, Eq)]
#[error("skybox `{path}`: {error}")]
pub struct SkyBoxError {
    pub path: GamePathBuf,
    pub error: VmtErrorInner,
}

impl SkyBoxError {
    pub fn new(path: GamePathBuf, error: VmtErrorInner) -> Self {
        Self { path, error }
    }
}

const CUBEMAP_SUFFIXES: [&str; 6] = ["lf", "rt", "up", "dn", "ft", "bk"];

fn load_skybox(sky_path: &GamePathBuf, fs: &OpenFileSystem) -> Result<SkyBox, VmtErrorInner> {
    let file_name = sky_path
        .file_name()
        .ok_or(VmtErrorInner::Custom("invalid sky filename"))?;

    let skybox_shaders: Vec<_> = CUBEMAP_SUFFIXES
        .iter()
        .map(|suffix| {
            let mut file_name = file_name.to_owned();
            file_name.push_str(suffix);

            let mut path = sky_path.clone();
            path.set_file_name(&file_name);

            get_shader(Path::from(&path), fs)
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
                .extract_param::<TexturePath>("$hdrbasetexture", sky_path.into())
                .or_else(|| shader.extract_param("$hdrcompressedtexture", sky_path.into()))
            {
                current_hdr = true;
                texture_path = value.absolute_path();
            } else if let Some(value) =
                shader.extract_param::<TexturePath>("$basetexture", sky_path.into())
            {
                current_hdr = false;
                texture_path = value.absolute_path();
            } else {
                return Err(VmtErrorInner::Custom(
                    "skybox material has no texture specified",
                ));
            }

            if current_hdr != hdr {
                return Err(VmtErrorInner::Custom(
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
                let texture_path = Path::Game(&path).ensure_extension("vtf");

                let vtf_bytes = fs
                    .read(&texture_path)
                    .map_err(|err| VtfErrorInner::from_io(&err, &texture_path))?;

                let mut vtf = VtfFile::new();
                vtf.load(&vtf_bytes)?;

                let data = vtf.data(0, 0, 0, 0).ok_or(vtflib2::Error::ImageNotLoaded)?;
                let format = vtf.format().ok_or(vtflib2::Error::InvalidFormat)?;
                let width = vtf.width();
                let height = vtf.height();

                let f32_data = match format {
                    vtflib2::ImageFormat::Rgba32323232F => LayoutVerified::new_slice(data)
                        .expect("vtflib should return properly aligned images")
                        .into_slice()
                        .to_vec(),
                    vtflib2::ImageFormat::Rgba16161616F => f16s_to_f32s(data),
                    vtflib2::ImageFormat::Bgra8888 => decompress_hdr(data),
                    _ => {
                        let data = VtfFile::convert_image_to_rgba8888(data, width, height, format)?;
                        data.into_iter().map(|b| f32::from(b) / 255.0).collect()
                    }
                };

                Ok(Rgba32FImage::from_raw(width, height, f32_data)
                    .expect("vtf should return valid images"))
            })
            .collect::<Result<_, VtfErrorInner>>()?;

        SkyBoxData::Hdr(textures.try_into().expect("vec should have correct length"))
    } else {
        let textures: Vec<_> = texture_paths
            .into_iter()
            .map(|path| {
                let texture_path = Path::Game(&path).ensure_extension("vtf");
                let bytes = fs
                    .read(&texture_path)
                    .map_err(|err| VtfErrorInner::from_io(&err, &texture_path))?;

                let mut vtf = VtfFile::new();
                vtf.load(&bytes)?;

                let data = vtf.data(0, 0, 0, 0).ok_or(vtflib2::Error::ImageNotLoaded)?;
                let format = vtf.format().ok_or(vtflib2::Error::InvalidFormat)?;
                let width = vtf.width();
                let height = vtf.height();

                let data = VtfFile::convert_image_to_rgba8888(data, width, height, format)?;

                Ok(RgbaImage::from_raw(width, height, data)
                    .expect("vtflib should return valid images"))
            })
            .collect::<Result<_, VtfErrorInner>>()?;

        SkyBoxData::Sdr(textures.try_into().expect("vec should have correct length"))
    };

    Ok(SkyBox {
        name: sky_path.clone(),
        data,
    })
}

fn f16s_to_f32s(data: &[u8]) -> Vec<f32> {
    let floats: &[f16] = LayoutVerified::new_slice(data)
        .expect("vtf should return properly aligned images")
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
