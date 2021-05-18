use std::{collections::HashMap, io, sync::Mutex};

use image::RgbaImage;
use thiserror::Error;
use uncased::UncasedStr;
use vtflib::{BoundVtfFile, VtfFile, VtfGuard, VtfLib};

use crate::{
    fs::{OpenFileSystem, Path, PathBuf},
    vdf,
    vmt::Vmt,
};

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

#[derive(Debug, Error)]
pub enum MaterialLoadError {
    #[error("io error: {0}")]
    Io(#[from] io::Error),
    #[error("error loading patch material: {0}")]
    Patch(#[from] ShaderResolveError),
    #[error("error deserializing material: {0}")]
    Deserialization(#[from] vdf::Error),
    #[error("error loading texture: {0}")]
    Texture(#[from] TextureLoadError),
}

#[derive(Debug, Error)]
pub enum TextureLoadError {
    #[error("io error: {0}")]
    Io(#[from] io::Error),
    #[error("error loading vtf file: {0}")]
    Vtf(#[from] vtflib::Error),
}

#[derive(Debug, Default)]
pub struct Loader {
    material_cache: Mutex<HashMap<PathBuf, MaterialInfo>>,
    texture_cache: Mutex<HashMap<PathBuf, TextureInfo>>,
}

impl Loader {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn load_materials<'a, I, O>(
        &'a self,
        material_paths: I,
        filesystem: &'a OpenFileSystem,
        vtf_lib: &'a mut (VtfLib, VtfGuard),
    ) -> LoadMaterials<'a, O>
    where
        I: IntoIterator<Item = PathBuf, IntoIter = O>,
    {
        LoadMaterials {
            loader: self,
            filesystem,
            vtf_lib,
            material_paths: material_paths.into_iter(),
        }
    }

    fn load_material(
        &self,
        material_path: PathBuf,
        filesystem: &OpenFileSystem,
        vtf_lib: &mut (VtfLib, VtfGuard),
    ) -> Result<(MaterialInfo, Option<LoadedMaterial>), MaterialLoadError> {
        if let Some(info) = self.material_cache.lock().unwrap().get(&material_path) {
            return Ok((info.clone(), None));
        }

        let shader = get_shader(&material_path, filesystem)?;

        let mut textures = Vec::new();

        // get material dimensions
        let (width, height) = match get_dimension_reference(&shader) {
            Some(texture_path) => self
                .load_texture(texture_path.to_owned().into(), filesystem, vtf_lib)
                .map(|(info, texture)| {
                    if let Some(texture) = texture {
                        textures.push(texture);
                    }
                    (info.width, info.height)
                }),
            None => Ok((512, 512)),
        }?;

        let no_draw = is_nodraw(&material_path, &shader);

        let loaded_material = LoadedMaterial {
            info: MaterialInfo {
                width,
                height,
                no_draw,
            },
            shader,
            textures,
        };
        self.material_cache
            .lock()
            .unwrap()
            .insert(material_path, loaded_material.info.clone());

        Ok((loaded_material.info.clone(), Some(loaded_material)))
    }

    fn load_texture(
        &self,
        texture_path: PathBuf,
        filesystem: &OpenFileSystem,
        vtf_lib: &mut (VtfLib, VtfGuard),
    ) -> Result<(TextureInfo, Option<LoadedTexture>), TextureLoadError> {
        if let Some(info) = self.texture_cache.lock().unwrap().get(&texture_path) {
            return Ok((info.clone(), None));
        }
        let loaded = LoadedTexture::load(&texture_path, filesystem, vtf_lib)?;
        self.texture_cache
            .lock()
            .unwrap()
            .insert(texture_path, loaded.info.clone());
        Ok((loaded.info.clone(), Some(loaded)))
    }
}

fn get_dimension_reference(shader: &Shader) -> Option<&String> {
    DIMENSION_REFERENCE_TEXTURES
        .iter()
        .find_map(|parameter| shader.parameters.get(UncasedStr::new(parameter)))
}

fn get_shader(
    material_path: &PathBuf,
    filesystem: &OpenFileSystem,
) -> Result<Shader, MaterialLoadError> {
    let material_contents = filesystem.read_to_string(material_path.with_extension(".vmt"))?;
    let material = Vmt::from_str(&material_contents)?;
    Ok(material.resolve_shader(filesystem)?)
}

fn is_nodraw(material_path: &PathBuf, shader: &Shader) -> bool {
    let no_draw = NODRAW_MATERIALS
        .iter()
        .any(|m| material_path.as_str() == *m)
        || NODRAW_PARAMS.iter().any(|p| {
            shader
                .parameters
                .get(UncasedStr::new(p))
                .map_or(false, |v| v == "1")
        });
    no_draw
}

#[derive(Debug)]
pub struct LoadMaterials<'a, I> {
    loader: &'a Loader,
    filesystem: &'a OpenFileSystem,
    vtf_lib: &'a mut (VtfLib, VtfGuard),
    material_paths: I,
}

impl<'a, I> Iterator for LoadMaterials<'a, I>
where
    I: Iterator<Item = PathBuf>,
{
    type Item = Result<LoadedMaterial, MaterialLoadError>;

    fn next(&mut self) -> Option<Self::Item> {
        for material_path in &mut self.material_paths {
            match self
                .loader
                .load_material(material_path, self.filesystem, self.vtf_lib)
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

#[derive(Debug, Clone)]
pub struct LoadedMaterial {
    info: MaterialInfo,
    shader: Shader,
    textures: Vec<LoadedTexture>,
}

impl LoadedMaterial {
    #[must_use]
    pub fn info(&self) -> &MaterialInfo {
        &self.info
    }

    #[must_use]
    pub fn shader(&self) -> &Shader {
        &self.shader
    }

    #[must_use]
    pub fn textures(&self) -> &[LoadedTexture] {
        &self.textures
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
        filesystem: &OpenFileSystem,
        vtf_lib: &mut (VtfLib, VtfGuard),
    ) -> Result<Self, TextureLoadError> {
        let (vtf_lib, guard) = vtf_lib;
        let vtf_bytes = filesystem.read(texture_path.with_extension(".vmt"))?;
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
            data: RgbaImage::from_raw(width, height, data).unwrap(),
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
