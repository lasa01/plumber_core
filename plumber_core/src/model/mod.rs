mod binary_utils;
mod mdl;
mod vtx;
mod vvd;

pub mod loader;

use std::{
    collections::BTreeMap,
    fmt::{self, Display},
    io,
    mem::size_of,
    result,
};

use mdl::Mdl;
pub use mdl::{AnimationData, AnimationDescFlags, BoneAnimationData};
pub use vtx::Face;
use vtx::Vtx;
use vvd::Vvd;
pub use vvd::{BoneWeight, Vertex};

use itertools::Itertools;
use thiserror::Error;

use crate::fs::{GameFile, GamePathBuf, OpenFileSystem, Path, PathBuf};

#[derive(Debug, Clone, Error, Hash, PartialEq, Eq)]
pub enum Error {
    #[error("io error reading `{path}`: {error}")]
    Io { path: String, error: String },
    #[error("not a {ty} file: invalid signature `{signature}`")]
    InvalidSignature { ty: FileType, signature: String },
    #[error("unsupported {ty} version {version}")]
    UnsupportedVersion { ty: FileType, version: i32 },
    #[error("{0} checksum doesn't match mdl checksum")]
    ChecksumMismatch(FileType),
    #[error("{ty} corrupted: {error}")]
    Corrupted { ty: FileType, error: &'static str },
    #[error("{ty} {feature} unsupported")]
    Unsupported { ty: FileType, feature: &'static str },
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum FileType {
    Mdl,
    Vvd,
    Vtx,
}

pub type Result<T> = result::Result<T, Error>;

impl Display for FileType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            FileType::Mdl => "mdl",
            FileType::Vvd => "vvd",
            FileType::Vtx => "vtx",
        })
    }
}

impl Error {
    fn from_io(err: &io::Error, path: &impl ToString) -> Self {
        Self::Io {
            path: path.to_string(),
            error: err.to_string(),
        }
    }
}

const VTX_EXTENSIONS: &[&str] = &["dx90.vtx", "dx80.vtx", "sw.vtx", "vtx"];

fn find_vtx<'a>(
    mdl_path: Path,
    file_system: &'a OpenFileSystem,
) -> Result<(PathBuf, GameFile<'a>)> {
    for &extension in VTX_EXTENSIONS {
        let path = mdl_path.with_extension(extension);
        match file_system.open_file(&path) {
            Ok(file) => return Ok((path, file)),
            Err(err) => {
                if err.kind() == io::ErrorKind::NotFound {
                    continue;
                }
                return Err(Error::from_io(&err, &path));
            }
        }
    }
    Err(Error::Io {
        path: mdl_path.with_extension("*.vtx").to_string(),
        error: "could not find a supported vtx file".to_owned(),
    })
}

#[derive(Debug, Clone)]
pub struct Model {
    mdl: Mdl,
    vvd: Vvd,
    vtx: Vtx,
}

impl Model {
    /// # Errors
    ///
    /// Returns `Err` if reading the mdl file fails or if reading an associated vvd or vtx file fails.
    pub fn read<'a>(path: impl Into<Path<'a>>, file_system: &OpenFileSystem) -> Result<Self> {
        let path = path.into();
        let mdl_file = file_system
            .open_file(path)
            .map_err(|err| Error::from_io(&err, &path))?;
        let mdl = Mdl::read(mdl_file).map_err(|err| Error::from_io(&err, &path))?;

        let vvd_path = path.with_extension("vvd");
        let vvd_file = file_system
            .open_file(&vvd_path)
            .map_err(|err| Error::from_io(&err, &vvd_path))?;
        let vvd = Vvd::read(vvd_file).map_err(|err| Error::from_io(&err, &vvd_path))?;

        let (vtx_path, vtx_file) = find_vtx(path, file_system)?;
        let vtx = Vtx::read(vtx_file).map_err(|err| Error::from_io(&err, &vtx_path))?;

        Ok(Model { mdl, vvd, vtx })
    }

    /// # Errors
    ///
    /// Returns `Err` if a signature or header is invalid or a version is unsupported.
    pub fn verify(&self) -> Result<Verified> {
        self.mdl.check_signature()?;
        self.mdl.check_version()?;

        self.vvd.check_signature()?;
        self.vvd.check_version()?;

        self.vtx.check_version()?;

        let mdl_header = self.mdl.header()?;
        let vvd_header = self.vvd.header()?;
        let vtx_header = self.vtx.header()?;

        if vvd_header.checksum() != mdl_header.checksum() {
            return Err(Error::ChecksumMismatch(FileType::Vvd));
        }
        if vtx_header.checksum() != mdl_header.checksum() {
            return Err(Error::ChecksumMismatch(FileType::Vtx));
        }

        Ok(Verified {
            mdl_header,
            vvd_header,
            vtx_header,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Verified<'a> {
    mdl_header: mdl::HeaderRef<'a>,
    vvd_header: vvd::HeaderRef<'a>,
    vtx_header: vtx::HeaderRef<'a>,
}

impl<'a> Verified<'a> {
    #[must_use]
    pub fn is_static_prop(&self) -> bool {
        self.mdl_header
            .flags()
            .contains(mdl::HeaderFlags::STATIC_PROP)
    }

    /// # Errors
    ///
    /// Returns `Err` if reading the name fails.
    pub fn name(&self) -> Result<&str> {
        self.mdl_header.name()
    }

    /// # Errors
    ///
    /// Returns `Err` if reading the meshes fails.
    pub fn meshes(&self) -> Result<Vec<Mesh>> {
        let vertices = self.vvd_header.lod_vertices(0)?.ok_or(Error::Corrupted {
            ty: FileType::Vvd,
            error: "lod 0 doesn't exist",
        })?;

        let vtx_body_parts = self.vtx_header.iter_body_parts()?;
        let mdl_body_parts = self.mdl_header.iter_body_parts()?;

        let mut meshes = Vec::new();

        for (vtx_body_part, mdl_body_part) in vtx_body_parts.zip(mdl_body_parts) {
            let vtx_models = vtx_body_part.iter_models()?;
            let mdl_models = mdl_body_part.iter_models()?;

            let body_part_name = mdl_body_part.name()?;

            meshes.reserve(vtx_models.len());

            for (vtx_model, mdl_model) in vtx_models.zip(mdl_models) {
                let name = mdl_model.name()?;

                let vertex_offset: usize =
                    mdl_model
                        .vertex_offset
                        .try_into()
                        .map_err(|_| Error::Corrupted {
                            ty: FileType::Mdl,
                            error: "model vertex offset is negative",
                        })?;
                let vertex_count: usize =
                    mdl_model
                        .vertex_count
                        .try_into()
                        .map_err(|_| Error::Corrupted {
                            ty: FileType::Mdl,
                            error: "model vertex count is negative",
                        })?;

                if vertex_offset % size_of::<Vertex>() != 0 {
                    return Err(Error::Corrupted {
                        ty: FileType::Mdl,
                        error: "model vertex offset is misaligned",
                    });
                }

                let vertex_index = vertex_offset / size_of::<Vertex>();

                let model_vertices = vertices
                    .get(vertex_index..vertex_index + vertex_count)
                    .ok_or(Error::Corrupted {
                        ty: FileType::Mdl,
                        error: "model vertex offset out of bounds",
                    })?;

                let lods = vtx_model.lods()?;
                let lod_0 = if let Some(lod) = lods.get(0) {
                    lod
                } else {
                    continue;
                };

                let (vertice_indices, faces) = lod_0.merged_meshes(mdl_model)?;

                let vertices: Vec<_> = vertice_indices
                    .into_iter()
                    .map(|i| {
                        model_vertices.get(i).copied().ok_or(Error::Corrupted {
                            ty: FileType::Vtx,
                            error: "vertice index out of bounds",
                        })
                    })
                    .try_collect()?;

                meshes.push(Mesh {
                    body_part_name,
                    name,
                    vertices,
                    faces,
                });
            }
        }

        Ok(meshes)
    }

    /// # Errors
    ///
    /// Returns `Err` if a material path reading fails or a material isn't found.
    pub fn materials<'f>(
        &self,
        file_system: &'f OpenFileSystem,
    ) -> Result<impl Iterator<Item = Result<GamePathBuf>> + 'f>
    where
        'a: 'f,
    {
        let texture_paths = self.mdl_header.texture_paths()?;

        Ok(self
            .mdl_header
            .iter_textures()?
            .map(move |texture| find_material(texture, &texture_paths, file_system)))
    }

    /// # Errors
    ///
    /// Returns `Err` if reading the bones fails due to corrupted mdl.
    pub fn bones(&self) -> Result<Vec<Bone>> {
        self.mdl_header
            .iter_bones()?
            .map(|bone| {
                Ok(Bone {
                    name: bone.name()?,
                    surface_prop: bone.surface_prop()?,
                    parent_bone_index: bone.parent_bone_index.try_into().ok(),
                    position: bone.position,
                    rotation: bone.rotation,
                    pose_to_bone: bone.pose_to_bone,
                })
            })
            .try_collect()
    }

    /// # Errors
    ///
    /// Returns `Err` if reading the animations fails due to corrupted mdl.
    pub fn animations(&self) -> Result<impl Iterator<Item = Result<Animation>>> {
        Ok(self
            .mdl_header
            .iter_animation_descs()?
            .map(|animation_desc| {
                let flags = animation_desc.flags();
                let name = animation_desc.name()?;
                let fps = animation_desc.animation_desc.fps;

                if animation_desc.iter_movements()?.count() > 0 {
                    return Err(Error::Unsupported {
                        ty: FileType::Mdl,
                        feature: "animation movements",
                    });
                }

                let data = animation_desc.data()?;

                Ok(Animation {
                    name,
                    flags,
                    fps,
                    data,
                })
            }))
    }
}

fn find_material<'a>(
    texture: mdl::TextureRef,
    texture_paths: &[&str],
    file_system: &'a OpenFileSystem,
) -> Result<GamePathBuf> {
    let name = GamePathBuf::from(texture.name()?);

    for &path in texture_paths {
        let mut candidate = GamePathBuf::from("materials");
        candidate.push(GamePathBuf::from(path));
        candidate.push(&name);
        candidate.set_extension("vmt");

        match file_system.open_file(&candidate) {
            Ok(_) => return Ok(candidate),
            Err(err) => {
                if err.kind() != io::ErrorKind::NotFound {
                    return Err(Error::from_io(&err, &candidate));
                }
            }
        }
    }

    Err(Error::Io {
        path: name.with_extension("vmt").into_string(),
        error: "could not find the material in any material paths".to_owned(),
    })
}

#[derive(Debug, Clone)]
pub struct Mesh<'a> {
    pub body_part_name: &'a str,
    pub name: &'a str,
    pub vertices: Vec<Vertex>,
    pub faces: Vec<Face>,
}

#[derive(Debug, Clone, Copy)]
pub struct Bone<'a> {
    pub name: &'a str,
    pub surface_prop: Option<&'a str>,
    pub parent_bone_index: Option<usize>,
    pub position: [f32; 3],
    pub rotation: [f32; 3],
    pub pose_to_bone: [f32; 12],
}

#[derive(Debug, Clone)]
pub struct Animation<'a> {
    pub name: &'a str,
    pub flags: AnimationDescFlags,
    pub fps: f32,
    pub data: Option<BTreeMap<usize, BoneAnimationData>>,
}

#[cfg(test)]
mod tests {
    use crate::{
        fs::{DirEntryType, GamePath, OpenFileSystem, ReadDir},
        steam::Libraries,
    };

    use super::*;

    /// Fails if steam is not installed
    #[test]
    #[ignore]
    fn read_models() {
        let libraries = Libraries::discover().unwrap();
        for result in libraries.apps().source().filesystems() {
            match result {
                Ok(filesystem) => {
                    eprintln!("reading from filesystem: {}", filesystem.name);
                    let filesystem = filesystem.open().unwrap();
                    recurse(
                        filesystem.read_dir(GamePath::try_from_str("models").unwrap()),
                        &filesystem,
                    );
                }
                Err(err) => eprintln!("warning: failed filesystem discovery: {}", err),
            }
        }
    }

    fn recurse(readdir: ReadDir, file_system: &OpenFileSystem) {
        for entry in readdir.map(result::Result::unwrap) {
            let name = entry.name();
            match entry.entry_type() {
                DirEntryType::File => {
                    if is_mdl_file(name.as_str()) {
                        if let Err(err) = read_mdl(&entry, file_system) {
                            if let Error::Corrupted { .. } = err {
                                panic!("failed: {:?}", err);
                            } else {
                                // ignore other errors, probably not our fault
                                eprintln!("failed: {:?}", err);
                            }
                        }
                    }
                }
                DirEntryType::Directory => recurse(entry.read_dir(), file_system),
            }
        }
    }

    fn read_mdl(entry: &crate::fs::DirEntry, file_system: &OpenFileSystem) -> Result<()> {
        let model = Model::read(entry.path(), file_system)?;
        let verified = model.verify()?;
        eprintln!("reading `{}`", verified.name()?);

        let material_results = verified.materials(file_system)?.collect_vec();

        for mesh in verified.meshes()? {
            for face in &mesh.faces {
                assert!(face.material_index < material_results.len());

                for i in face.vertice_indices {
                    assert!(i < mesh.vertices.len());
                }
            }
        }

        for result in material_results {
            result?;
        }

        verified.bones()?;
        verified.animations()?.try_for_each(|r| r.map(|_| ()))?;
        Ok(())
    }

    fn is_mdl_file(filename: &str) -> bool {
        filename
            .rsplit('.')
            .next()
            .map(|ext| ext.eq_ignore_ascii_case("mdl"))
            == Some(true)
    }
}
