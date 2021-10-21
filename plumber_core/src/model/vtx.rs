use std::fmt;
use std::io;
use std::mem::size_of;

use byteorder::NativeEndian;
use itertools::Itertools;
use maligned::A4;
use zerocopy::{
    byteorder::{I16, I32, U16},
    FromBytes, LayoutVerified, Unaligned,
};

use crate::binary_utils::read_file_aligned;
use crate::fs::GameFile;

use super::{mdl, Error, FileType, Result};

#[derive(Debug, Clone, FromBytes)]
#[repr(C)]
struct Header {
    version: i32,

    vertex_cache_size: i32,
    max_bones_per_stip: u16,
    max_bones_per_tri: u16,
    max_bones_per_vertex: i32,

    checksum: i32,

    lod_count: i32,

    material_replacement_list_offset: i32,

    body_part_count: i32,
    body_part_offset: i32,
}

#[derive(Debug, Clone, FromBytes)]
#[repr(C)]
struct BodyPart {
    model_count: i32,
    model_offset: i32,
}

#[derive(Debug, Clone, FromBytes)]
#[repr(C)]
struct Model {
    lod_count: i32,
    lod_offset: i32,
}

#[derive(Debug, Clone, FromBytes)]
#[repr(C)]
struct ModelLod {
    mesh_count: i32,
    mesh_offset: i32,
    switch_point: f32,
}

#[derive(Debug, Clone, FromBytes, Unaligned)]
#[repr(C)]
struct Mesh {
    strip_group_count: I32<NativeEndian>,
    strip_group_offset: I32<NativeEndian>,
    flags: u8,
}
pub trait StripGroup: FromBytes + Unaligned {
    type Strip: Strip;

    fn vertex_offset(&self) -> i32;
    fn vertex_count(&self) -> i32;
    fn index_offset(&self) -> i32;
    fn index_count(&self) -> i32;
    fn strip_offset(&self) -> i32;
    fn strip_count(&self) -> i32;
}

pub trait Strip: FromBytes + Unaligned {
    fn vertex_count(&self) -> i32;
}

#[derive(Debug, Clone, FromBytes, Unaligned)]
#[repr(C)]
struct StripGroupNormal {
    vertex_count: I32<NativeEndian>,
    vertex_offset: I32<NativeEndian>,
    index_count: I32<NativeEndian>,
    index_offset: I32<NativeEndian>,
    strip_count: I32<NativeEndian>,
    strip_offset: I32<NativeEndian>,
    flags: u8,
}

impl StripGroup for StripGroupNormal {
    type Strip = StripNormal;

    fn vertex_offset(&self) -> i32 {
        self.vertex_offset.get()
    }

    fn vertex_count(&self) -> i32 {
        self.vertex_count.get()
    }

    fn index_offset(&self) -> i32 {
        self.index_offset.get()
    }

    fn index_count(&self) -> i32 {
        self.index_count.get()
    }

    fn strip_offset(&self) -> i32 {
        self.strip_offset.get()
    }

    fn strip_count(&self) -> i32 {
        self.strip_count.get()
    }
}

#[derive(Debug, Clone, FromBytes, Unaligned)]
#[repr(C)]
struct StripGroupAlternate {
    vertex_count: I32<NativeEndian>,
    vertex_offset: I32<NativeEndian>,
    index_count: I32<NativeEndian>,
    index_offset: I32<NativeEndian>,
    strip_count: I32<NativeEndian>,
    strip_offset: I32<NativeEndian>,
    flags: u8,
    topology_index_count: I32<NativeEndian>,
    topology_index_offset: I32<NativeEndian>,
}

impl StripGroup for StripGroupAlternate {
    type Strip = StripAlternate;

    fn vertex_offset(&self) -> i32 {
        self.vertex_offset.get()
    }

    fn vertex_count(&self) -> i32 {
        self.vertex_count.get()
    }

    fn index_offset(&self) -> i32 {
        self.index_offset.get()
    }

    fn index_count(&self) -> i32 {
        self.index_count.get()
    }

    fn strip_offset(&self) -> i32 {
        self.strip_offset.get()
    }

    fn strip_count(&self) -> i32 {
        self.strip_count.get()
    }
}

#[derive(Debug, Clone, FromBytes, Unaligned)]
#[repr(C)]
pub struct Vertex {
    bone_weight_indices: [u8; 3],
    bone_count: u8,
    original_mesh_vertex_index: U16<NativeEndian>,
    bone_ids: [u8; 3],
}

#[derive(Debug, Clone, FromBytes, Unaligned)]
#[repr(C)]
pub struct StripNormal {
    index_count: I32<NativeEndian>,
    index_offset: I32<NativeEndian>,

    vertex_count: I32<NativeEndian>,
    vertex_offset: I32<NativeEndian>,

    bone_count: I16<NativeEndian>,
    flags: u8,

    bone_state_change_count: I32<NativeEndian>,
    bone_state_change_offset: I32<NativeEndian>,
}

impl Strip for StripNormal {
    fn vertex_count(&self) -> i32 {
        self.vertex_count.get()
    }
}

#[derive(Debug, Clone, FromBytes, Unaligned)]
#[repr(C)]
struct StripAlternate {
    index_count: I32<NativeEndian>,
    index_offset: I32<NativeEndian>,

    vertex_count: I32<NativeEndian>,
    vertex_offset: I32<NativeEndian>,

    bone_count: I16<NativeEndian>,
    flags: u8,

    bone_state_change_count: I32<NativeEndian>,
    bone_state_change_offset: I32<NativeEndian>,

    unknown_1: I32<NativeEndian>,
    unknown_2: I32<NativeEndian>,
}

impl Strip for StripAlternate {
    fn vertex_count(&self) -> i32 {
        self.vertex_count.get()
    }
}

#[derive(Debug, Clone, FromBytes, Unaligned)]
#[repr(C)]
struct BoneStateChange {
    hardware_id: I32<NativeEndian>,
    new_bone_id: I32<NativeEndian>,
}

#[derive(Debug, Clone, FromBytes, Unaligned)]
#[repr(C)]
struct MaterialReplacementList {
    replacement_count: I32<NativeEndian>,
    replacement_offset: I32<NativeEndian>,
}

#[derive(Debug, Clone, FromBytes, Unaligned)]
#[repr(C)]
struct MaterialReplacement {
    material_index: I16<NativeEndian>,
    name_offset: I32<NativeEndian>,
}

#[derive(Clone)]
pub struct Vtx {
    bytes: Vec<u8>,
}

impl Vtx {
    pub fn read(file: GameFile) -> io::Result<Self> {
        let bytes = read_file_aligned::<A4>(file)?;
        Ok(Self { bytes })
    }

    pub fn version(&self) -> Result<i32> {
        if self.bytes.len() < 4 {
            return Err(Error::Corrupted {
                ty: FileType::Vtx,
                error: "eof reading version",
            });
        }
        Ok(i32::from_ne_bytes(self.bytes[0..4].try_into().unwrap()))
    }

    pub fn check_version(&self) -> Result<i32> {
        let version = self.version()?;

        if version == 7 {
            Ok(version)
        } else {
            Err(Error::UnsupportedVersion {
                ty: FileType::Vtx,
                version,
            })
        }
    }

    pub fn header(&self) -> Result<HeaderRef> {
        let header = LayoutVerified::<_, Header>::new_from_prefix(self.bytes.as_ref())
            .ok_or(Error::Corrupted {
                ty: FileType::Vtx,
                error: "eof reading header",
            })?
            .0
            .into_ref();

        Ok(HeaderRef {
            header,
            bytes: &self.bytes,
        })
    }
}

impl fmt::Debug for Vtx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Vtx").finish_non_exhaustive()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct HeaderRef<'a> {
    header: &'a Header,
    bytes: &'a [u8],
}

impl<'a> HeaderRef<'a> {
    pub fn checksum(&self) -> i32 {
        self.header.checksum
    }

    pub fn body_parts(&self) -> Result<BodyPartsRef> {
        let offset = self
            .header
            .body_part_offset
            .try_into()
            .map_err(|_| Error::Corrupted {
                ty: FileType::Vtx,
                error: "body part offset is negative",
            })?;
        let count = self
            .header
            .body_part_count
            .try_into()
            .map_err(|_| Error::Corrupted {
                ty: FileType::Vtx,
                error: "body part count is negative",
            })?;

        let body_parts = self
            .bytes
            .get(offset..)
            .and_then(|bytes| LayoutVerified::new_slice_from_prefix(bytes, count))
            .ok_or(Error::Corrupted {
                ty: FileType::Vtx,
                error: "body parts out of bounds or misaligned",
            })?
            .0
            .into_slice();

        Ok(BodyPartsRef {
            body_parts,
            offset,
            bytes: self.bytes,
        })
    }

    pub fn iter_body_parts(&self) -> Result<impl Iterator<Item = BodyPartRef> + ExactSizeIterator> {
        let body_parts = self.body_parts()?;
        Ok(body_parts
            .body_parts
            .iter()
            .enumerate()
            .map(move |(i, body_part)| BodyPartRef {
                body_part,
                offset: body_parts.offset + i * size_of::<BodyPart>(),
                bytes: body_parts.bytes,
            }))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BodyPartsRef<'a> {
    body_parts: &'a [BodyPart],
    offset: usize,
    bytes: &'a [u8],
}

#[derive(Debug, Clone, Copy)]
pub struct BodyPartRef<'a> {
    body_part: &'a BodyPart,
    offset: usize,
    bytes: &'a [u8],
}

impl<'a> BodyPartRef<'a> {
    pub fn models(&self) -> Result<ModelsRef> {
        let offset = (self.offset as isize + self.body_part.model_offset as isize) as usize;
        let count = self
            .body_part
            .model_count
            .try_into()
            .map_err(|_| Error::Corrupted {
                ty: FileType::Vtx,
                error: "body part models count is negative",
            })?;

        let models = self
            .bytes
            .get(offset..)
            .and_then(|bytes| LayoutVerified::new_slice_from_prefix(bytes, count))
            .ok_or(Error::Corrupted {
                ty: FileType::Vtx,
                error: "body part models out of bounds or misaligned",
            })?
            .0
            .into_slice();

        Ok(ModelsRef {
            models,
            offset,
            bytes: self.bytes,
        })
    }

    pub fn iter_models(&self) -> Result<impl Iterator<Item = ModelRef> + ExactSizeIterator> {
        let models = self.models()?;
        Ok(models
            .models
            .iter()
            .enumerate()
            .map(move |(i, model)| ModelRef {
                model,
                offset: models.offset + i * size_of::<Model>(),
                bytes: models.bytes,
            }))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ModelsRef<'a> {
    models: &'a [Model],
    offset: usize,
    bytes: &'a [u8],
}

#[derive(Debug, Clone, Copy)]
pub struct ModelRef<'a> {
    model: &'a Model,
    offset: usize,
    bytes: &'a [u8],
}

impl<'a> ModelRef<'a> {
    pub fn lods(&self) -> Result<LodsRef> {
        let offset = (self.offset as isize + self.model.lod_offset as isize) as usize;
        let count = self
            .model
            .lod_count
            .try_into()
            .map_err(|_| Error::Corrupted {
                ty: FileType::Vtx,
                error: "model lod count is negative",
            })?;

        let lods = self
            .bytes
            .get(offset..)
            .and_then(|bytes| LayoutVerified::new_slice_from_prefix(bytes, count))
            .ok_or(Error::Corrupted {
                ty: FileType::Vtx,
                error: "model lods out of bounds or misaligned",
            })?
            .0
            .into_slice();

        Ok(LodsRef {
            lods,
            offset,
            bytes: self.bytes,
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LodsRef<'a> {
    lods: &'a [ModelLod],
    offset: usize,
    bytes: &'a [u8],
}

impl<'a> LodsRef<'a> {
    pub fn get(&self, index: usize) -> Option<LodRef> {
        self.lods.get(index).map(|lod| LodRef {
            lod,
            offset: self.offset + index * size_of::<ModelLod>(),
            bytes: self.bytes,
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LodRef<'a> {
    lod: &'a ModelLod,
    offset: usize,
    bytes: &'a [u8],
}

impl<'a> LodRef<'a> {
    fn meshes(&self) -> Result<MeshesRef> {
        let offset = (self.offset as isize + self.lod.mesh_offset as isize) as usize;
        let count = self
            .lod
            .mesh_count
            .try_into()
            .map_err(|_| Error::Corrupted {
                ty: FileType::Vtx,
                error: "lod mesh count is negative",
            })?;

        let meshes = self
            .bytes
            .get(offset..)
            .and_then(|bytes| LayoutVerified::new_slice_unaligned_from_prefix(bytes, count))
            .ok_or(Error::Corrupted {
                ty: FileType::Vtx,
                error: "lod meshes out of bounds",
            })?
            .0
            .into_slice();

        Ok(MeshesRef {
            meshes,
            offset,
            bytes: self.bytes,
        })
    }

    fn iter_meshes(&self) -> Result<impl Iterator<Item = MeshRef> + ExactSizeIterator> {
        let meshes = self.meshes()?;
        Ok(meshes
            .meshes
            .iter()
            .enumerate()
            .map(move |(i, mesh)| MeshRef {
                mesh,
                offset: meshes.offset + i * size_of::<Mesh>(),
                bytes: meshes.bytes,
            }))
    }

    pub fn merged_meshes(&self, mdl_model: mdl::ModelRef) -> Result<(Vec<usize>, Vec<Face>)> {
        let mut vertice_indices = Vec::new();
        let mut faces = Vec::new();
        let mut index_offset = 0_usize;

        for (vtx_mesh, mdl_mesh) in self.iter_meshes()?.zip(mdl_model.iter_meshes()?) {
            let vertex_index_start: usize =
                mdl_mesh
                    .vertex_index_start
                    .try_into()
                    .map_err(|_| Error::Corrupted {
                        ty: FileType::Mdl,
                        error: "mesh vertex index start is negative",
                    })?;
            let material_index: usize =
                mdl_mesh
                    .material_index
                    .try_into()
                    .map_err(|_| Error::Corrupted {
                        ty: FileType::Mdl,
                        error: "mesh material index is negative",
                    })?;

            let (mesh_face_indices, mesh_vertices, mesh_index_offset) = vtx_mesh
                .merged_strip_groups::<StripGroupNormal>()
                .or_else(|_| vtx_mesh.merged_strip_groups::<StripGroupAlternate>())?;

            faces.extend(
                mesh_face_indices
                    .into_iter()
                    .tuples()
                    .map(|(i_1, i_2, i_3)| Face {
                        vertice_indices: [
                            i_1 + index_offset,
                            i_2 + index_offset,
                            i_3 + index_offset,
                        ],
                        material_index,
                    }),
            );
            index_offset += mesh_index_offset;

            vertice_indices.extend(mesh_vertices.into_iter().map(|i| i + vertex_index_start));
        }

        Ok((vertice_indices, faces))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MeshesRef<'a> {
    meshes: &'a [Mesh],
    offset: usize,
    bytes: &'a [u8],
}

#[derive(Debug, Clone, Copy)]
pub struct MeshRef<'a> {
    mesh: &'a Mesh,
    offset: usize,
    bytes: &'a [u8],
}

impl<'a> MeshRef<'a> {
    fn strip_groups<S: StripGroup>(&self) -> Result<StripGroupsRef<S>> {
        let offset = (self.offset as isize + self.mesh.strip_group_offset.get() as isize) as usize;
        let count = self
            .mesh
            .strip_group_count
            .get()
            .try_into()
            .map_err(|_| Error::Corrupted {
                ty: FileType::Vtx,
                error: "mesh strip group count is negative",
            })?;

        StripGroupsRef::new(self.bytes, offset, count)
    }

    fn iter_strip_groups<'s, S: StripGroup + Clone + 's>(
        &'s self,
    ) -> Result<impl Iterator<Item = StripGroupRef<'s, S>> + ExactSizeIterator + Clone> {
        let strip_groups_ref = self.strip_groups()?;
        let strip_groups = strip_groups_ref.strip_groups;
        Ok(strip_groups
            .iter()
            .enumerate()
            .map(move |(i, strip_group)| StripGroupRef {
                strip_group,
                offset: strip_groups_ref.offset + i * size_of::<S>(),
                bytes: strip_groups_ref.bytes,
            }))
    }

    fn merged_strip_groups<S: StripGroup + Clone>(
        &self,
    ) -> Result<(Vec<usize>, Vec<usize>, usize)> {
        let mut face_indices = Vec::new();
        let mut vertices = Vec::new();
        let mut index_offset: usize = 0;

        for strip_group in self.iter_strip_groups::<S>()? {
            face_indices.extend(
                strip_group
                    .indices()?
                    .iter()
                    .map(|&i| i.get() as usize + index_offset),
            );
            vertices.extend(
                strip_group
                    .vertices()?
                    .iter()
                    .map(|v| v.original_mesh_vertex_index.get() as usize),
            );
            index_offset += strip_group
                .strips()?
                .iter()
                .map(|s| s.vertex_count() as usize)
                .sum::<usize>();
        }

        Ok((face_indices, vertices, index_offset))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StripGroupsRef<'a, S> {
    strip_groups: &'a [S],
    offset: usize,
    bytes: &'a [u8],
}

impl<'a, S> StripGroupsRef<'a, S>
where
    S: StripGroup,
{
    fn new(bytes: &'a [u8], offset: usize, count: usize) -> Result<Self> {
        let strip_groups: &[S] = bytes
            .get(offset..)
            .and_then(|bytes| LayoutVerified::new_slice_unaligned_from_prefix(bytes, count))
            .ok_or(Error::Corrupted {
                ty: FileType::Vtx,
                error: "mesh strip groups out of bounds",
            })?
            .0
            .into_slice();

        // validate strip groups to check if the current format is correct
        for strip_group in strip_groups {
            if strip_group.index_count() % 3 != 0
                || strip_group.index_count() < 0
                || strip_group.vertex_count() < 0
                || strip_group.strip_count() < 0
                || (offset as isize + strip_group.index_offset() as isize) as usize >= bytes.len()
                || (offset as isize + strip_group.vertex_offset() as isize) as usize >= bytes.len()
                || (offset as isize + strip_group.vertex_offset() as isize) as usize >= bytes.len()
            {
                return Err(Error::Corrupted {
                    ty: FileType::Vtx,
                    error: "mesh strip groups are invalid",
                });
            }
        }

        Ok(StripGroupsRef {
            strip_groups,
            offset,
            bytes,
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StripGroupRef<'a, S> {
    strip_group: &'a S,
    offset: usize,
    bytes: &'a [u8],
}

impl<'a, S> StripGroupRef<'a, S>
where
    S: StripGroup,
{
    pub fn vertices(&self) -> Result<&[Vertex]> {
        let offset = (self.offset as isize + self.strip_group.vertex_offset() as isize) as usize;
        let count = self
            .strip_group
            .vertex_count()
            .try_into()
            .map_err(|_| Error::Corrupted {
                ty: FileType::Vtx,
                error: "strip group vertices count is negative",
            })?;

        self.bytes
            .get(offset..)
            .and_then(|bytes| LayoutVerified::new_slice_unaligned_from_prefix(bytes, count))
            .map(|(verified, _)| verified.into_slice())
            .ok_or(Error::Corrupted {
                ty: FileType::Vtx,
                error: "strip group vertices out of bounds",
            })
    }

    pub fn indices(&self) -> Result<&[U16<NativeEndian>]> {
        let offset = (self.offset as isize + self.strip_group.index_offset() as isize) as usize;
        let count = self
            .strip_group
            .index_count()
            .try_into()
            .map_err(|_| Error::Corrupted {
                ty: FileType::Vtx,
                error: "strip group indices count is negative",
            })?;

        self.bytes
            .get(offset..)
            .and_then(|bytes| LayoutVerified::new_slice_unaligned_from_prefix(bytes, count))
            .map(|(verified, _)| verified.into_slice())
            .ok_or(Error::Corrupted {
                ty: FileType::Vtx,
                error: "strip group indices out of bounds",
            })
    }

    pub fn strips(&self) -> Result<&[<S as StripGroup>::Strip]> {
        let offset = (self.offset as isize + self.strip_group.strip_offset() as isize) as usize;
        let count = self
            .strip_group
            .strip_count()
            .try_into()
            .map_err(|_| Error::Corrupted {
                ty: FileType::Vtx,
                error: "strip groups strips count is negative",
            })?;

        self.bytes
            .get(offset..)
            .and_then(|bytes| LayoutVerified::new_slice_unaligned_from_prefix(bytes, count))
            .map(|(verified, _)| verified.into_slice())
            .ok_or(Error::Corrupted {
                ty: FileType::Vtx,
                error: "strip group strips out of bounds",
            })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Face {
    pub vertice_indices: [usize; 3],
    pub material_index: usize,
}
