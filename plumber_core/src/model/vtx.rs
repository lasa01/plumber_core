use std::convert::TryInto;
use std::io;
use std::mem::size_of;

use maligned::A4;
use zerocopy::{FromBytes, LayoutVerified};

use crate::binary_utils::read_file_aligned;
use crate::fs::GameFile;

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

#[derive(Debug, Clone, FromBytes)]
#[repr(C)]
struct Mesh {
    strip_group_count: i32,
    strip_group_offset: i32,
    flags: u8,
}

#[derive(Debug, Clone, FromBytes)]
#[repr(C)]
struct StripGroup {
    vertex_count: i32,
    vertex_offset: i32,
    index_count: i32,
    index_offset: i32,
    strip_count: i32,
    strip_offset: i32,
    flags: u8,
}

#[derive(Debug, Clone, FromBytes)]
#[repr(C)]
struct StripGroupMdl49 {
    vertex_count: i32,
    vertex_offset: i32,
    index_count: i32,
    index_offset: i32,
    strip_count: i32,
    strip_offset: i32,
    flags: u8,
    topology_index_count: i32,
    topology_index_offset: i32,
}

#[derive(Debug, Clone, FromBytes)]
#[repr(C)]
pub struct Vertex {
    bone_weight_indices: [u8; 3],
    bone_count: u8,
    original_mesh_vertex_index: u16,
    bone_ids: [u8; 3],
}

#[derive(Debug, Clone, FromBytes)]
#[repr(C)]
pub struct Strip {
    index_count: i32,
    index_offset: i32,

    vertex_count: i32,
    vertex_offset: i32,

    bone_count: i16,
    flags: u8,

    bone_state_change_count: i32,
    bone_state_change_offset: i32,
}

#[derive(Debug, Clone, FromBytes)]
#[repr(C)]
struct StripMdl49 {
    index_count: i32,
    index_offset: i32,

    vertex_count: i32,
    vertex_offset: i32,

    bone_count: i16,
    flags: u8,

    bone_state_change_count: i32,
    bone_state_change_offset: i32,

    unknown_1: i32,
    unknown_2: i32,
}

#[derive(Debug, Clone, FromBytes)]
#[repr(C)]
struct BoneStateChange {
    hardware_id: i32,
    new_bone_id: i32,
}

#[derive(Debug, Clone, FromBytes)]
#[repr(C)]
struct MaterialReplacementList {
    replacement_count: i32,
    replacement_offset: i32,
}

#[derive(Debug, Clone, FromBytes)]
#[repr(C)]
struct MaterialReplacement {
    material_index: i16,
    name_offset: i32,
}

#[derive(Debug, Clone)]
pub struct Vtx {
    bytes: Vec<u8>,
}

impl Vtx {
    pub fn read(file: GameFile) -> io::Result<Self> {
        let bytes = read_file_aligned::<A4>(file)?;
        Ok(Self { bytes })
    }

    pub fn version(&self) -> Option<i32> {
        if self.bytes.len() < 4 {
            return None;
        }
        Some(i32::from_ne_bytes(self.bytes[0..4].try_into().unwrap()))
    }

    pub fn check_version(&self) -> Result<i32, Option<i32>> {
        let version = if let Some(v) = self.version() {
            v
        } else {
            return Err(None);
        };

        if version == 7 {
            Ok(version)
        } else {
            Err(Some(version))
        }
    }

    pub fn header(&self) -> Option<HeaderRef> {
        let header = LayoutVerified::<_, Header>::new_from_prefix(self.bytes.as_ref())?
            .0
            .into_ref();

        Some(HeaderRef {
            header,
            bytes: &self.bytes,
        })
    }
}

#[derive(Debug, Clone)]
pub struct HeaderRef<'a> {
    header: &'a Header,
    bytes: &'a [u8],
}

impl<'a> HeaderRef<'a> {
    pub fn checksum(&self) -> i32 {
        self.header.checksum
    }

    pub fn body_parts(&self) -> Option<BodyPartsRef> {
        let offset = self.header.body_part_offset.try_into().ok()?;
        let count = self.header.body_part_count.try_into().ok()?;
        let body_parts = LayoutVerified::new_slice_from_prefix(self.bytes.get(offset..)?, count)?
            .0
            .into_slice();

        Some(BodyPartsRef {
            body_parts,
            offset,
            bytes: self.bytes,
        })
    }

    pub fn iter_body_parts(&self) -> Option<impl Iterator<Item = BodyPartRef>> {
        let body_parts = self.body_parts()?;
        Some(
            body_parts
                .body_parts
                .into_iter()
                .enumerate()
                .map(move |(i, body_part)| BodyPartRef {
                    body_part,
                    offset: body_parts.offset + i * size_of::<BodyPart>(),
                    bytes: body_parts.bytes,
                }),
        )
    }
}

#[derive(Debug, Clone)]
pub struct BodyPartsRef<'a> {
    body_parts: &'a [BodyPart],
    offset: usize,
    bytes: &'a [u8],
}

impl<'a> BodyPartsRef<'a> {
    pub fn get(&self, index: usize) -> Option<BodyPartRef> {
        self.body_parts.get(index).map(|body_part| BodyPartRef {
            body_part,
            offset: self.offset + index * size_of::<BodyPart>(),
            bytes: self.bytes,
        })
    }
}

#[derive(Debug, Clone)]
pub struct BodyPartRef<'a> {
    body_part: &'a BodyPart,
    offset: usize,
    bytes: &'a [u8],
}

impl<'a> BodyPartRef<'a> {
    pub fn models(&self) -> Option<ModelsRef> {
        let offset = (self.offset as isize + self.body_part.model_offset as isize) as usize;
        let count = self.body_part.model_count.try_into().ok()?;
        let models = LayoutVerified::new_slice_from_prefix(self.bytes.get(offset..)?, count)?
            .0
            .into_slice();

        Some(ModelsRef {
            models,
            offset,
            bytes: self.bytes,
        })
    }

    pub fn iter_models(&self) -> Option<impl Iterator<Item = ModelRef>> {
        let models = self.models()?;
        Some(
            models
                .models
                .into_iter()
                .enumerate()
                .map(move |(i, model)| ModelRef {
                    model,
                    offset: models.offset + i * size_of::<Model>(),
                    bytes: models.bytes,
                }),
        )
    }
}

#[derive(Debug, Clone)]
pub struct ModelsRef<'a> {
    models: &'a [Model],
    offset: usize,
    bytes: &'a [u8],
}

impl<'a> ModelsRef<'a> {
    pub fn get(&self, index: usize) -> Option<ModelRef> {
        self.models.get(index).map(|model| ModelRef {
            model,
            offset: self.offset + index * size_of::<Model>(),
            bytes: self.bytes,
        })
    }
}

#[derive(Debug, Clone)]
pub struct ModelRef<'a> {
    model: &'a Model,
    offset: usize,
    bytes: &'a [u8],
}

impl<'a> ModelRef<'a> {
    pub fn lods(&self) -> Option<LodsRef> {
        let offset = (self.offset as isize + self.model.lod_offset as isize) as usize;
        let count = self.model.lod_count.try_into().ok()?;
        let lods = LayoutVerified::new_slice_from_prefix(self.bytes.get(offset..)?, count)?
            .0
            .into_slice();

        Some(LodsRef {
            lods,
            offset,
            bytes: self.bytes,
        })
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct LodRef<'a> {
    lod: &'a ModelLod,
    offset: usize,
    bytes: &'a [u8],
}

impl<'a> LodRef<'a> {
    pub fn meshes(&self) -> Option<MeshesRef> {
        let offset = (self.offset as isize + self.lod.mesh_offset as isize) as usize;
        let count = self.lod.mesh_count.try_into().ok()?;
        let meshes = LayoutVerified::new_slice_from_prefix(self.bytes.get(offset..)?, count)?
            .0
            .into_slice();

        Some(MeshesRef {
            meshes,
            offset,
            bytes: self.bytes,
        })
    }

    pub fn iter_meshes(&self) -> Option<impl Iterator<Item = MeshRef>> {
        let meshes = self.meshes()?;
        Some(
            meshes
                .meshes
                .into_iter()
                .enumerate()
                .map(move |(i, mesh)| MeshRef {
                    mesh,
                    offset: meshes.offset + i * size_of::<Mesh>(),
                    bytes: meshes.bytes,
                }),
        )
    }
}

#[derive(Debug, Clone)]
pub struct MeshesRef<'a> {
    meshes: &'a [Mesh],
    offset: usize,
    bytes: &'a [u8],
}

impl<'a> MeshesRef<'a> {
    pub fn get(&self, index: usize) -> Option<MeshRef> {
        self.meshes.get(index).map(|mesh| MeshRef {
            mesh,
            offset: self.offset + index * size_of::<Mesh>(),
            bytes: self.bytes,
        })
    }
}

#[derive(Debug, Clone)]
pub struct MeshRef<'a> {
    mesh: &'a Mesh,
    offset: usize,
    bytes: &'a [u8],
}

impl<'a> MeshRef<'a> {
    pub fn strip_groups(&self) -> Option<StripGroupsRef> {
        let offset = (self.offset as isize + self.mesh.strip_group_offset as isize) as usize;
        let count = self.mesh.strip_group_count.try_into().ok()?;
        let strip_groups = LayoutVerified::new_slice_from_prefix(self.bytes.get(offset..)?, count)?
            .0
            .into_slice();

        Some(StripGroupsRef {
            strip_groups,
            offset,
            bytes: self.bytes,
        })
    }

    pub fn iter_strip_groups(&self) -> Option<impl Iterator<Item = StripGroupRef>> {
        let strip_groups = self.strip_groups()?;
        Some(
            strip_groups
                .strip_groups
                .into_iter()
                .enumerate()
                .map(move |(i, strip_group)| StripGroupRef {
                    strip_group,
                    offset: strip_groups.offset + i * size_of::<StripGroup>(),
                    bytes: strip_groups.bytes,
                }),
        )
    }

    pub fn merged_strip_groups(&self) -> Option<(Vec<usize>, Vec<usize>, usize)> {
        let mut indices = Vec::new();
        let mut vertices = Vec::new();
        let mut offset: usize = 0;

        for strip_group in self.iter_strip_groups()? {
            indices.extend(strip_group.indices()?.iter().map(|&i| i as usize + offset));
            vertices.extend(
                strip_group
                    .vertices()?
                    .iter()
                    .map(|v| v.original_mesh_vertex_index as usize),
            );
            offset += strip_group
                .strips()?
                .iter()
                .map(|s| s.vertex_count as usize)
                .sum::<usize>();
        }

        Some((indices, vertices, offset))
    }
}

#[derive(Debug, Clone)]
pub struct StripGroupsRef<'a> {
    strip_groups: &'a [StripGroup],
    offset: usize,
    bytes: &'a [u8],
}

impl<'a> StripGroupsRef<'a> {
    pub fn get(&self, index: usize) -> Option<StripGroupRef> {
        self.strip_groups
            .get(index)
            .map(|strip_group| StripGroupRef {
                strip_group,
                offset: self.offset + index * size_of::<StripGroup>(),
                bytes: self.bytes,
            })
    }
}

#[derive(Debug, Clone)]
pub struct StripGroupRef<'a> {
    strip_group: &'a StripGroup,
    offset: usize,
    bytes: &'a [u8],
}

impl<'a> StripGroupRef<'a> {
    pub fn vertices(&self) -> Option<&[Vertex]> {
        let offset = (self.offset as isize + self.strip_group.vertex_offset as isize) as usize;
        let count = self.strip_group.vertex_count.try_into().ok()?;
        Some(
            LayoutVerified::new_slice_from_prefix(self.bytes.get(offset..)?, count)?
                .0
                .into_slice(),
        )
    }

    pub fn indices(&self) -> Option<&[u16]> {
        let offset = (self.offset as isize + self.strip_group.index_offset as isize) as usize;
        let count = self.strip_group.vertex_count.try_into().ok()?;
        Some(
            LayoutVerified::new_slice_from_prefix(self.bytes.get(offset..)?, count)?
                .0
                .into_slice(),
        )
    }

    pub fn strips(&self) -> Option<&[Strip]> {
        let offset = (self.offset as isize + self.strip_group.strip_offset as isize) as usize;
        let count = self.strip_group.strip_count.try_into().ok()?;
        Some(
            LayoutVerified::new_slice_from_prefix(self.bytes.get(offset..)?, count)?
                .0
                .into_slice(),
        )
    }
}
