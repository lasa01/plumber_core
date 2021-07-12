use std::convert::TryInto;
use std::io;

use maligned::A4;
use zerocopy::{FromBytes, LayoutVerified};

use crate::binary_utils::read_file_aligned;
use crate::fs::GameFile;

#[derive(Debug, Clone, FromBytes)]
#[repr(C)]
struct Header {
    id: [u8; 4],
    version: i32,
    checksum: i32,
    lod_count: i32,
    lod_vertex_counts: [i32; 8],
    fixup_count: i32,
    fixup_table_offset: i32,
    vertex_data_offset: i32,
    tangent_data_offset: i32,
}

#[derive(Debug, Clone, FromBytes)]
#[repr(C)]
pub struct BoneWeight {
    weights: [f32; 3],
    bones: [u8; 3],
    bone_count: u8,
}

#[derive(Debug, Clone, FromBytes)]
#[repr(C)]
pub struct Vertex {
    bone_weight: BoneWeight,
    position: [f32; 3],
    normal: [f32; 3],
    tex_coord: [f32; 2],
}

#[derive(Debug, Clone, FromBytes)]
#[repr(C)]
pub struct Fixup {
    lod_index: i32,
    vertex_index: i32,
    vertex_count: i32,
}

#[derive(Debug, Clone)]
pub struct Vvd {
    bytes: Vec<u8>,
}

impl Vvd {
    pub fn read(file: GameFile) -> io::Result<Self> {
        let bytes = read_file_aligned::<A4>(file)?;
        Ok(Self { bytes })
    }

    pub fn check_signature(&self) -> Result<(), Option<&[u8]>> {
        if self.bytes.len() < 4 {
            return Err(None);
        }

        let signature = &self.bytes[0..4];

        if signature == b"IDSV" {
            Ok(())
        } else {
            Err(Some(signature))
        }
    }

    pub fn version(&self) -> Option<i32> {
        if self.bytes.len() < 8 {
            return None;
        }
        Some(i32::from_ne_bytes(self.bytes[4..8].try_into().unwrap()))
    }

    pub fn check_version(&self) -> Result<i32, Option<i32>> {
        let version = if let Some(v) = self.version() {
            v
        } else {
            return Err(None);
        };

        if version == 4 {
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

    pub fn vertices(&self) -> Option<&[Vertex]> {
        let offset = self.header.vertex_data_offset.try_into().ok()?;
        let count = self.header.lod_vertex_counts[0].try_into().ok()?;
        Some(
            LayoutVerified::new_slice_from_prefix(self.bytes.get(offset..)?, count)?
                .0
                .into_slice(),
        )
    }

    pub fn fixups(&self) -> Option<&[Fixup]> {
        let offset = self.header.fixup_count.try_into().ok()?;
        let count = self.header.fixup_table_offset.try_into().ok()?;
        Some(
            LayoutVerified::new_slice_from_prefix(self.bytes.get(offset..)?, count)?
                .0
                .into_slice(),
        )
    }
}
