use std::fmt;
use std::io;

use maligned::A4;
use zerocopy::FromBytes;

use crate::fs::GameFile;

use super::{
    binary_utils::{parse, parse_slice, read_file_aligned},
    Error, FileType, Result,
};

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
    pub weights: [f32; 3],
    pub bones: [u8; 3],
    pub bone_count: u8,
}

#[derive(Debug, Clone, FromBytes)]
#[repr(C)]
pub struct Vertex {
    pub bone_weight: BoneWeight,
    pub position: [f32; 3],
    pub normal: [f32; 3],
    pub tex_coord: [f32; 2],
}

#[derive(Debug, Clone, FromBytes)]
#[repr(C)]
pub struct Fixup {
    lod_index: i32,
    vertex_index: i32,
    vertex_count: i32,
}

#[derive(Clone)]
pub struct Vvd {
    bytes: Vec<u8>,
}

impl Vvd {
    pub fn read(file: GameFile) -> io::Result<Self> {
        let bytes = read_file_aligned::<A4>(file)?;
        Ok(Self { bytes })
    }

    pub fn check_signature(&self) -> Result<()> {
        let signature = self.bytes.get(0..4).ok_or(Error::Corrupted {
            ty: FileType::Mdl,
            error: "eof reading signature",
        })?;

        if signature == b"IDSV" {
            Ok(())
        } else {
            Err(Error::InvalidSignature {
                ty: FileType::Vvd,
                signature: String::from_utf8_lossy(signature).into_owned(),
            })
        }
    }

    pub fn version(&self) -> Result<i32> {
        if self.bytes.len() < 8 {
            return Err(Error::Corrupted {
                ty: FileType::Vvd,
                error: "eof reading version",
            });
        }
        Ok(i32::from_ne_bytes(self.bytes[4..8].try_into().unwrap()))
    }

    pub fn check_version(&self) -> Result<i32> {
        let version = self.version()?;

        if version == 4 {
            Ok(version)
        } else {
            Err(Error::UnsupportedVersion {
                ty: FileType::Vvd,
                version,
            })
        }
    }

    pub fn header(&self) -> Result<HeaderRef> {
        let header = parse(&self.bytes, 0).ok_or(Error::Corrupted {
            ty: FileType::Vvd,
            error: "eof reading header",
        })?;

        Ok(HeaderRef {
            header,
            bytes: &self.bytes,
        })
    }
}

impl fmt::Debug for Vvd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Vvd").finish_non_exhaustive()
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

    pub fn vertices(&self) -> Result<&[Vertex]> {
        let offset = self
            .header
            .vertex_data_offset
            .try_into()
            .map_err(|_| Error::Corrupted {
                ty: FileType::Vvd,
                error: "vertex offset is negative",
            })?;
        let count = self.header.lod_vertex_counts[0]
            .try_into()
            .map_err(|_| Error::Corrupted {
                ty: FileType::Vvd,
                error: "vertex count is negative",
            })?;

        parse_slice(self.bytes, offset, count).ok_or(Error::Corrupted {
            ty: FileType::Vvd,
            error: "vertices out of bounds or misaligned",
        })
    }
}
