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

#[derive(Debug, Clone, Copy, FromBytes)]
#[repr(C)]
pub struct BoneWeight {
    pub weights: [f32; 3],
    pub bones: [u8; 3],
    pub bone_count: u8,
}

#[derive(Debug, Clone, Copy, FromBytes)]
#[repr(C)]
pub struct Vertex {
    pub bone_weight: BoneWeight,
    pub position: [f32; 3],
    pub normal: [f32; 3],
    pub tex_coord: [f32; 2],
}

#[derive(Debug, Clone, FromBytes)]
#[repr(C)]
struct Fixup {
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

    pub fn lod_vertices(&self, lod: usize) -> Result<Option<Vec<Vertex>>> {
        let vertices = self.vertices()?;
        let fixups = self.fixups()?;

        if fixups.is_empty() {
            if lod == 0 {
                return Ok(Some(vertices.to_owned()));
            }
            return Ok(None);
        }

        let lod_count = self
            .header
            .lod_count
            .try_into()
            .map_err(|_| Error::Corrupted {
                ty: FileType::Vvd,
                error: "lod count is negative",
            })?;

        if lod_count > self.header.lod_vertex_counts.len() {
            return Err(Error::Corrupted {
                ty: FileType::Vvd,
                error: "lod count is invalid",
            });
        }

        if lod >= lod_count {
            return Ok(None);
        }

        let vertices_count =
            self.header.lod_vertex_counts[lod]
                .try_into()
                .map_err(|_| Error::Corrupted {
                    ty: FileType::Vvd,
                    error: "lod vertices count is negative",
                })?;

        let mut lod_vertices = Vec::with_capacity(vertices_count);

        for fixup in fixups.iter().filter(|f| f.lod_index as usize >= lod) {
            let vertex_index = fixup
                .vertex_index
                .try_into()
                .map_err(|_| Error::Corrupted {
                    ty: FileType::Vvd,
                    error: "fixup vertex index is negative",
                })?;

            let vertex_count: usize =
                fixup
                    .vertex_count
                    .try_into()
                    .map_err(|_| Error::Corrupted {
                        ty: FileType::Vvd,
                        error: "fixup vertex count is negative",
                    })?;

            let fixup_vertices = vertices
                .get(vertex_index..vertex_index + vertex_count)
                .ok_or(Error::Corrupted {
                    ty: FileType::Vvd,
                    error: "fixup vertices out of bounds",
                })?;

            lod_vertices.extend_from_slice(fixup_vertices);
        }

        Ok(Some(lod_vertices))
    }

    fn vertices(&self) -> Result<&'a [Vertex]> {
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

    fn fixups(&self) -> Result<&'a [Fixup]> {
        let offset = self
            .header
            .fixup_table_offset
            .try_into()
            .map_err(|_| Error::Corrupted {
                ty: FileType::Vvd,
                error: "fixup offset is negative",
            })?;

        let count = self
            .header
            .fixup_count
            .try_into()
            .map_err(|_| Error::Corrupted {
                ty: FileType::Vvd,
                error: "fixup count is negative",
            })?;

        parse_slice(self.bytes, offset, count).ok_or(Error::Corrupted {
            ty: FileType::Vvd,
            error: "fixups out of bounds or misaligned",
        })
    }
}
