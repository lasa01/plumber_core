use std::{collections::HashMap, str};

use byteorder::LE;
use thiserror::Error;
use zerocopy::{
    byteorder::{U16, U32},
    FromBytes, LayoutVerified, Unaligned,
};
use crc::crc32;

#[derive(Debug, PartialEq, FromBytes, Unaligned)]
#[repr(C)]
struct HeaderV1 {
    signature: U32<LE>,
    version: U32<LE>,
    tree_size: U32<LE>,
}

#[derive(Debug, PartialEq, FromBytes, Unaligned)]
#[repr(C)]
struct HeaderV2Ext {
    file_data_section_size: U32<LE>,
    archive_md5_section_size: U32<LE>,
    other_md5_section_size: U32<LE>,
    signature_section_size: U32<LE>,
}

#[derive(Debug, PartialEq, FromBytes, Unaligned)]
#[repr(C)]
struct DirectoryEntry {
    crc: U32<LE>,
    preload_bytes: U16<LE>,
    archive_index: U16<LE>,
    entry_offset: U32<LE>,
    entry_length: U32<LE>,
    terminator: U16<LE>,
}

#[derive(Debug, PartialEq, FromBytes, Unaligned)]
#[repr(C)]
struct ArchiveMd5SectionEntry {
    archive_index: U32<LE>,
    starting_offset: U32<LE>,
    count: U32<LE>,
    md5_checksum: [u8; 16],
}

#[derive(Debug, PartialEq, FromBytes, Unaligned)]
#[repr(C)]
struct OtherMd5Section {
    tree_checksum: [u8; 16],
    archive_md5_section_checksum: [u8; 16],
    unknown: [u8; 16],
}

struct TreeEntry {
    crc: u32,
    archive_index: u16,
    entry_offset: u32,
    entry_length: u32,
    preload_bytes: Vec<u8>,
}

impl TreeEntry {
    fn parse(bytes: &mut &[u8]) -> Result<Self, ParseError> {
        let entry: LayoutVerified<_, DirectoryEntry> = parse(bytes).ok_or(ParseError::EntryEof)?;
        let preload = entry.preload_bytes.get().into();
        if preload > bytes.len() {
            return Err(ParseError::PreloadEof);
        }
        let preload_data = {
            let (data, remaining) = bytes.split_at(preload);
            *bytes = remaining;
            data
        };

        Ok(Self {
            crc: entry.crc.get(),
            archive_index: entry.archive_index.get(),
            entry_offset: entry.entry_offset.get(),
            entry_length: entry.entry_length.get(),
            preload_bytes: preload_data.to_vec(),
        })
    }
}

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("encountered eof reading header")]
    HeaderEof,
    #[error("not a vpk (invalid signature)")]
    InvalidSignature,
    #[error("unsupported version {0}")]
    UnsupportedVersion(u32),
    #[error("tree size smaller than specified")]
    TreeTooSmall,
    #[error("invalid string in tree")]
    InvalidString,
    #[error("encountered eof reading entry")]
    EntryEof,
    #[error("encountered eof reading preload")]
    PreloadEof,
    #[error("encountered eof reading checksums")]
    ChecksumEof,
    #[error("corrupted vpk (checksum mismatch)")]
    ChecksumMismatch,
}

fn parse_nul_str<'a>(bytes: &mut &'a [u8]) -> Option<&'a str> {
    let mut split = bytes.splitn(2, |&b| b == 0);
    let str_bytes = split.next()?;
    if str_bytes == b"\0" {
        return None;
    }
    let str = str::from_utf8(str_bytes).ok()?;
    *bytes = split.next().unwrap_or_default();
    Some(str)
}

#[inline]
fn parse<'a, T: Unaligned>(bytes: &mut &'a [u8]) -> Option<LayoutVerified<&'a [u8], T>> {
    let (verified, remaining) = LayoutVerified::new_unaligned_from_prefix(*bytes)?;
    *bytes = remaining;
    Some(verified)
}

pub struct Directory {
    tree: HashMap<String, TreeEntry>,
    tree_size: usize,
}

impl Directory {
    /// # Errors
    ///
    /// Returns `Err` if `bytes` is not a valid vpk directory.
    pub fn parse(mut bytes: &[u8]) -> Result<Self, ParseError> {
        let header: LayoutVerified<_, HeaderV1> =
            parse(&mut bytes).ok_or(ParseError::HeaderEof)?;
        if header.signature.get() != 0x55aa_1234 {
            return Err(ParseError::InvalidSignature);
        }
        let header_v2: Option<LayoutVerified<_, HeaderV2Ext>> = match header.version.get() {
            1 => None,
            2 => Some(parse(&mut bytes).ok_or(ParseError::HeaderEof)?),
            other => return Err(ParseError::UnsupportedVersion(other)),
        };
        let tree_len = header.tree_size.get() as usize;
        if bytes.len() < tree_len {
            return Err(ParseError::TreeTooSmall);
        }
        let before_tree = <&[u8]>::clone(&bytes);

        let mut tree = HashMap::new();

        loop {
            let extension = parse_nul_str(&mut bytes).ok_or(ParseError::InvalidString)?;
            if extension.is_empty() {
                break;
            }
            let (dot, extension) = if extension == " " {
                ("", "")
            } else {
                (".", extension)
            };
            loop {
                let path = parse_nul_str(&mut bytes).ok_or(ParseError::InvalidString)?;
                if path.is_empty() {
                    break;
                }
                let (path, separator) = if path == " " {
                    ("", "")
                } else {
                    (path, "/")
                };
                loop {
                    let mut filename = parse_nul_str(&mut bytes).ok_or(ParseError::InvalidString)?;
                    if filename.is_empty() {
                        break;
                    }
                    if filename == " " {
                        filename = "";
                    }

                    tree.insert(format!("{}{}{}{}{}", path, separator, filename, dot, extension), TreeEntry::parse(&mut bytes)?);
                }
            }
        }

        if let Some(header_v2) = header_v2 {
            // verify checksums
            let skip_to_checksums = header_v2.file_data_section_size.get() as usize;
            let archive_md5_len = header_v2.file_data_section_size.get() as usize;
            if bytes.len() < skip_to_checksums + archive_md5_len {
                return Err(ParseError::ChecksumEof)
            }
            bytes = &bytes[..skip_to_checksums];
            let archive_md5_bytes = {
                let (data, remaining) = bytes.split_at(archive_md5_len);
                bytes = remaining;
                data
            };
            let other_md5: LayoutVerified<_, OtherMd5Section> = parse(&mut bytes).ok_or(ParseError::ChecksumEof)?;

            let tree_bytes = &before_tree[..tree_len];

            if *md5::compute(archive_md5_bytes) != other_md5.archive_md5_section_checksum {
                return Err(ParseError::ChecksumMismatch);
            }
            if *md5::compute(tree_bytes) != other_md5.tree_checksum {
                return Err(ParseError::ChecksumMismatch);
            }
        }

        Ok(Self { tree, tree_size: tree_len })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vpk_parse_header() {
        let mut header = &[
            0x34u8, 0x12, 0xaa, 0x55,
            0x02, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x30, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
        ][..];
        let parsed = HeaderV1 {
            signature: 0x55aa_1234.into(),
            version: 2.into(),
            tree_size: 0.into(),
        };
        let parsed_ext = HeaderV2Ext {
            file_data_section_size: 0.into(),
            archive_md5_section_size: 0.into(),
            other_md5_section_size: 48.into(),
            signature_section_size: 0.into(),
        };
        assert_eq!(
            parse::<HeaderV1>(&mut header).unwrap().into_ref(),
            &parsed,
        );
        assert_eq!(
            parse::<HeaderV2Ext>(&mut header).unwrap().into_ref(),
            &parsed_ext,
        );
    }

    #[test]
    fn test_parse_tree() {
        let mut tree = &[
            0x76u8, 0x6d, 0x74, 0x00, // extension
            0x6d, 0x61, 0x74, 0x65, // filepath
            0x72, 0x69, 0x61, 0x6c,
            0x73, 0x2f, 0x6d, 0x6f,
            0x64, 0x65, 0x6c, 0x73,
            0x2f, 0x70, 0x72, 0x6f,
            0x70, 0x73, 0x5f, 0x76,
            0x65, 0x68, 0x69, 0x63,
            0x6c, 0x65, 0x73, 0x00,
            0x62, 0x31, 0x37, 0x00, // filename
            0xE5, 0x3e, 0x0e, 0x95, // crc
            0x00, 0x00, 0xFF, 0x7F, // preload bytes, archive index
            0x54, 0x3D, 0x3A, 0x02, // entry offset
            0x47, 0x00, 0x00, 0x00, // entry length
            0xFF, 0xFF,             // terminator
            0x00, 0x00,  0x00,      // tree end
        ][..];
        assert_eq!(
            parse_nul_str(&mut tree),
            Some("vmt"),
        );
        assert_eq!(
            parse_nul_str(&mut tree),
            Some("materials/models/props_vehicles"),
        );
        assert_eq!(
            parse_nul_str(&mut tree),
            Some("b17"),
        );
        assert_eq!(
            parse::<DirectoryEntry>(&mut tree).unwrap().into_ref(),
            &DirectoryEntry {
                crc: 2500738789.into(),
                preload_bytes: 0.into(),
                archive_index: 0x7fff.into(),
                entry_offset: 37371220.into(),
                entry_length: 71.into(),
                terminator: 0xffff.into(),
            },
        );
    }
}
