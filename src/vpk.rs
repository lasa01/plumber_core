use std::{collections::HashMap, str};

use byteorder::LE;
use thiserror::Error;
use zerocopy::{
    byteorder::{U16, U32},
    FromBytes, LayoutVerified, Unaligned,
};
use crc::crc32;

#[derive(FromBytes, Unaligned)]
#[repr(C)]
struct HeaderV1 {
    signature: U32<LE>,
    version: U32<LE>,
    tree_size: U32<LE>,
}

#[derive(FromBytes, Unaligned)]
#[repr(C)]
struct HeaderV2Ext {
    file_data_section_size: U32<LE>,
    archive_md5_section_size: U32<LE>,
    other_md5_section_size: U32<LE>,
    signature_section_size: U32<LE>,
}

#[derive(FromBytes, Unaligned)]
#[repr(C)]
struct DirectoryEntry {
    crc: U32<LE>,
    preload_bytes: U16<LE>,
    archive_index: U16<LE>,
    entry_offset: U32<LE>,
    entry_length: U32<LE>,
    terminator: U16<LE>,
}

#[derive(FromBytes, Unaligned)]
#[repr(C)]
struct ArchiveMd5SectionEntry {
    archive_index: U32<LE>,
    starting_offset: U32<LE>,
    count: U32<LE>,
    md5_checksum: [u8; 16],
}

#[derive(FromBytes, Unaligned)]
#[repr(C)]
struct OtherMd5Section {
    tree_checksum: [u8; 16],
    archive_md5_section_checksum: [u8; 16],
    unknown: [u8; 16],
}

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("encountered eof reading header")]
    HeaderEof,
    #[error("invalid signature")]
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

pub struct Directory {
    tree: HashMap<String, TreeEntry>,
    tree_size: usize,
}

fn parse_nul_str<'a>(bytes: &mut &'a [u8]) -> Option<&'a str> {
    let mut split = bytes.splitn(1, |&b| b == 0);
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

        Ok(Self { tree, tree_size: header.tree_size.get() as usize })
    }
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
