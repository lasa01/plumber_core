use std::{collections::HashMap, convert::TryInto, fs::{File, self}, io::{self, Read, Seek, SeekFrom, Write}, path::{Path, PathBuf}, str};

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
    fn parse(bytes: &mut &[u8]) -> Result<Self, ReadError> {
        let entry: LayoutVerified<_, DirectoryEntry> = parse(bytes).ok_or(ReadError::EntryEof)?;
        let preload = entry.preload_bytes.get().into();
        if preload > bytes.len() {
            return Err(ReadError::PreloadEof);
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
pub enum ReadError {
    #[error("io error: {0}")]
    Io(#[from] io::Error),
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
    path: PathBuf,
    tree: HashMap<String, TreeEntry>,
    header_size: usize,
    tree_size: usize,
}

impl Directory {
    /// # Errors
    ///
    /// Returns `Err` if `path` can't be read or is not a valid vpk directory.
    pub fn read<P: AsRef<Path>>(path: P) -> Result<Self, ReadError> {
        let path = path.as_ref().to_owned();
        let file_content = fs::read(&path)?;
        let mut bytes = file_content.as_slice();
        let header: LayoutVerified<_, HeaderV1> =
            parse(&mut bytes).ok_or(ReadError::HeaderEof)?;
        if header.signature.get() != 0x55aa_1234 {
            return Err(ReadError::InvalidSignature);
        }
        let header_v2: Option<LayoutVerified<_, HeaderV2Ext>> = match header.version.get() {
            1 => None,
            2 => Some(parse(&mut bytes).ok_or(ReadError::HeaderEof)?),
            other => return Err(ReadError::UnsupportedVersion(other)),
        };
        let tree_len = header.tree_size.get() as usize;
        if bytes.len() < tree_len {
            return Err(ReadError::TreeTooSmall);
        }
        let before_tree = <&[u8]>::clone(&bytes);

        let mut tree = HashMap::new();

        loop {
            let extension = parse_nul_str(&mut bytes).ok_or(ReadError::InvalidString)?;
            if extension.is_empty() {
                break;
            }
            let (dot, extension) = if extension == " " {
                ("", "")
            } else {
                (".", extension)
            };
            loop {
                let path = parse_nul_str(&mut bytes).ok_or(ReadError::InvalidString)?;
                if path.is_empty() {
                    break;
                }
                let (path, separator) = if path == " " {
                    ("", "")
                } else {
                    (path, "/")
                };
                loop {
                    let mut filename = parse_nul_str(&mut bytes).ok_or(ReadError::InvalidString)?;
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

        if let Some(header_v2) = &header_v2 {
            // verify checksums
            let skip_to_checksums = header_v2.file_data_section_size.get() as usize;
            let archive_md5_len = header_v2.file_data_section_size.get() as usize;
            if bytes.len() < skip_to_checksums + archive_md5_len {
                return Err(ReadError::ChecksumEof)
            }
            bytes = &bytes[..skip_to_checksums];
            let archive_md5_bytes = {
                let (data, remaining) = bytes.split_at(archive_md5_len);
                bytes = remaining;
                data
            };
            let other_md5: LayoutVerified<_, OtherMd5Section> = parse(&mut bytes).ok_or(ReadError::ChecksumEof)?;

            let tree_bytes = &before_tree[..tree_len];

            if *md5::compute(archive_md5_bytes) != other_md5.archive_md5_section_checksum {
                return Err(ReadError::ChecksumMismatch);
            }
            if *md5::compute(tree_bytes) != other_md5.tree_checksum {
                return Err(ReadError::ChecksumMismatch);
            }
        }

        let header_size = if header_v2.is_none() {
            12
        } else {
            28
        };

        Ok(Self { path, tree, header_size, tree_size: tree_len })
    }

    pub fn open_file(&self, filename: String) -> io::Result<VpkFile> {
        let entry = self.tree.get(&filename).ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, filename))?;
        let file = if entry.entry_length == 0 {
            None
        } else {
            let mut file = match entry.archive_index {
                0x7fff => File::open(&self.path)?,
                index => {
                    let file_stem = self.path
                        .file_stem()
                        .unwrap()
                        .to_str()
                        .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "vpk directory filename is not utf8"))?;
                    let file_base = file_stem.strip_suffix("_dir").unwrap_or(file_stem);
                    File::open(self.path.with_file_name(format!("{}{:03}.vpk", file_base, index)))?
                }
            };
            if entry.archive_index == 0x7fff {
                file.seek(SeekFrom::Start((self.header_size + self.tree_size + entry.entry_offset as usize) as u64))?;
            } else {
                file.seek(SeekFrom::Start(entry.entry_offset as u64))?;
            };
            Some(file)
        };
        
        Ok(VpkFile {
            entry,
            file,
            cursor: 0,
        })
    }
}

pub struct VpkFile<'a> {
    entry: &'a TreeEntry,
    file: Option<File>,
    cursor: u64,
}

impl<'a> Read for VpkFile<'a> {
    fn read(&mut self, mut buf: &mut [u8]) -> io::Result<usize> {
        let preload_remaining = self.entry.preload_bytes.len() - self.cursor as usize - 1;
        if preload_remaining > 0 {
            let read_amount = buf.write(&self.entry.preload_bytes[self.cursor as usize..])?;
            self.cursor += read_amount as u64;
            return Ok(read_amount);
        } else {
            if let Some(file) = &mut self.file {
                let remaining = buf.len().min(self.entry.entry_length as usize - (self.cursor as usize - self.entry.preload_bytes.len() - 1));
                let read_amount = file.read(&mut buf[..remaining])?;
                self.cursor += read_amount as u64;
                return Ok(read_amount);
            } else {
                return Ok(0);
            }
        }
    }
}

impl<'a> Seek for VpkFile<'a> {
    fn seek(&mut self, pos: SeekFrom) -> io::Result<u64> {
        match pos {
            SeekFrom::Start(seek) => {
                if seek < self.entry.preload_bytes.len() as u64 {
                    self.cursor = seek;
                } else {
                    if let Some(file) = &mut self.file {
                        self.cursor = file.seek(SeekFrom::Start(seek - (self.entry.preload_bytes.len() as u64 - 1)))? + self.entry.preload_bytes.len() as u64 - 1;
                    } else {
                        self.cursor = self.entry.preload_bytes.len() as u64 - 1;
                    }
                }
            }
            SeekFrom::End(seek) => {
                
            }
            SeekFrom::Current(seek) => {
                let new_cursor = self.cursor as i64 + seek;
                let new_cursor = new_cursor.try_into().map_err(|_| io::Error::new(io::ErrorKind::InvalidInput, "tried to seek before start of file"))?;
                if new_cursor < self.entry.preload_bytes.len() as u64 {
                    if self.cursor > self.entry.preload_bytes.len() as u64 - 1 {
                        if let Some(file) = &mut self.file {
                            file.seek(SeekFrom::Start(0))?;
                        }
                    }
                    self.cursor = new_cursor;
                } else {
                    if let Some(file) = &mut self.file {
                        self.cursor = file.seek(SeekFrom::Start(new_cursor - (self.entry.preload_bytes.len() as u64 - 1)))? + self.entry.preload_bytes.len() as u64 - 1;
                    } else {
                        self.cursor = self.entry.preload_bytes.len() as u64 - 1;
                    }
                }
            }
        }
        Ok(self.cursor)
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
