use std::{
    collections::HashMap,
    fs,
    io::{self, Read, Seek, SeekFrom, Write},
    path::{Path, PathBuf},
    str,
};

use byteorder::LE;
use crc::crc32;
use thiserror::Error;
use zerocopy::{
    byteorder::{U16, U32},
    FromBytes, LayoutVerified, Unaligned,
};

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
    total_offset: u64,
    entry_length: u32,
    preload_bytes: Vec<u8>,
}

impl TreeEntry {
    fn parse(bytes: &mut &[u8], base_offset: u64) -> Result<Self, DirectoryReadError> {
        let entry: LayoutVerified<_, DirectoryEntry> =
            parse(bytes).ok_or(DirectoryReadError::EntryEof)?;
        let preload = entry.preload_bytes.get().into();
        if preload > bytes.len() {
            return Err(DirectoryReadError::PreloadEof);
        }
        let preload_data = {
            let (data, remaining) = bytes.split_at(preload);
            *bytes = remaining;
            data
        };

        let archive_index = entry.archive_index.get();
        let entry_offset = entry.entry_offset.get().into();
        let total_offset = if archive_index == IN_DIRECTORY {
            base_offset + entry_offset
        } else {
            entry_offset
        };

        Ok(Self {
            crc: entry.crc.get(),
            archive_index,
            total_offset,
            entry_length: entry.entry_length.get(),
            preload_bytes: preload_data.to_vec(),
        })
    }
}

#[derive(Debug, Error)]
pub enum DirectoryReadError {
    #[error("io error: {0}")]
    Io(#[from] io::Error),
    #[error("encountered eof reading header")]
    HeaderEof,
    #[error("not a vpk (invalid signature)")]
    InvalidSignature,
    #[error("unsupported version {0}")]
    UnsupportedVersion(u32),
    #[error("tree size smaller than expected")]
    TreeTooSmall,
    #[error("invalid string in tree")]
    InvalidString,
    #[error("encountered eof reading a tree entry")]
    EntryEof,
    #[error("encountered eof reading an entry's preload data")]
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

fn parse<'a, T: Unaligned>(bytes: &mut &'a [u8]) -> Option<LayoutVerified<&'a [u8], T>> {
    let (verified, remaining) = LayoutVerified::new_unaligned_from_prefix(*bytes)?;
    *bytes = remaining;
    Some(verified)
}

const IN_DIRECTORY: u16 = 0x7fff;

pub struct Directory {
    path: PathBuf,
    file_base: String,
    tree: HashMap<String, TreeEntry>,
}

impl Directory {
    /// # Errors
    ///
    /// Returns `Err` if `path` doesn't have an utf8 filename, can't be read or is not a valid vpk directory file.
    pub fn read<P: AsRef<Path>>(file_path: P) -> Result<Self, DirectoryReadError> {
        let file_path = file_path.as_ref().to_owned();
        let file_content = fs::read(&file_path)?;
        let bytes = file_content.as_slice();
        Self::parse(file_path, bytes)
    }

    /// # Errors
    ///
    /// Returns `Err` if `path` doesn't have an utf8 filename or `bytes` is not a valid vpk directory file.
    pub fn parse(file_path: PathBuf, mut bytes: &[u8]) -> Result<Self, DirectoryReadError> {
        let file_stem = file_path
            .file_stem()
            .ok_or_else(|| {
                io::Error::new(io::ErrorKind::InvalidInput, "path doesn't have a file name")
            })?
            .to_str()
            .ok_or_else(|| {
                io::Error::new(io::ErrorKind::InvalidInput, "file name is not valid utf8")
            })?;
        let file_base = file_stem
            .strip_suffix("_dir")
            .unwrap_or(file_stem)
            .to_string();
        let header: LayoutVerified<_, HeaderV1> =
            parse(&mut bytes).ok_or(DirectoryReadError::HeaderEof)?;
        if header.signature.get() != 0x55aa_1234 {
            return Err(DirectoryReadError::InvalidSignature);
        }
        let header_v2: Option<LayoutVerified<_, HeaderV2Ext>> = match header.version.get() {
            1 => None,
            2 => Some(parse(&mut bytes).ok_or(DirectoryReadError::HeaderEof)?),
            other => return Err(DirectoryReadError::UnsupportedVersion(other)),
        };
        let tree_len = header.tree_size.get() as usize;
        if bytes.len() < tree_len {
            return Err(DirectoryReadError::TreeTooSmall);
        }
        let before_tree = <&[u8]>::clone(&bytes);

        let header_size = if header_v2.is_none() { 12 } else { 28 };
        let base_offset = header_size + tree_len as u64;
        let mut tree = HashMap::new();

        loop {
            let extension = parse_nul_str(&mut bytes).ok_or(DirectoryReadError::InvalidString)?;
            if extension.is_empty() {
                break;
            }
            let (dot, extension) = if extension == " " {
                ("", "")
            } else {
                (".", extension)
            };
            loop {
                let path = parse_nul_str(&mut bytes).ok_or(DirectoryReadError::InvalidString)?;
                if path.is_empty() {
                    break;
                }
                let (path, separator) = if path == " " { ("", "") } else { (path, "/") };
                loop {
                    let mut filename =
                        parse_nul_str(&mut bytes).ok_or(DirectoryReadError::InvalidString)?;
                    if filename.is_empty() {
                        break;
                    }
                    if filename == " " {
                        filename = "";
                    }

                    tree.insert(
                        format!("{}{}{}{}{}", path, separator, filename, dot, extension),
                        TreeEntry::parse(&mut bytes, base_offset)?,
                    );
                }
            }
        }

        if let Some(header_v2) = &header_v2 {
            // verify checksums
            let skip_to_checksums = header_v2.file_data_section_size.get() as usize;
            let archive_md5_len = header_v2.archive_md5_section_size.get() as usize;
            if bytes.len() < skip_to_checksums + archive_md5_len {
                return Err(DirectoryReadError::ChecksumEof);
            }
            bytes = &bytes[skip_to_checksums..];
            let archive_md5_bytes = {
                let (data, remaining) = bytes.split_at(archive_md5_len);
                bytes = remaining;
                data
            };
            let other_md5: LayoutVerified<_, OtherMd5Section> =
                parse(&mut bytes).ok_or(DirectoryReadError::ChecksumEof)?;

            let tree_bytes = &before_tree[..tree_len];

            if *md5::compute(archive_md5_bytes) != other_md5.archive_md5_section_checksum {
                return Err(DirectoryReadError::ChecksumMismatch);
            }
            if *md5::compute(tree_bytes) != other_md5.tree_checksum {
                return Err(DirectoryReadError::ChecksumMismatch);
            }
        }

        Ok(Self {
            path: file_path,
            file_base,
            tree,
        })
    }

    /// # Errors
    ///
    /// Returns `Err` if `file_name` is not in the directory or if the file can't be opened.
    pub fn open_file(&self, file_name: &str) -> io::Result<File> {
        let entry = self
            .tree
            .get(file_name)
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, file_name))?;
        let file = if entry.entry_length == 0 {
            None
        } else {
            let mut file =
                if entry.archive_index == IN_DIRECTORY {
                    fs::File::open(&self.path)?
                } else {
                    fs::File::open(self.path.with_file_name(format!(
                        "{}_{:03}.vpk",
                        self.file_base, entry.archive_index
                    )))?
                };
            file.seek(SeekFrom::Start(entry.total_offset))?;
            Some(file)
        };

        Ok(File {
            entry,
            file,
            cursor: 0,
        })
    }
}

pub struct File<'a> {
    entry: &'a TreeEntry,
    file: Option<fs::File>,
    cursor: i64,
}

impl<'a> File<'a> {
    #[must_use]
    pub fn len(&self) -> usize {
        self.entry.preload_bytes.len() + self.entry.entry_length as usize
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[must_use]
    pub fn crc32(&self) -> u32 {
        self.entry.crc
    }

    /// Reads the file to a `Vec<u8>` and verifies the CRC checksum.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the file read fails or the CRC checksums don't match.
    pub fn verified_read(&mut self) -> io::Result<Vec<u8>> {
        let mut buf = Vec::with_capacity(self.len());
        self.read_to_end(&mut buf)?;
        if crc32::checksum_ieee(buf.as_slice()) != self.crc32() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "file is corrupted (checksum mismatch)",
            ));
        }
        Ok(buf)
    }
}

impl<'a> Read for File<'a> {
    fn read(&mut self, mut buf: &mut [u8]) -> io::Result<usize> {
        let cursor = self.cursor as usize;
        let preload_len = self.entry.preload_bytes.len();
        let preload_remaining = preload_len.saturating_sub(cursor);
        if preload_remaining > 0 {
            let read_amount = buf.write(&self.entry.preload_bytes[cursor..])?;
            self.cursor += read_amount as i64;
            return Ok(read_amount);
        }
        if let Some(file) = &mut self.file {
            let remaining = buf
                .len()
                .min((self.entry.entry_length as usize).saturating_sub(cursor - preload_len));
            let read_amount = file.read(&mut buf[..remaining])?;
            self.cursor += read_amount as i64;
            return Ok(read_amount);
        }
        Ok(0)
    }
}

impl<'a> Seek for File<'a> {
    fn seek(&mut self, pos: SeekFrom) -> io::Result<u64> {
        let preload_len = self.entry.preload_bytes.len() as i64;
        let delta_cursor = match pos {
            SeekFrom::Start(seek) => seek as i64 - self.cursor,
            SeekFrom::End(seek) => (self.len() as i64 + seek - self.cursor),
            SeekFrom::Current(seek) => seek,
        };
        let new_cursor = self.cursor + delta_cursor;
        if new_cursor <= preload_len {
            if new_cursor < 0 {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "seeked before 0",
                ));
            }
            if self.cursor > preload_len {
                if let Some(file) = &mut self.file {
                    file.seek(SeekFrom::Start(self.entry.total_offset))?;
                }
            }
            self.cursor = new_cursor;
        } else if let Some(file) = &mut self.file {
            let seeked = file.seek(SeekFrom::Current(delta_cursor))?;
            self.cursor = seeked as i64 - self.entry.total_offset as i64 + preload_len;
        } else {
            self.cursor = preload_len;
        }
        Ok(self.cursor as u64)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_header() {
        let mut header = &[
            0x34_u8, 0x12, 0xaa, 0x55, // signature
            0x02, 0x00, 0x00, 0x00, // version
            0x00, 0x00, 0x00, 0x00, // tree size
            0x00, 0x00, 0x00, 0x00, // file data size
            0x00, 0x00, 0x00, 0x00, // archive md5 size
            0x30, 0x00, 0x00, 0x00, // other md5 size
            0x00, 0x00, 0x00, 0x00, // signature size
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
        assert_eq!(parse::<HeaderV1>(&mut header).unwrap().into_ref(), &parsed,);
        assert_eq!(
            parse::<HeaderV2Ext>(&mut header).unwrap().into_ref(),
            &parsed_ext,
        );
    }

    #[test]
    fn parse_tree() {
        let mut tree = &[
            0x76_u8, 0x6d, 0x74, 0x00, // extension
            0x6d, 0x61, 0x74, 0x65, 0x72, 0x69, 0x61, 0x6c, 0x73, 0x2f, 0x6d, 0x6f, 0x64, 0x65,
            0x6c, 0x73, 0x2f, 0x70, 0x72, 0x6f, 0x70, 0x73, 0x5f, 0x76, 0x65, 0x68, 0x69, 0x63,
            0x6c, 0x65, 0x73, 0x00, // filepath
            0x62, 0x31, 0x37, 0x00, // filename
            0xE5, 0x3e, 0x0e, 0x95, // crc
            0x00, 0x00, 0xFF, 0x7F, // preload bytes, archive index
            0x54, 0x3D, 0x3A, 0x02, // entry offset
            0x47, 0x00, 0x00, 0x00, // entry length
            0xFF, 0xFF, // terminator
            0x00, 0x00, 0x00, // tree end
        ][..];
        assert_eq!(parse_nul_str(&mut tree), Some("vmt"),);
        assert_eq!(
            parse_nul_str(&mut tree),
            Some("materials/models/props_vehicles"),
        );
        assert_eq!(parse_nul_str(&mut tree), Some("b17"),);
        assert_eq!(
            parse::<DirectoryEntry>(&mut tree).unwrap().into_ref(),
            &DirectoryEntry {
                crc: 2_500_738_789.into(),
                preload_bytes: 0.into(),
                archive_index: IN_DIRECTORY.into(),
                entry_offset: 37_371_220.into(),
                entry_length: 71.into(),
                terminator: 0xffff.into(),
            },
        );
    }

    #[test]
    fn file_preload() {
        let entry = TreeEntry {
            crc: 0x627E_60A3,
            archive_index: IN_DIRECTORY,
            total_offset: 0,
            entry_length: 0,
            preload_bytes: b"preload".to_vec(),
        };
        let mut file = File {
            entry: &entry,
            file: None,
            cursor: 0,
        };

        file.seek(SeekFrom::Start(1)).unwrap();
        let mut buf = String::new();
        file.read_to_string(&mut buf).unwrap();
        assert_eq!(&buf, "reload");

        file.seek(SeekFrom::Current(-4)).unwrap();
        let mut buf = String::new();
        file.read_to_string(&mut buf).unwrap();
        assert_eq!(&buf, "load");

        file.seek(SeekFrom::End(-5)).unwrap();
        let mut buf = String::new();
        file.read_to_string(&mut buf).unwrap();
        assert_eq!(&buf, "eload");

        file.seek(SeekFrom::End(5)).unwrap();
        let mut buf = String::new();
        file.read_to_string(&mut buf).unwrap();
        assert_eq!(&buf, "");

        file.seek(SeekFrom::Start(0)).unwrap();
        file.verified_read().unwrap();
    }

    #[test]
    fn file_full() {
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests")
            .join("test.txt");

        let entry = TreeEntry {
            crc: 0x38CB_F779,
            archive_index: IN_DIRECTORY,
            total_offset: 2,
            entry_length: 8,
            preload_bytes: b"preload".to_vec(),
        };
        let mut opened = fs::File::open(path).unwrap();
        opened.seek(SeekFrom::Start(2)).unwrap();
        let mut file = File {
            entry: &entry,
            file: Some(opened),
            cursor: 0,
        };

        file.seek(SeekFrom::Start(7)).unwrap();
        let mut buf = String::new();
        file.read_to_string(&mut buf).unwrap();
        assert_eq!(&buf, "st1test2");

        file.seek(SeekFrom::Current(2)).unwrap();
        let mut buf = String::new();
        file.read_to_string(&mut buf).unwrap();
        assert_eq!(&buf, "");

        file.seek(SeekFrom::Current(-7)).unwrap();
        let mut buf = String::new();
        file.read_to_string(&mut buf).unwrap();
        assert_eq!(&buf, "test2");

        file.seek(SeekFrom::End(-14)).unwrap();
        let mut buf = String::new();
        file.read_to_string(&mut buf).unwrap();
        assert_eq!(&buf, "reloadst1test2");

        file.seek(SeekFrom::Start(0)).unwrap();
        file.verified_read().unwrap();
    }
}
