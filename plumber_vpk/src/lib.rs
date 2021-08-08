#![deny(clippy::all, clippy::pedantic, clippy::cargo)]
// this is intentional
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_possible_wrap)]

mod path;

pub use path::{Path, PathBuf};

use std::{
    collections::{hash_map::Keys, HashMap},
    convert::TryInto,
    ffi::OsStr,
    fs,
    io::{self, Read, Seek, SeekFrom, Write},
    path::{Path as StdPath, PathBuf as StdPathBuf},
    slice::Iter,
    str,
};

use byteorder::LE;
use crc::crc32;
use encoding::{all::ISO_8859_1, DecoderTrap, Encoding};
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

/// An error that can happen during reading a [`Directory`].
#[derive(Debug, Error)]
pub enum DirectoryReadError {
    #[error("io error: {0}")]
    Io(#[from] io::Error),
    #[error("not a vpk: invalid signature")]
    InvalidSignature,
    #[error("unsupported version: {0}")]
    UnsupportedVersion(u32),
    #[error("corrupted vpk: {0}")]
    Corrupted(&'static str),
}

fn parse_nul_str<'a>(bytes: &mut &'a [u8]) -> Option<&'a [u8]> {
    if bytes.is_empty() {
        return None;
    }
    let mut split = bytes.splitn(2, |&b| b == 0);
    let str_bytes = split.next()?;
    *bytes = split.next().unwrap_or_default();
    Some(str_bytes)
}

fn parse<'a, T: Unaligned>(bytes: &mut &'a [u8]) -> Option<LayoutVerified<&'a [u8], T>> {
    let (verified, remaining) = LayoutVerified::new_unaligned_from_prefix(*bytes)?;
    *bytes = remaining;
    Some(verified)
}

const IN_DIRECTORY: u16 = 0x7fff;

#[derive(Debug)]
struct Entry {
    crc: u32,
    archive_index: u16,
    total_offset: u64,
    entry_length: u32,
    preload_bytes: Vec<u8>,
}

impl Entry {
    fn parse(bytes: &mut &[u8], base_offset: u64) -> Result<Self, DirectoryReadError> {
        let entry: LayoutVerified<_, DirectoryEntry> =
            parse(bytes).ok_or(DirectoryReadError::Corrupted("eof reading directory entry"))?;
        let preload = entry.preload_bytes.get().into();
        if preload > bytes.len() {
            return Err(DirectoryReadError::Corrupted("eof reading preload bytes"));
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

/// A directory or file inside a vpk archive.
#[derive(Debug, Clone, PartialEq)]
pub enum DirectoryContent {
    Directory(PathBuf),
    File(PathBuf),
}

impl PartialEq<Path> for DirectoryContent {
    fn eq(&self, other: &Path) -> bool {
        match self {
            DirectoryContent::Directory(name) | DirectoryContent::File(name) => name == other,
        }
    }
}

/// A vpk archive directory.
/// Actual file data can be stored inside the directory
/// or separately in accompanying archive files.
#[derive(Debug)]
pub struct Directory {
    path: StdPathBuf,
    file_base: String,
    files: HashMap<PathBuf, Entry>,
    directory_contents: HashMap<PathBuf, Vec<DirectoryContent>>,
}

impl Directory {
    /// Read a [`Directory`] from a file path.
    ///
    /// # Errors
    ///
    /// Returns `Err` if `file_path` doesn't have an utf8 filename, can't be read or is not a valid vpk directory file.
    pub fn read<P: AsRef<StdPath>>(file_path: P) -> Result<Self, DirectoryReadError> {
        let file_path = file_path.as_ref().to_owned();
        let file_content = fs::read(&file_path)?;
        let bytes = file_content.as_slice();
        Self::parse(file_path, bytes)
    }

    /// Parse a [`Directory`] from a byte slice.
    /// `file_path` is required to open possible accompanying archive files.
    ///
    /// # Errors
    ///
    /// Returns `Err` if `vpk_path` doesn't have an utf8 filename or `bytes` is not a valid vpk directory file.
    pub fn parse(vpk_path: StdPathBuf, mut bytes: &[u8]) -> Result<Self, DirectoryReadError> {
        let vpk_base = Self::get_vpk_base(&vpk_path)?;
        let header: LayoutVerified<_, HeaderV1> =
            parse(&mut bytes).ok_or(DirectoryReadError::Corrupted("eof reading header"))?;
        if header.signature.get() != 0x55aa_1234 {
            return Err(DirectoryReadError::InvalidSignature);
        }
        let header_v2: Option<LayoutVerified<_, HeaderV2Ext>> = match header.version.get() {
            1 => None,
            2 => Some(
                parse(&mut bytes).ok_or(DirectoryReadError::Corrupted("eof reading header v2"))?,
            ),
            other => return Err(DirectoryReadError::UnsupportedVersion(other)),
        };
        let tree_len = header.tree_size.get() as usize;
        let before_tree = <&[u8]>::clone(&bytes);

        let header_size = if header_v2.is_none() { 12 } else { 28 };
        let base_offset = header_size + tree_len as u64;

        let mut files: HashMap<PathBuf, Entry> = HashMap::new();
        let mut directory_contents: HashMap<PathBuf, Vec<DirectoryContent>> = HashMap::new();

        Self::parse_tree(&mut bytes, &mut directory_contents, &mut files, base_offset)?;

        if let Some(header_v2) = &header_v2 {
            // verify checksums
            let skip_to_checksums = header_v2.file_data_section_size.get() as usize;
            let archive_md5_len = header_v2.archive_md5_section_size.get() as usize;
            if bytes.len() < skip_to_checksums + archive_md5_len {
                return Err(DirectoryReadError::Corrupted("eof reading checksums"));
            }
            bytes = &bytes[skip_to_checksums..];
            let archive_md5_bytes = {
                let (data, remaining) = bytes.split_at(archive_md5_len);
                bytes = remaining;
                data
            };
            let other_md5: LayoutVerified<_, OtherMd5Section> =
                parse(&mut bytes).ok_or(DirectoryReadError::Corrupted("eof reading checksums"))?;

            let tree_bytes = &before_tree[..tree_len];

            if *md5::compute(archive_md5_bytes) != other_md5.archive_md5_section_checksum {
                return Err(DirectoryReadError::Corrupted(
                    "archive md5 section checksum mismatch",
                ));
            }
            if *md5::compute(tree_bytes) != other_md5.tree_checksum {
                return Err(DirectoryReadError::Corrupted("tree checksum mismatch"));
            }
        }

        Ok(Self {
            path: vpk_path,
            file_base: vpk_base,
            files,
            directory_contents,
        })
    }

    fn parse_tree(
        bytes: &mut &[u8],
        directory_contents: &mut HashMap<PathBuf, Vec<DirectoryContent>>,
        files: &mut HashMap<PathBuf, Entry>,
        base_offset: u64,
    ) -> Result<(), DirectoryReadError> {
        loop {
            let extension = parse_nul_str(bytes).ok_or(DirectoryReadError::Corrupted(
                "eof reading a tree extension",
            ))?;
            if extension.is_empty() {
                break;
            }
            loop {
                let path = parse_nul_str(bytes)
                    .ok_or(DirectoryReadError::Corrupted("eof reading a tree path"))?;
                if path.is_empty() {
                    break;
                }
                let path = if path == b" " {
                    String::new()
                } else {
                    ISO_8859_1.decode(path, DecoderTrap::Strict).map_err(|_| {
                        DirectoryReadError::Corrupted("a tree path is not valid ISO 8859-1")
                    })?
                };
                if !path.is_empty() {
                    path.split('/').filter(|s| !s.is_empty()).fold(
                        PathBuf::new(),
                        |mut parent, child| {
                            let dir = directory_contents.entry(parent.clone()).or_default();
                            let child: PathBuf = child.to_string().into();
                            parent.push(&child);
                            if !dir.iter().any(|c| c == child.as_path()) {
                                dir.push(DirectoryContent::Directory(child));
                            }
                            parent
                        },
                    );
                }
                loop {
                    let mut file_stem = parse_nul_str(bytes).ok_or(
                        DirectoryReadError::Corrupted("eof reading a tree file stem"),
                    )?;
                    if file_stem.is_empty() {
                        break;
                    }
                    if file_stem == b" " {
                        file_stem = b"";
                    }

                    let mut file_name =
                        ISO_8859_1
                            .decode(file_stem, DecoderTrap::Strict)
                            .map_err(|_| {
                                DirectoryReadError::Corrupted(
                                    "a tree file stem is not valid ISO 8859-1",
                                )
                            })?;
                    if extension != b" " {
                        file_name.push('.');
                        ISO_8859_1
                            .decode_to(extension, DecoderTrap::Strict, &mut file_name)
                            .map_err(|_| {
                                DirectoryReadError::Corrupted(
                                    "a tree extension is not valid ISO 8859-1",
                                )
                            })?;
                    }

                    let mut full_path = path.clone();
                    if !path.is_empty() {
                        full_path.push('/');
                    }
                    full_path.push_str(&file_name);

                    if files
                        .insert(full_path.into(), Entry::parse(bytes, base_offset)?)
                        .is_none()
                    {
                        directory_contents
                            .entry(path.clone().into())
                            .or_default()
                            .push(DirectoryContent::File(file_name.into()));
                    }
                }
            }
        }
        Ok(())
    }

    fn get_vpk_base(vpk_path: &StdPath) -> io::Result<String> {
        let vpk_stem = vpk_path
            .file_stem()
            .and_then(OsStr::to_str)
            .ok_or_else(|| {
                io::Error::new(io::ErrorKind::InvalidInput, "file name is not valid utf8")
            })?;
        let vpk_base = vpk_stem
            .strip_suffix("_dir")
            .unwrap_or(vpk_stem)
            .to_string();
        Ok(vpk_base)
    }

    #[must_use]
    pub fn path(&self) -> &StdPath {
        &self.path
    }

    /// Opens the specified file if it exists.
    ///
    /// # Errors
    ///
    /// Returns `Err` if `file_path` is not in the directory or if the file can't be opened.
    pub fn open_file(&self, file_path: &Path) -> io::Result<File> {
        let entry = self.files.get(file_path).ok_or_else(|| {
            io::Error::new(io::ErrorKind::NotFound, file_path.as_str().to_string())
        })?;
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

    /// Returns an iterator over files in the archive.
    #[must_use]
    pub fn files(&self) -> Files {
        Files {
            files: self.files.keys(),
        }
    }

    /// Returns an iterator over contents of the specified directory if the directory exists.
    #[must_use]
    pub fn directory_contents(&self, directory: &Path) -> Option<DirectoryContents> {
        self.directory_contents
            .get(directory)
            .map(|c| DirectoryContents {
                contents: c.as_slice().iter(),
            })
    }
}

/// An iterator over files in a vpk archive.
#[derive(Debug, Clone)]
pub struct Files<'a> {
    files: Keys<'a, PathBuf, Entry>,
}

impl<'a> Iterator for Files<'a> {
    type Item = &'a Path;

    fn next(&mut self) -> Option<Self::Item> {
        self.files.next().map(PathBuf::as_path)
    }
}

/// An iterator over directory contents in a vpk archive.
#[derive(Debug, Clone)]
pub struct DirectoryContents<'a> {
    contents: Iter<'a, DirectoryContent>,
}

impl<'a> Iterator for DirectoryContents<'a> {
    type Item = &'a DirectoryContent;

    fn next(&mut self) -> Option<Self::Item> {
        self.contents.next()
    }
}

/// An open file inside a vpk archive.
#[derive(Debug)]
pub struct File<'a> {
    entry: &'a Entry,
    file: Option<fs::File>,
    cursor: u64,
}

impl<'a> File<'a> {
    /// Returns the size of the file in bytes.
    #[must_use]
    pub fn size(&self) -> usize {
        self.entry.preload_bytes.len() + self.entry.entry_length as usize
    }

    /// Returns the CRC cheksum of the file.
    #[must_use]
    pub fn crc32(&self) -> u32 {
        self.entry.crc
    }

    /// Reads the file to a `Vec<u8>` and verifies the CRC checksum.
    /// Returns the contents of the file.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the file read fails or the CRC checksums don't match.
    pub fn verify_contents(&mut self) -> io::Result<Vec<u8>> {
        let mut buf = Vec::with_capacity(self.size());
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
            self.cursor += read_amount as u64;
            return Ok(read_amount);
        }
        if let Some(file) = &mut self.file {
            let remaining = buf
                .len()
                .min((self.entry.entry_length as usize).saturating_sub(cursor - preload_len));
            let read_amount = file.read(&mut buf[..remaining])?;
            self.cursor += read_amount as u64;
            return Ok(read_amount);
        }
        Ok(0)
    }
}

impl<'a> Seek for File<'a> {
    fn seek(&mut self, pos: SeekFrom) -> io::Result<u64> {
        let preload_len = self.entry.preload_bytes.len() as u64;
        let new_cursor = match pos {
            SeekFrom::Start(seek) => seek,
            SeekFrom::End(seek) => (self.size() as i64 + seek)
                .try_into()
                .map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))?,
            SeekFrom::Current(seek) => (self.cursor as i64 + seek)
                .try_into()
                .map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))?,
        };
        if new_cursor <= preload_len {
            if self.cursor > preload_len {
                if let Some(file) = &mut self.file {
                    file.seek(SeekFrom::Start(self.entry.total_offset))?;
                }
            }
            self.cursor = new_cursor;
        } else if let Some(file) = &mut self.file {
            let seeked = file.seek(SeekFrom::Start(
                self.entry.total_offset + new_cursor - preload_len,
            ))?;
            self.cursor = seeked + preload_len - self.entry.total_offset;
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
    fn parse_nul_terminated_str() {
        let bytes = [0x62_u8, 0x31, 0x37, 0x00];
        let mut bytes_ref = bytes.as_ref();
        assert_eq!(parse_nul_str(&mut bytes_ref), Some(b"b17".as_ref()));
        assert_eq!(parse_nul_str(&mut bytes_ref), None);

        let bytes2 = [0x00_u8];
        bytes_ref = bytes2.as_ref();
        assert_eq!(parse_nul_str(&mut bytes_ref), Some(b"".as_ref()));
        assert_eq!(parse_nul_str(&mut bytes_ref), None);
    }

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
        assert_eq!(parse_nul_str(&mut tree), Some(b"vmt".as_ref()),);
        assert_eq!(
            parse_nul_str(&mut tree),
            Some(b"materials/models/props_vehicles".as_ref()),
        );
        assert_eq!(parse_nul_str(&mut tree), Some(b"b17".as_ref()),);
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
        let entry = Entry {
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
        file.verify_contents().unwrap();
    }

    #[test]
    fn file_full() {
        let path = StdPath::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests")
            .join("test.txt");

        let entry = Entry {
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
        file.verify_contents().unwrap();
    }
}
