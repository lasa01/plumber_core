use std::{
    borrow::Cow,
    ffi::OsStr,
    fmt::{Debug, Formatter},
    fs::{self, FileType},
    io::{self, Read, Seek},
    path::{Path as StdPath, PathBuf as StdPathBuf},
    slice,
};

use plumber_vdf as vdf;
use plumber_vpk as vpk;
use vpk::DirectoryReadError;
pub use vpk::{Path, PathBuf};

use crate::steam;

use serde::{
    de::{MapAccess, Visitor},
    Deserialize,
};
use thiserror::Error;
use uncased::AsUncased;

#[derive(Debug, PartialEq, Deserialize)]
#[serde(case_insensitive)]
struct GameInfoFile {
    #[serde(rename = "gameinfo")]
    game_info: GameInfo,
}

#[derive(Debug, PartialEq, Deserialize)]
#[serde(case_insensitive)]
struct GameInfo {
    #[serde(default)]
    game: String,
    #[serde(rename = "filesystem")]
    file_system: GameInfoFileSystem,
}

#[derive(Debug, PartialEq, Deserialize)]
#[serde(case_insensitive)]
struct GameInfoFileSystem {
    #[serde(rename = "steamappid")]
    steam_app_id: u32,
    #[serde(rename = "toolsappid")]
    tools_app_id: Option<u32>,
    #[serde(rename = "searchpaths")]
    search_paths: GameInfoSearchPaths,
}

#[derive(Debug, PartialEq)]
struct GameInfoSearchPaths {
    game_search_paths: Vec<String>,
}

impl<'de> Deserialize<'de> for GameInfoSearchPaths {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct SearchPathsVisitor;

        impl<'de> Visitor<'de> for SearchPathsVisitor {
            type Value = GameInfoSearchPaths;

            fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
                formatter.write_str("class SearchPaths")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let mut game_search_paths = Vec::new();

                while let Some(key) = map.next_key::<&str>()? {
                    let value: &str = map.next_value()?;
                    // only cares about `game` search paths (at least for now)
                    if key.split('+').any(|k| k.eq_ignore_ascii_case("game")) {
                        game_search_paths.push(value.to_string());
                    }
                }

                Ok(GameInfoSearchPaths { game_search_paths })
            }
        }

        deserializer.deserialize_map(SearchPathsVisitor)
    }
}

fn is_vpk_file(filename: &str) -> bool {
    filename
        .rsplit('.')
        .next()
        .map(|ext| ext.eq_ignore_ascii_case("vpk"))
        == Some(true)
}

/// Stores Source games' filesystems.
#[derive(Debug)]
pub struct Manager {
    pub games: Vec<FileSystem>,
}

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("io error reading `{path}`: {inner}")]
    Io { path: String, inner: io::Error },
    #[error("could not find gameinfo.txt")]
    NoGameInfo,
    #[error("error deserializing gameinfo.txt: {0}")]
    Deserialization(#[from] vdf::Error),
    #[error("error deserializing appmanifest: {0}")]
    AppDeserialization(vdf::Error),
}

impl ParseError {
    fn from_io(err: io::Error, path: &StdPath) -> Self {
        Self::Io {
            path: path.as_os_str().to_string_lossy().into_owned(),
            inner: err,
        }
    }
}

impl From<steam::AppError> for ParseError {
    fn from(e: steam::AppError) -> Self {
        match e {
            steam::AppError::Io { path, inner } => ParseError::Io { path, inner },
            steam::AppError::Deserialization(e) => ParseError::AppDeserialization(e),
        }
    }
}

#[derive(Debug, Error)]
#[error("error reading `{path}`: {ty}")]
pub struct OpenError {
    path: String,
    ty: OpenErrorType,
}

impl OpenError {
    fn new(path: &StdPath, ty: OpenErrorType) -> Self {
        Self {
            path: path.as_os_str().to_string_lossy().into_owned(),
            ty,
        }
    }
}

#[derive(Debug, Error)]
pub enum OpenErrorType {
    #[error("io error: {0}")]
    Io(#[from] io::Error),
    #[error("reading vpk failed: {0}")]
    Vpk(#[from] vpk::DirectoryReadError),
}

/// A search path in a [`FileSystem`].
#[derive(Debug, PartialEq)]
pub enum SearchPath {
    Vpk(StdPathBuf),
    Directory(StdPathBuf),
    Wildcard(StdPathBuf),
}

/// Represents a Source game's filesystem.
#[derive(Debug, PartialEq)]
pub struct FileSystem {
    pub name: String,
    pub search_paths: Vec<SearchPath>,
}

impl FileSystem {
    /// # Errors
    ///
    /// Returns `Err` if gameinfo.txt can't be found,
    /// the gameinfo.txt read fails or the gameinfo deserialization fails.
    pub fn from_app(app: &steam::App) -> Result<Self, ParseError> {
        let mut entries = fs::read_dir(&app.install_dir)
            .map_err(|err| ParseError::from_io(err, &app.install_dir))?;
        let gameinfo_path = loop {
            if let Some(entry) = entries.next() {
                let entry = entry.map_err(|err| ParseError::from_io(err, &app.install_dir))?;
                if !entry.file_type().as_ref().map_or(false, FileType::is_dir) {
                    continue;
                }
                let maybe_gameinfo_path = entry.path().join("gameinfo.txt");
                if maybe_gameinfo_path.is_file() {
                    break maybe_gameinfo_path;
                }
            } else {
                return Err(ParseError::NoGameInfo);
            }
        };
        Self::from_paths(&app.install_dir, &gameinfo_path)
    }

    /// # Errors
    ///
    /// Returns `Err` if the gameinfo.txt read fails or the deserialization fails.
    ///
    /// # Panics
    ///
    /// Panics if `game_info_path` has no parent.
    pub fn from_paths<P: AsRef<StdPath>>(
        root_path: P,
        game_info_path: P,
    ) -> Result<Self, ParseError> {
        let root_path = root_path.as_ref();
        let game_info_path = game_info_path.as_ref();

        let game_info = vdf::from_str::<GameInfoFile>(
            &fs::read_to_string(game_info_path)
                .map_err(|err| ParseError::from_io(err, game_info_path))?,
        )?
        .game_info;
        Ok(Self::from_game_info(
            game_info,
            game_info_path
                .parent()
                .expect("`game_info_path` has no parent"),
            root_path,
        ))
    }

    fn from_game_info(
        game_info: GameInfo,
        game_info_directory: &StdPath,
        root_path: &StdPath,
    ) -> Self {
        let mut search_paths = Vec::new();

        for path in &game_info.file_system.search_paths.game_search_paths {
            let path = path.as_uncased();
            let path = if path.starts_with("|gameinfo_path|") {
                match path[15..].as_str() {
                    "." => game_info_directory.into(),
                    other => game_info_directory.join(other),
                }
            } else if path.starts_with("|all_source_engine_paths|") {
                match path[25..].as_str() {
                    "." => root_path.into(),
                    other => root_path.join(other),
                }
            } else {
                root_path.join(path.as_str())
            };

            if let Some(file_name) = path.file_name().and_then(OsStr::to_str) {
                if is_vpk_file(file_name) {
                    let new_path = SearchPath::Vpk(path);
                    if !search_paths.contains(&new_path) {
                        search_paths.push(new_path);
                    }
                } else if file_name == "*" {
                    if let Some(parent) = path.parent() {
                        let new_path = SearchPath::Wildcard(parent.into());
                        if !search_paths.contains(&new_path) {
                            search_paths.push(new_path);
                        }
                    }
                } else {
                    let new_path = SearchPath::Directory(path);
                    if !search_paths.contains(&new_path) {
                        search_paths.push(new_path);
                    }
                }
            }
        }

        Self {
            name: game_info.game,
            search_paths,
        }
    }

    /// Opens the game's filesystem.
    /// Opens all vpk archives. Silently ignores non-existing search paths.
    ///
    /// # Errors
    ///
    /// Returns `Err` if a search path or a vpk archive can't be opened.
    pub fn open(&self) -> Result<OpenFileSystem, OpenError> {
        let mut open_search_paths = Vec::new();
        for search_path in &self.search_paths {
            match search_path {
                SearchPath::Vpk(path) => {
                    let alt_path = path.file_stem().map(|s| {
                        let mut s = s.to_os_string();
                        s.push("_dir.vpk");
                        path.with_file_name(s)
                    });
                    match vpk::Directory::read(path)
                        .or_else(|e| alt_path.map_or(Err(e), vpk::Directory::read))
                    {
                        Ok(dir) => open_search_paths.push(OpenSearchPath::Vpk(dir)),
                        Err(err) => {
                            if let DirectoryReadError::Io(inner) = &err {
                                if inner.kind() == io::ErrorKind::NotFound {
                                    continue;
                                }
                            }
                            return Err(OpenError::new(path, err.into()));
                        }
                    }
                }
                SearchPath::Directory(path) => match fs::read_dir(path) {
                    Ok(readdir) => {
                        for entry in readdir {
                            let entry = entry.map_err(|err| OpenError::new(path, err.into()))?;
                            if entry
                                .file_type()
                                .map_err(|err| OpenError::new(path, err.into()))?
                                .is_file()
                                && entry.file_name().to_str() == Some("pak01_dir.vpk")
                            {
                                let path = entry.path();
                                open_search_paths.push(OpenSearchPath::Vpk(
                                    vpk::Directory::read(&path)
                                        .map_err(|err| OpenError::new(&path, err.into()))?,
                                ));
                            }
                        }
                        open_search_paths.push(OpenSearchPath::Directory(path.clone()));
                    }
                    Err(err) => {
                        if err.kind() != io::ErrorKind::NotFound {
                            return Err(OpenError::new(path, err.into()));
                        }
                    }
                },
                SearchPath::Wildcard(path) => match fs::read_dir(path) {
                    Ok(readdir) => {
                        for entry in readdir {
                            let entry = entry.map_err(|err| OpenError::new(path, err.into()))?;
                            let file_type = entry
                                .file_type()
                                .map_err(|err| OpenError::new(path, err.into()))?;
                            if file_type.is_file() {
                                if entry.file_name().to_str().map_or(false, is_vpk_file) {
                                    open_search_paths.push(OpenSearchPath::Directory(entry.path()));
                                }
                            } else if file_type.is_dir() {
                                open_search_paths.push(OpenSearchPath::Directory(entry.path()));
                            }
                        }
                    }
                    Err(err) => {
                        if err.kind() != io::ErrorKind::NotFound {
                            return Err(OpenError::new(path, err.into()));
                        }
                    }
                },
            }
        }
        Ok(OpenFileSystem {
            search_paths: open_search_paths,
        })
    }
}

enum OpenSearchPath {
    Vpk(vpk::Directory),
    Directory(StdPathBuf),
}

impl PartialEq for OpenSearchPath {
    fn eq(&self, other: &Self) -> bool {
        match self {
            OpenSearchPath::Vpk(dir) => {
                matches!(other, OpenSearchPath::Vpk(other_dir) if other_dir.path() == dir.path())
            }
            OpenSearchPath::Directory(path) => {
                matches!(other, OpenSearchPath::Directory(other_path) if other_path == path)
            }
        }
    }
}

impl Debug for OpenSearchPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            OpenSearchPath::Vpk(dir) => write!(f, "OpenSearchPath::Vpk({:?})", dir.path()),
            OpenSearchPath::Directory(path) => write!(f, "OpenSearchPath::Directory({:?})", path),
        }
    }
}

impl OpenSearchPath {
    fn path(&self) -> &StdPath {
        match self {
            OpenSearchPath::Vpk(dir) => dir.path(),
            OpenSearchPath::Directory(path) => path,
        }
    }

    fn try_open_file(&self, file_path: &Path) -> io::Result<Option<GameFile>> {
        match self {
            OpenSearchPath::Vpk(vpk) => vpk.open_file(file_path).map_or_else(
                |e| {
                    if e.kind() == io::ErrorKind::NotFound {
                        Ok(None)
                    } else {
                        Err(e)
                    }
                },
                |f| Ok(Some(GameFile::Vpk(f))),
            ),
            OpenSearchPath::Directory(path) => fs::File::open(path.join(file_path.as_str()))
                .map_or_else(
                    |e| {
                        if e.kind() == io::ErrorKind::NotFound {
                            Ok(None)
                        } else {
                            Err(e)
                        }
                    },
                    |f| Ok(Some(GameFile::Fs(f))),
                ),
        }
    }

    fn try_read_dir<'a>(&'a self, path: &Path) -> io::Result<Option<ReadDirPart<'a>>> {
        match self {
            OpenSearchPath::Vpk(vpk) => vpk.directory_contents(path).map_or(Ok(None), |contents| {
                Ok(Some(ReadDirPart {
                    search_path: self,
                    ty: ReadDirPartType::Vpk(contents),
                }))
            }),
            OpenSearchPath::Directory(dir_path) => fs::read_dir(dir_path.join(path.as_str()))
                .map_or_else(
                    |e| {
                        if e.kind() == io::ErrorKind::NotFound {
                            Ok(None)
                        } else {
                            Err(e)
                        }
                    },
                    |readdir| {
                        Ok(Some(ReadDirPart {
                            search_path: self,
                            ty: ReadDirPartType::Fs(Box::new(readdir)),
                        }))
                    },
                ),
        }
    }
}

#[derive(Debug)]
struct ReadDirPart<'a> {
    search_path: &'a OpenSearchPath,
    ty: ReadDirPartType<'a>,
}

#[derive(Debug)]
enum ReadDirPartType<'a> {
    Vpk(vpk::DirectoryContents<'a>),
    Fs(Box<fs::ReadDir>),
}

impl<'a> ReadDirPart<'a> {
    fn next_entry(&mut self, path: &'a Path) -> Option<io::Result<DirEntry<'a>>> {
        match &mut self.ty {
            ReadDirPartType::Vpk(contents) => contents.next().map(|c| match c {
                vpk::DirectoryContent::Directory(p) => Ok(DirEntry::new_borrowed(
                    self.search_path,
                    p,
                    path,
                    DirEntryType::Directory,
                )),
                vpk::DirectoryContent::File(p) => Ok(DirEntry::new_borrowed(
                    self.search_path,
                    p,
                    path,
                    DirEntryType::File,
                )),
            }),
            ReadDirPartType::Fs(readdir) => {
                for result in readdir {
                    match result {
                        Ok(e) => {
                            let file_type = match e.file_type() {
                                Ok(ty) => ty,
                                Err(err) => return Some(Err(err)),
                            };
                            let name = match e.file_name().into_string().map_err(|_| {
                                io::Error::new(
                                    io::ErrorKind::InvalidData,
                                    "a directory entry is not utf8",
                                )
                            }) {
                                Ok(name) => name,
                                Err(err) => return Some(Err(err)),
                            };
                            if file_type.is_dir() {
                                return Some(Ok(DirEntry::new_owned(
                                    self.search_path,
                                    name.into(),
                                    path,
                                    DirEntryType::Directory,
                                )));
                            } else if file_type.is_file() {
                                return Some(Ok(DirEntry::new_owned(
                                    self.search_path,
                                    name.into(),
                                    path,
                                    DirEntryType::File,
                                )));
                            }
                        }
                        Err(err) => return Some(Err(err)),
                    }
                }
                None
            }
        }
    }
}

/// An open Source game's filesystem, ready to be read.
#[derive(Debug)]
pub struct OpenFileSystem {
    search_paths: Vec<OpenSearchPath>,
}

fn initial_buffer_size(file: &GameFile) -> usize {
    // Allocate one extra byte so the buffer doesn't need to grow before the
    // final `read` call at the end of the file.
    file.size().map_or(0, |s| s + 1)
}

impl OpenFileSystem {
    /// Opens the specified file if it exists.
    ///
    /// # Errors
    ///
    /// Returns `Err` if `file_path` doesn't exist or if the file can't be opened.
    pub fn open_file(&self, file_path: impl AsRef<Path>) -> io::Result<GameFile> {
        let file_path = file_path.as_ref();
        for path in &self.search_paths {
            if let Some(file) = path.try_open_file(file_path)? {
                return Ok(file);
            }
        }
        Err(io::Error::new(io::ErrorKind::NotFound, "no such file"))
    }

    /// Reads the specified file into a [`Vec`].
    ///
    /// # Errors
    ///
    /// Returns `Err` if `file_path` doesn't exist or if the file can't be read.
    pub fn read(&self, file_path: impl AsRef<Path>) -> io::Result<Vec<u8>> {
        let mut file = self.open_file(file_path)?;
        let mut buffer = Vec::with_capacity(initial_buffer_size(&file));
        file.read_to_end(&mut buffer)?;
        Ok(buffer)
    }

    /// Reads the specified file into a [`String`].
    ///
    /// # Errors
    ///
    /// Returns `Err` if `file_path` doesn't exist or if the file can't be read.
    pub fn read_to_string(&self, file_path: impl AsRef<Path>) -> io::Result<String> {
        let mut file = self.open_file(file_path)?;
        let mut buffer = String::with_capacity(initial_buffer_size(&file));
        file.read_to_string(&mut buffer)?;
        Ok(buffer)
    }

    /// Returns an iterator over the entries within a directory.
    pub fn read_dir<'a>(&'a self, path: &'a Path) -> ReadDir<'a> {
        ReadDir {
            search_paths: self.search_paths.iter(),
            path,
            current_readdir: None,
        }
    }
}

#[derive(Debug)]
#[must_use]
pub struct ReadDir<'a> {
    search_paths: slice::Iter<'a, OpenSearchPath>,
    current_readdir: Option<ReadDirPart<'a>>,
    path: &'a Path,
}

impl<'a> Iterator for ReadDir<'a> {
    type Item = Result<DirEntry<'a>, io::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let path = self.path;
        loop {
            if let Some(ret) = self
                .current_readdir
                .as_mut()
                .and_then(|r| r.next_entry(path))
            {
                return Some(ret);
            }
            if let Some(path) = self.search_paths.next() {
                match path.try_read_dir(self.path) {
                    Ok(r) => {
                        self.current_readdir = r;
                        continue;
                    }
                    Err(err) => return Some(Err(err)),
                }
            }
            return None;
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DirEntryType {
    File,
    Directory,
}

impl DirEntryType {
    /// Returns `true` if the entry is a file.
    #[must_use]
    pub fn is_file(&self) -> bool {
        matches!(self, Self::File)
    }

    /// Returns `true` if the entry is a directory.
    #[must_use]
    pub fn is_directory(&self) -> bool {
        matches!(self, Self::Directory)
    }
}

#[derive(Debug, PartialEq)]
pub struct DirEntry<'a> {
    search_path: &'a OpenSearchPath,
    name: Cow<'a, Path>,
    path: PathBuf,
    ty: DirEntryType,
}

impl<'a> DirEntry<'a> {
    fn new_borrowed(
        search_path: &'a OpenSearchPath,
        name: &'a Path,
        path: &'a Path,
        ty: DirEntryType,
    ) -> Self {
        Self {
            search_path,
            name: Cow::Borrowed(name),
            path: path.join(name),
            ty,
        }
    }

    fn new_owned(
        search_path: &'a OpenSearchPath,
        name: PathBuf,
        path: &'a Path,
        ty: DirEntryType,
    ) -> Self {
        Self {
            search_path,
            path: path.join(&name),
            name: Cow::Owned(name),
            ty,
        }
    }

    #[must_use]
    pub fn entry_type(&self) -> &DirEntryType {
        &self.ty
    }

    #[must_use]
    pub fn name(&self) -> &Path {
        &self.name
    }

    #[must_use]
    pub fn path(&self) -> &Path {
        &self.path
    }

    #[must_use]
    pub fn search_path(&self) -> &StdPath {
        self.search_path.path()
    }

    /// Opens the entry if it is a file.
    ///
    /// # Errors
    ///
    /// Returns `Err` if `self` is not a file or if the file can't be opened.
    pub fn open(&self) -> io::Result<GameFile> {
        self.search_path
            .try_open_file(self.path())
            .and_then(|maybe_file| {
                maybe_file.ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "no such file"))
            })
    }

    /// Reads the entry into a [`Vec`].
    ///
    /// # Errors
    ///
    /// Returns `Err` if `self` is not a file or if the file can't be read.
    pub fn read(&self) -> io::Result<Vec<u8>> {
        let mut file = self.open()?;
        let mut buffer = Vec::with_capacity(initial_buffer_size(&file));
        file.read_to_end(&mut buffer)?;
        Ok(buffer)
    }

    /// Reads the entry into a [`String`].
    ///
    /// # Errors
    ///
    /// Returns `Err` if `self` is not a file or if the file can't be read.
    pub fn read_to_string(&self) -> io::Result<String> {
        let mut file = self.open()?;
        let mut buffer = String::with_capacity(initial_buffer_size(&file));
        file.read_to_string(&mut buffer)?;
        Ok(buffer)
    }

    /// Returns an iterator over the entries within this entry.
    pub fn read_dir(&self) -> ReadDir {
        ReadDir {
            search_paths: slice::from_ref(self.search_path).iter(),
            path: &self.path,
            current_readdir: None,
        }
    }
}

#[derive(Debug)]
pub enum GameFile<'a> {
    Fs(fs::File),
    Vpk(vpk::File<'a>),
}

impl<'a> GameFile<'a> {
    #[must_use]
    pub fn size(&self) -> Option<usize> {
        match self {
            GameFile::Fs(f) => f.metadata().map_or(None, |m| Some(m.len() as usize)),
            GameFile::Vpk(f) => Some(f.size()),
        }
    }
}

impl<'a> Read for GameFile<'a> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        match self {
            GameFile::Fs(f) => f.read(buf),
            GameFile::Vpk(f) => f.read(buf),
        }
    }
}

impl<'a> Seek for GameFile<'a> {
    fn seek(&mut self, pos: io::SeekFrom) -> io::Result<u64> {
        match self {
            GameFile::Fs(f) => f.seek(pos),
            GameFile::Vpk(f) => f.seek(pos),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn get_test_game_info() -> GameInfo {
        vdf::from_str::<GameInfoFile>(
            r#"
        "GameInfo"
        {
            game	"Team Fortress 2"        
        
            FileSystem
            {
                SteamAppId				440
                
                //
                // Setup engine search paths.
                //
                SearchPaths
                {
        
                    game+mod+custom_mod	tf/custom/*
        
                    game_lv				tf/tf2_lv.vpk
                    game+mod			tf/tf2_textures.vpk
                    game+mod+vgui		tf/tf2_misc.vpk
                    game				|all_source_engine_paths|hl2/hl2_textures.vpk
                    game+vgui			|all_source_engine_paths|hl2/hl2_misc.vpk
                    platform+vgui			|all_source_engine_paths|platform/platform_misc.vpk
        
                    mod+mod_write+default_write_path		|gameinfo_path|.
        
                    game+game_write		tf
        
                    gamebin				tf/bin
        
                    game				|all_source_engine_paths|hl2
                    platform			|all_source_engine_paths|platform
        
                    game+download	tf/download
                }
            }
        }        
        "#,
        )
        .unwrap()
        .game_info
    }

    #[test]
    fn test_gameinfo_deserialization() {
        assert_eq!(
            get_test_game_info(),
            GameInfo {
                game: "Team Fortress 2".into(),
                file_system: GameInfoFileSystem {
                    steam_app_id: 440,
                    tools_app_id: None,
                    search_paths: GameInfoSearchPaths {
                        game_search_paths: vec![
                            "tf/custom/*".into(),
                            "tf/tf2_textures.vpk".into(),
                            "tf/tf2_misc.vpk".into(),
                            "|all_source_engine_paths|hl2/hl2_textures.vpk".into(),
                            "|all_source_engine_paths|hl2/hl2_misc.vpk".into(),
                            "tf".into(),
                            "|all_source_engine_paths|hl2".into(),
                            "tf/download".into(),
                        ],
                    }
                }
            }
        );
    }

    #[test]
    fn file_system_creation() {
        let game_info = get_test_game_info();
        let file_system = FileSystem::from_game_info(
            game_info,
            &StdPathBuf::from("game_info_directory"),
            &StdPathBuf::from("root_path"),
        );
        assert_eq!(
            file_system,
            FileSystem {
                name: "Team Fortress 2".into(),
                search_paths: vec![
                    SearchPath::Wildcard("root_path/tf/custom".into()),
                    SearchPath::Vpk("root_path/tf/tf2_textures.vpk".into()),
                    SearchPath::Vpk("root_path/tf/tf2_misc.vpk".into()),
                    SearchPath::Vpk("root_path/hl2/hl2_textures.vpk".into()),
                    SearchPath::Vpk("root_path/hl2/hl2_misc.vpk".into()),
                    SearchPath::Directory("root_path/tf".into()),
                    SearchPath::Directory("root_path/hl2".into()),
                    SearchPath::Directory("root_path/tf/download".into())
                ]
            }
        );
    }
}
