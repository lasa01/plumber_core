use std::{
    borrow::Borrow,
    ffi::OsStr,
    fmt::Formatter,
    fs,
    io::{self, Read, Seek},
    ops::Deref,
    path::{Path as StdPath, PathBuf as StdPathBuf},
};

use crate::{vdf, vpk};

#[cfg(feature = "steam")]
use crate::steam;

use serde::{
    de::{MapAccess, Visitor},
    Deserialize, Serialize,
};
use serde_derive::Deserialize;
use thiserror::Error;
use uncased::UncasedStr;

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
    #[error("io error: {0}")]
    Io(#[from] io::Error),
    #[error("could not find gameinfo.txt")]
    NoGameInfo,
    #[error("error deserializing gameinfo.txt: {0}")]
    Deserialization(#[from] vdf::Error),
    #[error("error deserializing appmanifest: {0}")]
    AppDeserialization(vdf::Error),
}

#[cfg(feature = "steam")]
impl From<steam::AppError> for ParseError {
    fn from(e: steam::AppError) -> Self {
        match e {
            steam::AppError::Io(e) => ParseError::Io(e),
            steam::AppError::Deserialization(e) => ParseError::AppDeserialization(e),
        }
    }
}

#[derive(Debug, Error)]
pub enum OpenError {
    #[error("io error: {0}")]
    Io(#[from] io::Error),
    #[error("error reading vpk: {0}")]
    Vpk(#[from] vpk::DirectoryReadError),
}

/// A search path in a [`Game`].
#[derive(Debug, PartialEq)]
pub enum SearchPath {
    Vpk(StdPathBuf),
    Directory(StdPathBuf),
    Wildcard(StdPathBuf),
}

/// A slice of a game filesystem path.
#[repr(transparent)]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Path(str);

impl Path {
    fn new<S: AsRef<str> + ?Sized>(s: &S) -> &Self {
        unsafe { &*(s.as_ref() as *const str as *const Self) }
    }

    /// Checks that the str is lowercase and doesn't contain backslashes.
    #[must_use]
    pub fn from_str(str: &str) -> Option<&Self> {
        if str.chars().any(|c| c.is_ascii_uppercase()) {
            return None;
        }
        if str.contains('\\') {
            return None;
        }
        Some(Self::new(str))
    }

    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.0
    }

    #[must_use]
    pub fn to_path_buf(&self) -> PathBuf {
        self.to_owned()
    }

    #[must_use]
    pub fn parent(&self) -> Option<&Self> {
        let mut path = &self.0;
        if path.ends_with('/') {
            path = &path[..path.len() - 1]
        }
        path.rfind('/').map(|i| Path::new(&path[..i]))
    }

    #[must_use]
    pub fn file_name(&self) -> Option<&str> {
        let mut path = &self.0;
        if path.ends_with('/') {
            path = &path[..path.len() - 1]
        }
        let file_name = path.rfind('/').map_or(path, |i| &path[i + 1..]);
        if file_name.is_empty() {
            None
        } else {
            Some(file_name)
        }
    }

    #[must_use]
    pub fn file_stem(&self) -> Option<&str> {
        self.file_name().map(|file_name| {
            let mut split = file_name.rsplitn(2, '.');
            let _after = split.next();
            let before = match split.next() {
                Some(s) => s,
                None => return file_name,
            };
            if before.is_empty() {
                file_name
            } else {
                before
            }
        })
    }

    #[must_use]
    pub fn extension(&self) -> Option<&str> {
        self.file_name().and_then(|file_name| {
            let mut split = file_name.rsplitn(2, '.');
            let after = split.next()?;
            let before = split.next()?;
            if before.is_empty() {
                None
            } else {
                Some(after)
            }
        })
    }

    pub fn join(&self, path: impl AsRef<Path>) -> PathBuf {
        let mut buf = self.to_path_buf();
        buf.push(path);
        buf
    }

    pub fn with_file_name(&self, file_name: impl AsRef<str>) -> PathBuf {
        let mut buf = self.to_path_buf();
        buf.set_file_name(file_name);
        buf
    }

    pub fn with_extension(&self, extension: impl AsRef<str>) -> PathBuf {
        let mut buf = self.to_path_buf();
        buf.set_extension(extension);
        buf
    }
}

impl ToOwned for Path {
    type Owned = PathBuf;

    fn to_owned(&self) -> Self::Owned {
        PathBuf(self.0.to_owned())
    }
}

impl AsRef<Path> for &Path {
    fn as_ref(&self) -> &Path {
        self
    }
}

/// An owned, mutable game filesystem path.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Default)]
pub struct PathBuf(String);

impl PathBuf {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn with_capacity(capacity: usize) -> Self {
        Self(String::with_capacity(capacity))
    }

    #[must_use]
    pub fn as_path(&self) -> &Path {
        &*self
    }

    pub fn push(&mut self, path: impl AsRef<Path>) {
        if self.0.as_bytes().last().map_or(false, |b| *b != b'/') {
            self.0.push('/');
        }
        self.0.push_str(path.as_ref().as_str());
    }

    pub fn pop(&mut self) -> bool {
        self.parent()
            .map(|p| p.as_str().len())
            .map_or(false, |len| {
                self.0.truncate(len);
                true
            })
    }

    #[allow(clippy::missing_panics_doc)]
    pub fn set_file_name(&mut self, file_name: impl AsRef<str>) {
        if self.file_name().is_some() {
            let popped = self.pop();
            debug_assert!(popped)
        }
        self.push(Path::new(file_name.as_ref()));
    }

    pub fn set_extension(&mut self, extension: impl AsRef<str>) -> bool {
        let extension = extension.as_ref();

        let file_stem = match self.file_stem() {
            None => return false,
            Some(s) => s,
        };

        // truncate until right after the file stem
        let end_file_stem = file_stem[file_stem.len()..].as_ptr() as usize;
        let start = self.0.as_ptr() as usize;
        self.0.truncate(end_file_stem.wrapping_sub(start));

        // add the new extension, if any
        if !extension.is_empty() {
            self.0.reserve_exact(extension.len() + 1);
            self.0.push('.');
            self.0.push_str(extension);
        }

        true
    }

    #[must_use]
    pub fn into_string(self) -> String {
        self.0
    }

    #[must_use]
    pub fn capacity(&self) -> usize {
        self.0.capacity()
    }

    pub fn clear(&mut self) {
        self.0.clear()
    }

    pub fn reserve(&mut self, additional: usize) {
        self.0.reserve(additional)
    }

    pub fn reserve_exact(&mut self, additional: usize) {
        self.0.reserve_exact(additional)
    }

    pub fn shrink_to_fit(&mut self) {
        self.0.shrink_to_fit()
    }
}

impl Deref for PathBuf {
    type Target = Path;

    fn deref(&self) -> &Self::Target {
        Path::new(&self.0)
    }
}

impl Borrow<Path> for PathBuf {
    fn borrow(&self) -> &Path {
        &*self
    }
}

impl AsRef<Path> for PathBuf {
    fn as_ref(&self) -> &Path {
        &*self
    }
}

impl From<String> for PathBuf {
    fn from(mut s: String) -> Self {
        s.make_ascii_lowercase();
        let mut s = s.into_bytes();
        for byte in &mut s {
            if *byte == b'\\' {
                *byte = b'/';
            }
        }
        Self(String::from_utf8(s).unwrap())
    }
}

impl<'de> Deserialize<'de> for PathBuf {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let string = String::deserialize(deserializer)?;
        Ok(string.into())
    }
}

impl Serialize for PathBuf {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        str::serialize(self.as_str(), serializer)
    }
}

/// Represents a Source game's filesystem.
#[derive(Debug)]
pub struct FileSystem {
    pub name: String,
    pub search_paths: Vec<SearchPath>,
}

impl FileSystem {
    /// # Errors
    ///
    /// Returns `Err` if gameinfo.txt can't be found,
    /// the gameinfo.txt read fails or the gameinfo deserialization fails.
    #[cfg(feature = "steam")]
    pub fn from_app(app: &steam::App) -> Result<Self, ParseError> {
        let mut entries = fs::read_dir(&app.install_dir)?;
        let gameinfo_path = loop {
            if let Some(entry) = entries.next() {
                let entry = entry?;
                if !entry.file_type()?.is_dir() {
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

        let game_info =
            vdf::from_str::<GameInfoFile>(&fs::read_to_string(game_info_path)?)?.game_info;
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
            let path = UncasedStr::new(path);
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
    /// Opens all vpk archives and verifies that search directories exist.
    ///
    /// # Errors
    ///
    /// Returns `Err` if a search path doesn't exist or a vpk archive can't be opened.
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
                    open_search_paths.push(OpenSearchPath::Vpk(
                        vpk::Directory::read(path)
                            .or_else(|e| alt_path.map_or(Err(e), vpk::Directory::read))?,
                    ));
                }
                SearchPath::Directory(path) => {
                    for entry in fs::read_dir(path)? {
                        let entry = entry?;
                        if entry.file_type()?.is_file()
                            && entry.file_name().to_str() == Some("pak01_dir.vpk")
                        {
                            open_search_paths
                                .push(OpenSearchPath::Vpk(vpk::Directory::read(entry.path())?));
                        }
                    }
                    open_search_paths.push(OpenSearchPath::Directory(path.clone()));
                }
                SearchPath::Wildcard(path) => {
                    for entry in fs::read_dir(path)? {
                        let entry = entry?;
                        let file_type = entry.file_type()?;
                        if file_type.is_file() {
                            if entry.file_name().to_str().map_or(false, is_vpk_file) {
                                open_search_paths.push(OpenSearchPath::Directory(entry.path()));
                            }
                        } else if file_type.is_dir() {
                            open_search_paths.push(OpenSearchPath::Directory(entry.path()));
                        }
                    }
                }
            }
        }
        Ok(OpenFileSystem {
            search_paths: open_search_paths,
        })
    }
}

#[derive(Debug)]
enum OpenSearchPath {
    Vpk(vpk::Directory),
    Directory(StdPathBuf),
}

impl OpenSearchPath {
    fn try_open_file(&self, file_path: &Path) -> io::Result<Option<GameFile>> {
        match self {
            OpenSearchPath::Vpk(vpk) => vpk.open_file(&file_path.0).map_or_else(
                |e| {
                    if e.kind() == io::ErrorKind::NotFound {
                        Ok(None)
                    } else {
                        Err(e)
                    }
                },
                |f| Ok(Some(GameFile::Vpk(f))),
            ),
            OpenSearchPath::Directory(path) => fs::File::open(path.join(&file_path.0)).map_or_else(
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
        Err(io::Error::new(io::ErrorKind::NotFound, &file_path.0))
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
}

#[derive(Debug)]
pub enum GameFile<'a> {
    Fs(fs::File),
    Vpk(vpk::File<'a>),
}

impl<'a> GameFile<'a> {
    fn size(&self) -> Option<usize> {
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

    #[test]
    fn test_gameinfo_deserialization() {
        let game_info = vdf::from_str::<GameInfoFile>(
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
        .game_info;

        assert_eq!(
            game_info,
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
        )
    }
}