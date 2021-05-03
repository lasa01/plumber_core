use std::{ffi::OsStr, fmt::Formatter, fs, io::{self, Read, Seek}, path::{Path, PathBuf}};

use crate::{vdf, vpk};

use serde::{
    de::{MapAccess, Visitor},
    Deserialize,
};
use serde_derive::Deserialize;
use thiserror::Error;
use uncased::UncasedStr;

pub mod discovery;

#[derive(Debug, PartialEq, Deserialize)]
#[serde(case_insensitive)]
struct GameInfoFile {
    #[serde(rename = "gameinfo")]
    pub game_info: GameInfo,
}

#[derive(Debug, PartialEq, Deserialize)]
#[serde(case_insensitive)]
struct GameInfo {
    pub game: String,
    #[serde(rename = "filesystem")]
    pub file_system: GameInfoFileSystem,
}

#[derive(Debug, PartialEq, Deserialize)]
#[serde(case_insensitive)]
struct GameInfoFileSystem {
    #[serde(rename = "steamappid")]
    pub steam_app_id: u32,
    #[serde(rename = "toolsappid")]
    pub tools_app_id: Option<u32>,
    #[serde(rename = "searchpaths")]
    pub search_paths: GameInfoSearchPaths,
}

#[derive(Debug, PartialEq)]
struct GameInfoSearchPaths {
    pub game_search_paths: Vec<String>,
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
pub struct GameManager {
    pub games: Vec<Game>,
}

#[derive(Debug, Error)]
pub enum GameReadError {
    #[error("io error: {0}")]
    Io(#[from] io::Error),
    #[error("error deserializing gameinfo.txt: {0}")]
    Deserialization(#[from] vdf::Error),
}

#[derive(Debug, Error)]
pub enum GameOpenError {
    #[error("io error: {0}")]
    Io(#[from] io::Error),
    #[error("error reading vpk: {0}")]
    Vpk(#[from] vpk::DirectoryReadError),
}

/// A search path in a [`Game`].
#[derive(Debug, PartialEq)]
pub enum SearchPath {
    Vpk(PathBuf),
    Directory(PathBuf),
    Wildcard(PathBuf),
}

/// Represents a Source game's filesystem.
#[derive(Debug)]
pub struct Game {
    pub name: String,
    pub search_paths: Vec<SearchPath>,
}

impl Game {
    /// # Errors
    ///
    /// Returns `Err` if the gameinfo.txt read fails or the deserialization fails.
    ///
    /// # Panics
    ///
    /// Panics if `game_info_path` has no parent.
    pub fn from_paths<P: AsRef<Path>>(
        root_path: P,
        game_info_path: P,
    ) -> Result<Self, GameReadError> {
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

    fn from_game_info(game_info: GameInfo, game_info_directory: &Path, root_path: &Path) -> Self {
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
                    search_paths.push(SearchPath::Vpk(path));
                } else if file_name == "*" {
                    if let Some(parent) = path.parent() {
                        search_paths.push(SearchPath::Wildcard(parent.into()));
                    }
                } else {
                    search_paths.push(SearchPath::Directory(path))
                }
            }
        }

        Self {
            name: game_info.game,
            search_paths,
        }
    }

    /// Opens the game's filesystem.
    /// Opens all vpk archives, verifies that search directories exist and reads wildcard directories' contents.
    /// 
    /// # Errors
    ///
    /// Returns `Err` if a search path doesn't exist or a vpk archive can't be opened.
    pub fn open(&self) -> Result<OpenGame, GameOpenError> {
        let mut open_search_paths = Vec::new();
        for search_path in &self.search_paths {
            match search_path {
                SearchPath::Vpk(path) => {
                    open_search_paths.push(OpenSearchPath::Vpk(vpk::Directory::read(path)?));
                }
                SearchPath::Directory(path) => {
                    if !path.is_dir() {
                        return Err(GameOpenError::Io(io::Error::new(io::ErrorKind::NotFound, path.to_string_lossy())));
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
        Ok(OpenGame {
            search_paths: open_search_paths,
        })
    }
}

#[derive(Debug)]
enum OpenSearchPath {
    Vpk(vpk::Directory),
    Directory(PathBuf),
}

impl OpenSearchPath {
    fn try_open_file(&self, file_path: &str) -> io::Result<Option<GameFile>> {
        match self {
            OpenSearchPath::Vpk(vpk) => {
                vpk
                    .open_file(file_path)
                    .map(|f| Some(GameFile::Vpk(f)))
                    .or_else(|e| 
                        if e.kind() == io::ErrorKind::NotFound {
                            Ok(None)
                        } else {
                            Err(e)
                        }
                    )
            }
            OpenSearchPath::Directory(path) => {
                fs::File::open(path.join(file_path))
                    .map(|f| Some(GameFile::Fs(f)))
                    .or_else(|e| 
                        if e.kind() == io::ErrorKind::NotFound {
                            Ok(None)
                        } else {
                            Err(e)
                        }
                    )
            }
        }
    }
}

/// An open Source game's filesystem, ready to be read.
#[derive(Debug)]
pub struct OpenGame {
    search_paths: Vec<OpenSearchPath>
}

impl OpenGame {
    /// Opens the specified file if it exists.
    ///
    /// # Errors
    ///
    /// Returns `Err` if `file_path` doesn't exist or if the file can't be opened.
    pub fn open_file(&self, file_path: &str) -> io::Result<GameFile> {
        for path in &self.search_paths {
            if let Some(file) = path.try_open_file(file_path)? {
                return Ok(file);
            }
        }
        Err(io::Error::new(io::ErrorKind::NotFound, file_path))
    }
}

#[derive(Debug)]
pub enum GameFile<'a> {
    Fs(fs::File),
    Vpk(vpk::File<'a>),
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
