use std::{
    fs, io,
    path::{Path, PathBuf},
};

use crate::vdf;

use serde::{
    de::{MapAccess, Visitor},
    Deserialize,
};
use serde_derive::Deserialize;
use thiserror::Error;
use uncased::UncasedStr;

pub mod discovery;

/// Stores Source games' filesystems.
#[derive(Debug, PartialEq)]
pub struct GameManager {
    pub games: Vec<Game>,
}

fn is_vpk_file(filename: &str) -> bool {
    filename
        .rsplit('.')
        .next()
        .map(|ext| ext.eq_ignore_ascii_case("vpk"))
        == Some(true)
}

#[derive(Debug, Error)]
pub enum GameReadError {
    #[error("io error: {0}")]
    Io(#[from] io::Error),
    #[error("error deserializing gameinfo.txt: {0}")]
    Deserialization(#[from] vdf::Error),
}

/// Represents a Source game's filesystem
#[derive(Debug, PartialEq)]
pub struct Game {
    pub name: String,
    pub vpk_files: Vec<PathBuf>,
    pub directories: Vec<PathBuf>,
    pub wildcard_directories: Vec<PathBuf>,
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

    #[must_use]
    #[allow(clippy::missing_panics_doc)]
    pub fn from_game_info(
        game_info: GameInfo,
        game_info_directory: &Path,
        root_path: &Path,
    ) -> Self {
        let mut vpk_files = Vec::new();
        let mut directories = Vec::new();
        let mut wildcard_directories = Vec::new();

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
                path.as_str().into()
            };

            if let Some(file_name) = path.file_name() {
                if let Some(file_name) = file_name.to_str() {
                    if is_vpk_file(file_name) {
                        vpk_files.push(path);
                    } else if file_name == "*" {
                        // unwrap cannot panic since path with a filename also has a parent
                        wildcard_directories.push(path.parent().unwrap().into());
                    } else {
                        directories.push(path);
                    }
                }
            } else {
                directories.push(path);
            }
        }

        Self {
            name: game_info.game,
            vpk_files,
            directories,
            wildcard_directories,
        }
    }
}

#[derive(Debug, PartialEq, Deserialize)]
#[serde(case_insensitive)]
pub struct GameInfoFile {
    #[serde(rename = "gameinfo")]
    pub game_info: GameInfo,
}

#[derive(Debug, PartialEq, Deserialize)]
#[serde(case_insensitive)]
pub struct GameInfo {
    pub game: String,
    #[serde(rename = "filesystem")]
    pub file_system: GameInfoFileSystem,
}

#[derive(Debug, PartialEq, Deserialize)]
#[serde(case_insensitive)]
pub struct GameInfoFileSystem {
    #[serde(rename = "steamappid")]
    pub steam_app_id: u32,
    #[serde(rename = "toolsappid")]
    pub tools_app_id: Option<u32>,
    #[serde(rename = "searchpaths")]
    pub search_paths: GameInfoSearchPaths,
}

#[derive(Debug, PartialEq)]
pub struct GameInfoSearchPaths {
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

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
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
