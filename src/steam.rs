use std::{
    ffi::OsStr,
    fs, io,
    path::{Path, PathBuf},
};

use crate::vdf;

use serde::{
    de::{IgnoredAny, MapAccess, Visitor},
    Deserialize,
};
use serde_derive::Deserialize;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum LibraryDiscoveryError {
    #[error("io error: {0}")]
    Io(#[from] io::Error),
    #[error("error deserializing libraryfolders.vdf: {0}")]
    Deserialization(#[from] vdf::Error),
    #[error("home directory is unknown")]
    NoHome,
}

#[derive(Debug, Error)]
pub enum AppDiscoveryError {
    #[error("io error: {0}")]
    Io(#[from] io::Error),
    #[error("error deserializing appmanifest: {0}")]
    Deserialization(#[from] vdf::Error),
}

fn is_acf_file(filename: &str) -> bool {
    filename
        .rsplit('.')
        .next()
        .map(|ext| ext.eq_ignore_ascii_case("acf"))
        == Some(true)
}

/// A steam app manifest holding a [`AppState`].
#[derive(Debug, PartialEq, Deserialize)]
pub struct AppManifest {
    #[serde(rename = "AppState")]
    app_state: AppState,
}

/// A steam app state. `install_dir` is relative to a library.
#[derive(Debug, PartialEq, Deserialize)]
#[serde(case_insensitive)]
pub struct AppState {
    #[serde(rename = "appid")]
    app_id: i32,
    name: String,
    #[serde(rename = "installdir")]
    install_dir: PathBuf,
}

impl AppState {
    pub fn into_app<P: AsRef<Path>>(self, steamapps_folder: P) -> App {
        App {
            app_id: self.app_id,
            name: self.name,
            install_dir: steamapps_folder
                .as_ref()
                .join("common")
                .join(self.install_dir),
        }
    }
}

#[derive(Debug, PartialEq)]
/// A steam app. Unlike [`AppState`], `install_dir` is absolute.
pub struct App {
    pub app_id: i32,
    pub name: String,
    pub install_dir: PathBuf,
}

#[derive(Debug, PartialEq, Deserialize)]
pub struct LibraryFoldersFile {
    #[serde(rename = "LibraryFolders")]
    library_folders: LibraryFolders,
}

#[derive(Debug, PartialEq)]
pub struct LibraryFolders {
    paths: Vec<PathBuf>,
}

impl LibraryFolders {
    #[must_use]
    pub fn new(library_folders: Vec<PathBuf>) -> Self {
        Self { paths: library_folders }
    }

    #[cfg(windows)]
    /// # Errors
    ///
    /// Returns `Err` if the registry read fails, the libraryfolders.vdf read fails or the deserialization fails.
    pub fn discover() -> Result<Self, LibraryDiscoveryError> {
        use winreg::{enums::HKEY_CURRENT_USER, RegKey};

        let hkcu = RegKey::predef(HKEY_CURRENT_USER);
        let steam = hkcu.open_subkey("SOFTWARE\\Valve\\Steam")?;
        let steam_path: String = steam.get_value("SteamPath")?;
        Self::discover_from_steam_path(Path::new(&steam_path))
    }

    #[cfg(all(unix, not(target_os = "macos")))]
    /// # Errors
    ///
    /// Returns `Err` if the home directory is unknown, steam is not found, the libraryfolders.vdf read fails or the deserialization fails.
    pub fn discover() -> Result<Self, LibraryDiscoveryError> {
        use home::home_dir;

        let steam_path = home_dir().ok_or(LibraryDiscoveryError::NoHome)?.join(".steam").join("root");
        let steam_path = fs::read_link(steam_path)?;
        Self::discover_from_steam_path(steam_path)
    }

    #[cfg(target_os = "macos")]
    /// # Errors
    ///
    /// Returns `Err` if the home directory is unknown, the libraryfolders.vdf read fails or the deserialization fails.
    pub fn discover() -> Result<Self, LibraryDiscoveryError> {
        use home::home_dir;

        let steam_path = home_dir().ok_or(LibraryDiscoveryError::NoHome)?.join("Library").join("Application Support").join("Steam");
        Self::discover_from_steam_path(steam_path)
    }

    fn discover_from_steam_path<P: AsRef<Path>>(steam_path: P) -> Result<Self, LibraryDiscoveryError> {
        let steam_path = steam_path.as_ref();
        let libraryfolders_path = steam_path.join("steamapps").join("libraryfolders.vdf");

        let mut libraryfolders =
            vdf::from_str::<LibraryFoldersFile>(&fs::read_to_string(libraryfolders_path)?)?.library_folders;
        libraryfolders.paths.push(steam_path.to_path_buf());

        Ok(libraryfolders)
    }

    /// # Errors
    ///
    /// Returns `Err` if a library folder read fails, an appmanifest read fails or a deserialization fails.
    pub fn discover_apps(&self) -> Result<Vec<App>, AppDiscoveryError> {
        let mut apps = Vec::new();
        for library_folder in &self.paths {
            let steamapps_path = library_folder.join("steamapps");
            for entry in fs::read_dir(&steamapps_path)? {
                let path = entry?.path();
                if !path.is_file() {
                    continue;
                }
                if let Some(Some(filename)) = path.file_name().map(OsStr::to_str) {
                    if !filename.starts_with("appmanifest_") || !is_acf_file(filename) {
                        continue;
                    }
                    apps.push(
                        vdf::from_str::<AppManifest>(&fs::read_to_string(path)?)?
                            .app_state
                            .into_app(&steamapps_path),
                    );
                }
            }
        }
        Ok(apps)
    }
}

impl<'de> Deserialize<'de> for LibraryFolders {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct LibraryFoldersVisitor;

        impl<'de> Visitor<'de> for LibraryFoldersVisitor {
            type Value = LibraryFolders;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("class LibraryFolders")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let mut library_folders = Vec::new();

                while let Some(key) = map.next_key::<&str>()? {
                    if key.parse::<u64>().is_ok() {
                        library_folders.push(map.next_value()?);
                    } else {
                        map.next_value::<IgnoredAny>()?;
                    }
                }

                Ok(LibraryFolders { paths: library_folders })
            }
        }

        deserializer.deserialize_map(LibraryFoldersVisitor)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_app_manifest_deserialization() {
        let app_state = vdf::from_str::<AppManifest>(r#"
        "AppState"
        {
            "appid"		"440"
            "Universe"		"1"
            "LauncherPath"		"C:\\Program Files (x86)\\Steam\\steam.exe"
            "name"		"Team Fortress 2"
            "StateFlags"		"1542"
            "installdir"		"Team Fortress 2"
            "LastUpdated"		"1569517103"
            "UpdateResult"		"12"
            "SizeOnDisk"		"22950744170"
            "buildid"		"4226121"
            "LastOwner"		"0"
            "BytesToDownload"		"1612410080"
            "BytesDownloaded"		"0"
            "BytesToStage"		"11233613113"
            "BytesStaged"		"0"
            "AutoUpdateBehavior"		"0"
            "AllowOtherDownloadsWhileRunning"		"0"
            "ScheduledAutoUpdate"		"0"
            "InstalledDepots"
            {
                "441"
                {
                    "manifest"		"7381680709773015636"
                    "size"		"0"
                }
                "440"
                {
                    "manifest"		"1118032470228587934"
                    "size"		"0"
                }
                "232251"
                {
                    "manifest"		"1678072318420789394"
                    "size"		"0"
                }
            }
            "SharedDepots"
            {
                "228990"		"228980"
            }
            "UserConfig"
            {
                "language"		"english"
                "betakey"		""
            }
        }
        "#).unwrap().app_state;

        assert_eq!(app_state, AppState {
            app_id: 440,
            name: "Team Fortress 2".to_string(),
            install_dir: "Team Fortress 2".into(),
        });
    }

    #[test]
    fn test_libraryfolders_deserialization() {
        let libraryfolders = vdf::from_str::<LibraryFoldersFile>(r#"
        "LibraryFolders"
        {
        	"TimeNextStatsReport"		"1619642796"
        	"ContentStatsID"		"3393887322297456883"
        	"1"		"D:\\Games\\Steam"
        	"2"		"E:\\Games\\Steam"
        	"3"		"F:\\Games\\Steam"
        }
        "#).unwrap().library_folders;

        assert_eq!(libraryfolders, LibraryFolders {
            paths: vec!["D:\\\\Games\\\\Steam".into(), "E:\\\\Games\\\\Steam".into(), "F:\\\\Games\\\\Steam".into()],
        })
    }

    #[test]
    #[ignore]
    fn test_library_discovery() {
        let library_folders = LibraryFolders::discover().unwrap();
        eprintln!("discovered libraries: {:?}", library_folders.paths);
        let apps = library_folders.discover_apps().unwrap();
        eprintln!("discovered apps: {:?}", apps);
    }
}
