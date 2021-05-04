use std::{
    fs, io,
    path::{Path, PathBuf},
    slice::Iter,
};

use crate::vdf;

use serde::{
    de::{IgnoredAny, MapAccess, Visitor},
    Deserialize,
};
use serde_derive::Deserialize;
use thiserror::Error;

fn is_acf_file(filename: &str) -> bool {
    filename
        .rsplit('.')
        .next()
        .map(|ext| ext.eq_ignore_ascii_case("acf"))
        == Some(true)
}

#[derive(Debug, PartialEq, Deserialize)]
struct AppManifest {
    #[serde(rename = "AppState")]
    pub app_state: AppState,
}

#[derive(Debug, PartialEq, Deserialize)]
#[serde(case_insensitive)]
struct AppState {
    #[serde(rename = "appid")]
    pub app_id: u32,
    pub name: String,
    #[serde(rename = "installdir")]
    pub install_dir: PathBuf,
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

#[derive(Debug, PartialEq, Deserialize)]
struct LibraryFoldersFile {
    #[serde(rename = "LibraryFolders")]
    pub library_folders: LibraryFolders,
}

#[derive(Debug, PartialEq)]
struct LibraryFolders(Libraries);

impl<'de> Deserialize<'de> for LibraryFolders {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct LibraryFoldersKey;

        impl<'de> Deserialize<'de> for LibraryFoldersKey {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                struct LibraryFoldersKeyVisitor;

                impl<'de> Visitor<'de> for LibraryFoldersKeyVisitor {
                    type Value = LibraryFoldersKey;

                    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                        formatter.write_str("an int")
                    }

                    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
                    where
                        E: serde::de::Error,
                    {
                        v.parse::<u64>().map_or_else(
                            |_| {
                                Err(serde::de::Error::invalid_type(
                                    serde::de::Unexpected::Str(v),
                                    &Self,
                                ))
                            },
                            |_| Ok(LibraryFoldersKey),
                        )
                    }
                }

                deserializer.deserialize_str(LibraryFoldersKeyVisitor)
            }
        }

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

                while let Some(key) = if let Ok(key) = map.next_key::<LibraryFoldersKey>() {
                    key.map(Some)
                } else {
                    Some(None)
                } {
                    if key.is_some() {
                        library_folders.push(map.next_value()?);
                    } else {
                        map.next_value::<IgnoredAny>()?;
                    }
                }

                Ok(LibraryFolders(Libraries {
                    paths: library_folders,
                }))
            }
        }

        deserializer.deserialize_map(LibraryFoldersVisitor)
    }
}

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
pub enum AppError {
    #[error("io error: {0}")]
    Io(#[from] io::Error),
    #[error("error deserializing appmanifest: {0}")]
    Deserialization(#[from] vdf::Error),
}

/// A steam app. `install_dir` is absolute.
#[derive(Debug, PartialEq)]
pub struct App {
    pub app_id: u32,
    pub name: String,
    pub install_dir: PathBuf,
}

/// A list of steam's libraries.
#[derive(Debug, PartialEq)]
pub struct Libraries {
    pub paths: Vec<PathBuf>,
}

impl Libraries {
    #[must_use]
    pub fn new(paths: Vec<PathBuf>) -> Self {
        Self { paths }
    }

    /// Discover local Steam libraries.
    /// Steam needs to be installed.
    ///
    /// # Errors
    ///
    /// Returns [`Err`] if the libraryfolders.vdf read fails or the deserialization fails.
    /// On Windows, also returns [`Err`] if Steam's registry entries can't be read.
    /// On other platforms, also returns [`Err`] if the home directroy can't be determined.
    pub fn discover() -> Result<Self, LibraryDiscoveryError> {
        Self::discover_impl()
    }

    #[cfg(windows)]
    fn discover_impl() -> Result<Self, LibraryDiscoveryError> {
        use winreg::{enums::HKEY_CURRENT_USER, RegKey};

        let hkcu = RegKey::predef(HKEY_CURRENT_USER);
        let steam = hkcu.open_subkey("SOFTWARE\\Valve\\Steam")?;
        let steam_path: String = steam.get_value("SteamPath")?;
        Self::discover_from_steam_path(Path::new(&steam_path))
    }

    #[cfg(all(unix, not(target_os = "macos")))]
    fn discover_impl() -> Result<Self, LibraryDiscoveryError> {
        use home::home_dir;

        let steam_path = home_dir()
            .ok_or(LibraryDiscoveryError::NoHome)?
            .join(".steam")
            .join("root");
        let steam_path = fs::read_link(steam_path)?;
        Self::discover_from_steam_path(steam_path)
    }

    #[cfg(target_os = "macos")]
    fn discover_impl() -> Result<Self, LibraryDiscoveryError> {
        use home::home_dir;

        let steam_path = home_dir()
            .ok_or(LibraryDiscoveryError::NoHome)?
            .join("Library")
            .join("Application Support")
            .join("Steam");
        Self::discover_from_steam_path(steam_path)
    }

    fn discover_from_steam_path<P: AsRef<Path>>(
        steam_path: P,
    ) -> Result<Self, LibraryDiscoveryError> {
        let steam_path = steam_path.as_ref();
        let libraryfolders_path = steam_path.join("steamapps").join("libraryfolders.vdf");

        let mut libraries =
            vdf::escaped_from_str::<LibraryFoldersFile>(&fs::read_to_string(libraryfolders_path)?)?
                .library_folders
                .0;
        libraries.paths.push(steam_path.to_path_buf());

        Ok(libraries)
    }

    /// Returns an iterator over apps in all libraries.
    #[must_use]
    pub fn apps(&self) -> Apps {
        Apps {
            paths: self.paths.iter(),
            current_path: None,
        }
    }
}

/// Iterator over apps in libraries.
///
/// # Errors
///
/// The [`Result`] will be an [`Err`] if a library directory read fails,
/// an appmanifest can't be read or the appmanifest deserialization fails.
pub struct Apps<'a> {
    paths: Iter<'a, PathBuf>,
    current_path: Option<(PathBuf, fs::ReadDir)>,
}

impl<'a> Iterator for Apps<'a> {
    type Item = Result<App, AppError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some((current_path, current_iter)) = &mut self.current_path {
                for entry in current_iter {
                    let entry = match entry {
                        Ok(entry) => entry,
                        Err(e) => return Some(Err(e.into())),
                    };
                    if !entry.file_type().map_or(false, |t| t.is_file()) {
                        continue;
                    }
                    if let Some(filename) = entry.file_name().to_str() {
                        if !filename.starts_with("appmanifest_") || !is_acf_file(filename) {
                            continue;
                        }
                        return Some(
                            fs::read_to_string(entry.path())
                                .map_err(AppError::from)
                                .and_then(|s| {
                                    vdf::from_str::<AppManifest>(&s).map_err(AppError::from)
                                })
                                .map(|m| m.app_state.into_app(&current_path)),
                        );
                    }
                }
            }
            let steamapps_path = self.paths.next()?.join("steamapps");
            match fs::read_dir(&steamapps_path) {
                Ok(iter) => {
                    self.current_path = Some((steamapps_path, iter));
                }
                Err(e) => return Some(Err(e.into())),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_app_manifest_deserialization() {
        let app_state = vdf::from_str::<AppManifest>(
            r#"
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
        "#,
        )
        .unwrap()
        .app_state;

        assert_eq!(
            app_state,
            AppState {
                app_id: 440,
                name: "Team Fortress 2".to_string(),
                install_dir: "Team Fortress 2".into(),
            }
        );
    }

    #[test]
    fn test_libraryfolders_deserialization() {
        let libraryfolders = vdf::escaped_from_str::<LibraryFoldersFile>(
            r#"
        "LibraryFolders"
        {
        	"TimeNextStatsReport"		"1619642796"
        	"ContentStatsID"		"3393887322297456883"
        	"1"		"D:\\Games\\Steam"
        	"2"		"E:\\Games\\Steam"
        	"3"		"F:\\Games\\Steam"
        }
        "#,
        )
        .unwrap()
        .library_folders;

        assert_eq!(
            libraryfolders,
            LibraryFolders(Libraries {
                paths: vec![
                    "D:\\Games\\Steam".into(),
                    "E:\\Games\\Steam".into(),
                    "F:\\Games\\Steam".into()
                ],
            })
        )
    }

    /// Fails if steam is not installed
    #[test]
    #[ignore]
    fn test_library_discovery() {
        let libraries = Libraries::discover().unwrap();
        eprintln!("discovered libraries: {:?}", libraries.paths);
        let apps: Vec<App> = libraries.apps().map(Result::unwrap).collect();
        eprintln!("discovered apps: {:?}", apps);
    }
}
