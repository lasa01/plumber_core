use std::{
    collections::BTreeSet,
    fs, io,
    path::{Path, PathBuf},
    slice::Iter,
};

use log::warn;
use plumber_vdf as vdf;

use itertools::Itertools;
use serde::{
    de::{IgnoredAny, MapAccess, Visitor},
    Deserialize,
};
use thiserror::Error;

static SOURCE_APPS: [u32; 90] = [
    219, 220, 240, 260, 280, 300, 320, 340, 360, 380, 400, 410, 420, 440, 500, 550, 570, 590, 620,
    630, 730, 1300, 1800, 2100, 2120, 2130, 2400, 2430, 2450, 2600, 4000, 17500, 17510, 17520,
    17530, 17550, 17570, 17580, 17700, 17710, 17730, 17740, 17750, 90007, 222_880, 224_260,
    235_780, 238_430, 252_530, 261_820, 261_980, 265_630, 270_370, 280_740, 286_080, 287_820,
    290_930, 313_240, 317_360, 317_400, 317_790, 334_370, 346_290, 346_330, 349_480, 353_220,
    362_890, 397_680, 433_970, 440_000, 447_820, 563_560, 587_650, 601_360, 628_410, 638_800,
    669_270, 747_250, 869_480, 6_626_680, 1_054_600, 1_057_700, 1_104_390, 1_117_390, 1_154_130,
    1_255_980, 1_341_060, 1_367_890, 1_372_780, 1_389_950,
];

fn is_acf_file(filename: &str) -> bool {
    filename
        .rsplit('.')
        .next()
        .map(|ext| ext.eq_ignore_ascii_case("acf"))
        == Some(true)
}

#[derive(Debug, PartialEq, Deserialize)]
#[serde(case_insensitive)]
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
#[serde(case_insensitive)]
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
                let mut library_folders: Vec<LibraryFolder> = Vec::new();

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
                    paths: library_folders.into_iter().map(|f| f.path).collect(),
                }))
            }
        }

        deserializer.deserialize_map(LibraryFoldersVisitor)
    }
}

#[derive(Debug, PartialEq, Deserialize)]
#[serde(case_insensitive)]
struct LibraryFolder {
    path: PathBuf,
}

#[derive(Debug, Error)]
pub enum LibraryDiscoveryError {
    #[error("io error reading `{path}`: {inner}")]
    Io { path: String, inner: io::Error },
    #[error("error deserializing libraryfolders.vdf: {0}")]
    Deserialization(#[from] vdf::Error),
    #[error("home directory is unknown")]
    NoHome,
}

impl LibraryDiscoveryError {
    fn from_io(err: io::Error, path: &Path) -> Self {
        Self::Io {
            path: path.as_os_str().to_string_lossy().into_owned(),
            inner: err,
        }
    }
}

#[derive(Debug, Error)]
pub enum AppError {
    #[error("io error reading `{path}`: {inner}")]
    Io { path: String, inner: io::Error },
    #[error("error deserializing appmanifest: {0}")]
    Deserialization(#[from] vdf::Error),
}

impl AppError {
    fn from_io(err: io::Error, path: &Path) -> Self {
        Self::Io {
            path: path.as_os_str().to_string_lossy().into_owned(),
            inner: err,
        }
    }
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
        let steam = hkcu.open_subkey("SOFTWARE\\Valve\\Steam").map_err(|err| {
            LibraryDiscoveryError::Io {
                inner: err,
                path: "registry HKEY_CURRENT_USER\\SOFTWARE\\Valve\\Steam".to_string(),
            }
        })?;
        let steam_path: String =
            steam
                .get_value("SteamPath")
                .map_err(|err| LibraryDiscoveryError::Io {
                    inner: err,
                    path: "registry HKEY_CURRENT_USER\\SOFTWARE\\Valve\\Steam\\SteamPath"
                        .to_string(),
                })?;
        Self::discover_from_steam_path(Path::new(&steam_path))
    }

    #[cfg(all(unix, not(target_os = "macos")))]
    fn discover_impl() -> Result<Self, LibraryDiscoveryError> {
        use home::home_dir;

        let steam_path = home_dir()
            .ok_or(LibraryDiscoveryError::NoHome)?
            .join(".steam")
            .join("root");
        let steam_path = fs::read_link(&steam_path)
            .map_err(|err| LibraryDiscoveryError::from_io(err, &steam_path))?;
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

        let mut libraries = vdf::escaped_from_str::<LibraryFoldersFile>(
            &fs::read_to_string(&libraryfolders_path)
                .map_err(|err| LibraryDiscoveryError::from_io(err, &libraryfolders_path))?,
        )?
        .library_folders
        .0;

        let mut normalized_paths = libraries
            .paths
            .iter()
            .filter_map(|p| match p.canonicalize() {
                Ok(p) => Some(p),
                Err(err) => {
                    warn!(
                        "error reading steam library folder `{}`: {}",
                        p.display(),
                        err
                    );
                    None
                }
            });

        let normalized_steam_path = steam_path
            .canonicalize()
            .map_err(|err| LibraryDiscoveryError::from_io(err, steam_path))?;

        if !normalized_paths.contains(&normalized_steam_path) {
            libraries.paths.push(steam_path.to_path_buf());
        }

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
#[derive(Debug)]
pub struct Apps<'a> {
    paths: Iter<'a, PathBuf>,
    current_path: Option<(PathBuf, fs::ReadDir)>,
}

impl<'a> Apps<'a> {
    /// Filter the iterator to only return Source apps based on a bundled set of Source app ids.
    #[must_use]
    pub fn source(self) -> SourceApps<'a> {
        SourceApps {
            apps: self,
            source_app_ids: SOURCE_APPS.into(),
        }
    }

    /// Filter the iterator to only return Source apps based on a custom set of Source app ids.
    #[must_use]
    pub fn defined_source(self, source_app_ids: BTreeSet<u32>) -> SourceApps<'a> {
        SourceApps {
            apps: self,
            source_app_ids,
        }
    }
}

impl<'a> Iterator for Apps<'a> {
    type Item = Result<App, AppError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some((current_path, current_iter)) = &mut self.current_path {
                for entry in current_iter {
                    let entry = match entry {
                        Ok(entry) => entry,
                        Err(err) => return Some(Err(AppError::from_io(err, current_path))),
                    };
                    if !entry.file_type().map_or(false, |t| t.is_file()) {
                        continue;
                    }
                    if let Some(filename) = entry.file_name().to_str() {
                        if !filename.starts_with("appmanifest_") || !is_acf_file(filename) {
                            continue;
                        }
                        let path = entry.path();
                        return Some(
                            fs::read_to_string(&path)
                                .map_err(|err| AppError::from_io(err, &path))
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
                Err(err) => return Some(Err(AppError::from_io(err, &steamapps_path))),
            }
        }
    }
}

/// Iterator over Source apps in libraries.
/// Apps' ids are checked against a static set of known Source ids and filtered.
///
/// # Errors
///
/// The [`Result`] will be an [`Err`] if a library directory read fails,
/// an appmanifest can't be read or the appmanifest deserialization fails.
#[derive(Debug)]
pub struct SourceApps<'a> {
    apps: Apps<'a>,
    source_app_ids: BTreeSet<u32>,
}

impl<'a> Iterator for SourceApps<'a> {
    type Item = Result<App, AppError>;

    fn next(&mut self) -> Option<Self::Item> {
        for result in &mut self.apps {
            if result
                .as_ref()
                .map_or(true, |app| self.source_app_ids.contains(&app.app_id))
            {
                return Some(result);
            }
        }
        None
    }
}

impl<'a> SourceApps<'a> {
    /// Parse the filesystems from the Source apps.
    #[must_use]
    pub fn filesystems(self) -> FileSystems<'a> {
        FileSystems(self)
    }
}

/// Iterator over Source apps' filesystems in libraries.
/// Apps' ids are checked against a static set of known Source ids and filtered.
///
/// # Errors
///
/// The [`Result`] will be an [`Err`] if the library directory read fails,
/// the appmanifest can't be read, the appmanifest deserialization fails,
/// the gameinfo.txt read fails or the gameinfo deserialization fails.
#[derive(Debug)]
pub struct FileSystems<'a>(SourceApps<'a>);

impl<'a> Iterator for FileSystems<'a> {
    type Item = Result<crate::fs::FileSystem, crate::fs::ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|result| {
            result.map_or_else(
                |e| Err(crate::fs::ParseError::from(e)),
                |a| crate::fs::FileSystem::from_app(&a),
            )
        })
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
            "libraryfolders"
            {
                "contentstatsid"		"3393887322297456883"
                "0"
                {
                    "path"		"C:\\Program Files (x86)\\Steam"
                    "label"		""
                    "contentid"		"3393887322297456883"
                    "totalsize"		"0"
                    "update_clean_bytes_tally"		"476447588"
                    "time_last_update_corruption"		"0"
                    "apps"
                    {
                        "211"		"2173258931"
                    }
                }
                "1"
                {
                    "path"		"D:\\Games\\Steam"
                    "label"		""
                    "contentid"		"2825139553531466896"
                    "totalsize"		"1000068870144"
                    "update_clean_bytes_tally"		"40799269"
                    "time_last_update_corruption"		"0"
                    "apps"
                    {
                        "215"		"2811981136"
                    }
                }
            }            
        "#,
        )
        .unwrap()
        .library_folders;

        assert_eq!(
            libraryfolders,
            LibraryFolders(Libraries {
                paths: vec![
                    "C:\\Program Files (x86)\\Steam".into(),
                    "D:\\Games\\Steam".into(),
                ],
            })
        );
    }
}
