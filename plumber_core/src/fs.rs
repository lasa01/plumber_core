use std::{
    borrow::Cow,
    ffi::OsStr,
    fmt::{self, Debug, Display, Formatter},
    fs::{self, FileType},
    io::{self, Read, Seek},
    path::{Path as StdPath, PathBuf as StdPathBuf},
    slice, collections::BTreeSet,
};

use log::{debug, warn};
use plumber_uncased::{AsUncased, UncasedStr};
use plumber_vdf as vdf;
use plumber_vpk as vpk;
use vpk::DirectoryReadError;
pub use vpk::{Path as GamePath, PathBuf as GamePathBuf};

use crate::steam;

use serde::{
    de::{MapAccess, Visitor},
    Deserialize,
};
use thiserror::Error;

/// A borrowed path to import an asset from.
/// Can be either a `Game` path to import assets from the game file system,
/// or an `Os` path to import assets from the os file system.
#[derive(Debug, PartialEq, Clone, Copy, Eq, Hash)]
pub enum Path<'a> {
    Game(&'a GamePath),
    Os(&'a StdPath),
}

impl<'a> Path<'a> {
    pub fn with_extension(self, extension: impl AsRef<str>) -> PathBuf {
        match self {
            Path::Game(p) => PathBuf::Game(p.with_extension(extension)),
            Path::Os(p) => PathBuf::Os(p.with_extension(extension.as_ref())),
        }
    }

    /// Makes sure game paths have the provided extension,
    /// does nothing for os paths since they may have non-default extension.
    #[must_use]
    pub fn ensure_extension(self, extension: impl AsRef<str>) -> PathBuf {
        match self {
            Path::Game(p) => PathBuf::Game(p.with_extension(extension)),
            Path::Os(p) => PathBuf::Os(p.to_path_buf()),
        }
    }
}

impl PartialEq<str> for Path<'_> {
    fn eq(&self, other: &str) -> bool {
        match self {
            Path::Game(p) => *p == other,
            Path::Os(p) => p.as_os_str() == other,
        }
    }
}

impl Display for Path<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Path::Game(p) => Display::fmt(&p, f),
            Path::Os(p) => Display::fmt(&p.to_string_lossy(), f),
        }
    }
}

impl<'a> From<&'a PathBuf> for Path<'a> {
    fn from(p: &'a PathBuf) -> Self {
        match p {
            PathBuf::Game(p) => Self::Game(p),
            PathBuf::Os(p) => Self::Os(p),
        }
    }
}

impl<'a> From<&'a StdPath> for Path<'a> {
    fn from(p: &'a StdPath) -> Self {
        Self::Os(p)
    }
}

impl<'a> From<&'a GamePath> for Path<'a> {
    fn from(p: &'a GamePath) -> Self {
        Self::Game(p)
    }
}

impl<'a> From<&'a GamePathBuf> for Path<'a> {
    fn from(p: &'a GamePathBuf) -> Self {
        Self::Game(p)
    }
}

impl<'a> From<&'a StdPathBuf> for Path<'a> {
    fn from(p: &'a StdPathBuf) -> Self {
        Self::Os(p)
    }
}

/// An owned path to import an asset from.
/// Can be either a `Game` path to import assets from the game file system,
/// or an `Os` path to import assets from the os file system.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PathBuf {
    Game(GamePathBuf),
    Os(StdPathBuf),
}

impl PathBuf {
    #[must_use]
    pub fn with_extension(&self, extension: impl AsRef<str>) -> PathBuf {
        match self {
            PathBuf::Game(p) => PathBuf::Game(p.with_extension(extension)),
            PathBuf::Os(p) => PathBuf::Os(p.with_extension(extension.as_ref())),
        }
    }

    /// Makes sure game paths have the provided extension,
    /// does nothing for os paths since they may have non-default extension.
    #[must_use]
    pub fn ensure_extension(&self, extension: impl AsRef<str>) -> PathBuf {
        match self {
            PathBuf::Game(p) => PathBuf::Game(p.with_extension(extension)),
            PathBuf::Os(p) => PathBuf::Os(p.clone()),
        }
    }

    /// Makes sure game paths have the provided extension,
    /// does nothing for os paths since they may have non-default extension.
    pub fn ensure_extension_mut(&mut self, extension: impl AsRef<str>) {
        if let PathBuf::Game(p) = self {
            p.set_extension(extension);
        }
    }

    pub fn set_extension(&mut self, extension: impl AsRef<str>) -> bool {
        match self {
            PathBuf::Game(p) => p.set_extension(extension),
            PathBuf::Os(p) => p.set_extension(extension.as_ref()),
        }
    }

    /// Removes the extension on game paths,
    /// doesn't touch os paths (they may have non-default extension).
    pub fn normalize_extension(&mut self) {
        if let PathBuf::Game(p) = self {
            p.set_extension("");
        }
    }

    #[must_use]
    pub fn join(&self, other: impl AsRef<GamePath>) -> PathBuf {
        let other = other.as_ref();
        match self {
            PathBuf::Game(p) => PathBuf::Game(p.join(other)),
            PathBuf::Os(p) => PathBuf::Os(p.join(other.as_str())),
        }
    }

    #[must_use]
    pub fn file_name(&self) -> Option<&str> {
        match self {
            PathBuf::Game(p) => p.file_name(),
            PathBuf::Os(p) => p.file_name().and_then(OsStr::to_str),
        }
    }

    pub fn set_file_name(&mut self, file_name: impl AsRef<str>) {
        let file_name = file_name.as_ref();

        match self {
            PathBuf::Game(p) => p.set_file_name(file_name),
            PathBuf::Os(p) => p.set_file_name(file_name),
        }
    }
}

impl From<GamePathBuf> for PathBuf {
    fn from(p: GamePathBuf) -> Self {
        Self::Game(p)
    }
}

impl From<StdPathBuf> for PathBuf {
    fn from(p: StdPathBuf) -> Self {
        Self::Os(p)
    }
}

impl PartialEq<str> for PathBuf {
    fn eq(&self, other: &str) -> bool {
        match self {
            PathBuf::Game(p) => p == other,
            PathBuf::Os(p) => p.as_os_str() == other,
        }
    }
}

impl Display for PathBuf {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            PathBuf::Game(p) => Display::fmt(p, f),
            PathBuf::Os(p) => Display::fmt(&p.to_string_lossy(), f),
        }
    }
}

#[derive(Debug, PartialEq, Deserialize, Default)]
#[serde(case_insensitive)]
struct GameInfoFile {
    #[serde(rename = "gameinfo")]
    game_info: GameInfo,
}

#[derive(Debug, PartialEq, Deserialize, Default)]
#[serde(case_insensitive)]
struct GameInfo {
    #[serde(default)]
    game: String,
    #[serde(rename = "filesystem")]
    file_system: GameInfoFileSystem,
}

#[derive(Debug, PartialEq, Deserialize, Default)]
#[serde(case_insensitive)]
struct GameInfoFileSystem {
    #[serde(rename = "steamappid")]
    steam_app_id: u32,
    #[serde(rename = "toolsappid")]
    tools_app_id: Option<u32>,
    #[serde(rename = "searchpaths")]
    search_paths: GameInfoSearchPaths,
}

#[derive(Debug, PartialEq, Default)]
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

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("io error reading `{path}`: {inner}")]
    Io { path: String, inner: io::Error },
    #[error("could not find gameinfo.txt in `{path}`")]
    NoGameInfo { path: String },
    #[error("error deserializing `{path}`: {inner}")]
    Deserialization { path: String, inner: vdf::Error },
}

impl ParseError {
    fn from_io(err: io::Error, path: &StdPath) -> Self {
        Self::Io {
            path: path.as_os_str().to_string_lossy().into_owned(),
            inner: err,
        }
    }

    fn from_vdf(err: vdf::Error, path: &StdPath) -> Self {
        Self::Deserialization {
            path: path.as_os_str().to_string_lossy().into_owned(),
            inner: err,
        }
    }
}

impl From<steam::AppError> for ParseError {
    fn from(e: steam::AppError) -> Self {
        match e {
            steam::AppError::Io { path, inner } => ParseError::Io { path, inner },
            steam::AppError::Deserialization { path, inner } => {
                ParseError::Deserialization { path, inner }
            }
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
#[derive(Debug, PartialEq, Clone)]
pub enum SearchPath {
    Vpk(StdPathBuf),
    Directory(StdPathBuf),
    Wildcard(StdPathBuf),
}

impl SearchPath {
    fn open(&self, open_search_paths: &mut Vec<OpenSearchPath>) -> Result<(), OpenError> {
        match self {
            SearchPath::Vpk(path) => open_vpk(path, open_search_paths)?,
            SearchPath::Directory(path) => open_directory(path, open_search_paths)?,
            SearchPath::Wildcard(path) => open_wildcard_dir(path, open_search_paths)?,
        }

        Ok(())
    }
}

fn open_vpk(
    path: &StdPathBuf,
    open_search_paths: &mut Vec<OpenSearchPath>,
) -> Result<(), OpenError> {
    debug!("opening vpk file `{}`", path.display());

    let alt_path = path.file_stem().map(|s| {
        let mut s = s.to_os_string();
        s.push("_dir.vpk");

        path.with_file_name(s)
    });

    match vpk::Directory::read(path).or_else(|e| {
        alt_path.map_or(Err(e), |path| {
            debug!(
                "opening failed, trying alternative path `{}`",
                path.display()
            );

            vpk::Directory::read(path)
        })
    }) {
        Ok(dir) => open_search_paths.push(OpenSearchPath::Vpk(dir)),
        Err(err) => {
            if let DirectoryReadError::Io(inner) = &err {
                if inner.kind() == io::ErrorKind::NotFound {
                    warn!(
                        "opening filesystem: vpk file `{}` not found",
                        path.to_string_lossy()
                    );

                    return Ok(());
                }
            }

            return Err(OpenError::new(path, err.into()));
        }
    }

    Ok(())
}

fn open_directory(
    path: &StdPathBuf,
    open_search_paths: &mut Vec<OpenSearchPath>,
) -> Result<(), OpenError> {
    match fs::read_dir(path) {
        Ok(readdir) => {
            debug!("reading directory `{}`", path.display());

            for entry in readdir {
                let entry = entry.map_err(|err| OpenError::new(path, err.into()))?;

                if entry
                    .file_type()
                    .map_err(|err| OpenError::new(path, err.into()))?
                    .is_file()
                    && entry.file_name().to_str() == Some("pak01_dir.vpk")
                {
                    let path = entry.path();

                    debug!("found pak01_dir.vpk at `{}`, opening", path.display());

                    open_search_paths.push(OpenSearchPath::Vpk(
                        vpk::Directory::read(&path)
                            .map_err(|err| OpenError::new(&path, err.into()))?,
                    ));
                }
            }

            open_search_paths.push(OpenSearchPath::Directory(path.clone()));
        }
        Err(err) => {
            if err.kind() == io::ErrorKind::NotFound {
                warn!(
                    "opening filesystem: directory `{}` not found",
                    path.to_string_lossy()
                );
            } else {
                return Err(OpenError::new(path, err.into()));
            }
        }
    }

    Ok(())
}

fn open_wildcard_dir(
    path: &StdPathBuf,
    open_search_paths: &mut Vec<OpenSearchPath>,
) -> Result<(), OpenError> {
    match fs::read_dir(path) {
        Ok(readdir) => {
            debug!("reading wildcard directory `{}`", path.display());

            for entry in readdir {
                let entry = entry.map_err(|err| OpenError::new(path, err.into()))?;

                let file_type = entry
                    .file_type()
                    .map_err(|err| OpenError::new(path, err.into()))?;

                if file_type.is_file() {
                    if entry.file_name().to_str().map_or(false, is_vpk_file) {
                        let path = entry.path();
                        debug!(
                            "found vpk `{}` in wildcard directory, opening",
                            path.display()
                        );

                        // Silently ignore errors, these could be multipart vpk files
                        if let Ok(vpk) = vpk::Directory::read(path) {
                            open_search_paths.push(OpenSearchPath::Vpk(vpk));

                            debug!("vpk opened successfully");
                        } else {
                            debug!("could not read vpk, ignoring");
                        }
                    }
                } else if file_type.is_dir() {
                    debug!(
                        "found directory `{}` in wildcard directory, adding to search paths",
                        path.display()
                    );

                    open_search_paths.push(OpenSearchPath::Directory(entry.path()));
                }
            }
        }
        Err(err) => {
            if err.kind() == io::ErrorKind::NotFound {
                warn!(
                    "opening filesystem: directory `{}` not found",
                    path.to_string_lossy()
                );
            } else {
                return Err(OpenError::new(path, err.into()));
            }
        }
    }

    Ok(())
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

        let (best_game_info, best_path) = entries.try_fold((GameInfo::default(), StdPathBuf::new()), |best_candidate, entry| {
            let entry = entry.map_err(|err| ParseError::from_io(err, &app.install_dir))?;

            if !entry.file_type().as_ref().map_or(false, FileType::is_dir) {
                return <Result<_, ParseError>>::Ok(best_candidate);
            }

            let maybe_gameinfo_path = entry.path().join("gameinfo.txt");

            if !maybe_gameinfo_path.is_file() {
                return Ok(best_candidate);
            }

            debug!(
                "gameinfo.txt candidate for `{}` found in `{}`",
                app.name,
                maybe_gameinfo_path.to_string_lossy()
            );

            let game_info_str = fs::read_to_string(&maybe_gameinfo_path)
                .map_err(|err| ParseError::from_io(err, &maybe_gameinfo_path))?;
            let game_info = vdf::from_str::<GameInfoFile>(&game_info_str)
                .map_err(|err| ParseError::from_vdf(err, &maybe_gameinfo_path))?.game_info;
            
            if game_info_is_better(&best_candidate.0, &game_info) {
                debug!(
                    "gameinfo.txt candidate for `{}` found in `{}` is better than the current best candidate",
                    app.name,
                    maybe_gameinfo_path.to_string_lossy()
                );

                Ok((game_info, maybe_gameinfo_path))
            } else {
                Ok(best_candidate)
            }
        })?;

        if best_path.as_os_str().is_empty() {
            return Err(ParseError::NoGameInfo {
                path: app.install_dir.as_os_str().to_string_lossy().into_owned(),
            });
        }
        
        Ok(Self::from_game_info(
            best_game_info,
            best_path
                .parent()
                .expect("`game_info_path` has no parent"),
            &app.install_dir,
        ))
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

        let game_info_str = fs::read_to_string(game_info_path)
            .map_err(|err| ParseError::from_io(err, game_info_path))?;
        let game_info = vdf::from_str::<GameInfoFile>(&game_info_str)
            .map_err(|err| ParseError::from_vdf(err, game_info_path))?
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
            debug!("got search path from gameinfo: `{}`", path);

            let path = path.as_uncased();
            let path = if path.starts_with("|gameinfo_path|".as_uncased()) {
                match path[15..].as_str() {
                    "." => game_info_directory.into(),
                    other => game_info_directory.join(other),
                }
            } else if path.starts_with("|all_source_engine_paths|".as_uncased()) {
                match path[25..].as_str() {
                    "." => root_path.into(),
                    other => root_path.join(other),
                }
            } else {
                root_path.join(path.as_str())
            };

            debug!("search path resolved as: `{}`", path.display());

            if let Some(file_name) = path.file_name().and_then(OsStr::to_str) {
                if is_vpk_file(file_name) {
                    debug!("search path detected as a vpk file");

                    let new_path = SearchPath::Vpk(path);
                    if !search_paths.contains(&new_path) {
                        search_paths.push(new_path);
                    }
                } else if file_name == "*" {
                    if let Some(parent) = path.parent() {
                        debug!("search path detected as a wildcard directory");

                        let new_path = SearchPath::Wildcard(parent.into());
                        if !search_paths.contains(&new_path) {
                            search_paths.push(new_path);
                        }
                    } else {
                        warn!(
                            "search path `{}` in gameinfo.txt is invalid",
                            path.display()
                        );
                    }
                } else {
                    debug!("search path detected as a directory");

                    let new_path = SearchPath::Directory(path);
                    if !search_paths.contains(&new_path) {
                        search_paths.push(new_path);
                    }
                }
            } else {
                warn!(
                    "search path `{}` in gameinfo.txt is invalid",
                    path.display()
                );
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
        debug!("opening search paths for file system `{}`", self.name);

        let mut open_search_paths = Vec::new();

        for search_path in &self.search_paths {
            search_path.open(&mut open_search_paths)?;
        }

        Ok(OpenFileSystem {
            search_paths: open_search_paths,
        })
    }

    /// Clones `self` with additional `search_paths` taking predecence over the existing search paths.
    /// The first supplied search path is searched first.
    #[must_use]
    pub fn with_search_paths(&self, mut search_paths: Vec<SearchPath>) -> Self {
        search_paths.extend(self.search_paths.iter().cloned());

        Self {
            name: self.name.clone(),
            search_paths,
        }
    }
}

fn game_info_is_better(old: &GameInfo, new: &GameInfo) -> bool {
    let old_set: BTreeSet<_> = old.file_system.search_paths.game_search_paths.iter().map(UncasedStr::new).collect();
    let new_set: BTreeSet<_> = new.file_system.search_paths.game_search_paths.iter().map(UncasedStr::new).collect();

    new_set.is_superset(&old_set)
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

    fn try_open_file(&self, file_path: &GamePath) -> io::Result<Option<GameFile>> {
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
            OpenSearchPath::Directory(path) => open_fs_file(path, file_path).map_or_else(
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

    fn try_read_dir<'a>(&'a self, path: &GamePath) -> io::Result<Option<ReadDirPart<'a>>> {
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

#[cfg(not(unix))]
fn open_fs_file(path: &StdPath, file_path: &GamePath) -> Result<fs::File, io::Error> {
    fs::File::open(path.join(file_path.as_str()))
}

#[cfg(unix)]
// "manual" case-insensitive file opening on Linux
fn open_fs_file(root_path: &StdPath, file_path: &GamePath) -> Result<fs::File, io::Error> {
    use std::io::ErrorKind;

    match fs::File::open(root_path.join(file_path.as_str())) {
        Ok(f) => Ok(f),
        Err(err) => {
            if err.kind() != ErrorKind::NotFound {
                return Err(err);
            }

            let mut target_path = root_path.to_path_buf();
            for path_part in file_path.as_str().split('/') {
                let target_part = fs::read_dir(&target_path)?.find_map(|res| {
                    res.ok().and_then(|entry| {
                        let file_name = entry.file_name();
                        if file_name.eq_ignore_ascii_case(path_part) {
                            Some(file_name)
                        } else {
                            None
                        }
                    })
                });

                if let Some(target_part) = target_part {
                    target_path.push(target_part);
                } else {
                    // return the original "not found" error
                    return Err(err);
                }
            }

            fs::File::open(target_path)
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
    fn next_entry(&mut self, path: &'a GamePath) -> Option<io::Result<DirEntry<'a>>> {
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
    /// The path is case-insensitive even when the underlying filesystem is not.
    ///
    /// # Errors
    ///
    /// Returns `Err` if `file_path` doesn't exist or if the file can't be opened.
    pub fn open_file<'a>(&self, file_path: impl Into<Path<'a>>) -> io::Result<GameFile> {
        let file_path = file_path.into();

        match file_path {
            Path::Game(file_path) => {
                debug!("opening `{}` from game file system", file_path);

                for path in &self.search_paths {
                    debug!("looking in `{}`", path.path().display());

                    if let Some(file) = path.try_open_file(file_path)? {
                        debug!("file `{}` found in `{}`", file_path, path.path().display());

                        return Ok(file);
                    }
                }

                debug!("file `{}` not found in any search path", file_path);

                Err(io::Error::new(io::ErrorKind::NotFound, "no such file"))
            }
            Path::Os(file_path) => {
                debug!("opening `{}` from os file system", file_path.display());

                let file = fs::File::open(file_path)?;
                Ok(GameFile::Fs(file))
            }
        }
    }

    /// Reads the specified file into a [`Vec`].
    /// The path is case-insensitive even when the underlying filesystem is not.
    ///
    /// # Errors
    ///
    /// Returns `Err` if `file_path` doesn't exist or if the file can't be read.
    pub fn read<'a>(&self, file_path: impl Into<Path<'a>>) -> io::Result<Vec<u8>> {
        let mut file = self.open_file(file_path)?;
        let mut buffer = Vec::with_capacity(initial_buffer_size(&file));
        file.read_to_end(&mut buffer)?;
        Ok(buffer)
    }

    /// Reads the specified file into a [`String`].
    /// The path is case-insensitive even when the underlying filesystem is not.
    ///
    /// # Errors
    ///
    /// Returns `Err` if `file_path` doesn't exist or if the file can't be read.
    pub fn read_to_string<'a>(&self, file_path: impl Into<Path<'a>>) -> io::Result<String> {
        let mut file = self.open_file(file_path)?;
        let mut buffer = String::with_capacity(initial_buffer_size(&file));
        file.read_to_string(&mut buffer)?;
        Ok(buffer)
    }

    /// Returns an iterator over the entries within a directory.
    pub fn read_dir<'a>(&'a self, path: &'a GamePath) -> ReadDir<'a> {
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
    path: &'a GamePath,
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
    name: Cow<'a, GamePath>,
    path: GamePathBuf,
    ty: DirEntryType,
}

impl<'a> DirEntry<'a> {
    fn new_borrowed(
        search_path: &'a OpenSearchPath,
        name: &'a GamePath,
        path: &'a GamePath,
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
        name: GamePathBuf,
        path: &'a GamePath,
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
    pub fn name(&self) -> &GamePath {
        &self.name
    }

    #[must_use]
    pub fn path(&self) -> &GamePath {
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

    #[test]
    fn case_insensitive_file_opening() {
        let root_path = StdPath::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests")
            .join("test_filesystem");
        let game_info_path = root_path.join("game").join("gameinfo.txt");

        let file_system = FileSystem::from_paths(root_path, game_info_path)
            .unwrap()
            .open()
            .unwrap();

        assert!(file_system
            .open_file(&PathBuf::Game("materials/de_test/gRiD.VmT".into()))
            .is_ok());
        assert!(file_system
            .open_file(&PathBuf::Game("MOdelS/props/De_Test/table.mdl".into()))
            .is_ok());

        let does_not_exist =
            file_system.open_file(&PathBuf::Game("MOdelS/props/De_Test/table_abc.mdl".into()));

        assert!(does_not_exist.unwrap_err().kind() == io::ErrorKind::NotFound);
    }
}
