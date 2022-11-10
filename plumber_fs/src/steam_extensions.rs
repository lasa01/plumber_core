use plumber_steam::SourceApps;

use crate::{FileSystem, ParseError};

pub trait SourceAppsExt<'a> {
    /// Parse the filesystems from the Source apps.
    #[must_use]
    fn filesystems(self) -> FileSystems<'a>;
}

impl<'a> SourceAppsExt<'a> for SourceApps<'a> {
    fn filesystems(self) -> FileSystems<'a> {
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
    type Item = Result<FileSystem, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|result| {
            result.map_or_else(|e| Err(ParseError::from(e)), |a| FileSystem::from_app(&a))
        })
    }
}
