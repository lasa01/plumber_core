use std::{borrow::Borrow, ops::Deref, path::PathBuf as StdPathBuf};

use serde::{Deserialize, Serialize};

/// A slice of a vpk path.
#[repr(transparent)]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Path(str);

impl Path {
    fn new<S: AsRef<str> + ?Sized>(s: &S) -> &Self {
        unsafe { &*(s.as_ref() as *const str as *const Self) }
    }

    #[must_use]
    pub fn empty() -> &'static Self {
        Self::new("")
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
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
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

impl Default for &Path {
    fn default() -> Self {
        Path::empty()
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

impl PartialEq<str> for Path {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

/// An owned, mutable vpk path.
#[allow(clippy::module_name_repetitions)]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Default)]
pub struct PathBuf(String);

impl PathBuf {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// # Errors
    ///
    /// Returns `Err` if the path is absolute or if it is not valid utf8.
    pub fn from_std_path_buf(path: StdPathBuf) -> Result<Self, StdPathBuf> {
        if path.is_absolute() {
            return Err(path);
        }
        match path.into_os_string().into_string() {
            Ok(string) => Ok(string.into()),
            Err(path) => Err(path.into()),
        }
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

impl From<&str> for PathBuf {
    fn from(s: &str) -> Self {
        let s = s.to_ascii_lowercase();
        let mut s = s.into_bytes();
        for byte in &mut s {
            if *byte == b'\\' {
                *byte = b'/';
            }
        }
        Self(String::from_utf8(s).unwrap())
    }
}

impl PartialEq<Path> for PathBuf {
    fn eq(&self, other: &Path) -> bool {
        self.as_str() == other.as_str()
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
