#![deny(clippy::all, clippy::pedantic, clippy::cargo)]

//! Case-preserving, ascii case-insensitive string wrapper.

use std::{
    borrow::Borrow,
    cmp::Ordering,
    fmt::{self, Display},
    hash::{Hash, Hasher},
    ops::{Deref, Index},
    slice::SliceIndex,
};

use serde::{Deserialize, Serialize};

#[derive(Debug)]
#[repr(transparent)]
pub struct UncasedStr(str);

impl UncasedStr {
    pub fn new<S: AsRef<str> + ?Sized>(s: &S) -> &Self {
        unsafe { &*(s.as_ref() as *const str as *const Self) }
    }

    #[must_use]
    pub fn empty() -> &'static Self {
        Self::new("")
    }

    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.0
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
    pub fn starts_with(&self, prefix: &Self) -> bool {
        if self.len() < prefix.len() {
            false
        } else {
            &self[..prefix.len()] == prefix
        }
    }
}

impl Default for &UncasedStr {
    fn default() -> Self {
        UncasedStr::empty()
    }
}

impl ToOwned for UncasedStr {
    type Owned = UncasedString;

    fn to_owned(&self) -> Self::Owned {
        UncasedString::from(self)
    }
}

impl AsRef<UncasedStr> for &UncasedStr {
    fn as_ref(&self) -> &UncasedStr {
        self
    }
}

impl PartialEq for UncasedStr {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq_ignore_ascii_case(&other.0)
    }
}

impl Eq for UncasedStr {}

impl PartialEq<str> for UncasedStr {
    fn eq(&self, other: &str) -> bool {
        self.0.eq_ignore_ascii_case(other)
    }
}

impl PartialOrd for UncasedStr {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for UncasedStr {
    fn cmp(&self, other: &Self) -> Ordering {
        let a = self.0.as_bytes().iter().map(u8::to_ascii_lowercase);
        let b = other.0.as_bytes().iter().map(u8::to_ascii_lowercase);

        a.cmp(b)
    }
}

impl Hash for UncasedStr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0
            .bytes()
            .for_each(|b| state.write_u8(b.to_ascii_lowercase()));
    }
}

impl Display for UncasedStr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<I: SliceIndex<str, Output = str>> Index<I> for UncasedStr {
    type Output = UncasedStr;

    fn index(&self, index: I) -> &Self::Output {
        Self::new(&self.as_str()[index])
    }
}

impl Serialize for UncasedStr {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0.serialize(serializer)
    }
}

impl<'de: 'a, 'a> Deserialize<'de> for &'a UncasedStr {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        <&str>::deserialize(deserializer).map(UncasedStr::new)
    }
}

#[derive(Debug, Clone)]
pub struct UncasedString(String);

impl UncasedString {
    #[must_use]
    pub fn new() -> Self {
        Self(String::new())
    }

    #[must_use]
    pub fn with_capacity(capacity: usize) -> Self {
        Self(String::with_capacity(capacity))
    }

    #[must_use]
    pub fn as_uncased_str(&self) -> &UncasedStr {
        &*self
    }
}

impl Default for UncasedString {
    fn default() -> Self {
        Self::new()
    }
}

impl Deref for UncasedString {
    type Target = UncasedStr;

    fn deref(&self) -> &Self::Target {
        UncasedStr::new(&self.0)
    }
}

impl AsRef<UncasedStr> for UncasedString {
    fn as_ref(&self) -> &UncasedStr {
        &*self
    }
}

impl Borrow<UncasedStr> for UncasedString {
    fn borrow(&self) -> &UncasedStr {
        &*self
    }
}

impl<'a> From<&'a UncasedStr> for UncasedString {
    fn from(s: &'a UncasedStr) -> Self {
        Self::from(&s.0)
    }
}

impl<'a> From<&'a str> for UncasedString {
    fn from(s: &'a str) -> Self {
        Self(String::from(s))
    }
}

impl From<String> for UncasedString {
    fn from(s: String) -> Self {
        Self(s)
    }
}

impl PartialEq for UncasedString {
    fn eq(&self, other: &Self) -> bool {
        UncasedStr::eq(self.as_uncased_str(), other.as_uncased_str())
    }
}

impl Eq for UncasedString {}

impl PartialEq<UncasedStr> for UncasedString {
    fn eq(&self, other: &UncasedStr) -> bool {
        UncasedStr::eq(&*self, other)
    }
}

impl PartialEq<str> for UncasedString {
    fn eq(&self, other: &str) -> bool {
        UncasedStr::eq(&*self, other)
    }
}

impl PartialOrd for UncasedString {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        UncasedStr::partial_cmp(&*self, &*other)
    }
}

impl Ord for UncasedString {
    fn cmp(&self, other: &Self) -> Ordering {
        UncasedStr::cmp(&*self, &*other)
    }
}

impl Hash for UncasedString {
    fn hash<H: Hasher>(&self, state: &mut H) {
        UncasedStr::hash(self, state);
    }
}

impl Display for UncasedString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Serialize for UncasedString {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for UncasedString {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        String::deserialize(deserializer).map(Self::from)
    }
}

pub trait AsUncased {
    fn as_uncased(&self) -> &UncasedStr;
}

impl AsUncased for str {
    fn as_uncased(&self) -> &UncasedStr {
        UncasedStr::new(self)
    }
}

impl AsUncased for String {
    fn as_uncased(&self) -> &UncasedStr {
        UncasedStr::new(self)
    }
}
