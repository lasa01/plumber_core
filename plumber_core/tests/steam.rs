#![cfg(feature = "steam")]

use plumber_core::{
    fs::{Path, PathBuf},
    steam::{App, Libraries},
};
use std::{collections::HashSet, path::PathBuf as StdPathBuf};

/// Fails if steam is not installed
#[test]
#[ignore]
fn test_library_discovery() {
    let libraries = Libraries::discover().unwrap();
    eprintln!("discovered libraries: {:?}", libraries.paths);
    let apps: Vec<App> = libraries.apps().map(Result::unwrap).collect();
    eprintln!("discovered apps: {:?}", apps);
    let source_apps: Vec<App> = libraries.apps().source().map(Result::unwrap).collect();
    eprintln!("discovered source apps: {:?}", source_apps);
}

/// Fails if steam is not installed
#[cfg(feature = "fs")]
#[test]
#[ignore]
fn test_filesystem_discovery() {
    let libraries = Libraries::discover().unwrap();
    for filesystem in libraries.apps().source().filesystems().map(Result::unwrap) {
        eprintln!("filesystem: {:?}", filesystem);
    }
}

/// Fails if steam is not installed
#[cfg(feature = "fs")]
#[test]
#[ignore]
fn open_discovered_filesystems() {
    let libraries = Libraries::discover().unwrap();
    for filesystem in libraries.apps().source().filesystems().map(Result::unwrap) {
        eprintln!("filesystem: {:?}", filesystem);
        filesystem.open().unwrap();
    }
}

/// Fails if steam is not installed
#[cfg(feature = "fs")]
#[test]
#[ignore]
fn opened_discovered_filesystems_readdir() {
    let libraries = Libraries::discover().unwrap();
    for filesystem in libraries.apps().source().filesystems().map(Result::unwrap) {
        eprintln!("filesystem: {:?}", filesystem.name);
        let opened = filesystem.open().unwrap();
        let mut encountered = HashSet::new();
        recurse_readdir(
            opened.read_dir(Path::try_from_str("").unwrap()),
            &mut encountered,
        );
    }
}

#[cfg(feature = "fs")]
use plumber_core::fs::ReadDir;

#[cfg(feature = "fs")]
fn recurse_readdir(readdir: ReadDir, encountered: &mut HashSet<(StdPathBuf, PathBuf)>) {
    // check that recursing yields no duplicates
    for entry in readdir.map(Result::unwrap) {
        let search_path = entry.search_path().to_path_buf();
        let entry_path = entry.path().to_path_buf();
        if entry.entry_type().is_directory() {
            recurse_readdir(entry.read_dir(), encountered);
        }
        if let Some(old) = encountered.replace((search_path, entry_path)) {
            panic!("readdir encountered duplicate: {:?}", old);
        }
    }
}
