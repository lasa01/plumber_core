use std::{
    collections::HashSet,
    io,
    path::{Path as StdPath, PathBuf as StdPathBuf},
};

use plumber_fs::{FileSystem, GamePath, GamePathBuf, PathBuf, SourceAppsExt};

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

/// Fails if steam is not installed
#[test]
#[ignore]
fn test_filesystem_discovery() {
    let libraries = Libraries::discover().unwrap();
    for filesystem in libraries.apps().source().filesystems().map(Result::unwrap) {
        eprintln!("filesystem: {:?}", filesystem);
    }
}

/// Fails if steam is not installed
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
#[test]
#[ignore]
fn opened_discovered_filesystems_readdir() {
    let libraries = Libraries::discover().unwrap();
    for filesystem in libraries.apps().source().filesystems().map(Result::unwrap) {
        eprintln!("filesystem: {:?}", filesystem.name);
        let opened = filesystem.open().unwrap();
        let mut encountered = HashSet::new();
        recurse_readdir(
            opened.read_dir(GamePath::try_from_str("").unwrap()),
            &mut encountered,
        );
    }
}

use plumber_fs::ReadDir;
use plumber_steam::Libraries;

fn recurse_readdir(readdir: ReadDir, encountered: &mut HashSet<(StdPathBuf, GamePathBuf)>) {
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
