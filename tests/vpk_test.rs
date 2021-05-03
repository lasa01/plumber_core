#![cfg(feature = "vpk")]
use std::path::Path;

use valveflow::vpk::{Directory, DirectoryContent};

#[test]
fn test_vpk_single_file() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("test.vpk");
    let vpk = Directory::read(path).unwrap();
    let mut files: Vec<&str> = vpk.files().collect();
    files.sort_unstable();
    assert_eq!(files, vec!["test.vdf", "test.vmf"]);
    let mut file = vpk.open_file("test.vdf").unwrap();
    let contents = String::from_utf8(file.verify_contents().unwrap()).unwrap();
    assert_eq!(&contents, include_str!("test.vdf"));
}

#[test]
fn test_vpk_multi_part() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("test_dir.vpk");
    let vpk = Directory::read(path).unwrap();
    let mut files: Vec<&str> = vpk.files().collect();
    files.sort_unstable();
    assert_eq!(files, vec!["test.txt", "test/test2"]);
    let test_contents: Vec<&DirectoryContent> = vpk.directory_contents("test").unwrap().collect();
    assert_eq!(test_contents, vec![&DirectoryContent::File("test2".into())]);
    let mut file = vpk.open_file("test/test2").unwrap();
    let contents = String::from_utf8(file.verify_contents().unwrap()).unwrap();
    assert_eq!(&contents, "test 2");
}
