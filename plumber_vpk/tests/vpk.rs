use std::path::Path as StdPath;

use plumber_vpk::{Directory, DirectoryContent, Path};

#[test]
fn test_vpk_single_file() {
    let path = StdPath::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("test.vpk");
    let vpk = Directory::read(path).unwrap();
    let mut files: Vec<&Path> = vpk.files().collect();
    files.sort_unstable();
    assert_eq!(files, vec!["test.vdf", "test.vmf"]);
    let mut file = vpk
        .open_file(Path::try_from_str("test.vdf").unwrap())
        .unwrap();
    let contents = String::from_utf8(file.verify_contents().unwrap())
        .unwrap()
        .replace("\r\n", "\n");
    assert_eq!(contents, include_str!("test.vdf").replace("\r\n", "\n"));
}

#[test]
fn test_vpk_multi_part() {
    let path = StdPath::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("test_dir.vpk");
    let vpk = Directory::read(path).unwrap();
    let mut files: Vec<&Path> = vpk.files().collect();
    files.sort_unstable();
    assert_eq!(files, vec!["test.txt", "test/test2"]);
    let test_contents: Vec<&DirectoryContent> = vpk
        .directory_contents(Path::try_from_str("test").unwrap())
        .unwrap()
        .collect();
    assert_eq!(test_contents, vec![&DirectoryContent::File("test2".into())]);
    let mut file = vpk
        .open_file(Path::try_from_str("test/test2").unwrap())
        .unwrap();
    let contents = String::from_utf8(file.verify_contents().unwrap()).unwrap();
    assert_eq!(&contents, "test 2");
}
