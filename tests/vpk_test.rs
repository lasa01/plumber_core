#![cfg(feature = "vpk")]
use std::path::Path;

use valveflow::vpk::Directory;

#[test]
fn test_vpk_single_file() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("test.vpk");
    let vpk = Directory::read(path).unwrap();
    let mut file = vpk.open_file("test.vdf").unwrap();
    let contents = String::from_utf8(file.verified_read().unwrap()).unwrap();
    eprintln!("{}", contents);
}

#[test]
fn test_vpk_multi_part() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
    .join("tests")
    .join("test_dir.vpk");
    let vpk = Directory::read(path).unwrap();
    let mut file = vpk.open_file("test/test2").unwrap();
    let contents = String::from_utf8(file.verified_read().unwrap()).unwrap();
    eprintln!("{}", contents);
}
