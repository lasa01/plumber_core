use std::collections::BTreeSet;

use crate::{fs::GamePath, steam::Libraries};

pub(crate) fn read_game_file(appid: u32, path: &str) -> Vec<u8> {
    let fs = Libraries::discover()
        .unwrap()
        .apps()
        .defined_source(BTreeSet::from([appid]))
        .filesystems()
        .next()
        .expect("Required steam game is not installed")
        .unwrap();

    eprintln!("Opening file `{}` from {}", path, fs.name);

    let opened = fs.open().unwrap();

    opened.read(GamePath::try_from_str(path).unwrap()).unwrap()
}
