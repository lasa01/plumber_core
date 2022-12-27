use std::{
    collections::BTreeSet,
    fs::File,
    io::{BufReader, ErrorKind},
    path::{Path, PathBuf},
};

use serde::Deserialize;
use serde_json::de::from_reader;
use walkdir::WalkDir;

use plumber_fs::{GamePath, SourceAppsExt};
use plumber_steam::Libraries;

pub fn read_game_file(appid: u32, path: &str) -> Vec<u8> {
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

pub trait FileSpec
where
    for<'de> Self: Deserialize<'de>,
{
    type Type;

    fn extension() -> &'static str;

    fn read(file: File) -> Self::Type;

    fn verify(&self, data: Self::Type);

    fn verify_from_path(path: &Path) {
        let files = discover_test_files(path, Self::extension());

        for file in files {
            let spec_path = file.path.with_extension("json");

            let spec_file = match File::open(spec_path) {
                Ok(f) => f,
                Err(e) => {
                    if e.kind() == ErrorKind::NotFound {
                        continue;
                    }
                    Err(e).unwrap()
                }
            };

            eprintln!("Verifying against {}", file.name);

            let data = Self::read(File::open(&file.path).unwrap());
            let spec: Self = from_reader(BufReader::new(spec_file)).unwrap();

            spec.verify(data);
        }
    }
}

struct TestFile {
    name: String,
    path: PathBuf,
}

fn discover_test_files(path: &Path, extension: &str) -> Vec<TestFile> {
    let mut files = Vec::new();

    for result in WalkDir::new(path) {
        let entry = result.unwrap();

        let file_name = entry.path().strip_prefix(path).unwrap();
        let name_with_ext = file_name.to_string_lossy();
        let Some(name) = name_with_ext.strip_suffix(extension) else {
            continue;
        };

        files.push(TestFile {
            name: name.to_owned(),
            path: entry.into_path(),
        });
    }

    files
}
