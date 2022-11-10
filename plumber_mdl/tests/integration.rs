use std::result;

use itertools::Itertools;

use plumber_fs::{DirEntry, DirEntryType, GamePath, OpenFileSystem, ReadDir, SourceAppsExt};
use plumber_mdl::{Model, Result};
use plumber_steam::Libraries;

/// Fails if steam is not installed
#[test]
#[ignore]
fn read_models() {
    let libraries = Libraries::discover().unwrap();
    for result in libraries.apps().source().filesystems() {
        match result {
            Ok(filesystem) => {
                eprintln!("reading from filesystem: {}", filesystem.name);
                let filesystem = filesystem.open().unwrap();
                recurse(
                    filesystem.read_dir(GamePath::try_from_str("models").unwrap()),
                    &filesystem,
                );
            }
            Err(err) => eprintln!("warning: failed filesystem discovery: {}", err),
        }
    }
}

fn recurse(readdir: ReadDir, file_system: &OpenFileSystem) {
    for entry in readdir.map(result::Result::unwrap) {
        let name = entry.name();
        match entry.entry_type() {
            DirEntryType::File => {
                if is_mdl_file(name.as_str()) {
                    if let Err(err) = read_mdl(&entry, file_system) {
                        // ignore errors, probably not our fault
                        eprintln!("failed: {:?}", err);
                    }
                }
            }
            DirEntryType::Directory => recurse(entry.read_dir(), file_system),
        }
    }
}

fn read_mdl(entry: &DirEntry, file_system: &OpenFileSystem) -> Result<()> {
    let model = Model::read(entry.path(), file_system)?;
    let verified = model.verify()?;
    eprintln!("reading `{}`", verified.name()?);

    let material_results = verified.materials(file_system)?.collect_vec();

    for mesh in verified.meshes()? {
        for face in &mesh.faces {
            assert!(face.material_index < material_results.len());

            for i in face.vertice_indices {
                assert!(i < mesh.vertices.len());
            }
        }
    }

    for result in material_results {
        result?;
    }

    verified.bones()?;
    verified.animations()?.try_for_each(|r| r.map(|_| ()))?;
    Ok(())
}

fn is_mdl_file(filename: &str) -> bool {
    filename
        .rsplit('.')
        .next()
        .map(|ext| ext.eq_ignore_ascii_case("mdl"))
        == Some(true)
}
