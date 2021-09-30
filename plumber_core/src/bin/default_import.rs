use std::{
    fs::{read, File},
    io::Write,
    path::Path,
    time::Instant,
};

use plumber_core::{
    fs::FileSystem,
    vmf::{loader::Settings, Vmf},
    vmt::loader::EmptyMaterialBuilder,
};

fn main() {
    let start = Instant::now();

    let vmf_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("vmf")
        .join("de_dust2_d.vmf");
    let vmf_bytes = read(vmf_path).unwrap();
    let vmf = Vmf::from_bytes(&vmf_bytes).unwrap();

    let root_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("test_filesystem");
    let game_info_path = root_path.join("game").join("gameinfo.txt");

    let file_system = FileSystem::from_paths(root_path, game_info_path).unwrap();
    let scene = vmf.scene(
        file_system.open().unwrap(),
        Settings::<EmptyMaterialBuilder>::default(),
    );

    let mut output_file = File::create("default_import.log").unwrap();
    scene.load_assets_sequential(|asset| {
        writeln!(output_file, "{:?}", asset).unwrap();
    });

    let elapsed = start.elapsed();
    eprintln!("Elapsed: {} s", elapsed.as_secs());
}
