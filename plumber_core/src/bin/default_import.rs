use std::{
    fs::{read, File},
    io::Write,
    path::Path,
    sync::{Arc, Mutex},
    time::Instant,
};

use plumber_core::{
    asset::{Handler, Importer},
    fs::FileSystem,
    vmf::loader::Settings,
};

#[derive(Clone)]
struct AssetHandler {
    file: Arc<Mutex<File>>,
}

impl Handler for AssetHandler {
    type MaterialData = ();

    fn handle_error(&mut self, error: plumber_core::asset::Error) {
        writeln!(self.file.lock().unwrap(), "{:?}", &error).unwrap();
    }

    fn build_material(
        &mut self,
        _vmt: plumber_core::vmt::loader::LoadedVmt,
    ) -> Result<Self::MaterialData, plumber_core::vmt::loader::MaterialLoadError> {
        Ok(())
    }

    fn handle_material(
        &mut self,
        material: plumber_core::vmt::loader::LoadedMaterial<Self::MaterialData>,
    ) {
        writeln!(self.file.lock().unwrap(), "{:?}", &material).unwrap();
    }

    fn handle_model(&mut self, model: plumber_core::model::loader::LoadedModel) {
        writeln!(self.file.lock().unwrap(), "{:?}", &model).unwrap();
    }

    fn handle_entity(&mut self, entity: plumber_core::vmf::entities::TypedEntity) {
        writeln!(self.file.lock().unwrap(), "{:?}", &entity).unwrap();
    }

    fn handle_brush(&mut self, brush: plumber_core::vmf::loader::BuiltBrushEntity) {
        writeln!(self.file.lock().unwrap(), "{:?}", &brush).unwrap();
    }

    fn handle_overlay(&mut self, overlay: plumber_core::vmf::loader::BuiltOverlay) {
        writeln!(self.file.lock().unwrap(), "{:?}", &overlay).unwrap();
    }

    fn handle_prop(&mut self, prop: plumber_core::vmf::loader::LoadedProp) {
        writeln!(self.file.lock().unwrap(), "{:?}", &prop).unwrap();
    }
}

fn main() {
    let start = Instant::now();

    let vmf_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("vmf")
        .join("de_dust2_d.vmf");
    let vmf_bytes = read(vmf_path).unwrap();

    let root_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("test_filesystem");
    let game_info_path = root_path.join("game").join("gameinfo.txt");

    let file_system = FileSystem::from_paths(root_path, game_info_path).unwrap();

    let output_file = File::create("default_import.log").unwrap();

    let importer = Importer::new(
        file_system.open().unwrap(),
        AssetHandler {
            file: Arc::new(Mutex::new(output_file)),
        },
    );

    importer
        .import_vmf_blocking(&vmf_bytes, &Settings::default(), || ())
        .unwrap();

    let elapsed = start.elapsed();
    eprintln!("Elapsed: {} s", elapsed.as_secs());
}
