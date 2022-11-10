use std::{
    fs::{read, File},
    io::Write,
    path::Path,
    sync::{Arc, Mutex},
    thread,
    time::Duration,
};

use plumber_asset::{
    mdl::LoadedModel,
    vmf::{LoadedProp, Settings},
    vmt::{LoadedMaterial, LoadedTexture, LoadedVmt, MaterialLoadError, SkyBox},
    Error,
};
use plumber_core::{
    asset::{Handler, Importer},
    fs::FileSystem,
};
use plumber_vmf::{
    builder::{BuiltBrushEntity, BuiltOverlay},
    entities::TypedEntity,
};

#[derive(Clone)]
struct AssetHandler {
    file: Arc<Mutex<File>>,
}

impl Handler for AssetHandler {
    type MaterialData = ();

    fn handle_error(&mut self, error: Error) {
        writeln!(self.file.lock().unwrap(), "{:?}", &error).unwrap();
    }

    fn build_material(&mut self, _vmt: LoadedVmt) -> Result<Self::MaterialData, MaterialLoadError> {
        Ok(())
    }

    fn handle_material(&mut self, material: LoadedMaterial<Self::MaterialData>) {
        writeln!(self.file.lock().unwrap(), "{:?}", &material).unwrap();
    }

    fn handle_texture(&mut self, texture: LoadedTexture) {
        writeln!(self.file.lock().unwrap(), "{:?}", &texture).unwrap();
    }

    fn handle_skybox(&mut self, skybox: SkyBox) {
        writeln!(self.file.lock().unwrap(), "{:?}", &skybox).unwrap();
    }

    fn handle_model(&mut self, model: LoadedModel) {
        writeln!(self.file.lock().unwrap(), "{:?}", &model).unwrap();
    }

    fn handle_entity(&mut self, entity: TypedEntity) {
        writeln!(self.file.lock().unwrap(), "{:?}", &entity).unwrap();
    }

    fn handle_brush(&mut self, brush: BuiltBrushEntity) {
        writeln!(self.file.lock().unwrap(), "{:?}", &brush).unwrap();
    }

    fn handle_overlay(&mut self, overlay: BuiltOverlay) {
        writeln!(self.file.lock().unwrap(), "{:?}", &overlay).unwrap();
    }

    fn handle_prop(&mut self, prop: LoadedProp) {
        writeln!(self.file.lock().unwrap(), "{:?}", &prop).unwrap();
    }
}

fn main() {
    let vmf_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("plumber_asset")
        .join("tests")
        .join("build_scene_test.vmf");
    let vmf_bytes = read(vmf_path).unwrap();

    let root_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("plumber_fs")
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
        2,
    );

    importer
        .import_vmf_blocking(&vmf_bytes, &Settings::default(), || ())
        .unwrap();

    // make sure the process doesn't exist before the import is complete
    thread::sleep(Duration::from_secs(3));
}
