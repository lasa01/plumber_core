use std::{
    path::Path,
    sync::mpsc::{self, RecvTimeoutError},
    time::Duration,
};

use plumber_core::{
    asset::{Handler, Importer},
    fs::FileSystem,
    model::loader::LoadedModel,
    vmf::{
        entities::TypedEntity,
        loader::{BuiltBrushEntity, BuiltOverlay, LoadedProp, Settings},
    },
    vmt::loader::{LoadedMaterial, LoadedTexture, LoadedVmt, MaterialLoadError, SkyBox},
};

#[derive(Clone)]
struct TestHandler {
    _sender: mpsc::Sender<()>,
}

impl Handler for TestHandler {
    type MaterialData = ();

    fn build_material(&mut self, _vmt: LoadedVmt) -> Result<Self::MaterialData, MaterialLoadError> {
        Ok(())
    }

    fn handle_material(&mut self, _material: LoadedMaterial<Self::MaterialData>) {}

    fn handle_texture(&mut self, _texture: LoadedTexture) {}

    fn handle_skybox(&mut self, _skybox: SkyBox) {}

    fn handle_model(&mut self, _model: LoadedModel) {}

    fn handle_entity(&mut self, _entity: TypedEntity) {}

    fn handle_brush(&mut self, _brush: BuiltBrushEntity) {}

    fn handle_overlay(&mut self, _overlay: BuiltOverlay) {}

    fn handle_prop(&mut self, _prop: LoadedProp) {}
}

/// Make sure all copies of asset handler are dropped in import process.
#[test]
fn no_deadlocks() {
    let (sender, receiver) = mpsc::channel();

    let vmf = include_bytes!("./vmf/build_scene_test.vmf");
    let root_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("test_filesystem");
    let game_info_path = root_path.join("game").join("gameinfo.txt");

    let file_system = FileSystem::from_paths(root_path, game_info_path).unwrap();

    let importer = Importer::new(
        file_system.open().unwrap(),
        TestHandler { _sender: sender },
        2,
    );

    importer
        .import_vmf_blocking(vmf, &Settings::default(), || {
            // make sure channel disconnects
            match receiver.recv_timeout(Duration::from_secs(10)) {
                Ok(()) => panic!("nothing should be received"),
                Err(RecvTimeoutError::Timeout) => {
                    panic!("all asset handler copies not dropped")
                }
                Err(RecvTimeoutError::Disconnected) => {}
            }
        })
        .unwrap();
}
