use std::{sync::Arc, thread};

use crate::{
    fs::{OpenFileSystem, PathBuf},
    model::{
        loader::{LoadedModel, Loader as ModelLoader, ModelInfo},
        Error as ModelError,
    },
    vmf::{
        entities::TypedEntity,
        loader::{
            BuiltBrushEntity, BuiltOverlay, LoadedProp, OverlayError, PropError, Settings,
            SolidError,
        },
        Vmf,
    },
    vmt::loader::{
        LoadedMaterial, LoadedVmt, Loader as MaterialLoader, MaterialInfo, MaterialLoadError,
    },
};

use plumber_vdf as vdf;

use rayon::ThreadPoolBuilder;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("material `{path}`: {error}")]
    Material {
        path: PathBuf,
        error: MaterialLoadError,
    },
    #[error("model `{path}`: {error}")]
    Model { path: PathBuf, error: ModelError },
    #[error("solid `{id}`: {error}")]
    Solid { id: i32, error: SolidError },
    #[error("overlay `{id}`: {error}")]
    Overlay { id: i32, error: OverlayError },
    #[error("prop `{id}`: {error}")]
    Prop { id: i32, error: PropError },
}

pub trait Handler: Clone + Send + Sync + 'static {
    type MaterialData: Send + Sync + 'static;

    fn handle_error(&mut self, error: Error);

    /// # Errors
    ///
    /// Returns `Err` if an unrecoverable error was encountered during the material building.
    fn build_material(&mut self, vmt: LoadedVmt) -> Result<Self::MaterialData, MaterialLoadError>;

    fn handle_material(&mut self, material: LoadedMaterial<Self::MaterialData>);

    fn handle_model(&mut self, model: LoadedModel);
    fn handle_entity(&mut self, entity: TypedEntity);
    fn handle_brush(&mut self, brush: BuiltBrushEntity);
    fn handle_overlay(&mut self, overlay: BuiltOverlay);
    fn handle_prop(&mut self, prop: LoadedProp);
}

#[derive(Clone)]
pub struct DebugHandler;

impl Handler for DebugHandler {
    type MaterialData = ();

    fn handle_error(&mut self, error: Error) {
        eprintln!("handle_error(): {:?}", error);
    }

    fn build_material(&mut self, _vmt: LoadedVmt) -> Result<Self::MaterialData, MaterialLoadError> {
        Ok(())
    }

    fn handle_material(&mut self, material: LoadedMaterial<Self::MaterialData>) {
        eprintln!("handle_material(): {:?}", material);
    }

    fn handle_model(&mut self, model: LoadedModel) {
        eprintln!("handle_model(): {:?}", model);
    }

    fn handle_entity(&mut self, entity: TypedEntity) {
        eprintln!("handle_entity(): {:?}", entity);
    }

    fn handle_brush(&mut self, brush: BuiltBrushEntity) {
        eprintln!("handle_brush(): {:?}", brush);
    }

    fn handle_overlay(&mut self, overlay: BuiltOverlay) {
        eprintln!("handle_overlay(): {:?}", overlay);
    }

    fn handle_prop(&mut self, prop: LoadedProp) {
        eprintln!("handle_prop(): {:?}", prop);
    }
}

#[derive(Debug)]
pub struct Importer<H>
where
    H: Handler,
{
    pub(crate) file_system: Arc<OpenFileSystem>,
    pub(crate) material_loader: Arc<MaterialLoader>,
    pub(crate) material_job_sender: crossbeam_channel::Sender<PathBuf>,
    pub(crate) model_loader: Arc<ModelLoader>,
    pub(crate) pool: rayon::ThreadPool,
    pub(crate) asset_handler: H,
}

impl<H> Importer<H>
where
    H: Handler,
{
    pub fn new(file_system: OpenFileSystem, asset_handler: H) -> Self {
        let file_system = Arc::new(file_system);
        let material_loader = Arc::new(MaterialLoader::new());
        let (material_job_sender, material_job_receiver) = crossbeam_channel::unbounded();

        {
            let material_loader = material_loader.clone();
            let file_system = file_system.clone();
            let mut asset_handler = asset_handler.clone();
            let mut asset_handler_2 = asset_handler.clone();
            thread::spawn(move || {
                for result in
                    material_loader.load_materials(material_job_receiver, &file_system, |vmt| {
                        asset_handler.build_material(vmt)
                    })
                {
                    match result {
                        Ok(mat) => asset_handler_2.handle_material(mat),
                        Err((path, error)) => {
                            asset_handler_2.handle_error(Error::Material { path, error });
                        }
                    }
                }
            });
        }

        let pool = build_thread_pool();

        Self {
            file_system,
            material_loader,
            material_job_sender,
            model_loader: Arc::new(ModelLoader::new()),
            pool,
            asset_handler,
        }
    }

    /// Errors are handled in [Handler].
    pub fn import_vmt(&self, path: PathBuf) {
        self.material_job_sender
            .send(path)
            .expect("material job channel shouldn't be disconnected");
    }

    /// # Errors
    ///
    /// Returns `Err` if the material loading fails or has failed in the past
    pub fn import_vmt_blocking(&self, path: PathBuf) -> Result<MaterialInfo, MaterialLoadError> {
        self.material_job_sender
            .send(path.clone())
            .expect("material job channel shouldn't be disconnected");
        self.material_loader.wait_for_material(path)
    }

    /// Errors are handled in [Handler].
    pub fn import_mdl(&self, path: PathBuf) {
        let model_loader = self.model_loader.clone();
        let mut asset_handler = self.asset_handler.clone();
        let file_system = self.file_system.clone();

        self.pool.spawn(move || {
            match model_loader.load_model(path.clone(), &file_system) {
                Ok((_info, model)) => {
                    if let Some(model) = model {
                        asset_handler.handle_model(model);
                    }
                }
                Err(error) => asset_handler.handle_error(Error::Model { path, error }),
            };
        });
    }

    /// # Errors
    ///
    /// Returns `Err` if the model loading fails or has failed in the past
    pub fn import_mdl_blocking(&mut self, path: PathBuf) -> Result<ModelInfo, ModelError> {
        let (info, model) = self.model_loader.load_model(path, &self.file_system)?;
        if let Some(model) = model {
            self.asset_handler.handle_model(model);
        }
        Ok(info)
    }

    /// The passed function `f` is ran on the thread that called this function,
    /// while other threads load the assets.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the vmf deserialization fails.
    /// Other errors are handled in [Handler].
    pub fn import_vmf_blocking(
        self,
        bytes: &[u8],
        settings: &Settings,
        f: impl FnOnce(),
    ) -> Result<(), vdf::Error> {
        let vmf = Vmf::from_bytes(bytes)?;
        vmf.load(self, settings, f);
        Ok(())
    }
}

fn build_thread_pool() -> rayon::ThreadPool {
    // this is 2 less than number of cpus since one thread is for material loading and one for asset callback
    // rest of the cpus are used for parallel asset loading
    let num_threads = num_cpus::get().saturating_sub(2).max(1);
    ThreadPoolBuilder::new()
        .num_threads(num_threads)
        .build()
        .expect("thread pool building shouldn't fail")
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use crate::fs::FileSystem;

    use super::*;

    #[test]
    fn scene_loading() {
        let vmf = include_bytes!("../tests/vmf/build_scene_test.vmf");
        let root_path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests")
            .join("test_filesystem");
        let game_info_path = root_path.join("game").join("gameinfo.txt");

        let parsed = Vmf::from_bytes(vmf).unwrap();
        let file_system = FileSystem::from_paths(root_path, game_info_path).unwrap();

        let importer = Importer::new(file_system.open().unwrap(), DebugHandler);

        parsed.load(importer, &Settings::default(), || ());
    }
}
