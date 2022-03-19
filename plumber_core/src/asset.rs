use std::sync::Arc;

use crate::{
    fs::{OpenFileSystem, PathBuf},
    model::{
        loader::{LoadedModel, Loader as ModelLoader, ModelInfo, Settings as MdlSettings},
        Error as ModelError,
    },
    vmf::{
        entities::{EntityParseError, TypedEntity},
        loader::{
            BuiltBrushEntity, BuiltOverlay, LoadedProp, OverlayError, PropError,
            Settings as VmfSettings, SolidError,
        },
        Vmf,
    },
    vmt::loader::{
        LoadedMaterial, LoadedVmt, Loader as MaterialLoader, MaterialInfo, MaterialLoadError,
    },
};

use plumber_vdf as vdf;

use log::error;
use rayon::ThreadPoolBuilder;
use thiserror::Error;

#[derive(Debug, Error, Clone, Hash, PartialEq, Eq)]
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
    #[error("entity {class_name} `{id}`: {error}")]
    Entity {
        id: i32,
        class_name: String,
        error: EntityParseError,
    },
}

pub trait Handler: Clone + Send + Sync + 'static {
    type MaterialData: Send + Sync + 'static;

    fn handle_error(&mut self, error: Error) {
        error!("{}", error);
    }

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
    pub(crate) material_loader: MaterialLoader,
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
        let material_loader = MaterialLoader::new();

        {
            let file_system = file_system.clone();
            let mut asset_handler = asset_handler.clone();
            let mut asset_handler_2 = asset_handler.clone();

            material_loader.start_worker_thread(
                file_system,
                move |vmt| asset_handler.build_material(vmt),
                move |result| match result {
                    Ok(mat) => asset_handler_2.handle_material(mat),
                    Err((path, error)) => {
                        asset_handler_2.handle_error(Error::Material { path, error });
                    }
                },
            );
        }

        let pool = build_thread_pool();

        Self {
            file_system,
            material_loader,
            model_loader: Arc::new(ModelLoader::new()),
            pool,
            asset_handler,
        }
    }

    /// Errors are handled in [Handler].
    pub fn import_vmt(&self, path: &PathBuf) {
        self.material_loader.load_material(path);
    }

    /// The passed function `f` is ran on the thread that called this function,
    /// while another thread loads the material.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the material loading fails or has failed in the past
    pub fn import_vmt_blocking(
        self,
        path: &PathBuf,
        f: impl FnOnce(),
    ) -> Result<MaterialInfo, MaterialLoadError> {
        self.material_loader.load_material(path);

        // Make sure no deadlocks
        drop(self.asset_handler);

        self.material_loader.parallel_block_on_material(path, f)
    }

    /// Errors are handled in [Handler].
    pub fn import_mdl(&self, path: PathBuf, settings: MdlSettings, import_materials: bool) {
        let model_loader = self.model_loader.clone();
        let material_loader = self.material_loader.clone();
        let mut asset_handler = self.asset_handler.clone();
        let file_system = self.file_system.clone();

        self.pool.spawn(move || {
            match model_loader.load_model(path.clone(), &file_system, settings) {
                Ok((_info, model)) => {
                    if let Some(model) = model {
                        if import_materials {
                            for material in model.materials.iter().flatten() {
                                material_loader.load_material(&PathBuf::Game(material.clone()));
                            }
                        }

                        asset_handler.handle_model(model);
                    }
                }
                Err(error) => asset_handler.handle_error(Error::Model { path, error }),
            };
        });
    }

    /// The passed function `f` is ran on the thread that called this function,
    /// while other threads load the model.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the model loading fails or has failed in the past.
    pub fn import_mdl_blocking(
        mut self,
        path: PathBuf,
        settings: MdlSettings,
        import_materials: bool,
        f: impl FnOnce(),
    ) -> Result<ModelInfo, ModelError> {
        let mut result = None;

        self.pool.in_place_scope(|scope| {
            scope.spawn(|_| {
                match self
                    .model_loader
                    .load_model(path, &self.file_system, settings)
                {
                    Ok((info, model)) => {
                        if let Some(model) = model {
                            if import_materials {
                                for material in model.materials.iter().flatten() {
                                    self.material_loader
                                        .load_material(&PathBuf::Game(material.clone()));
                                }
                            }

                            self.asset_handler.handle_model(model);
                        }
                        result = Some(Ok(info));
                    }
                    Err(error) => result = Some(Err(error)),
                };

                drop(self.asset_handler);
                drop(self.material_loader);
                // Dropping these disconnects the channels that keep the import process alive,
                // signaling that the import is finished (and preventing deadlocks)
            });

            f();
        });

        result.expect("a value should have been assigned to result")
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
        settings: &VmfSettings,
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

        parsed.load(importer, &VmfSettings::default(), || ());
    }
}
