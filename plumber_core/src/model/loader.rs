use std::{
    collections::HashMap,
    sync::{Condvar, Mutex},
};

use super::{Face, Mesh, Model, Result, Vertex};

use crate::fs::{OpenFileSystem, PathBuf};

#[derive(Debug, Clone)]
pub struct LoadedModel {
    pub name: PathBuf,
    pub info: ModelInfo,
    pub meshes: Vec<LoadedMesh>,
    pub materials: Vec<PathBuf>,
}

#[derive(Debug, Clone)]
pub struct LoadedMesh {
    pub body_part_name: String,
    pub name: String,
    pub vertices: Vec<Vertex>,
    pub faces: Vec<Face>,
}

#[derive(Debug, Clone)]
pub struct ModelInfo {
    pub static_prop: bool,
}

impl LoadedMesh {
    fn new(mesh: Mesh) -> Self {
        Self {
            body_part_name: mesh.body_part_name.to_owned(),
            name: mesh.name.to_owned(),
            vertices: mesh.vertices.into_iter().map(Clone::clone).collect(),
            faces: mesh.faces,
        }
    }
}

#[derive(Debug)]
enum ModelStatus {
    Loading,
    Done(Result<ModelInfo>),
}

#[derive(Debug)]
pub struct Loader {
    model_cache: Mutex<HashMap<PathBuf, ModelStatus>>,
    model_condvar: Condvar,
}

impl Loader {
    #[must_use]
    pub fn new() -> Self {
        Self {
            model_cache: Mutex::new(HashMap::new()),
            model_condvar: Condvar::new(),
        }
    }

    /// Loads the model.
    /// If another thread is already loading the model, waits for it to finish.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the model loading failed or has failed in the past.
    pub fn load_model(
        &self,
        model_path: PathBuf,
        file_system: &OpenFileSystem,
    ) -> Result<(ModelInfo, Option<LoadedModel>)> {
        let mut guard = self
            .model_cache
            .lock()
            .expect("the mutex shouldn't be poisoned");

        match guard.get(&model_path) {
            Some(ModelStatus::Done(r)) => return r.clone().map(|i| (i, None)),
            Some(ModelStatus::Loading) => loop {
                match guard
                    .get(&model_path)
                    .expect("a loading model shouldn't be removed")
                {
                    ModelStatus::Loading => {
                        guard = self
                            .model_condvar
                            .wait(guard)
                            .expect("the mutex shouldn't be poisoned");
                    }
                    ModelStatus::Done(result) => return result.clone().map(|i| (i, None)),
                }
            },
            None => (),
        }

        guard.insert(model_path.clone(), ModelStatus::Loading);
        // release lock before importing model
        drop(guard);

        let result = Self::load_model_inner(model_path.clone(), file_system);

        let info_result = match &result {
            Ok(LoadedModel { info, .. }) => Ok(info.clone()),
            Err(err) => Err(err.clone()),
        };

        self.model_cache
            .lock()
            .expect("the mutex shouldn't be poisoned")
            .insert(model_path, ModelStatus::Done(info_result));
        self.model_condvar.notify_all();

        result.map(|m| (m.info.clone(), Some(m)))
    }

    fn load_model_inner(model_path: PathBuf, file_system: &OpenFileSystem) -> Result<LoadedModel> {
        let model = Model::read(&model_path, file_system)?;
        let verified = model.verify()?;

        let meshes = verified
            .meshes()?
            .into_iter()
            .map(LoadedMesh::new)
            .collect();
        let materials = verified.materials(file_system)?;
        let info = ModelInfo {
            static_prop: verified.is_static_prop(),
        };

        Ok(LoadedModel {
            name: model_path,
            info,
            meshes,
            materials,
        })
    }
}

impl Default for Loader {
    fn default() -> Self {
        Self::new()
    }
}
