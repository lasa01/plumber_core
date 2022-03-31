use std::{
    collections::{BTreeMap, HashMap},
    sync::{Condvar, Mutex},
};

use log::{error, warn};

use super::{
    Animation, AnimationDescFlags, Bone, BoneAnimationData, Face, Mesh, Model, Result, Vertex,
};

use crate::fs::{GamePathBuf, OpenFileSystem, PathBuf};

#[derive(Debug, Clone, Copy)]
pub struct Settings {
    import_animations: bool,
}

impl Default for Settings {
    fn default() -> Self {
        Self {
            import_animations: true,
        }
    }
}

impl Settings {
    pub fn import_animations(&mut self, import_animations: bool) {
        self.import_animations = import_animations;
    }
}

#[derive(Debug, Clone)]
pub struct LoadedModel {
    pub name: GamePathBuf,
    pub info: ModelInfo,
    pub meshes: Vec<LoadedMesh>,
    pub materials: Vec<Option<GamePathBuf>>,
    pub bones: Vec<LoadedBone>,
    pub animations: Vec<LoadedAnimation>,
}

#[derive(Debug, Clone)]
pub struct LoadedMesh {
    pub body_part_name: String,
    pub name: String,
    pub vertices: Vec<Vertex>,
    pub faces: Vec<Face>,
}

impl LoadedMesh {
    fn new(mesh: Mesh) -> Self {
        Self {
            body_part_name: mesh.body_part_name.to_owned(),
            name: mesh.name.to_owned(),
            vertices: mesh.vertices,
            faces: mesh.faces,
        }
    }
}

#[derive(Debug, Clone)]
pub struct LoadedBone {
    pub name: String,
    pub surface_prop: Option<String>,
    pub parent_bone_index: Option<usize>,
    pub position: [f32; 3],
    pub rotation: [f32; 3],
}

impl LoadedBone {
    fn new(bone: Bone) -> Self {
        Self {
            name: bone.name.to_owned(),
            surface_prop: bone.surface_prop.map(ToString::to_string),
            parent_bone_index: bone.parent_bone_index,
            position: bone.position,
            rotation: bone.rotation,
        }
    }
}

#[derive(Debug, Clone)]
pub struct LoadedAnimation {
    pub name: String,
    pub flags: AnimationDescFlags,
    pub fps: f32,
    pub data: Option<BTreeMap<usize, BoneAnimationData>>,
}

impl LoadedAnimation {
    fn new(animation: Animation) -> Self {
        Self {
            name: animation.name.to_owned(),
            flags: animation.flags,
            fps: animation.fps,
            data: animation.data,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ModelInfo {
    pub static_prop: bool,
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
        model_path: impl Into<PathBuf>,
        file_system: &OpenFileSystem,
        settings: Settings,
    ) -> Result<(ModelInfo, Option<LoadedModel>)> {
        let model_path = model_path.into();
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

        let result = Self::load_model_inner(model_path.clone(), file_system, settings);

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

    fn load_model_inner(
        model_path: PathBuf,
        file_system: &OpenFileSystem,
        settings: Settings,
    ) -> Result<LoadedModel> {
        let model = Model::read(&model_path, file_system)?;
        let verified = model.verify()?;

        let meshes = verified
            .meshes()?
            .into_iter()
            .map(LoadedMesh::new)
            .collect();

        let mut materials = Vec::new();
        for result in verified.materials(file_system)? {
            match result {
                Ok(material) => materials.push(Some(material)),
                Err(err) => {
                    warn!("model `{}`: material: {}", model_path, err);
                    materials.push(None);
                }
            }
        }

        let bones = verified.bones()?.into_iter().map(LoadedBone::new).collect();

        let animations = if settings.import_animations {
            verified
                .animations()?
                .filter_map(|res| match res {
                    Ok(animation) => Some(LoadedAnimation::new(animation)),
                    Err(err) => {
                        error!("model `{}`: animation loading failed: {}", model_path, err);
                        None
                    }
                })
                .collect()
        } else {
            Vec::new()
        };

        let info = ModelInfo {
            static_prop: verified.is_static_prop(),
        };

        let name = match model_path {
            PathBuf::Game(path) => path,
            // if the model is from outside the game file system, just use the filename as the name
            PathBuf::Os(path) => path
                .file_name()
                .expect("file read succeeded, file_name should exist")
                .to_string_lossy()
                .into_owned()
                .into(),
        };

        Ok(LoadedModel {
            name,
            info,
            meshes,
            materials,
            bones,
            animations,
        })
    }
}

impl Default for Loader {
    fn default() -> Self {
        Self::new()
    }
}
