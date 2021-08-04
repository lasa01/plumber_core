use crate::{
    fs::{OpenFileSystem, PathBuf},
    model::{self, Face, Model, Vertex},
};

use super::entities::{EntityParseError, Prop};

use thiserror::Error;

#[derive(Debug, Error)]
pub enum PropError {
    #[error("error parsing prop: {0}")]
    Parse(#[from] EntityParseError),
    #[error("error loading model: {0}")]
    Model(#[from] model::Error),
}

#[derive(Debug)]
pub struct LoadedProp<'a> {
    pub prop: Prop<'a>,
    pub model_name: String,
    pub model_meshes: Vec<LoadedMesh>,
    pub materials: Vec<PathBuf>,
}

#[derive(Debug)]
pub struct LoadedMesh {
    pub body_part_name: String,
    pub name: String,
    pub vertices: Vec<Vertex>,
    pub faces: Vec<Face>,
}

impl From<model::Mesh<'_>> for LoadedMesh {
    fn from(mesh: model::Mesh) -> Self {
        Self {
            body_part_name: mesh.body_part_name.to_owned(),
            name: mesh.name.to_owned(),
            vertices: mesh.vertices.into_iter().map(Clone::clone).collect(),
            faces: mesh.faces,
        }
    }
}

impl<'a> Prop<'a> {
    /// # Errors
    ///
    /// Returns `Err` if the parameter `model` doesn't exist or if the model reading fails.
    pub fn load(self, file_system: &OpenFileSystem) -> Result<LoadedProp<'a>, PropError> {
        let model_path = PathBuf::from(self.model()?);
        let model = Model::read(&model_path, file_system)?;
        let verified = model.verify()?;

        let model_name = verified.name()?.to_owned();
        let model_meshes = verified
            .meshes()?
            .into_iter()
            .map(LoadedMesh::from)
            .collect();
        let materials = verified.materials(file_system)?;

        Ok(LoadedProp {
            prop: self,
            model_name,
            model_meshes,
            materials,
        })
    }
}
