use crate::{
    fs::{GamePathBuf, OpenFileSystem},
    model::{
        self,
        loader::{LoadedModel, ModelInfo, Settings},
    },
};

use super::entities::{AngledEntity, EntityParseError, PointEntity, Prop};

use glam::Vec3;
use thiserror::Error;

#[derive(Debug, Error, Clone, Hash, PartialEq, Eq)]
pub enum PropError {
    #[error("error parsing prop: {0}")]
    Parse(#[from] EntityParseError),
    #[error("error loading model `{model}`: {error}")]
    Model { model: String, error: model::Error },
}

#[derive(Debug)]
pub struct LoadedProp<'a> {
    pub prop: Prop<'a>,
    pub model_path: GamePathBuf,
    pub model_info: ModelInfo,
    pub position: Vec3,
    /// Rotation in in pitch, yaw, roll order (YZX), in degrees.
    pub rotation: [f32; 3],
    pub scale: f32,
}

impl<'a> Prop<'a> {
    /// # Errors
    ///
    /// Returns `Err` if the parameter `model` doesn't exist or if the model reading fails.
    pub fn load(
        self,
        model_loader: &model::loader::Loader,
        file_system: &OpenFileSystem,
        scale: f32,
    ) -> Result<(LoadedProp<'a>, Option<LoadedModel>), PropError> {
        let model = self.model()?;
        let model_path = GamePathBuf::from(model);

        let (model_info, model) =
            match model_loader.load_model(model_path.clone(), file_system, Settings::default()) {
                Ok(r) => r,
                Err(error) => {
                    let model = model.to_string();
                    return Err(PropError::Model { model, error });
                }
            };

        let scale = self.scale()? * scale;
        let position = self.origin()? * scale;
        let rotation = self.angles()?;

        Ok((
            LoadedProp {
                prop: self,
                model_path,
                model_info,
                position,
                rotation,
                scale,
            },
            model,
        ))
    }
}
