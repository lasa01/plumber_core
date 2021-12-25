use crate::{
    fs::{OpenFileSystem, PathBuf},
    model::{
        self,
        loader::{LoadedModel, ModelInfo},
    },
};

use super::entities::{EntityParseError, Prop};

use thiserror::Error;

#[derive(Debug, Error)]
pub enum PropError {
    #[error("error parsing prop: {0}")]
    Parse(#[from] EntityParseError),
    #[error("error loading model `{model}`: {error}")]
    Model { model: String, error: model::Error },
}

#[derive(Debug)]
pub struct LoadedProp<'a> {
    pub prop: Prop<'a>,
    pub model_info: ModelInfo,
}

impl<'a> Prop<'a> {
    /// # Errors
    ///
    /// Returns `Err` if the parameter `model` doesn't exist or if the model reading fails.
    pub fn load(
        self,
        model_loader: &model::loader::Loader,
        file_system: &OpenFileSystem,
    ) -> Result<(LoadedProp<'a>, Option<LoadedModel>), (Self, PropError)> {
        let model = match self.model() {
            Ok(r) => r,
            Err(e) => return Err((self, e.into())),
        };
        let model_path = PathBuf::from(model);
        let (model_info, model) = match model_loader.load_model(model_path, file_system) {
            Ok(r) => r,
            Err(error) => {
                let model = model.to_string();
                return Err((self, PropError::Model { model, error }));
            }
        };
        Ok((
            LoadedProp {
                prop: self,
                model_info,
            },
            model,
        ))
    }
}
