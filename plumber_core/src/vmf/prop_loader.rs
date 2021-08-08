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
    #[error("error loading model: {0}")]
    Model(#[from] model::Error),
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
    ) -> Result<(LoadedProp<'a>, Option<LoadedModel>), PropError> {
        let model_path = PathBuf::from(self.model()?);
        let (model_info, model) = model_loader.load_model(model_path, file_system)?;
        Ok((
            LoadedProp {
                prop: self,
                model_info,
            },
            model,
        ))
    }
}
