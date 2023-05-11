use std::fmt::{self, Debug, Formatter};

use plumber_asset_core::{Asset, AssetConfig, Cached, Context, Handler};
use plumber_asset_mdl::{MdlConfig, MdlError, MdlInfo};
use plumber_asset_vmt::VmtConfig;
use plumber_fs::GamePathBuf;
use plumber_vmf::entities::{AngledEntity, BaseEntity, EntityParseError, PointEntity, Prop};

use glam::Vec3;
use rgb::RGBA8;
use thiserror::Error;

#[derive(Clone, Copy)]
pub struct PropConfig<M> {
    pub mdl_config: MdlConfig<M>,
    pub scale: f32,
}

impl<H, M> AssetConfig<H> for PropConfig<M>
where
    H: Handler<Asset<Self>> + Handler<Cached<MdlConfig<M>>> + Handler<Cached<M>>,
    M: VmtConfig<H>,
{
    type Input<'a> = Prop<'a>;
    type Output<'a> = LoadedProp<'a>;
    type Error<'a> = PropError;

    fn process<'a>(
        self,
        input: Self::Input<'a>,
        context: &mut Context<H>,
    ) -> Result<LoadedProp<'a>, PropError> {
        self.process_prop(input, context)
            .map_err(|e| PropError::new(input.entity().id, e))
    }
}

impl<M> Debug for PropConfig<M> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("PropConfig")
    }
}

impl<M> PropConfig<M> {
    fn process_prop<'a, H>(
        self,
        input: Prop<'a>,
        context: &mut Context<H>,
    ) -> Result<LoadedProp<'a>, PropErrorInner>
    where
        H: Handler<Asset<Self>> + Handler<Cached<MdlConfig<M>>> + Handler<Cached<M>>,
        M: VmtConfig<H>,
    {
        let model = input.model()?;
        let model_path = GamePathBuf::from(model);

        let model_info = context.depend_on(self.mdl_config, model_path.clone().into())?;

        let position = input.origin()? * self.scale;
        let scale = input.scale()?.map(|s| s * self.scale);
        let rotation = input.angles()?;
        let color = input.render_color()?.alpha(input.render_amt()?);

        Ok(LoadedProp {
            prop: input,
            model_path,
            model_info,
            position,
            rotation,
            scale,
            color,
        })
    }
}

#[derive(Debug)]
pub struct LoadedProp<'a> {
    pub prop: Prop<'a>,
    pub model_path: GamePathBuf,
    pub model_info: MdlInfo,
    pub position: Vec3,
    /// Rotation in in pitch, yaw, roll order (YZX), in degrees.
    pub rotation: [f32; 3],
    pub scale: [f32; 3],
    pub color: RGBA8,
}

#[derive(Debug, Error, Clone, Hash, PartialEq, Eq)]
#[error("prop `{id}`: {error}")]
pub struct PropError {
    id: i32,
    error: PropErrorInner,
}

impl PropError {
    pub fn new(id: i32, error: PropErrorInner) -> Self {
        Self { id, error }
    }
}

#[derive(Debug, Error, Clone, Hash, PartialEq, Eq)]
pub enum PropErrorInner {
    #[error("error parsing prop: {0}")]
    Parse(#[from] EntityParseError),
    #[error("error loading model `{model}`: {error}")]
    Model {
        model: String,
        error: plumber_mdl::Error,
    },
}

impl From<MdlError> for PropErrorInner {
    fn from(value: MdlError) -> Self {
        Self::Model {
            model: value.path.to_string(),
            error: value.error,
        }
    }
}
