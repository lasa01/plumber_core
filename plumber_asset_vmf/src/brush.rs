use std::{
    collections::BTreeMap,
    fmt::{self, Debug, Formatter},
    sync::Mutex,
};

use glam::Vec3;
use plumber_asset_core::{Asset, AssetConfig, Cached, Context, Handler, NoError};
use plumber_asset_vmt::VmtConfig;
use plumber_vmf::{
    builder::{BuiltBrushEntity, GeometrySettings},
    vmf::{Entity, World},
};

#[derive(Clone, Copy)]
pub struct BrushConfig<'a, M> {
    pub vmt_config: M,
    pub side_faces_map: &'a Mutex<BTreeMap<i32, Vec<Vec<Vec3>>>>,
    pub geometry_settings: GeometrySettings,
    pub scale: f32,
}

impl<'a, M> Debug for BrushConfig<'a, M> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("BrushConfig")
    }
}

pub enum BrushInput<'a> {
    World(&'a World),
    Entity(&'a Entity),
}

impl<'a, M, H> AssetConfig<H> for BrushConfig<'a, M>
where
    H: Handler<Asset<Self>> + Handler<Cached<M>>,
    M: VmtConfig<H>,
{
    type Input<'b> = BrushInput<'b>;
    type Output<'b> = BuiltBrushEntity<'b>;
    type Error<'b> = NoError;

    fn process<'b>(
        self,
        input: Self::Input<'b>,
        context: &mut Context<H>,
    ) -> Result<Self::Output<'b>, Self::Error<'b>> {
        let built = match input {
            BrushInput::World(world) => world.build_brush(
                |path| context.depend_on(self.vmt_config, path.clone()).ok(),
                self.side_faces_map,
                &self.geometry_settings,
                self.scale,
            ),
            BrushInput::Entity(entity) => entity.build_brush(
                |path| context.depend_on(self.vmt_config, path.clone()).ok(),
                self.side_faces_map,
                &self.geometry_settings,
                self.scale,
            ),
        };

        Ok(built)
    }
}
