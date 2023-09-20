use std::{
    collections::BTreeMap,
    fmt::{self, Debug, Formatter},
};

use plumber_asset_core::{Asset, AssetConfig, Cached, Context, Handler};
use plumber_asset_vmt::VmtConfig;
use plumber_fs::GamePathBuf;
use plumber_vmf::{
    builder::{BuiltOverlay, GeometrySettings, OverlayError as VmfOverlayError},
    entities::{BaseEntity, Overlay},
};

use glam::Vec3;
use thiserror::Error;

#[derive(Clone, Copy)]
pub struct OverlayConfig<'a, M> {
    pub vmt_config: M,
    pub side_faces_map: &'a BTreeMap<i32, Vec<Vec<Vec3>>>,
    pub geometry_settings: GeometrySettings,
    pub scale: f32,
}

impl<'a, M> Debug for OverlayConfig<'a, M> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("OverlayConfig")
    }
}

impl<'a, M, H> AssetConfig<H> for OverlayConfig<'a, M>
where
    H: Handler<Asset<Self>> + Handler<Cached<M>>,
    M: VmtConfig<H>,
{
    type Input<'b> = Overlay<'b>;
    type Output<'b> = BuiltOverlay<'b>;
    type Error<'b> = OverlayError;

    fn process<'b>(
        self,
        input: Self::Input<'b>,
        context: &mut Context<H>,
    ) -> Result<Self::Output<'b>, Self::Error<'b>> {
        let mut material_path = GamePathBuf::from("materials");
        let material_path_part = input
            .material()
            .map_err(|e| OverlayError::new(input.entity().id, e.into()))?;

        material_path.push(&material_path_part);

        context.process(self.vmt_config, material_path.into());

        input
            .build_mesh(self.side_faces_map, &self.geometry_settings, self.scale)
            .map_err(|e| OverlayError::new(input.entity().id, e))
    }
}

#[derive(Debug, Error, Clone, Hash, PartialEq, Eq)]
#[error("overlay `{id}`: {error}")]
pub struct OverlayError {
    id: i32,
    error: VmfOverlayError,
}

impl OverlayError {
    pub fn new(id: i32, error: VmfOverlayError) -> Self {
        Self { id, error }
    }
}
