use std::{collections::BTreeMap, sync::Mutex};

use rayon::prelude::*;

use plumber_asset_core::{Asset, AssetConfig, Cached, Context, Handler, NoError};
use plumber_asset_mdl::MdlConfig;
use plumber_asset_vmt::{skybox::SkyBoxConfig, VmtConfig};
use plumber_fs::GamePathBuf;
use plumber_vmf::{builder::GeometrySettings, entities::TypedEntity, vmf::Vmf};

pub mod brush;
pub mod other_entity;
pub mod overlay;
pub mod prop;

use brush::{BrushConfig, BrushInput};
use other_entity::OtherEntityConfig;
use overlay::OverlayConfig;
use prop::PropConfig;

#[derive(Debug, Clone, Copy)]
pub enum BrushSetting {
    Skip,
    Import(GeometrySettings),
}

impl Default for BrushSetting {
    fn default() -> Self {
        Self::Import(GeometrySettings::default())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct VmfConfig<M> {
    pub vmt_config: M,
    pub scale: f32,
    pub brushes: BrushSetting,
    pub import_overlays: bool,
    pub import_props: bool,
    pub import_animations: bool,
    pub import_other_entities: bool,
    pub import_skybox: bool,
}

impl<M> VmfConfig<M> {
    pub fn new(vmt_config: M) -> Self {
        Self {
            vmt_config,
            scale: 1.0,
            brushes: BrushSetting::Import(GeometrySettings::default()),
            import_overlays: true,
            import_props: true,
            import_animations: true,
            import_other_entities: true,
            import_skybox: true,
        }
    }
}

impl<H, M> AssetConfig<H> for VmfConfig<M>
where
    H: Handler<Asset<OtherEntityConfig>>
        + for<'a> Handler<Asset<BrushConfig<'a, M>>>
        + for<'a> Handler<Asset<OverlayConfig<'a, M>>>
        + Handler<Asset<PropConfig<M>>>
        + Handler<Asset<SkyBoxConfig>>
        + Handler<Cached<MdlConfig<M>>>
        + Handler<Cached<M>>
        + 'static,
    M: VmtConfig<H>,
{
    type Input<'a> = Vmf;
    type Output<'a> = ();
    type Error<'a> = NoError;

    fn process<'a>(
        self,
        input: Self::Input<'a>,
        context: &mut Context<H>,
    ) -> Result<Self::Output<'a>, Self::Error<'a>> {
        context.scope(|context| {
            if self.import_props {
                let prop_config = PropConfig {
                    mdl_config: MdlConfig {
                        vmt_config: self.vmt_config,
                        import_animations: self.import_animations,
                    },
                    scale: self.scale,
                };

                let props = input.entities.par_iter().filter_map(|e| {
                    if !e.solids.is_empty() {
                        return None;
                    }

                    let typed = e.typed();

                    if let TypedEntity::Prop(prop) = typed {
                        Some(prop)
                    } else {
                        None
                    }
                });

                context.queue_each(prop_config, props);
            }

            if self.import_other_entities {
                let other_entities = input.entities.par_iter().filter_map(|e| {
                    if !e.solids.is_empty() {
                        return None;
                    }

                    let typed = e.typed();

                    // overlays and props are loaded separately
                    if let TypedEntity::Overlay(..) | TypedEntity::Prop(..) = &typed {
                        return None;
                    }

                    Some(typed)
                });

                context.queue_each(OtherEntityConfig, other_entities);
            }

            if let BrushSetting::Import(ref geometry_settings) = self.brushes {
                context.spawn(|context| {
                    let geometry_settings = *geometry_settings;
                    let side_faces_map = Mutex::new(BTreeMap::new());

                    let brush_config = BrushConfig {
                        vmt_config: self.vmt_config,
                        side_faces_map: &side_faces_map,
                        geometry_settings,
                        scale: self.scale,
                    };

                    let world_brush = BrushInput::World(&input.world);

                    let entity_brushes = input
                        .entities
                        .par_iter()
                        .filter(|entity| !entity.solids.is_empty())
                        .map(BrushInput::Entity);

                    context.process_each(
                        brush_config,
                        rayon::iter::once(world_brush).chain(entity_brushes),
                    );

                    let side_faces_map = side_faces_map
                        .into_inner()
                        .expect("mutex shouldn't be poisoned");

                    if self.import_overlays {
                        let overlay_config = OverlayConfig {
                            vmt_config: self.vmt_config,
                            side_faces_map: &side_faces_map,
                            geometry_settings,
                            scale: self.scale,
                        };

                        let overlays = input.entities.par_iter().filter_map(|e| {
                            if let TypedEntity::Overlay(o) = e.typed() {
                                Some(o)
                            } else {
                                None
                            }
                        });

                        context.process_each(overlay_config, overlays);
                    }
                });
            }

            if self.import_skybox {
                let mut sky_path = GamePathBuf::from("materials/skybox");
                sky_path.push(&input.world.sky_name);

                context.queue(SkyBoxConfig, sky_path);
            }
        });

        Ok(())
    }
}
