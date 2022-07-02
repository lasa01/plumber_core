use std::{collections::BTreeMap, sync::Mutex};

use rayon::prelude::*;

use crate::{
    asset::{Error, Handler, Importer},
    fs::{GamePathBuf, OpenFileSystem, PathBuf},
    model::loader::Loader as ModelLoader,
    vmt::loader::Loader as MaterialLoader,
};

pub use super::builder_utils::{GeometrySettings, InvisibleSolids, MergeSolids};
pub use super::overlay_builder::{BuiltOverlay, BuiltOverlayFace, OverlayError};
pub use super::prop_loader::{LoadedProp, PropError};
pub use super::solid_builder::{BuiltBrushEntity, BuiltSolid, MergedSolids, SolidError, SolidFace};

pub(crate) use super::overlay_builder::SideFacesMap;

use super::{
    entities::{BaseEntity, TypedEntity},
    Vmf,
};

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

#[allow(clippy::struct_excessive_bools)]
#[derive(Debug, Clone, Copy)]
pub struct Settings {
    brushes: BrushSetting,
    import_materials: bool,
    import_props: bool,
    import_entities: bool,
    import_skybox: bool,
    scale: f32,
}

impl Default for Settings {
    fn default() -> Self {
        Self {
            brushes: BrushSetting::default(),
            import_materials: true,
            import_props: true,
            import_entities: true,
            import_skybox: true,
            scale: 0.01,
        }
    }
}

impl Settings {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn brushes(&mut self, brushes: BrushSetting) {
        self.brushes = brushes;
    }

    pub fn import_materials(&mut self, import_materials: bool) {
        self.import_materials = import_materials;
    }

    pub fn import_props(&mut self, import_props: bool) {
        self.import_props = import_props;
    }

    pub fn import_entities(&mut self, import_entities: bool) {
        self.import_entities = import_entities;
    }

    pub fn import_skybox(&mut self, import_skybox: bool) {
        self.import_skybox = import_skybox;
    }

    pub fn scale(&mut self, scale: f32) {
        self.scale = scale;
    }
}

impl Vmf {
    pub(crate) fn load(
        &self,
        importer: Importer<impl Handler>,
        settings: &Settings,
        f: impl FnOnce(),
    ) {
        let side_faces_map = Mutex::new(BTreeMap::new());
        self.send_material_jobs(&importer.material_loader, settings.import_materials);

        if settings.import_skybox {
            self.send_skybox_job(&importer.material_loader);
        }

        importer.pool.in_place_scope(|s| {
            s.spawn(|_| {
                if settings.import_props {
                    self.load_props(
                        &importer.file_system,
                        &importer.model_loader,
                        &importer.material_loader,
                        importer.asset_handler.clone(),
                        settings.scale,
                    )
                    .for_each_with(
                        importer.asset_handler.clone(),
                        |handler, r| match r {
                            Ok(prop) => handler.handle_prop(prop),
                            Err((id, error)) => handler.handle_error(Error::Prop { id, error }),
                        },
                    );
                }

                if settings.import_entities {
                    self.load_other_entities().for_each_with(
                        importer.asset_handler.clone(),
                        |handler, entity| {
                            handler.handle_entity(entity);
                        },
                    );
                }

                if let BrushSetting::Import(geometry_settings) = settings.brushes {
                    self.load_brushes(
                        &importer.material_loader,
                        &side_faces_map,
                        geometry_settings,
                        settings.scale,
                    )
                    .for_each_with(
                        importer.asset_handler.clone(),
                        |handler, brush| {
                            handler.handle_brush(brush);
                        },
                    );

                    if geometry_settings.overlays {
                        let side_faces_map = side_faces_map
                            .into_inner()
                            .expect("mutex shouldn't be poisoned");

                        self.load_overlays(&side_faces_map, geometry_settings, settings.scale)
                            .for_each_with(importer.asset_handler, |handler, r| match r {
                                Ok(overlay) => handler.handle_overlay(overlay),
                                Err((id, error)) => {
                                    handler.handle_error(Error::Overlay { id, error });
                                }
                            });
                    }
                }

                // drop material loader so that the loader thread can terminate
                drop(importer.material_loader);
                // final copy of the asset handler is also dropped above, possibly signaling the closure f to terminate
            });

            f();
        });
    }

    fn send_material_jobs(&self, material_loader: &MaterialLoader, import_materials: bool) {
        // make sure solids' materials are loaded first
        // because solid loading later requires the material info to be available
        //
        // even if material importing is disabled the materials are needed to calculate uvs
        for solid in &self.world.solids {
            for side in &solid.sides {
                let mut material = GamePathBuf::from("materials");
                material.push(&side.material);
                material_loader.load_material(PathBuf::Game(material));
            }
        }
        for entity in &self.entities {
            for solid in &entity.solids {
                for side in &solid.sides {
                    let mut material = GamePathBuf::from("materials");
                    material.push(&side.material);
                    material_loader.load_material(PathBuf::Game(material));
                }
            }
        }

        if import_materials {
            // send overlay materials too so the material thread doesn't have to wait for other progress
            for entity in &self.entities {
                if let TypedEntity::Overlay(overlay) = entity.typed() {
                    if let Ok(material) = overlay.material() {
                        let mut path = GamePathBuf::from("materials");
                        path.push(&material);
                        material_loader.load_material(PathBuf::Game(path));
                    }
                }
            }
        }
    }

    fn load_props<'a>(
        &'a self,
        file_system: &'a OpenFileSystem,
        model_loader: &'a ModelLoader,
        material_loader: &'a MaterialLoader,
        asset_handler: impl Handler,
        scale: f32,
    ) -> impl ParallelIterator<Item = Result<LoadedProp, (i32, PropError)>> + 'a {
        self.entities
            .par_iter()
            .filter_map(|e| {
                if !e.solids.is_empty() {
                    return None;
                }
                let typed = e.typed();
                if let TypedEntity::Prop(prop) = typed {
                    Some(prop)
                } else {
                    None
                }
            })
            .map_with(asset_handler, move |asset_handler, prop| {
                let (prop, model) = prop
                    .load(model_loader, file_system, scale)
                    .map_err(|e| (prop.entity().id, e))?;

                if let Some(model) = model {
                    for material in model.materials.iter().flatten() {
                        material_loader.load_material(PathBuf::Game(material.clone()));
                    }
                    asset_handler.handle_model(model);
                }

                Ok(prop)
            })
    }

    fn load_other_entities(&self) -> impl ParallelIterator<Item = TypedEntity> {
        self.entities.par_iter().filter_map(|e| {
            if !e.solids.is_empty() {
                return None;
            }
            let typed = e.typed();
            // overlays and props are loaded separately
            if let TypedEntity::Overlay(..) | TypedEntity::Prop(..) = &typed {
                return None;
            }
            Some(typed)
        })
    }

    fn load_brushes<'a>(
        &'a self,
        material_loader: &'a MaterialLoader,
        side_faces_map: &'a Mutex<SideFacesMap>,
        geometry_settings: GeometrySettings,
        scale: f32,
    ) -> impl ParallelIterator<Item = BuiltBrushEntity> + 'a {
        let world_brush_iter = rayon::iter::once(&self.world).map(move |world| {
            world.build_brush(
                |path| material_loader.block_on_material(path.clone()),
                side_faces_map,
                &geometry_settings,
                scale,
            )
        });

        let entity_brushes_iter = {
            self.entities
                .par_iter()
                .filter(|entity| !entity.solids.is_empty())
                .map(move |entity| {
                    entity.build_brush(
                        |path| material_loader.block_on_material(path.clone()),
                        side_faces_map,
                        &geometry_settings,
                        scale,
                    )
                })
        };

        world_brush_iter.chain(entity_brushes_iter)
    }

    fn load_overlays<'a>(
        &'a self,
        side_faces_map: &'a SideFacesMap,
        geometry_settings: GeometrySettings,
        scale: f32,
    ) -> impl ParallelIterator<Item = Result<BuiltOverlay, (i32, OverlayError)>> + 'a {
        self.entities
            .par_iter()
            .filter_map(|e| {
                if let TypedEntity::Overlay(o) = e.typed() {
                    Some(o)
                } else {
                    None
                }
            })
            .map(move |o| {
                o.build_mesh(side_faces_map, &geometry_settings, scale)
                    .map_err(|(overlay, e)| (overlay.entity().id, e))
            })
    }

    fn send_skybox_job(&self, material_loader: &MaterialLoader) {
        let mut sky_path = GamePathBuf::from("materials/skybox");
        sky_path.push(&self.world.sky_name);

        material_loader.load_skybox(PathBuf::Game(sky_path));
    }
}
