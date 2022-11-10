use std::{collections::BTreeMap, sync::Mutex};

use glam::Vec3;
use rayon::prelude::*;
use rgb::RGBA8;

use plumber_fs::{GamePathBuf, OpenFileSystem, PathBuf};
use plumber_vmf::{
    builder::{BuiltBrushEntity, BuiltOverlay, GeometrySettings, OverlayError},
    entities::{AngledEntity, BaseEntity, EntityParseError, PointEntity, Prop, TypedEntity},
    Vmf,
};

use crate::{mdl, Error, Handler, Importer, LoadedModel, MaterialLoader, ModelInfo, ModelLoader};

#[derive(Debug, Error, Clone, Hash, PartialEq, Eq)]
pub enum PropError {
    #[error("error parsing prop: {0}")]
    Parse(#[from] EntityParseError),
    #[error("error loading model `{model}`: {error}")]
    Model {
        model: String,
        error: plumber_mdl::Error,
    },
}

#[derive(Debug)]
pub struct LoadedProp<'a> {
    pub prop: Prop<'a>,
    pub model_path: GamePathBuf,
    pub model_info: ModelInfo,
    pub position: Vec3,
    /// Rotation in in pitch, yaw, roll order (YZX), in degrees.
    pub rotation: [f32; 3],
    pub scale: [f32; 3],
    pub color: RGBA8,
}

impl<'a> LoadedProp<'a> {
    /// # Errors
    ///
    /// Returns `Err` if the parameter `model` doesn't exist or if the model reading fails.
    pub fn load(
        prop: Prop<'a>,
        model_loader: &mdl::Loader,
        file_system: &OpenFileSystem,
        scale: f32,
    ) -> Result<(LoadedProp<'a>, Option<LoadedModel>), PropError> {
        let model = prop.model()?;
        let model_path = GamePathBuf::from(model);

        let (model_info, model) = match model_loader.load_model(
            model_path.clone(),
            file_system,
            mdl::Settings::default(),
        ) {
            Ok(r) => r,
            Err(error) => {
                let model = model.to_string();
                return Err(PropError::Model { model, error });
            }
        };

        let position = prop.origin()? * scale;
        let scale = prop.scale()?.map(|s| s * scale);
        let rotation = prop.angles()?;
        let color = prop.render_color()?.alpha(prop.render_amt()?);

        Ok((
            LoadedProp {
                prop,
                model_path,
                model_info,
                position,
                rotation,
                scale,
                color,
            },
            model,
        ))
    }
}

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

type SideFacesMap = BTreeMap<i32, Vec<Vec<Vec3>>>;

pub(crate) fn load(
    vmf: &Vmf,
    importer: Importer<impl Handler>,
    settings: &Settings,
    f: impl FnOnce(),
) {
    let side_faces_map = Mutex::new(BTreeMap::new());
    send_material_jobs(vmf, &importer.material_loader, settings.import_materials);

    if settings.import_skybox {
        send_skybox_job(vmf, &importer.material_loader);
    }

    importer.pool.in_place_scope(|s| {
        s.spawn(|_| {
            if settings.import_props {
                load_props(
                    vmf,
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
                load_other_entities(vmf).for_each_with(
                    importer.asset_handler.clone(),
                    |handler, entity| {
                        handler.handle_entity(entity);
                    },
                );
            }

            if let BrushSetting::Import(geometry_settings) = settings.brushes {
                load_brushes(
                    vmf,
                    &importer.material_loader,
                    &side_faces_map,
                    geometry_settings,
                    settings.scale,
                )
                .for_each_with(importer.asset_handler.clone(), |handler, brush| {
                    handler.handle_brush(brush);
                });

                if geometry_settings.overlays {
                    let side_faces_map = side_faces_map
                        .into_inner()
                        .expect("mutex shouldn't be poisoned");

                    load_overlays(vmf, &side_faces_map, geometry_settings, settings.scale)
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

fn send_material_jobs(vmf: &Vmf, material_loader: &MaterialLoader, import_materials: bool) {
    // make sure solids' materials are loaded first
    // because solid loading later requires the material info to be available
    //
    // even if material importing is disabled the materials are needed to calculate uvs
    for solid in &vmf.world.solids {
        for side in &solid.sides {
            let mut material = GamePathBuf::from("materials");
            material.push(&side.material);
            material_loader.load_material(PathBuf::Game(material));
        }
    }
    for entity in &vmf.entities {
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
        for entity in &vmf.entities {
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
    vmf: &'a Vmf,
    file_system: &'a OpenFileSystem,
    model_loader: &'a ModelLoader,
    material_loader: &'a MaterialLoader,
    asset_handler: impl Handler,
    scale: f32,
) -> impl ParallelIterator<Item = Result<LoadedProp<'a>, (i32, PropError)>> + 'a {
    vmf.entities
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
            let (prop, model) = LoadedProp::load(prop, model_loader, file_system, scale)
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

fn load_other_entities(vmf: &Vmf) -> impl ParallelIterator<Item = TypedEntity> {
    vmf.entities.par_iter().filter_map(|e| {
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
    vmf: &'a Vmf,
    material_loader: &'a MaterialLoader,
    side_faces_map: &'a Mutex<SideFacesMap>,
    geometry_settings: GeometrySettings,
    scale: f32,
) -> impl ParallelIterator<Item = BuiltBrushEntity<'a>> + 'a {
    let world_brush_iter = rayon::iter::once(&vmf.world).map(move |world| {
        world.build_brush(
            |path| material_loader.block_on_material(path.clone()).ok(),
            side_faces_map,
            &geometry_settings,
            scale,
        )
    });

    let entity_brushes_iter = {
        vmf.entities
            .par_iter()
            .filter(|entity| !entity.solids.is_empty())
            .map(move |entity| {
                entity.build_brush(
                    |path| material_loader.block_on_material(path.clone()).ok(),
                    side_faces_map,
                    &geometry_settings,
                    scale,
                )
            })
    };

    world_brush_iter.chain(entity_brushes_iter)
}

fn load_overlays<'a>(
    vmf: &'a Vmf,
    side_faces_map: &'a SideFacesMap,
    geometry_settings: GeometrySettings,
    scale: f32,
) -> impl ParallelIterator<Item = Result<BuiltOverlay<'a>, (i32, OverlayError)>> + 'a {
    vmf.entities
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

fn send_skybox_job(vmf: &Vmf, material_loader: &MaterialLoader) {
    let mut sky_path = GamePathBuf::from("materials/skybox");
    sky_path.push(&vmf.world.sky_name);

    material_loader.load_skybox(PathBuf::Game(sky_path));
}
