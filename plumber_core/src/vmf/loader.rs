use std::{
    collections::BTreeMap,
    sync::{Arc, Mutex},
    thread,
};

use plumber_vpk::PathBuf;
use rayon::{prelude::*, ThreadPoolBuilder};

use crate::{
    fs::OpenFileSystem,
    vmt::loader::{DefaultMaterialBuilder, Loader, MaterialBuilder, MaterialLoadError},
};

pub use super::builder_utils::GeometrySettings;
pub use super::overlay_builder::{BuiltOverlay, BuiltOverlayFace, OverlayError};
pub use super::prop_loader::{LoadedProp, PropError};
pub use super::solid_builder::{BuiltSide, BuiltSolid, SolidError};

use super::{entities::TypedEntity, overlay_builder::SideFacesMap, Vmf};

#[derive(Debug, Clone)]
pub struct Settings<M: MaterialBuilder> {
    material_loader: M,
    geometry_settings: GeometrySettings,
}

impl Default for Settings<DefaultMaterialBuilder> {
    fn default() -> Self {
        Self {
            material_loader: DefaultMaterialBuilder,
            geometry_settings: GeometrySettings::default(),
        }
    }
}

impl Vmf {
    #[must_use]
    pub fn scene<M>(&self, file_system: OpenFileSystem, settings: Settings<M>) -> Scene<M>
    where
        M: MaterialBuilder,
    {
        let file_system = Arc::new(file_system);
        let material_loader = Arc::new(Loader::new(settings.material_loader.clone()));
        let (material_job_sender, material_job_receiver) = crossbeam_channel::unbounded();
        let (material_result_sender, material_result_receiver) = crossbeam_channel::bounded(10);

        {
            let material_loader = material_loader.clone();
            let file_system = file_system.clone();
            thread::spawn(move || {
                for result in material_loader.load_materials(material_job_receiver, &file_system) {
                    if material_result_sender.send(result).is_err() {
                        // the receiving end was dropped, terminate the thread
                        break;
                    }
                }
            });
        }

        let side_faces_map = Arc::new(Mutex::new(BTreeMap::new()));

        Scene {
            vmf: self,
            file_system,
            material_loader,
            material_job_sender,
            material_result_receiver,
            side_faces_map,
            settings,
        }
    }
}

#[derive(Debug)]
pub struct Scene<'a, M: MaterialBuilder> {
    vmf: &'a Vmf,
    file_system: Arc<OpenFileSystem>,
    material_loader: Arc<Loader<M>>,
    material_job_sender: crossbeam_channel::Sender<PathBuf>,
    material_result_receiver:
        crossbeam_channel::Receiver<Result<<M as MaterialBuilder>::Built, MaterialLoadError>>,
    side_faces_map: Arc<Mutex<SideFacesMap>>,
    settings: Settings<M>,
}

impl<'a, M> Scene<'a, M>
where
    M: MaterialBuilder,
{
    /// This should be called first when loading the scene.
    fn send_material_jobs(&self) {
        // make sure solids' materials are loaded first
        // because solid loading later requires the material info to be available
        for solid in &self.vmf.world.solids {
            for side in &solid.sides {
                let mut material = PathBuf::from("materials");
                material.push(&side.material);
                self.material_job_sender
                    .send(material)
                    .expect("material job channel shouldn't be disconnected");
            }
        }
        for entity in &self.vmf.entities {
            for solid in &entity.solids {
                for side in &solid.sides {
                    let mut material = PathBuf::from("materials");
                    material.push(&side.material);
                    self.material_job_sender
                        .send(material)
                        .expect("material job channel shouldn't be disconnected");
                }
            }
        }

        // send overlay materials too so the material thread doesn't have to wait for other progress
        for entity in &self.vmf.entities {
            if let TypedEntity::Overlay(overlay) = entity.typed() {
                match overlay.material() {
                    Ok(material) => {
                        let mut path = PathBuf::from("materials");
                        path.push(&material);
                        self.material_job_sender
                            .send(path)
                            .expect("material job channel shouldn't be disconnected")
                    }
                    Err(err) => todo!("handle err: {}", err),
                }
            }
        }
    }

    fn load_props(
        file_system: Arc<OpenFileSystem>,
        material_job_sender: crossbeam_channel::Sender<PathBuf>,
        vmf: &'a Vmf,
    ) -> impl ParallelIterator<Item = Result<LoadedProp<'a>, PropError>> {
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
            .map_with(
                (file_system, material_job_sender),
                |(file_system, material_job_sender), prop| {
                    let loaded = prop.load(file_system)?;
                    for material in &loaded.materials {
                        material_job_sender
                            .send(material.clone())
                            .expect("material job channel shouldn't be disconnected");
                    }
                    Ok(loaded)
                },
            )
    }

    fn load_other_entities(vmf: &'a Vmf) -> impl ParallelIterator<Item = TypedEntity<'a>> {
        vmf.entities.par_iter().filter_map(|e| {
            if !e.solids.is_empty() {
                return None;
            }
            let typed = e.typed();
            // overlays must be loaded after brushes, props need asset loading
            if let TypedEntity::Overlay(..) | &TypedEntity::Prop(..) = &typed {
                return None;
            }
            Some(typed)
        })
    }

    fn load_brushes(
        material_loader: Arc<Loader<M>>,
        side_faces_map: Arc<Mutex<SideFacesMap>>,
        vmf: &'a Vmf,
        geometry_settings: GeometrySettings,
    ) -> impl ParallelIterator<Item = Result<BuiltSolid, SolidError>> {
        let world_solids_iter = {
            vmf.world.solids.par_iter().map_with(
                (
                    material_loader.clone(),
                    side_faces_map.clone(),
                    geometry_settings.clone(),
                ),
                |(material_loader, side_faces_map, geometry_settings), s| {
                    s.build_mesh(
                        |path| material_loader.wait_for_material(path),
                        side_faces_map,
                        &geometry_settings,
                    )
                },
            )
        };

        let entity_solids_iter = {
            vmf.entities
                .par_iter()
                .flat_map_iter(|e| &e.solids)
                .map_with(
                    (material_loader, side_faces_map, geometry_settings),
                    |(material_loader, side_faces_map, geometry_settings), s| {
                        s.build_mesh(
                            |path| material_loader.wait_for_material(path),
                            side_faces_map,
                            &geometry_settings,
                        )
                    },
                )
        };

        world_solids_iter.chain(entity_solids_iter)
    }

    fn load_overlays(
        side_faces_map: Arc<Mutex<SideFacesMap>>,
        vmf: &'a Vmf,
        geometry_settings: GeometrySettings,
    ) -> impl ParallelIterator<Item = Result<BuiltOverlay, OverlayError>> {
        vmf.entities
            .par_iter()
            .filter_map(|e| {
                if let TypedEntity::Overlay(o) = e.typed() {
                    Some(o)
                } else {
                    None
                }
            })
            .map_with(
                (side_faces_map, geometry_settings),
                |(side_faces_map, geometry_settings), o| {
                    o.build_mesh(&side_faces_map, &geometry_settings)
                },
            )
    }

    pub fn load_assets_sequential(self, mut asset_callback: impl FnMut(Asset<M>)) {
        self.send_material_jobs();
        let material_receiver = self.material_result_receiver;
        let (asset_sender, asset_receiver) = crossbeam_channel::bounded(10);

        let vmf = self.vmf;
        let material_loader = self.material_loader;
        let side_faces_map = self.side_faces_map;
        let material_job_sender = self.material_job_sender;
        let file_system = self.file_system;
        let settings = self.settings;

        // this is 2 less than number of cpus since one thread is for material loading and one for asset callback
        // rest of the cpus are used for parallel asset loading
        let num_threads = num_cpus::get().saturating_sub(2).max(1);
        let pool = ThreadPoolBuilder::new()
            .num_threads(num_threads)
            .build()
            .expect("thread pool building shouldn't fail");

        pool.in_place_scope(|s| {
            s.spawn(|_| {
                Self::load_props(file_system, material_job_sender, vmf).for_each_with(
                    asset_sender.clone(),
                    |asset_sender, r| {
                        asset_sender
                            .send(Asset::Prop(r))
                            .expect("the channel should outlive the thread")
                    },
                );
                Self::load_other_entities(vmf).for_each_with(
                    asset_sender.clone(),
                    |asset_sender, e| {
                        asset_sender
                            .send(Asset::Entity(e))
                            .expect("the channel should outlive the thread")
                    },
                );
                Self::load_brushes(
                    material_loader,
                    side_faces_map.clone(),
                    vmf,
                    settings.geometry_settings.clone(),
                )
                .for_each_with(asset_sender.clone(), |asset_sender, r| {
                    asset_sender
                        .send(Asset::Solid(r))
                        .expect("the channel should outlive the thread")
                });
                Self::load_overlays(side_faces_map, vmf, settings.geometry_settings.clone())
                    .for_each_with(asset_sender, |asset_sender, r| {
                        asset_sender
                            .send(Asset::Overlay(r))
                            .expect("the channel should outlive the thread")
                    });
            });

            let mut select = crossbeam_channel::Select::new();
            let select_material = select.recv(&material_receiver);
            let select_asset = select.recv(&asset_receiver);
            let mut materials_exhausted = false;
            let mut assets_exhausted = false;

            loop {
                let oper = select.select();
                let i = oper.index();
                if i == select_material {
                    let res = oper.recv(&material_receiver);
                    if let Ok(material) = res {
                        asset_callback(Asset::Material(material))
                    } else {
                        if assets_exhausted {
                            break;
                        }
                        select.remove(select_material);
                        materials_exhausted = true;
                    }
                } else if i == select_asset {
                    let res = oper.recv(&asset_receiver);
                    if let Ok(asset) = res {
                        asset_callback(asset)
                    } else {
                        if materials_exhausted {
                            break;
                        }
                        select.remove(select_asset);
                        assets_exhausted = true;
                    }
                } else {
                    unreachable!()
                };
            }
        });
    }
}

#[derive(Debug)]
#[non_exhaustive]
pub enum Asset<'a, M: MaterialBuilder> {
    Material(Result<<M as MaterialBuilder>::Built, MaterialLoadError>),
    Entity(TypedEntity<'a>),
    Solid(Result<BuiltSolid<'a>, SolidError>),
    Overlay(Result<BuiltOverlay<'a>, OverlayError>),
    Prop(Result<LoadedProp<'a>, PropError>),
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use crate::fs::FileSystem;

    use super::*;

    #[test]
    fn scene_loading() {
        let vmf = include_bytes!("../../tests/vmf/build_scene_test.vmf");
        let root_path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests")
            .join("test_filesystem");
        let game_info_path = root_path.join("game").join("gameinfo.txt");

        let parsed = Vmf::from_bytes(vmf).unwrap();
        let file_system = FileSystem::from_paths(root_path, game_info_path).unwrap();
        let scene = parsed.scene(file_system.open().unwrap(), Settings::default());
        scene.load_assets_sequential(|asset| {
            dbg!(asset);
        });
    }
}
