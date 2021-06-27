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

pub use super::overlay_builder::{BuiltOverlay, BuiltOverlayFace, OverlayError};
pub use super::solid_builder::{BuiltSide, BuiltSolid, SolidError};

use super::{entities::TypedEntity, overlay_builder::SideFacesMap, Vmf};

impl Vmf {
    #[must_use]
    pub fn scene(&self, file_system: OpenFileSystem) -> Scene {
        let file_system = Arc::new(file_system);
        let material_loader = Arc::new(Loader::new(DefaultMaterialBuilder));
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

        // this is 1 less than number of cpus since one thread is for material loading only
        let num_threads = (num_cpus::get() - 1).max(1);
        // ignore the error if the thread pool is already built
        let _res = ThreadPoolBuilder::new()
            .num_threads(num_threads)
            .build_global();

        Scene {
            vmf: self,
            file_system,
            material_loader,
            material_job_sender,
            material_result_receiver,
            side_faces_map,
        }
    }
}

#[derive(Debug)]
pub struct Scene<'a> {
    vmf: &'a Vmf,
    file_system: Arc<OpenFileSystem>,
    material_loader: Arc<Loader<DefaultMaterialBuilder>>,
    material_job_sender: crossbeam_channel::Sender<PathBuf>,
    material_result_receiver: crossbeam_channel::Receiver<
        Result<<DefaultMaterialBuilder as MaterialBuilder>::Built, MaterialLoadError>,
    >,
    side_faces_map: Arc<Mutex<SideFacesMap>>,
}

impl<'a> Scene<'a> {
    /// This should be called first when loading the scene.
    /// Material loading starts as soon as this is called.
    ///
    /// The returned channel is bounded, so items must be receiver from it
    /// concurrently with loading other assets to prevent deadlocks.
    #[must_use]
    pub fn load_materials(
        &self,
    ) -> &crossbeam_channel::Receiver<
        Result<<DefaultMaterialBuilder as MaterialBuilder>::Built, MaterialLoadError>,
    > {
        self.send_material_jobs();

        &self.material_result_receiver
    }

    fn send_material_jobs(&self) {
        // make sure solids' materials are loaded first
        // because solid loading later requires the material info to be available
        for solid in &self.vmf.world.solids {
            for side in &solid.sides {
                self.material_job_sender
                    .send(side.material.clone())
                    .expect("material job channel shouldn't be disconnected");
            }
        }
        for entity in &self.vmf.entities {
            for solid in &entity.solids {
                for side in &solid.sides {
                    self.material_job_sender
                        .send(side.material.clone())
                        .expect("material job channel shouldn't be disconnected");
                }
            }
        }

        // send overlay materials too so the material thread doesn't have to wait for other progress
        for entity in &self.vmf.entities {
            if let TypedEntity::Overlay(overlay) = entity.typed() {
                match overlay.material() {
                    Ok(material) => self
                        .material_job_sender
                        .send(material)
                        .expect("material job channel shouldn't be disconnected"),
                    Err(err) => todo!("handle err: {}", err),
                }
            }
        }
    }

    fn load_other_entities(vmf: &'a Vmf) -> impl ParallelIterator<Item = TypedEntity<'a>> {
        vmf.entities.par_iter().filter_map(|e| {
            if !e.solids.is_empty() {
                return None;
            }
            let typed = e.typed();
            // overlays must be loaded after brushes
            if let TypedEntity::Overlay(..) = &typed {
                return None;
            }
            Some(typed)
        })
    }

    fn load_brushes(
        material_loader: Arc<Loader<DefaultMaterialBuilder>>,
        side_faces_map: Arc<Mutex<SideFacesMap>>,
        vmf: &'a Vmf,
    ) -> impl ParallelIterator<Item = Result<BuiltSolid, SolidError>> {
        let world_solids_iter = {
            vmf.world.solids.par_iter().map_with(
                (material_loader.clone(), side_faces_map.clone()),
                |(material_loader, side_faces_map), s| {
                    s.build_mesh(
                        |path| material_loader.wait_for_material(path),
                        side_faces_map,
                    )
                },
            )
        };

        let entity_solids_iter = {
            vmf.entities
                .par_iter()
                .flat_map_iter(|e| &e.solids)
                .map_with(
                    (material_loader, side_faces_map),
                    |(material_loader, side_faces_map), s| {
                        s.build_mesh(
                            |path| material_loader.wait_for_material(path),
                            side_faces_map,
                        )
                    },
                )
        };

        world_solids_iter.chain(entity_solids_iter)
    }

    fn load_overlays(
        side_faces_map: Arc<Mutex<SideFacesMap>>,
        vmf: &'a Vmf,
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
            .map_with(side_faces_map, |side_faces_map, o| {
                o.build_mesh(&side_faces_map)
            })
    }

    pub fn load_assets_sequential(self, mut asset_callback: impl FnMut(Asset)) {
        let material_receiver = self.load_materials().clone();
        let (asset_sender, asset_receiver) = crossbeam_channel::bounded(10);

        let vmf = self.vmf;
        let material_loader = self.material_loader;
        let side_faces_map = self.side_faces_map;
        let material_job_sender = self.material_job_sender;

        crossbeam_utils::thread::scope(|s| {
            s.spawn(|_| {
                Self::load_other_entities(vmf).for_each_with(
                    asset_sender.clone(),
                    |asset_sender, e| {
                        asset_sender
                            .send(Asset::Entity(e))
                            .expect("the channel should outlive the thread")
                    },
                );
                Self::load_brushes(material_loader, side_faces_map.clone(), vmf).for_each_with(
                    asset_sender.clone(),
                    |asset_sender, r| {
                        asset_sender
                            .send(Asset::Solid(r))
                            .expect("the channel should outlive the thread")
                    },
                );
                Self::load_overlays(side_faces_map, vmf).for_each_with(
                    asset_sender,
                    |asset_sender, r| {
                        asset_sender
                            .send(Asset::Overlay(r))
                            .expect("the channel should outlive the thread")
                    },
                );
                // at this point no more materials to load, signal the thread to stop
                // this will also cause the loop below to terminate
                drop(material_job_sender);
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
        })
        .expect("asset loader thread shouldn't panic");
    }
}

#[derive(Debug)]
pub enum Asset<'a> {
    Material(Result<<DefaultMaterialBuilder as MaterialBuilder>::Built, MaterialLoadError>),
    Entity(TypedEntity<'a>),
    Solid(Result<BuiltSolid<'a>, SolidError>),
    Overlay(Result<BuiltOverlay<'a>, OverlayError>),
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
        let scene = parsed.scene(file_system.open().unwrap());
        scene.load_assets_sequential(|asset| {
            dbg!(asset);
        });
    }
}
