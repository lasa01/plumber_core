use std::{
    collections::BTreeMap,
    fmt::{self, Debug, Formatter},
};

use plumber_asset_core::{Cached, CachedAssetConfig, Context, Handler};
use plumber_asset_vmt::VmtConfig;
use plumber_fs::{GamePathBuf, PathBuf};
use plumber_mdl::{
    Animation, AnimationDescFlags, Bone, BoneAnimationData, Face, Mesh, Model, Vertex,
};

use log::{error, warn};
use thiserror::Error;

#[derive(Clone, Copy)]
pub struct MdlConfig<M> {
    pub vmt_config: M,
    pub import_animations: bool,
}

impl<M> Debug for MdlConfig<M> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("MdlConfig")
    }
}

impl<M> MdlConfig<M> {
    pub fn new(vmt_config: M) -> Self {
        Self {
            vmt_config,
            import_animations: true,
        }
    }
}

impl<H, M> CachedAssetConfig<H> for MdlConfig<M>
where
    H: Handler<Cached<Self>> + Handler<Cached<M>>,
    M: VmtConfig<H>,
{
    type Input<'a> = PathBuf;
    type Id = PathBuf;
    type Output<'a> = LoadedMdl;
    type CachedOutput = MdlInfo;
    type Error = MdlError;

    fn cache_id(self, input: &Self::Input<'_>) -> Self::Id {
        let mut id = input.clone();

        id.normalize_extension();
        id
    }

    fn process<'a>(
        self,
        input: Self::Input<'a>,
        context: &mut Context<H>,
    ) -> Result<(Self::Output<'a>, Self::CachedOutput), Self::Error> {
        self.process_mdl(input.ensure_extension("mdl"), context)
            .map_err(|e| MdlError::new(input, e))
    }
}

impl<M> MdlConfig<M> {
    fn process_mdl<H>(
        self,
        input: PathBuf,
        context: &mut Context<H>,
    ) -> Result<(LoadedMdl, MdlInfo), plumber_mdl::Error>
    where
        H: Handler<Cached<Self>> + Handler<Cached<M>>,
        M: VmtConfig<H>,
    {
        let model = Model::read(&input, context.fs())?;
        let verified = model.verify()?;

        let meshes = verified
            .meshes()?
            .into_iter()
            .map(LoadedMesh::new)
            .collect();

        let mut materials = Vec::new();
        for result in verified.materials(context.fs())? {
            match result {
                Ok(mut material) => {
                    material.set_extension("");

                    context.process(self.vmt_config, material.clone().into());
                    materials.push(Some(material));
                }
                Err(err) => {
                    warn!("model `{}`: material: {}", input, err);

                    materials.push(None);
                }
            }
        }

        let bones = verified.bones()?.into_iter().map(LoadedBone::new).collect();

        let animations = if self.import_animations {
            verified
                .animations()?
                .filter_map(|res| match res {
                    Ok(animation) => Some(LoadedAnimation::new(animation)),
                    Err(err) => {
                        error!("model `{}`: animation loading failed: {}", input, err);
                        None
                    }
                })
                .collect()
        } else {
            Vec::new()
        };

        let info = MdlInfo {
            static_prop: verified.is_static_prop(),
        };

        let name = match input {
            PathBuf::Game(path) => path,
            // if the model is from outside the game file system, just use the filename as the name
            PathBuf::Os(path) => path
                .file_name()
                .expect("file read succeeded, file_name should exist")
                .to_string_lossy()
                .into_owned()
                .into(),
        };

        Ok((
            LoadedMdl {
                name,
                info: info.clone(),
                meshes,
                materials,
                bones,
                animations,
            },
            info,
        ))
    }
}

#[derive(Debug, Clone, Error, Hash, PartialEq, Eq)]
#[error("model `{path}`: {error}")]
pub struct MdlError {
    pub path: PathBuf,
    pub error: plumber_mdl::Error,
}

impl MdlError {
    pub fn new(path: PathBuf, error: plumber_mdl::Error) -> Self {
        Self { path, error }
    }
}

#[derive(Debug, Clone)]
#[non_exhaustive]
pub struct MdlInfo {
    pub static_prop: bool,
}

#[derive(Debug, Clone)]
pub struct LoadedMdl {
    pub name: GamePathBuf,
    pub info: MdlInfo,
    pub meshes: Vec<LoadedMesh>,
    pub materials: Vec<Option<GamePathBuf>>,
    pub bones: Vec<LoadedBone>,
    pub animations: Vec<LoadedAnimation>,
}

#[derive(Debug, Clone)]
pub struct LoadedMesh {
    pub body_part_name: String,
    pub name: String,
    pub vertices: Vec<Vertex>,
    pub faces: Vec<Face>,
}

impl LoadedMesh {
    fn new(mesh: Mesh) -> Self {
        Self {
            body_part_name: mesh.body_part_name.to_owned(),
            name: mesh.name.to_owned(),
            vertices: mesh.vertices,
            faces: mesh.faces,
        }
    }
}

#[derive(Debug, Clone)]
pub struct LoadedBone {
    pub name: String,
    pub surface_prop: Option<String>,
    pub parent_bone_index: Option<usize>,
    pub position: [f32; 3],
    pub rotation: [f32; 3],
}

impl LoadedBone {
    fn new(bone: Bone) -> Self {
        Self {
            name: bone.name.to_owned(),
            surface_prop: bone.surface_prop.map(ToString::to_string),
            parent_bone_index: bone.parent_bone_index,
            position: bone.position,
            rotation: bone.rotation,
        }
    }
}

#[derive(Debug, Clone)]
pub struct LoadedAnimation {
    pub name: String,
    pub flags: AnimationDescFlags,
    pub fps: f32,
    pub data: BTreeMap<usize, BoneAnimationData>,
}

impl LoadedAnimation {
    fn new(animation: Animation) -> Self {
        Self {
            name: animation.name.to_owned(),
            flags: animation.flags,
            fps: animation.fps,
            data: animation.data,
        }
    }
}
