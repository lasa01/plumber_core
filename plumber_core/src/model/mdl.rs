use std::fmt;
use std::ops::Deref;
use std::{convert::TryInto, io, mem::size_of, str, usize};

use bitflags::bitflags;
use itertools::Itertools;
use maligned::A4;
use nalgebra::{Matrix3x4, Point3, Quaternion, Vector3};
use ndarray::Array2;
use zerocopy::{FromBytes, LayoutVerified};

use crate::binary_utils::{null_terminated_prefix, read_file_aligned};
use crate::fs::GameFile;

use super::{Error, FileType, Result};

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
struct Header1 {
    id: [u8; 4],
    version: i32,
    checksum: i32,
    name: [u8; 64],
    data_length: i32,

    eye_position: [f32; 3],
    illum_position: [f32; 3],
    hull_min: [f32; 3],
    hull_max: [f32; 3],
    view_bb_min: [f32; 3],
    view_bb_max: [f32; 3],

    flags: i32,

    bone_count: i32,
    bone_offset: i32,

    bone_controller_count: i32,
    bone_controller_offset: i32,

    hit_box_set_count: i32,
    hit_box_set_offset: i32,

    local_anim_count: i32,
    local_anim_offset: i32,

    local_seq_count: i32,
    local_seq_offset: i32,

    activity_list_version: i32,
    events_indexed: i32,

    texture_count: i32,
    texture_offset: i32,

    texture_dir_count: i32,
    texture_dir_offset: i32,

    skin_reference_count: i32,
    skin_family_count: i32,
    skin_family_offset: i32,

    body_part_count: i32,
    body_part_offset: i32,

    attachment_count: i32,
    attachment_offset: i32,

    local_node_count: i32,
    local_node_offset: i32,
    local_node_name_offset: i32,

    flex_desc_count: i32,
    flex_desc_offset: i32,

    flex_controller_count: i32,
    flex_controller_offset: i32,

    flex_rules_count: i32,
    flex_rules_offset: i32,

    ik_chain_count: i32,
    ik_chain_offset: i32,

    mouths_count: i32,
    mouths_offset: i32,

    local_pose_param_count: i32,
    local_pose_param_offset: i32,

    surface_prop_offset: i32,

    key_value_offset: i32,
    key_value_count: i32,

    ik_lock_count: i32,
    ik_lock_offset: i32,

    mass: f32,
    contents: i32,

    include_model_count: i32,
    include_model_offset: i32,

    virtual_model: i32,

    anim_block_name_offset: i32,
    anim_block_count: i32,
    anim_block_offset: i32,

    anim_block_model_p: i32,

    bone_table_name_offset: i32,

    vertex_base_p: i32,
    offset_base_p: i32,

    directional_dot_product: u8,
    root_lod: u8,
    num_allowed_root_lods: u8,

    unused: u8,
    zero_frame_cache_index: i32,

    flex_controller_ui_count: i32,
    flex_controller_ui_offset: i32,

    header_2_offset: i32,

    unused_2: i32,
}

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
struct Header2 {
    src_bone_transform_count: i32,
    src_bone_transform_offset: i32,

    illum_position_attachment_index: i32,

    max_eye_deflection: f32,

    linear_bone_offset: i32,

    name_offset: i32,
    bone_flex_driver_count: i32,
    bone_flex_driver_offset: i32,
}

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
struct Bone {
    name_offset: i32,
    parent_bone_index: i32,
    bone_controller_indexes: [i32; 6],

    position: [f32; 3],
    quat: [f32; 4],
    rotation: [f32; 3],
    position_scale: [f32; 3],
    rotation_scale: [f32; 3],

    pose_to_bone: [f32; 12],

    q_alignment: [f32; 4],

    flags: i32,

    procedural_rule_type: i32,
    procedural_rule_offset: i32,
    physics_bone_index: i32,
    surface_prop_name_offset: i32,
    contents: i32,

    unused: [i32; 8],
}

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
pub struct BoneController {
    bone_index: i32,
    kind: i32,
    start_blah: f32,
    end_blah: f32,
    rest_index: i32,
    input_field: i32,
    unused: [i32; 8],
}

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
struct Attachment {
    name_offset: i32,
    flags: i32,
    local_bone_index: i32,
    matrix: [f32; 12],
    unused: [i32; 8],
}

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
struct HitBoxSet {
    name_offset: i32,
    hit_box_count: i32,
    hit_box_offset: i32,
}

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
struct HitBox {
    bone_index: i32,
    group_index: i32,
    bounding_box_min: [f32; 3],
    bounding_box_max: [f32; 3],
    name_offset: i32,
    bounding_box_angles: [f32; 3],
    unknown: f32,
    unused: [i32; 4],
}

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
struct AnimationDesc {
    base_header_offset: i32,
    name_offset: i32,
    fps: f32,
    flags: i32,
    frame_count: i32,
    movement_count: i32,
    movement_offset: i32,

    ik_rule_zero_frame_offset: i32,
    unused: [i32; 5],

    anim_block: i32,
    anim_offset: i32,
    ik_rule_count: i32,
    ik_rule_offset: i32,
    anim_block_ik_rule_offset: i32,
    local_hierarchy_count: i32,
    local_hierarchy_offset: i32,
    section_offset: i32,
    section_frame_count: i32,

    span_frame_count: i32,
    span_count: i32,
    span_offset: i32,
    span_stall_time: i32,
}

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
struct Movement {
    end_frame_index: i32,
    motion_flags: i32,
    v0: f32,
    v1: f32,
    angle: f32,
    vector: [f32; 3],
    position: [f32; 3],
}

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
struct AnimationBlock {
    data_start: i32,
    data_end: i32,
}

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
struct AnimationSection {
    anim_block: i32,
    anim_offset: i32,
}

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
struct FrameAnimation {
    constants_offset: i32,
    frame_offset: i32,
    frame_length: i32,
    unused: [i32; 3],
}

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
struct Animation {
    bone_index: u8,
    flags: u8,
    next_offset: i16,
}

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
struct IkRule {
    index: i32,
    kind: i32,
    chain: i32,
    bone: i32,

    slot: i32,
    height: f32,
    radius: f32,
    floor: f32,

    pos: [f32; 3],
    q: [f32; 4],

    compressed_ik_error_offset: i32,
    unused_1: i32,
    ik_error_index_start: i32,
    ik_error_offset: i32,

    influence_start: f32,
    influence_peak: f32,
    influence_tail: f32,
    influence_end: f32,

    unused_2: f32,
    contact: f32,
    drop: f32,
    top: f32,

    unused_3: i32,
    unused_4: i32,
    unused_5: i32,

    attachment_name_offset: i32,

    unused: [i32; 7],
}

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
struct LocalHierarchy {
    bone_index: i32,
    bone_new_parent_index: i32,

    start_influence: f32,
    peak_influence: f32,
    tail_influence: f32,
    end_influence: f32,

    start_frame_index: i32,
    local_anim_offset: i32,
    unused: [i32; 4],
}

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
struct SequenceDesc {
    base_header_offset: i32,
    name_offset: i32,
    activity_name_offset: i32,
    flags: i32,
    activity: i32,
    activity_weight: i32,
    event_count: i32,
    event_offset: i32,

    bb_min: [f32; 3],
    bb_max: [f32; 3],

    blend_count: i32,
    anim_index_offset: i32,
    movement_index: i32,
    group_size: [i32; 2],
    param_index: [i32; 2],
    param_start: [f32; 2],
    param_end: [f32; 2],
    param_parent: i32,

    fade_in_time: f32,
    fade_out_time: f32,

    local_entry_node_index: i32,
    local_exit_node_index: i32,
    node_flags: i32,

    entry_phrase: f32,
    exit_phase: f32,
    last_frame: f32,

    next_seq: i32,
    pose: i32,

    ik_rule_count: i32,
    auto_layer_count: i32,
    auto_layer_offset: i32,
    weight_offset: i32,
    pose_key_offset: i32,

    ik_lock_count: i32,
    ik_lock_offset: i32,
    key_value_offset: i32,
    key_value_size: i32,
    cycle_pose_index: i32,

    activity_modifier_offset: i32,
    activity_modifier_count: i32,

    unused: [i32; 5],
}

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
struct FlexDesc {
    name_offset: i32,
}

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
struct BodyPart {
    name_offset: i32,
    model_count: i32,
    base: i32,
    model_offset: i32,
}

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
pub struct Model {
    name: [u8; 64],
    pub kind: i32,
    pub bounding_radius: f32,

    mesh_count: i32,
    mesh_offset: i32,

    pub vertex_count: i32,
    pub vertex_offset: i32,
    tangent_offset: i32,

    attachment_count: i32,
    attachment_offset: i32,

    eye_ball_count: i32,
    eye_ball_offset: i32,

    vertex_data_p: i32,
    tangent_data_p: i32,

    unused: [i32; 8],
}

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
pub struct Mesh {
    pub material_index: i32,
    pub model_offset: i32,

    pub vertex_count: i32,
    pub vertex_index_start: i32,

    pub flex_count: i32,
    pub flex_offset: i32,

    pub material_type: i32,
    pub material_param: i32,

    pub id: i32,
    pub center: [f32; 3],

    vertex_data_p: i32,

    pub lod_vertex_counts: [i32; 8],

    unused: [i32; 8],
}

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
struct FlexController {
    type_offset: i32,
    name_offset: i32,
    local_to_global: i32,
    min: f32,
    max: f32,
}

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
struct FlexRule {
    flex_index: i32,
    op_count: i32,
    op_offset: i32,
}

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
struct FlexOp {
    op: i32,
    value: u32,
}

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
struct IkChain {
    name_offset: i32,
    link_type: i32,
    link_count: i32,
    link_offset: i32,
}

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
struct IkLink {
    bone_index: i32,
    ideal_bending_direction: [f32; 3],
    unused: [f32; 3],
}

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
struct IkLock {
    chain_index: i32,
    pos_weight: f32,
    local_q_weight: f32,
    flags: i32,
    unused: [i32; 4],
}

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
struct Mouth {
    bone_index: i32,
    forward: [f32; 3],
    flex_desc_index: i32,
}

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
struct PoseParamDesc {
    name_offset: i32,
    flags: i32,
    starting_value: f32,
    ending_value: f32,
    looping_range: f32,
}

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
struct ModelGroup {
    label_offset: i32,
    file_name_offset: i32,
}

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
struct Texture {
    name_offset: i32,
    flags: i32,
    used: i32,
    unused_1: i32,
    material_p: i32,
    client_material_p: i32,
    unused: [i32; 10],
}

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
struct BoneTransform {
    name_offset: i32,
    pre_transform: [f32; 12],
    post_transform: [f32; 12],
}

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
struct LinearBone {
    bone_count: i32,
    flags_offset: i32,
    parent_offset: i32,
    pos_offset: i32,
    quat_offset: i32,
    rot_offset: i32,
    pose_to_bone_offset: i32,
    pos_scale_offset: i32,
    rot_scale_offset: i32,
    q_alignment_offset: i32,
    unused: [i32; 6],
}

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
struct FlexControllerUi {
    name_offset: i32,
    config_0: i32,
    config_1: i32,
    config_2: i32,
    remap_type: u8,
    control_is_stereo: u8,
    unused: [u8; 2],
}

#[derive(Clone)]
pub struct Mdl {
    bytes: Vec<u8>,
}

impl Mdl {
    pub fn read(file: GameFile) -> io::Result<Self> {
        let bytes = read_file_aligned::<A4>(file)?;
        Ok(Self { bytes })
    }

    pub fn check_signature(&self) -> Result<()> {
        let signature = self.bytes.get(0..4).ok_or(Error::Corrupted {
            ty: FileType::Mdl,
            error: "eof reading signature",
        })?;

        if signature == b"IDST" {
            Ok(())
        } else {
            Err(Error::InvalidSignature {
                ty: FileType::Mdl,
                signature: String::from_utf8_lossy(signature).into_owned(),
            })
        }
    }

    pub fn version(&self) -> Result<i32> {
        if self.bytes.len() < 8 {
            return Err(Error::Corrupted {
                ty: FileType::Mdl,
                error: "eof reading version",
            });
        }
        Ok(i32::from_ne_bytes(self.bytes[4..8].try_into().unwrap()))
    }

    pub fn check_version(&self) -> Result<i32> {
        let version = self.version()?;

        if let 44 | 45 | 46 | 47 | 48 | 49 = version {
            Ok(version)
        } else {
            Err(Error::UnsupportedVersion {
                ty: FileType::Mdl,
                version,
            })
        }
    }

    pub fn header(&self) -> Result<HeaderRef> {
        let header_1 = LayoutVerified::<_, Header1>::new_from_prefix(self.bytes.as_ref())
            .ok_or(Error::Corrupted {
                ty: FileType::Mdl,
                error: "eof reading header",
            })?
            .0
            .into_ref();

        let header_2 = if header_1.header_2_offset > 0 {
            Some(
                self.bytes
                    .get(header_1.header_2_offset as usize..)
                    .and_then(LayoutVerified::<_, Header2>::new_from_prefix)
                    .ok_or(Error::Corrupted {
                        ty: FileType::Mdl,
                        error: "header 2 out of bounds or misaligned",
                    })?
                    .0
                    .into_ref(),
            )
        } else {
            None
        };

        Ok(HeaderRef {
            header_1,
            header_2,
            bytes: &self.bytes,
        })
    }
}

impl fmt::Debug for Mdl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Mdl").finish_non_exhaustive()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct HeaderRef<'a> {
    header_1: &'a Header1,
    header_2: Option<&'a Header2>,
    bytes: &'a [u8],
}

impl<'a> HeaderRef<'a> {
    pub fn checksum(&self) -> i32 {
        self.header_1.checksum
    }

    pub fn name(&self) -> Result<&'a str> {
        if let Some(header_2) = self.header_2 {
            if header_2.name_offset > 0 {
                let offset = self.header_1.header_2_offset as usize
                    + size_of::<Header2>()
                    + header_2.name_offset as usize;
                return str::from_utf8(
                    null_terminated_prefix(self.bytes.get(offset..).ok_or(Error::Corrupted {
                        ty: FileType::Mdl,
                        error: "header 2 name offset out of bounds",
                    })?)
                    .ok_or(Error::Corrupted {
                        ty: FileType::Mdl,
                        error: "eof reading header 2 name",
                    })?,
                )
                .map_err(|_| Error::Corrupted {
                    ty: FileType::Mdl,
                    error: "header 2 name is not valid utf8",
                });
            }
        }
        str::from_utf8(&self.header_1.name).map_err(|_| Error::Corrupted {
            ty: FileType::Mdl,
            error: "header name is not valid utf8",
        })
    }

    pub fn flags(&self) -> HeaderFlags {
        HeaderFlags::from_bits_truncate(self.header_1.flags)
    }

    pub fn bones(&self) -> Option<BonesRef<'a>> {
        let offset: usize = self.header_1.bone_offset.try_into().ok()?;
        let bones_bytes = self.bytes.get(offset..)?;
        let count = self.header_1.bone_count.try_into().ok()?;
        let bones = LayoutVerified::new_slice_from_prefix(bones_bytes, count)?
            .0
            .into_slice();

        Some(BonesRef {
            bones,
            offset,
            bytes: self.bytes,
        })
    }

    pub fn bone_controllers(&self) -> Option<&[BoneController]> {
        let offset: usize = self.header_1.bone_controller_offset.try_into().ok()?;
        let count = self.header_1.bone_controller_count.try_into().ok()?;
        Some(
            LayoutVerified::new_slice_from_prefix(self.bytes.get(offset..)?, count)?
                .0
                .into_slice(),
        )
    }

    pub fn textures(&self) -> Result<TexturesRef<'a>> {
        let offset: usize =
            self.header_1
                .texture_offset
                .try_into()
                .map_err(|_| Error::Corrupted {
                    ty: FileType::Mdl,
                    error: "texture offset is negative",
                })?;
        let count = self
            .header_1
            .texture_count
            .try_into()
            .map_err(|_| Error::Corrupted {
                ty: FileType::Mdl,
                error: "texture count is negative",
            })?;

        let textures = self
            .bytes
            .get(offset..)
            .and_then(|bytes| LayoutVerified::new_slice_from_prefix(bytes, count))
            .ok_or(Error::Corrupted {
                ty: FileType::Mdl,
                error: "textures out of bounds or misaligned",
            })?
            .0
            .into_slice();

        Ok(TexturesRef {
            textures,
            offset,
            bytes: self.bytes,
        })
    }

    pub fn iter_textures(&self) -> Result<impl Iterator<Item = TextureRef<'a>>> {
        let textures = self.textures()?;
        Ok(textures
            .textures
            .iter()
            .enumerate()
            .map(move |(i, texture)| TextureRef {
                texture,
                offset: textures.offset + i * size_of::<Texture>(),
                bytes: textures.bytes,
            }))
    }

    pub fn texture_paths(&self) -> Result<Vec<&str>> {
        let offset = self
            .header_1
            .texture_dir_offset
            .try_into()
            .map_err(|_| Error::Corrupted {
                ty: FileType::Mdl,
                error: "texture paths offset is negative",
            })?;
        let count = self
            .header_1
            .texture_dir_count
            .try_into()
            .map_err(|_| Error::Corrupted {
                ty: FileType::Mdl,
                error: "texture paths count is negative",
            })?;

        let path_offsets: &[i32] = self
            .bytes
            .get(offset..)
            .and_then(|bytes| LayoutVerified::new_slice_from_prefix(bytes, count))
            .ok_or(Error::Corrupted {
                ty: FileType::Mdl,
                error: "texture paths out of bounds or misaligned",
            })?
            .0
            .into_slice();

        path_offsets
            .iter()
            .map(|&offset| {
                let offset = offset.try_into().map_err(|_| Error::Corrupted {
                    ty: FileType::Mdl,
                    error: "a texture path offset is negative",
                })?;

                if offset == 0 {
                    Ok("")
                } else {
                    let bytes = self.bytes.get(offset..).ok_or(Error::Corrupted {
                        ty: FileType::Mdl,
                        error: "a texture path is out of bounds",
                    })?;

                    str::from_utf8(null_terminated_prefix(bytes).ok_or(Error::Corrupted {
                        ty: FileType::Mdl,
                        error: "eof reading a texture path",
                    })?)
                    .map_err(|_| Error::Corrupted {
                        ty: FileType::Mdl,
                        error: "a texture path is not valid utf8",
                    })
                }
            })
            .try_collect()
    }

    pub fn skin_families(&self) -> Option<Array2<i16>> {
        let offset: usize = self.header_1.skin_family_offset.try_into().ok()?;
        let family_count: usize = self.header_1.skin_family_count.try_into().ok()?;
        let reference_count: usize = self.header_1.skin_reference_count.try_into().ok()?;

        let values = LayoutVerified::new_slice_from_prefix(
            self.bytes.get(offset..)?,
            family_count * reference_count,
        )?
        .0
        .into_slice();

        Some(
            Array2::from_shape_vec((family_count, reference_count), values.to_vec())
                .expect("slice length should be correct"),
        )
    }

    pub fn body_parts(&self) -> Result<BodyPartsRef<'a>> {
        let offset = self
            .header_1
            .body_part_offset
            .try_into()
            .map_err(|_| Error::Corrupted {
                ty: FileType::Mdl,
                error: "body part offset is negative",
            })?;
        let count = self
            .header_1
            .body_part_count
            .try_into()
            .map_err(|_| Error::Corrupted {
                ty: FileType::Mdl,
                error: "body part count is negative",
            })?;

        let body_parts = self
            .bytes
            .get(offset..)
            .and_then(|bytes| LayoutVerified::new_slice_from_prefix(bytes, count))
            .ok_or(Error::Corrupted {
                ty: FileType::Mdl,
                error: "body parts out of bounds or misaligned",
            })?
            .0
            .into_slice();

        Ok(BodyPartsRef {
            body_parts,
            offset,
            bytes: self.bytes,
        })
    }

    pub fn iter_body_parts(
        &self,
    ) -> Result<impl Iterator<Item = BodyPartRef<'a>> + ExactSizeIterator> {
        let body_parts = self.body_parts()?;
        Ok(body_parts
            .body_parts
            .iter()
            .enumerate()
            .map(move |(i, body_part)| BodyPartRef {
                body_part,
                offset: body_parts.offset + i * size_of::<BodyPart>(),
                bytes: body_parts.bytes,
            }))
    }

    pub fn surface_prop(&self) -> Option<&str> {
        if self.header_1.surface_prop_offset > 0 {
            str::from_utf8(null_terminated_prefix(
                self.bytes
                    .get(self.header_1.surface_prop_offset as usize..)?,
            )?)
            .ok()
        } else {
            None
        }
    }
}

bitflags! {
    pub struct HeaderFlags: i32 {
        const AUTO_GENERATED_HITBOX = 1 << 0;
        const USES_ENV_CUBEMAP = 1 << 1;
        const FORCE_OPAQUE = 1 << 2;
        const TRANSLUCENT_TWO_PASS = 1 << 3;
        const STATIC_PROP = 1 << 4;
        const USES_FB_TEXTURE = 1 << 5;
        const HAS_SHADOW_LOD = 1 << 6;
        const USES_BUMP_MAPPING = 1 << 7;
        const USE_SHADOW_LOD_MATERIALS = 1 << 8;
        const OBSOLETE = 1 << 9;
        const UNUSED = 1 << 10;
        const NO_FORCED_FADE = 1 << 11;
        const FORCE_PHONEME_CROSS_FADE = 1 << 12;
        const CONSTANT_DIRECTIONAL_LIGHT_DOT = 1 << 13;
        const FLEXES_CONVERTED = 1 << 14;
        const BUILT_IN_PREVIEW_MODE = 1 << 15;
        const AMBIENT_BOOST = 1 << 16;
        const DO_NOT_CAST_SHADOWS = 1 << 17;
        const CAST_TEXTURE_SHADOWS = 1 << 18;
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BonesRef<'a> {
    bones: &'a [Bone],
    offset: usize,
    bytes: &'a [u8],
}

impl<'a> BonesRef<'a> {
    pub fn get(&self, index: usize) -> Option<BoneRef<'a>> {
        self.bones.get(index).map(|bone| BoneRef {
            bone,
            offset: self.offset + index * size_of::<Bone>(),
            bytes: self.bytes,
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BoneRef<'a> {
    bone: &'a Bone,
    offset: usize,
    bytes: &'a [u8],
}

impl<'a> BoneRef<'a> {
    pub fn name(&self) -> Option<&'a str> {
        let offset = self.offset as isize + self.bone.name_offset as isize;
        str::from_utf8(null_terminated_prefix(self.bytes.get(offset as usize..)?)?).ok()
    }

    pub fn position(&self) -> Point3<f32> {
        self.bone.position.into()
    }

    pub fn quat(&self) -> Quaternion<f32> {
        self.bone.quat.into()
    }

    pub fn rotation(&self) -> Vector3<f32> {
        self.bone.rotation.into()
    }

    pub fn position_scale(&self) -> Vector3<f32> {
        self.bone.position_scale.into()
    }

    pub fn rotation_scale(&self) -> Vector3<f32> {
        self.bone.rotation_scale.into()
    }

    pub fn pose_to_bone(&self) -> Matrix3x4<f32> {
        Matrix3x4::from_row_slice(&self.bone.pose_to_bone)
    }

    pub fn q_alignment(&self) -> Quaternion<f32> {
        self.bone.q_alignment.into()
    }

    pub fn flags(&self) -> i32 {
        self.bone.flags
    }

    pub fn surface_prop(&self) -> Option<&str> {
        if self.bone.surface_prop_name_offset == 0 {
            return None;
        }
        let offset = self.offset as isize + self.bone.surface_prop_name_offset as isize;
        str::from_utf8(null_terminated_prefix(self.bytes.get(offset as usize..)?)?).ok()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TexturesRef<'a> {
    textures: &'a [Texture],
    offset: usize,
    bytes: &'a [u8],
}

impl<'a> TexturesRef<'a> {
    pub fn get(&self, index: usize) -> Option<TextureRef<'a>> {
        self.textures.get(index).map(|texture| TextureRef {
            texture,
            offset: self.offset + index * size_of::<Texture>(),
            bytes: self.bytes,
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TextureRef<'a> {
    texture: &'a Texture,
    offset: usize,
    bytes: &'a [u8],
}

impl<'a> TextureRef<'a> {
    pub fn name(&self) -> Result<&'a str> {
        let offset = self.offset as isize + self.texture.name_offset as isize;
        str::from_utf8(
            null_terminated_prefix(self.bytes.get(offset as usize..).ok_or(Error::Corrupted {
                ty: FileType::Mdl,
                error: "texture name out of bounds",
            })?)
            .ok_or(Error::Corrupted {
                ty: FileType::Mdl,
                error: "eof reading texture name",
            })?,
        )
        .map_err(|_| Error::Corrupted {
            ty: FileType::Mdl,
            error: "texture name is not valid utf8",
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BodyPartsRef<'a> {
    body_parts: &'a [BodyPart],
    offset: usize,
    bytes: &'a [u8],
}

impl<'a> BodyPartsRef<'a> {
    pub fn get(&self, index: usize) -> Option<BodyPartRef<'a>> {
        self.body_parts.get(index).map(|body_part| BodyPartRef {
            body_part,
            offset: self.offset + index * size_of::<BodyPart>(),
            bytes: self.bytes,
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BodyPartRef<'a> {
    body_part: &'a BodyPart,
    offset: usize,
    bytes: &'a [u8],
}

impl<'a> BodyPartRef<'a> {
    pub fn models(&self) -> Result<ModelsRef<'a>> {
        let offset = (self.offset as isize + self.body_part.model_offset as isize) as usize;
        let count = self
            .body_part
            .model_count
            .try_into()
            .map_err(|_| Error::Corrupted {
                ty: FileType::Mdl,
                error: "body part models count is negative",
            })?;

        let models = self
            .bytes
            .get(offset..)
            .and_then(|bytes| LayoutVerified::new_slice_from_prefix(bytes, count))
            .ok_or(Error::Corrupted {
                ty: FileType::Mdl,
                error: "body part models out of bounds or misaligned",
            })?
            .0
            .into_slice();

        Ok(ModelsRef {
            models,
            offset,
            bytes: self.bytes,
        })
    }

    pub fn iter_models(&self) -> Result<impl Iterator<Item = ModelRef<'a>> + ExactSizeIterator> {
        let models = self.models()?;
        Ok(models
            .models
            .iter()
            .enumerate()
            .map(move |(i, model)| ModelRef {
                model,
                offset: models.offset + i * size_of::<Model>(),
                bytes: models.bytes,
            }))
    }

    pub fn name(&self) -> Result<&'a str> {
        let offset = (self.offset as isize + self.body_part.name_offset as isize) as usize;

        str::from_utf8(
            null_terminated_prefix(self.bytes.get(offset..).ok_or(Error::Corrupted {
                ty: FileType::Mdl,
                error: "body part name offset out of bounds",
            })?)
            .ok_or(Error::Corrupted {
                ty: FileType::Mdl,
                error: "eof reading body part name",
            })?,
        )
        .map_err(|_| Error::Corrupted {
            ty: FileType::Mdl,
            error: "body part name is not valid utf8",
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ModelsRef<'a> {
    models: &'a [Model],
    offset: usize,
    bytes: &'a [u8],
}

impl<'a> ModelsRef<'a> {
    pub fn get(&self, index: usize) -> Option<ModelRef<'a>> {
        self.models.get(index).map(|model| ModelRef {
            model,
            offset: self.offset + index * size_of::<Model>(),
            bytes: self.bytes,
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ModelRef<'a> {
    model: &'a Model,
    offset: usize,
    bytes: &'a [u8],
}

impl<'a> ModelRef<'a> {
    pub fn meshes(&self) -> Result<MeshesRef<'a>> {
        let offset = (self.offset as isize + self.model.mesh_offset as isize) as usize;
        let count = self
            .model
            .mesh_count
            .try_into()
            .map_err(|_| Error::Corrupted {
                ty: FileType::Mdl,
                error: "model meshes count is negative",
            })?;

        let meshes = self
            .bytes
            .get(offset..)
            .and_then(|bytes| LayoutVerified::new_slice_from_prefix(bytes, count))
            .ok_or(Error::Corrupted {
                ty: FileType::Mdl,
                error: "model meshes out of bounds or misaligned",
            })?
            .0
            .into_slice();

        Ok(MeshesRef {
            meshes,
            offset,
            bytes: self.bytes,
        })
    }

    pub fn iter_meshes(&self) -> Result<impl Iterator<Item = &Mesh> + ExactSizeIterator> {
        let meshes = self.meshes()?;
        Ok(meshes.meshes.iter())
    }

    pub fn name(&self) -> Result<&'a str> {
        str::from_utf8(&self.model.name).map_err(|_| Error::Corrupted {
            ty: FileType::Mdl,
            error: "model name is not valid utf8",
        })
    }
}

impl<'a> Deref for ModelRef<'a> {
    type Target = Model;

    fn deref(&self) -> &Self::Target {
        self.model
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MeshesRef<'a> {
    meshes: &'a [Mesh],
    offset: usize,
    bytes: &'a [u8],
}

impl<'a> MeshesRef<'a> {
    pub fn get(&self, index: usize) -> Option<MeshRef<'a>> {
        self.meshes.get(index).map(|mesh| MeshRef {
            mesh,
            offset: self.offset + index * size_of::<Mesh>(),
            bytes: self.bytes,
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MeshRef<'a> {
    mesh: &'a Mesh,
    offset: usize,
    bytes: &'a [u8],
}

#[cfg(all(test, feature = "steam"))]
mod tests {
    use std::{collections::BTreeMap, result};

    use crate::{
        fs::{DirEntryType, OpenFileSystem, Path, ReadDir},
        steam::Libraries,
    };

    use super::*;

    /// Fails if steam is not installed
    #[test]
    #[ignore]
    fn count_mdl_versions() {
        let libraries = Libraries::discover().unwrap();
        for result in libraries.apps().source().filesystems() {
            match result {
                Ok(filesystem) => {
                    eprintln!("reading from filesystem: {}", filesystem.name);
                    let filesystem = filesystem.open().unwrap();
                    let mut version_counter = BTreeMap::new();
                    recurse(
                        filesystem.read_dir(Path::try_from_str("models").unwrap()),
                        &filesystem,
                        &mut version_counter,
                    );
                    eprintln!("mdl versions: {:?}", version_counter);
                }
                Err(err) => eprintln!("warning: failed filesystem discovery: {}", err),
            }
        }
    }

    fn recurse(
        readdir: ReadDir,
        file_system: &OpenFileSystem,
        version_counter: &mut BTreeMap<i32, usize>,
    ) {
        for entry in readdir.map(result::Result::unwrap) {
            let name = entry.name();
            match entry.entry_type() {
                DirEntryType::File => {
                    if is_mdl_file(name.as_str()) {
                        let file = entry.open().unwrap();
                        let mdl = Mdl::read(file).unwrap();
                        mdl.check_signature().unwrap();
                        let version = mdl.version().unwrap();
                        *version_counter.entry(version).or_default() += 1;
                    }
                }
                DirEntryType::Directory => recurse(entry.read_dir(), file_system, version_counter),
            }
        }
    }

    fn is_mdl_file(filename: &str) -> bool {
        filename
            .rsplit('.')
            .next()
            .map(|ext| ext.eq_ignore_ascii_case("mdl"))
            == Some(true)
    }
}
