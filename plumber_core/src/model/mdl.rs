use std::collections::BTreeMap;
use std::fmt;
use std::ops::Deref;
use std::{io, mem::size_of, str};

use bitflags::bitflags;
use glam::{EulerRot, Quat, Vec3};
use itertools::Itertools;
use maligned::A4;
use zerocopy::FromBytes;

use crate::fs::GameFile;

use super::binary_utils::parse_mut;
use super::{
    binary_utils::{
        null_terminated_prefix, parse, parse_slice, parse_slice_mut, read_file_aligned,
    },
    Error, FileType, Result,
};

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
pub struct Bone {
    name_offset: i32,
    pub parent_bone_index: i32,
    bone_controller_indexes: [i32; 6],

    pub position: [f32; 3],
    pub quat: [f32; 4],
    pub rotation: [f32; 3],
    pub position_scale: [f32; 3],
    pub rotation_scale: [f32; 3],

    pub pose_to_bone: [f32; 12],

    pub q_alignment: [f32; 4],

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
pub struct AnimationDesc {
    base_header_offset: i32,
    name_offset: i32,
    pub fps: f32,
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

    span_frame_count: i16,
    span_count: i16,
    span_offset: i32,
    span_stall_time: f32,
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

fn corrupted(error: &'static str) -> Error {
    Error::Corrupted {
        ty: FileType::Mdl,
        error,
    }
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
        let signature = self
            .bytes
            .get(0..4)
            .ok_or_else(|| corrupted("eof reading signature"))?;

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
            return Err(corrupted("eof reading version"));
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
        let header_1: &Header1 =
            parse(&self.bytes, 0).ok_or_else(|| corrupted("eof reading header"))?;

        let header_2 = if header_1.header_2_offset > 0 {
            Some(
                parse(&self.bytes, header_1.header_2_offset as usize)
                    .ok_or_else(|| corrupted("header 2 out of bounds or misaligned"))?,
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
                    null_terminated_prefix(
                        self.bytes
                            .get(offset..)
                            .ok_or_else(|| corrupted("header 2 name offset out of bounds"))?,
                    )
                    .ok_or_else(|| corrupted("eof reading header 2 name"))?,
                )
                .map_err(|_| corrupted("header 2 name is not valid utf8"));
            }
        }
        str::from_utf8(null_terminated_prefix(&self.header_1.name).expect("name can not be empty"))
            .map_err(|_| corrupted("header name is not valid utf8"))
    }

    pub fn flags(&self) -> HeaderFlags {
        HeaderFlags::from_bits_truncate(self.header_1.flags)
    }

    pub fn iter_bones(&self) -> Result<impl Iterator<Item = BoneRef<'a>>> {
        let (offset, bones) = self.bones()?;
        let bytes = self.bytes;

        Ok(bones.iter().enumerate().map(move |(i, bone)| BoneRef {
            bone,
            offset: offset + i * size_of::<Bone>(),
            bytes,
        }))
    }

    fn bones(&self) -> Result<(usize, &'a [Bone])> {
        let offset: usize = self
            .header_1
            .bone_offset
            .try_into()
            .map_err(|_| corrupted("bone offset is negative"))?;
        let count = self
            .header_1
            .bone_count
            .try_into()
            .map_err(|_| corrupted("bone count is negative"))?;
        let bones = parse_slice(self.bytes, offset, count)
            .ok_or_else(|| corrupted("bones out of bounds or misaligned"))?;

        Ok((offset, bones))
    }

    pub fn iter_textures(&self) -> Result<impl Iterator<Item = TextureRef<'a>>> {
        let offset = self
            .header_1
            .texture_offset
            .try_into()
            .map_err(|_| corrupted("texture offset is negative"))?;
        let count = self
            .header_1
            .texture_count
            .try_into()
            .map_err(|_| corrupted("texture count is negative"))?;

        let textures = parse_slice(self.bytes, offset, count)
            .ok_or_else(|| corrupted("textures out of bounds or misaligned"))?;

        let bytes = self.bytes;

        Ok(textures
            .iter()
            .enumerate()
            .map(move |(i, texture)| TextureRef {
                texture,
                offset: offset + i * size_of::<Texture>(),
                bytes,
            }))
    }

    pub fn texture_paths(&self) -> Result<Vec<&'a str>> {
        let offset = self
            .header_1
            .texture_dir_offset
            .try_into()
            .map_err(|_| corrupted("texture paths offset is negative"))?;
        let count = self
            .header_1
            .texture_dir_count
            .try_into()
            .map_err(|_| corrupted("texture paths count is negative"))?;

        let path_offsets: &[i32] = parse_slice(self.bytes, offset, count)
            .ok_or_else(|| corrupted("texture paths out of bounds or misaligned"))?;

        path_offsets
            .iter()
            .map(|&offset| {
                let offset = offset
                    .try_into()
                    .map_err(|_| corrupted("a texture path offset is negative"))?;

                if offset == 0 {
                    Ok("")
                } else {
                    let bytes = self
                        .bytes
                        .get(offset..)
                        .ok_or_else(|| corrupted("a texture path is out of bounds"))?;

                    str::from_utf8(
                        null_terminated_prefix(bytes)
                            .ok_or_else(|| corrupted("eof reading a texture path"))?,
                    )
                    .map_err(|_| corrupted("a texture path is not valid utf8"))
                }
            })
            .try_collect()
    }

    pub fn iter_body_parts(
        &self,
    ) -> Result<impl Iterator<Item = BodyPartRef<'a>> + ExactSizeIterator> {
        let offset = self
            .header_1
            .body_part_offset
            .try_into()
            .map_err(|_| corrupted("body part offset is negative"))?;
        let count = self
            .header_1
            .body_part_count
            .try_into()
            .map_err(|_| corrupted("body part count is negative"))?;

        let body_parts = parse_slice(self.bytes, offset, count)
            .ok_or_else(|| corrupted("body parts out of bounds or misaligned"))?;

        let bytes = self.bytes;

        Ok(body_parts
            .iter()
            .enumerate()
            .map(move |(i, body_part)| BodyPartRef {
                body_part,
                offset: offset + i * size_of::<BodyPart>(),
                bytes,
            }))
    }

    pub fn iter_animation_descs(&self) -> Result<impl Iterator<Item = AnimationDescRef<'a>>> {
        let offset: usize = self
            .header_1
            .local_anim_offset
            .try_into()
            .map_err(|_| corrupted("local animation offset is negative"))?;
        let count = self
            .header_1
            .local_anim_count
            .try_into()
            .map_err(|_| corrupted("local animation count is negative"))?;
        let animation_descs = parse_slice(self.bytes, offset, count)
            .ok_or_else(|| corrupted("local animations out of bounds or misaligned"))?;

        let bytes = self.bytes;

        let (_, bones) = self.bones()?;
        Ok(animation_descs
            .iter()
            .enumerate()
            .map(move |(i, animation_desc)| AnimationDescRef {
                animation_desc,
                offset: offset + i * size_of::<AnimationDesc>(),
                bones,
                bytes,
            }))
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
pub struct BoneRef<'a> {
    bone: &'a Bone,
    offset: usize,
    bytes: &'a [u8],
}

impl<'a> BoneRef<'a> {
    pub fn name(&self) -> Result<&'a str> {
        let offset = self.offset as isize + self.bone.name_offset as isize;
        str::from_utf8(
            null_terminated_prefix(
                self.bytes
                    .get(offset as usize..)
                    .ok_or_else(|| corrupted("bone name out of bounds"))?,
            )
            .ok_or_else(|| corrupted("eof reading bone name"))?,
        )
        .map_err(|_| corrupted("bone name is not valid utf8"))
    }

    pub fn surface_prop(&self) -> Result<Option<&'a str>> {
        if self.bone.surface_prop_name_offset == 0 {
            return Ok(None);
        }
        let offset = self.offset as isize + self.bone.surface_prop_name_offset as isize;
        str::from_utf8(
            null_terminated_prefix(
                self.bytes
                    .get(offset as usize..)
                    .ok_or_else(|| corrupted("bone surface prop out of bounds"))?,
            )
            .ok_or_else(|| corrupted("eof reading bone surface prop"))?,
        )
        .map_err(|_| corrupted("bone surface prop is not valid utf8"))
        .map(Some)
    }
}

impl<'a> Deref for BoneRef<'a> {
    type Target = Bone;

    fn deref(&self) -> &Self::Target {
        self.bone
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
            null_terminated_prefix(
                self.bytes
                    .get(offset as usize..)
                    .ok_or_else(|| corrupted("texture name out of bounds"))?,
            )
            .ok_or_else(|| corrupted("eof reading texture name"))?,
        )
        .map_err(|_| corrupted("texture name is not valid utf8"))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BodyPartRef<'a> {
    body_part: &'a BodyPart,
    offset: usize,
    bytes: &'a [u8],
}

impl<'a> BodyPartRef<'a> {
    pub fn iter_models(&self) -> Result<impl Iterator<Item = ModelRef<'a>> + ExactSizeIterator> {
        let offset = (self.offset as isize + self.body_part.model_offset as isize) as usize;
        let count = self
            .body_part
            .model_count
            .try_into()
            .map_err(|_| corrupted("body part models count is negative"))?;

        let models = parse_slice(self.bytes, offset, count)
            .ok_or_else(|| corrupted("body part models out of bounds or misaligned"))?;

        let bytes = self.bytes;

        Ok(models.iter().enumerate().map(move |(i, model)| ModelRef {
            model,
            offset: offset + i * size_of::<Model>(),
            bytes,
        }))
    }

    pub fn name(&self) -> Result<&'a str> {
        let offset = (self.offset as isize + self.body_part.name_offset as isize) as usize;

        str::from_utf8(
            null_terminated_prefix(
                self.bytes
                    .get(offset..)
                    .ok_or_else(|| corrupted("body part name offset out of bounds"))?,
            )
            .ok_or_else(|| corrupted("eof reading body part name"))?,
        )
        .map_err(|_| corrupted("body part name is not valid utf8"))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ModelRef<'a> {
    model: &'a Model,
    offset: usize,
    bytes: &'a [u8],
}

impl<'a> ModelRef<'a> {
    pub fn iter_meshes(&self) -> Result<impl Iterator<Item = &Mesh> + ExactSizeIterator> {
        let offset = (self.offset as isize + self.model.mesh_offset as isize) as usize;
        let count = self
            .model
            .mesh_count
            .try_into()
            .map_err(|_| corrupted("model meshes count is negative"))?;

        let meshes = parse_slice(self.bytes, offset, count)
            .ok_or_else(|| corrupted("model meshes out of bounds or misaligned"))?;

        Ok(meshes.iter())
    }

    pub fn name(&self) -> Result<&'a str> {
        str::from_utf8(
            null_terminated_prefix(&self.model.name).expect("model name cannot be empty"),
        )
        .map_err(|_| corrupted("model name is not valid utf8"))
    }
}

impl<'a> Deref for ModelRef<'a> {
    type Target = Model;

    fn deref(&self) -> &Self::Target {
        self.model
    }
}

bitflags! {
    pub struct AnimationDescFlags: i32 {
        const LOOPING = 0x0001;
        const SNAP = 0x0002;
        const DELTA = 0x0004;
        const AUTOPLAY = 0x0008;
        const POST = 0x0010;
        const ALLZEROS = 0x0020;
        const FRAMEANIM = 0x0040;
        const CYCLEPOSE = 0x0080;
        const REALTIME = 0x0100;
        const LOCAL = 0x0200;
        const HIDDEN = 0x0400;
        const OVERRIDE = 0x0800;
        const ACTIVITY = 0x1000;
        const EVENT = 0x2000;
        const WORLD = 0x4000;
        const NOFORCELOOP = 0x8000;
        const EVENT_CLIENT = 0x10000;
    }
}

#[derive(Debug, Clone, Copy)]
pub struct AnimationDescRef<'a> {
    pub animation_desc: &'a AnimationDesc,
    offset: usize,
    bones: &'a [Bone],
    bytes: &'a [u8],
}

impl<'a> AnimationDescRef<'a> {
    pub fn name(&self) -> Result<&'a str> {
        let offset = self.offset as isize + self.animation_desc.name_offset as isize;
        str::from_utf8(
            null_terminated_prefix(
                self.bytes
                    .get(offset as usize..)
                    .ok_or_else(|| corrupted("animation name out of bounds"))?,
            )
            .ok_or_else(|| corrupted("eof reading animation name"))?,
        )
        .map_err(|_| corrupted("animation name is not valid utf8"))
    }

    pub fn flags(&self) -> AnimationDescFlags {
        AnimationDescFlags::from_bits_truncate(self.animation_desc.flags)
    }

    pub fn iter_movements(&self) -> Result<impl Iterator<Item = MovementRef<'a>>> {
        let offset = (self.offset as isize + self.animation_desc.movement_offset as isize) as usize;
        let count = self
            .animation_desc
            .movement_count
            .try_into()
            .map_err(|_| corrupted("animation movements count is negative"))?;

        let movements = parse_slice(self.bytes, offset, count)
            .ok_or_else(|| corrupted("animation movements out of bounds or misaligned"))?;

        let bytes = self.bytes;

        Ok(movements
            .iter()
            .enumerate()
            .map(move |(i, movement)| MovementRef {
                _movement: movement,
                _offset: offset + i * size_of::<Movement>(),
                _bytes: bytes,
            }))
    }

    pub fn data(&self) -> Result<BTreeMap<usize, BoneAnimationData>> {
        let frame_animation = self.flags().contains(AnimationDescFlags::FRAMEANIM);

        if let Some(sections) = self.iter_animation_sections()? {
            merge_animation_sections(
                sections,
                self.bones,
                frame_animation,
                self.animation_desc.frame_count as usize,
            )
            .map(|mut data| {
                make_animation_quats_compatible(&mut data);
                data
            })
        } else {
            self.animation_section()?
                .data(frame_animation)
                .map(|mut data| {
                    make_animation_quats_compatible(&mut data);
                    data
                })
        }
    }

    fn iter_animation_sections(
        &self,
    ) -> Result<Option<impl Iterator<Item = AnimationSectionRef<'a>>>> {
        if self.animation_desc.section_offset == 0 || self.animation_desc.section_frame_count < 0 {
            return Ok(None);
        }

        let offset = (self.offset as isize + self.animation_desc.section_offset as isize) as usize;
        let count = (self.animation_desc.frame_count / self.animation_desc.section_frame_count + 2)
            .try_into()
            .map_err(|_| corrupted("calculated animation section count is negative"))?;

        let animation_sections: &[AnimationSection] = parse_slice(self.bytes, offset, count)
            .ok_or_else(|| corrupted("animation sections out of bounds or misaligned"))?;

        if animation_sections.iter().any(|sec| sec.anim_block != 0) {
            return Err(Error::Unsupported {
                ty: FileType::Mdl,
                feature: "external animation in .ani file",
            });
        }

        let first_section_anim_offset = match animation_sections.get(0) {
            None => 0,
            Some(section) => section.anim_offset as isize,
        };
        let anim_offset = (self.offset as isize + self.animation_desc.anim_offset as isize
            - first_section_anim_offset) as usize;

        let section_frame_count: usize = self
            .animation_desc
            .section_frame_count
            .try_into()
            .map_err(|_| corrupted("animation section frame count is negative"))?;

        let frame_count: usize = self
            .animation_desc
            .frame_count
            .try_into()
            .map_err(|_| corrupted("animation frame count is negative"))?;

        let bytes = self.bytes;
        let bones = self.bones;

        Ok(Some(animation_sections.iter().enumerate().map(
            move |(i, animation_section)| AnimationSectionRef {
                anim_offset: (anim_offset as isize + animation_section.anim_offset as isize)
                    as usize,
                bones,
                // check for last section (there are apparently 2 last sections)
                frame_count: if i < count - 2 {
                    section_frame_count
                } else {
                    frame_count - (count - 2) * section_frame_count
                },
                // I have no idea but this is what Crowbar does
                last_section: i >= count - 2 || frame_count == (i + 1) * section_frame_count,
                bytes,
            },
        )))
    }

    fn animation_section(&self) -> Result<AnimationSectionRef<'a>> {
        if self.animation_desc.anim_block != 0 {
            return Err(Error::Unsupported {
                ty: FileType::Mdl,
                feature: "external animation in .ani file",
            });
        }

        let anim_offset =
            (self.offset as isize + self.animation_desc.anim_offset as isize) as usize;
        let frame_count: usize = self
            .animation_desc
            .frame_count
            .try_into()
            .map_err(|_| corrupted("animation frame count is negative"))?;

        Ok(AnimationSectionRef {
            anim_offset,
            bones: self.bones,
            frame_count,
            last_section: true,
            bytes: self.bytes,
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MovementRef<'a> {
    _movement: &'a Movement,
    _offset: usize,
    _bytes: &'a [u8],
}

#[derive(Debug, Clone, Copy)]
struct AnimationSectionRef<'a> {
    anim_offset: usize,
    bones: &'a [Bone],
    frame_count: usize,
    last_section: bool,
    bytes: &'a [u8],
}

impl<'a> AnimationSectionRef<'a> {
    fn data(&self, frame_animation: bool) -> Result<BTreeMap<usize, BoneAnimationData>> {
        let data: BTreeMap<usize, BoneAnimationData> = if frame_animation {
            Ok(self
                .frame_animation()?
                .animation_data()?
                .into_iter()
                .enumerate()
                .filter(|(_, d)| {
                    !matches!(
                        (&d.position, &d.rotation),
                        (AnimationData::None, AnimationData::None)
                    )
                })
                .collect())
        } else {
            self.iter_bone_animations()
                .map(|res| {
                    res.and_then(|anim| {
                        let bone_index = anim.animation.bone_index as usize;
                        anim.animation_data().map(|data| (bone_index, data))
                    })
                })
                .try_collect()
        }?;

        Ok(data)
    }

    fn frame_animation(&self) -> Result<FrameAnimationRef<'a>> {
        let offset = self.anim_offset;

        let frame_animation = parse(self.bytes, offset).ok_or_else(|| {
            corrupted("animation section frame animation out of bounds or misaligned")
        })?;

        Ok(FrameAnimationRef {
            frame_animation,
            offset,
            bone_count: self.bones.len(),
            frame_count: self.frame_count,
            last_section: self.last_section,
            bytes: self.bytes,
        })
    }

    fn iter_bone_animations(&self) -> impl Iterator<Item = Result<AnimationRef<'a>>> {
        IterBoneAnimations {
            offset: self.anim_offset,
            bones: self.bones,
            frame_count: self.frame_count,
            finished: false,
            bytes: self.bytes,
        }
    }
}

fn make_animation_quats_compatible(animation_data: &mut BTreeMap<usize, BoneAnimationData>) {
    for data in animation_data.values_mut() {
        if let AnimationData::Animated(values) = &mut data.rotation {
            let mut previous = None;

            for value in values {
                if let Some(previous) = previous {
                    make_quat_compatible(value, previous);
                }
                previous = Some(*value);
            }
        }
    }
}

fn make_quat_compatible(quat: &mut Quat, previous: Quat) {
    if previous.dot(*quat) < 0.0 {
        *quat = -*quat;
    }
}

fn merge_animation_sections<'a>(
    sections: impl Iterator<Item = AnimationSectionRef<'a>>,
    bones: &[Bone],
    frame_animation: bool,
    total_frames: usize,
) -> Result<BTreeMap<usize, BoneAnimationData>> {
    let mut acc_data = BTreeMap::new();
    let mut first_section = true;
    let mut accumulated_frames = 0;

    for section in sections {
        if accumulated_frames == total_frames {
            break;
        }

        let section_data = section.data(frame_animation)?;
        let current_frames = section.frame_count.min(total_frames - accumulated_frames);

        if first_section {
            acc_data = section_data;
            first_section = false;
            accumulated_frames += current_frames;
            continue;
        }

        for (bone_index, bone_data) in section_data {
            let bone_acc = acc_data.entry(bone_index).or_default();

            let bone_rotation = euler_to_quat(bones[bone_index].rotation.into());
            let bone_position = bones[bone_index].position.into();

            accumulate_animation(
                &mut bone_acc.rotation,
                bone_data.rotation,
                bone_rotation,
                accumulated_frames,
                current_frames,
            );

            accumulate_animation(
                &mut bone_acc.position,
                bone_data.position,
                bone_position,
                accumulated_frames,
                current_frames,
            );
        }

        accumulated_frames += current_frames;
    }

    Ok(acc_data)
}

fn accumulate_animation<T: Copy + PartialEq>(
    acc: &mut AnimationData<T>,
    next: AnimationData<T>,
    bone_data: T,
    accumulated_frames: usize,
    next_frames: usize,
) {
    use AnimationData::{Animated, Constant, None};

    match (&mut *acc, next) {
        (None, None) => {}
        (None, Constant(next)) => {
            *acc = Animated(merge_constant_constant(
                bone_data,
                accumulated_frames,
                next,
                next_frames,
            ));
        }
        (None, Animated(next)) => {
            *acc = Animated(merge_constant_animated(
                bone_data,
                accumulated_frames,
                &next,
                next_frames,
            ));
        }
        (Constant(prev), None) => {
            *acc = Animated(merge_constant_constant(
                *prev,
                accumulated_frames,
                bone_data,
                next_frames,
            ));
        }
        (Constant(prev), Constant(next)) => {
            if *prev != next {
                *acc = Animated(merge_constant_constant(
                    *prev,
                    accumulated_frames,
                    next,
                    next_frames,
                ));
            }
        }
        (Constant(prev), Animated(next)) => {
            *acc = Animated(merge_constant_animated(
                *prev,
                accumulated_frames,
                &next,
                next_frames,
            ));
        }
        (Animated(prev), None) => {
            merge_animated_constant(prev, bone_data, next_frames);
        }
        (Animated(prev), Animated(next)) => {
            prev.extend_from_slice(next.get(0..next_frames).unwrap_or(&next));
        }
        (Animated(prev), Constant(next)) => {
            merge_animated_constant(prev, next, next_frames);
        }
    }
}

fn merge_constant_constant<T: Copy>(prev: T, prev_n: usize, next: T, next_n: usize) -> Vec<T> {
    let mut data = Vec::with_capacity(prev_n + next_n);

    for _ in 0..prev_n {
        data.push(prev);
    }

    for _ in 0..next_n {
        data.push(next);
    }

    data
}

fn merge_constant_animated<T: Copy>(prev: T, prev_n: usize, next: &[T], next_n: usize) -> Vec<T> {
    let mut data = Vec::with_capacity(prev_n + next_n);

    for _ in 0..prev_n {
        data.push(prev);
    }

    data.extend_from_slice(next.get(0..next_n).unwrap_or(next));

    data
}

fn merge_animated_constant<T: Copy>(prev: &mut Vec<T>, next: T, next_n: usize) {
    prev.reserve(next_n);

    for _ in 0..next_n {
        prev.push(next);
    }
}

struct IterBoneAnimations<'a> {
    offset: usize,
    bones: &'a [Bone],
    frame_count: usize,
    finished: bool,
    bytes: &'a [u8],
}

impl<'a> Iterator for IterBoneAnimations<'a> {
    type Item = Result<AnimationRef<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.finished {
            return None;
        }

        let animation: &Animation = match parse(self.bytes, self.offset) {
            Some(animation) => animation,
            None => {
                return Some(Err(corrupted(
                    "animation section animation out of bounds or misaligned",
                )))
            }
        };

        if animation.bone_index == 255 || animation.bone_index as usize >= self.bones.len() {
            self.finished = true;
            return None;
        }

        let animation_ref = AnimationRef {
            animation,
            offset: self.offset,
            bone: &self.bones[animation.bone_index as usize],
            frame_count: self.frame_count,
            bytes: self.bytes,
        };

        let next_offset: usize = match animation.next_offset.try_into() {
            Ok(offset) => offset,
            Err(_) => {
                return Some(Err(corrupted(
                    "animation section animation next offset is negative",
                )))
            }
        };

        if next_offset == 0 {
            self.finished = true;
        } else {
            self.offset += next_offset;
        }

        Some(Ok(animation_ref))
    }
}

bitflags! {
    struct BoneFlags: u8 {
        const RAWPOS = 0x01;
        const RAWROT = 0x02;
        const ANIMPOS = 0x04;
        const ANIMROT = 0x08;
        const FULLANIMPOS = 0x10;
        const CONST_ROT2 = 0x40;
        const ANIM_ROT2 = 0x80;
    }
}

fn quat_from_bytes_48(bytes: [u8; 6]) -> Quat {
    let a = (u16::from(bytes[1] & 0x7f) << 8) | u16::from(bytes[0]);
    let b = (u16::from(bytes[3] & 0x7f) << 8) | u16::from(bytes[2]);
    let c = (u16::from(bytes[5] & 0x7f) << 8) | u16::from(bytes[4]);

    let missing_component_index = ((bytes[1] & 0x80) >> 6) | ((bytes[3] & 0x80) >> 7);
    let missing_component_sign = if bytes[5] & 0x80 > 0 { -1.0 } else { 1.0 };

    let a = (f32::from(a) - 16384.0) / 23168.0;
    let b = (f32::from(b) - 16384.0) / 23168.0;
    let c = (f32::from(c) - 16384.0) / 23168.0;

    let missing_component = (1.0 - a * a - b * b - c * c).sqrt() * missing_component_sign;

    match missing_component_index {
        1 => Quat::from_xyzw(missing_component, a, b, c),
        2 => Quat::from_xyzw(c, missing_component, a, b),
        3 => Quat::from_xyzw(b, c, missing_component, a),
        0 => Quat::from_xyzw(a, b, c, missing_component),
        4.. => {
            unreachable!("missing component index has only 2 nonzero bits, so maximum value is 3")
        }
    }
}

fn quat_from_bytes_64(bytes: [u8; 8]) -> Quat {
    let x = u32::from(bytes[0]) | u32::from(bytes[1]) << 8 | u32::from(bytes[2] & 0x1f) << 16;
    let x = (x as f32 - 1_048_576.0) / 1_048_576.5;

    let y = u32::from(bytes[2] & 0xe0) >> 5
        | u32::from(bytes[3]) << 3
        | u32::from(bytes[4]) << 11
        | u32::from(bytes[5] & 0x3) << 19;
    let y = (y as f32 - 1_048_576.0) / 1_048_576.5;

    let z = u32::from(bytes[5] & 0xfc) >> 2
        | u32::from(bytes[6]) << 6
        | u32::from(bytes[7] & 0x7f) << 14;
    let z = (z as f32 - 1_048_576.0) / 1_048_576.5;

    let w_sign = if bytes[7] & 0x80 > 0 { -1.0 } else { 1.0 };
    let w = (1.0 - x * x - y * y - z * z).sqrt() * w_sign;

    Quat::from_xyzw(x, y, z, w)
}

fn quat_from_u16s(u16s: [u16; 3]) -> Quat {
    let x = (f32::from(u16s[0]) - 32768.0) / 32768.0;
    let y = (f32::from(u16s[1]) - 32768.0) / 32768.0;
    let z = (f32::from(u16s[2] & 0x7fff) - 16384.0) / 16384.0;

    let w_sign = if u16s[2] & 0x8000 > 0 { -1.0 } else { 1.0 };

    let w = (1.0 - x * x - y * y - z * z).sqrt() * w_sign;

    Quat::from_xyzw(x, y, z, w)
}

fn f16_to_f32(f16: u16) -> f32 {
    let mantissa = u32::from(f16 & 0x3ff);
    let biased_exponent = u32::from((f16 & 0x7c00) >> 10);
    let sign = u32::from((f16 & 0x8000) >> 15);

    let float_sign = if sign == 1 { -1.0 } else { 1.0 };

    if biased_exponent == 31 {
        if mantissa == 0 {
            // Infinity
            return 65504.0 * float_sign;
        }
        // NaN
        return 0.0;
    }

    if biased_exponent == 0 && mantissa != 0 {
        let float_mantissa = mantissa as f32 / 1024.0;
        float_sign * float_mantissa / 16384.0
    } else {
        f32::from_bits(sign << 31 | (biased_exponent + 127 - 15) << 23 | mantissa << (23 - 10))
    }
}

pub fn vec3_from_u16s(u16s: [u16; 3]) -> Vec3 {
    Vec3::new(
        f16_to_f32(u16s[0]),
        f16_to_f32(u16s[1]),
        f16_to_f32(u16s[2]),
    )
}

#[cfg(test)]
use serde::Deserialize;

// Generic animation data of a bone.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(test, derive(Deserialize))]
pub enum AnimationData<T> {
    /// The data of the bone stays constant during the animation.
    Constant(T),
    /// The data of the bone is animated. Contains one value for each frame.
    Animated(Vec<T>),
    /// The animation has no data for the bone.
    None,
}

impl<T> Default for AnimationData<T> {
    fn default() -> Self {
        Self::None
    }
}

/// Rotation and position animation data of a bone.
#[derive(Debug, Clone, Default, PartialEq)]
#[cfg_attr(test, derive(Deserialize))]
pub struct BoneAnimationData {
    pub rotation: AnimationData<Quat>,
    pub position: AnimationData<Vec3>,
}

#[derive(Debug, Clone, Copy)]
struct FrameAnimationRef<'a> {
    frame_animation: &'a FrameAnimation,
    offset: usize,
    bone_count: usize,
    frame_count: usize,
    last_section: bool,
    bytes: &'a [u8],
}

impl<'a> FrameAnimationRef<'a> {
    fn bone_flags(&self) -> Result<&'a [u8]> {
        let offset = self.offset + size_of::<FrameAnimation>();

        self.bytes
            .get(offset..offset + self.bone_count)
            .ok_or_else(|| corrupted("frame animation bone flags out of bounds"))
    }

    fn animation_data(&self) -> Result<Vec<BoneAnimationData>> {
        let bone_flags = self.bone_flags()?;

        let mut data = vec![BoneAnimationData::default(); bone_flags.len()];

        self.read_bone_constants(bone_flags, &mut data)?;
        self.read_bone_frames(bone_flags, &mut data)?;

        Ok(data)
    }

    fn read_bone_constants(&self, bone_flags: &[u8], data: &mut [BoneAnimationData]) -> Result<()> {
        if self.frame_animation.constants_offset == 0 {
            return Ok(());
        }

        let offset =
            (self.offset as isize + self.frame_animation.constants_offset as isize) as usize;
        let mut bytes = self
            .bytes
            .get(offset..)
            .ok_or_else(|| corrupted("frame animation bone constants out of bounds"))?;

        for (&flags, data) in bone_flags.iter().zip(data) {
            let flags = BoneFlags::from_bits_truncate(flags);

            if flags.contains(BoneFlags::CONST_ROT2) {
                let value_bytes = bytes
                    .get(..6)
                    .ok_or_else(|| corrupted("frame animation bone constants out of bounds"))?
                    .try_into()
                    .expect("slice must have correct length");
                data.rotation = AnimationData::Constant(quat_from_bytes_48(value_bytes));

                bytes = &bytes[6..];
            }

            if flags.contains(BoneFlags::RAWROT) {
                let u16s = parse_slice_mut(&mut bytes, 3).ok_or_else(|| {
                    corrupted("frame animation bone constants out of bounds or misaligned")
                })?;

                data.rotation = AnimationData::Constant(quat_from_u16s(
                    u16s.try_into().expect("slice must have correct length"),
                ));
            }

            if flags.contains(BoneFlags::RAWPOS) {
                let u16s = parse_slice_mut(&mut bytes, 3).ok_or_else(|| {
                    corrupted("frame animation bone constants out of bounds or misaligned")
                })?;

                data.position = AnimationData::Constant(vec3_from_u16s(
                    u16s.try_into().expect("slice must have correct length"),
                ));
            }
        }

        Ok(())
    }

    fn read_bone_frames(&self, bone_flags: &[u8], data: &mut [BoneAnimationData]) -> Result<()> {
        if self.frame_animation.frame_offset == 0 {
            return Ok(());
        }

        let offset = (self.offset as isize + self.frame_animation.frame_offset as isize) as usize;

        let mut bytes = self
            .bytes
            .get(offset..)
            .ok_or_else(|| corrupted("frame animation bone frames out of bounds"))?;

        let frame_count = if self.last_section {
            self.frame_count
        } else {
            self.frame_count + 1
        };

        for (&flags, data) in bone_flags.iter().zip(&mut *data) {
            let flags = BoneFlags::from_bits_truncate(flags);

            if flags.contains(BoneFlags::ANIM_ROT2) || flags.contains(BoneFlags::ANIMROT) {
                data.rotation = AnimationData::Animated(Vec::with_capacity(self.frame_count));
            }

            if flags.contains(BoneFlags::ANIMPOS) || flags.contains(BoneFlags::FULLANIMPOS) {
                data.position = AnimationData::Animated(Vec::with_capacity(self.frame_count));
            }
        }

        for _ in 0..frame_count {
            for (&flags, data) in bone_flags.iter().zip(&mut *data) {
                let flags = BoneFlags::from_bits_truncate(flags);

                if flags.contains(BoneFlags::ANIM_ROT2) {
                    let value_bytes = bytes
                        .get(..6)
                        .ok_or_else(|| corrupted("frame animation bone frames out of bounds"))?
                        .try_into()
                        .expect("slice must have correct length");

                    if let AnimationData::Animated(frames) = &mut data.rotation {
                        frames.push(quat_from_bytes_48(value_bytes));
                    } else {
                        unreachable!();
                    }

                    bytes = &bytes[6..];
                }

                if flags.contains(BoneFlags::ANIMROT) {
                    let u16s = parse_slice_mut(&mut bytes, 3).ok_or_else(|| {
                        corrupted("frame animation bone frames out of bounds or misaligned")
                    })?;

                    if let AnimationData::Animated(frames) = &mut data.rotation {
                        frames.push(quat_from_u16s(
                            u16s.try_into().expect("slice must have correct length"),
                        ));
                    } else {
                        unreachable!();
                    }
                }

                if flags.contains(BoneFlags::ANIMPOS) {
                    let u16s = parse_slice_mut(&mut bytes, 3).ok_or_else(|| {
                        corrupted("frame animation bone frames out of bounds or misaligned")
                    })?;

                    if let AnimationData::Animated(frames) = &mut data.position {
                        frames.push(vec3_from_u16s(
                            u16s.try_into().expect("slice must have correct length"),
                        ));
                    } else {
                        unreachable!();
                    }
                }

                if flags.contains(BoneFlags::FULLANIMPOS) {
                    let f32s: &[f32] = parse_slice_mut(&mut bytes, 3).ok_or_else(|| {
                        corrupted("frame animation bone frames out of bounds or misaligned")
                    })?;

                    if let AnimationData::Animated(frames) = &mut data.position {
                        frames.push(Vec3::from_slice(f32s));
                    } else {
                        unreachable!();
                    }
                }
            }
        }

        Ok(())
    }
}

bitflags! {
    struct AnimationFlags: u8 {
        const RAWPOS = 0x01;
        const RAWROT = 0x02;
        const ANIMPOS = 0x04;
        const ANIMROT = 0x08;
        const DELTA = 0x10;
        const RAWROT2 = 0x20;
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
struct AnimationValue(i16);

impl AnimationValue {
    fn from_bytes(bytes: [u8; 2]) -> Self {
        Self(i16::from_ne_bytes(bytes))
    }

    fn valid(self) -> u8 {
        self.0.to_ne_bytes()[0]
    }

    fn total(self) -> u8 {
        self.0.to_ne_bytes()[1]
    }
}

#[derive(Debug, Clone, Copy)]
struct AnimationRef<'a> {
    animation: &'a Animation,
    offset: usize,
    frame_count: usize,
    bone: &'a Bone,
    bytes: &'a [u8],
}

impl<'a> AnimationRef<'a> {
    fn animation_data(&self) -> Result<BoneAnimationData> {
        let mut bytes = self
            .bytes
            .get(self.offset + size_of::<Animation>()..)
            .expect("cannot fail: animation already parsed at offset");

        let flags = AnimationFlags::from_bits_truncate(self.animation.flags);

        let mut data = BoneAnimationData::default();

        Self::read_animation_constants(flags, &mut bytes, &mut data)?;
        self.read_animation_frames(flags, bytes, &mut data)?;

        Ok(data)
    }

    fn read_animation_constants(
        flags: AnimationFlags,
        bytes: &mut &[u8],
        data: &mut BoneAnimationData,
    ) -> Result<()> {
        if flags.contains(AnimationFlags::RAWROT2) {
            let value_bytes = bytes
                .get(..8)
                .ok_or_else(|| corrupted("animation constants out of bounds"))?
                .try_into()
                .expect("slice must have correct length");
            data.rotation = AnimationData::Constant(quat_from_bytes_64(value_bytes));

            *bytes = &bytes[8..];
        }

        if flags.contains(AnimationFlags::RAWROT) {
            let u16s = parse_slice_mut(bytes, 3)
                .ok_or_else(|| corrupted("animation constants out of bounds"))?;

            data.rotation = AnimationData::Constant(quat_from_u16s(
                u16s.try_into().expect("slice must have correct length"),
            ));
        }

        if flags.contains(AnimationFlags::RAWPOS) {
            let u16s = parse_slice_mut(bytes, 3)
                .ok_or_else(|| corrupted("animation constants out of bounds"))?;

            data.position = AnimationData::Constant(vec3_from_u16s(
                u16s.try_into().expect("slice must have correct length"),
            ));
        };

        Ok(())
    }

    fn read_animation_frames(
        &self,
        flags: AnimationFlags,
        mut bytes: &[u8],
        data: &mut BoneAnimationData,
    ) -> Result<()> {
        if flags.contains(AnimationFlags::ANIMROT) {
            let reference_bytes = bytes;

            let x_offset: i16 = *parse_mut(&mut bytes)
                .ok_or_else(|| corrupted("animation offsets out of bounds"))?;

            let y_offset: i16 = *parse_mut(&mut bytes)
                .ok_or_else(|| corrupted("animation offsets out of bounds"))?;

            let z_offset: i16 = *parse_mut(&mut bytes)
                .ok_or_else(|| corrupted("animation offsets out of bounds"))?;

            let mut frames = vec![Vec3::ZERO; self.frame_count];

            if x_offset > 0 {
                let x_values = self.read_animation_values(
                    reference_bytes
                        .get(x_offset as usize..)
                        .ok_or_else(|| corrupted("animation rotation x values out of bounds"))?,
                )?;

                for (i, frame) in frames.iter_mut().enumerate() {
                    frame.x = extract_animation_value(i, &x_values, self.bone.rotation_scale[0]);
                }
            }

            if y_offset > 0 {
                let y_values = self.read_animation_values(
                    reference_bytes
                        .get(y_offset as usize..)
                        .ok_or_else(|| corrupted("animation rotation y values out of bounds"))?,
                )?;

                for (i, frame) in frames.iter_mut().enumerate() {
                    frame.y = extract_animation_value(i, &y_values, self.bone.rotation_scale[1]);
                }
            }

            if z_offset > 0 {
                let z_values = self.read_animation_values(
                    reference_bytes
                        .get(z_offset as usize..)
                        .ok_or_else(|| corrupted("animation rotation z values out of bounds"))?,
                )?;

                for (i, frame) in frames.iter_mut().enumerate() {
                    frame.z = extract_animation_value(i, &z_values, self.bone.rotation_scale[2]);
                }
            }

            for frame in &mut frames {
                frame.x += self.bone.rotation[0];
                frame.y += self.bone.rotation[1];
                frame.z += self.bone.rotation[2];
            }

            let quat_frames = frames.into_iter().map(euler_to_quat).collect();

            data.rotation = AnimationData::Animated(quat_frames);
        }

        if flags.contains(AnimationFlags::ANIMPOS) {
            let reference_bytes = bytes;

            let x_offset: i16 = *parse_mut(&mut bytes)
                .ok_or_else(|| corrupted("animation offsets out of bounds"))?;

            let y_offset: i16 = *parse_mut(&mut bytes)
                .ok_or_else(|| corrupted("animation offsets out of bounds"))?;

            let z_offset: i16 = *parse_mut(&mut bytes)
                .ok_or_else(|| corrupted("animation offsets out of bounds"))?;

            let mut frames = vec![Vec3::ZERO; self.frame_count];

            if x_offset > 0 {
                let x_values = self.read_animation_values(
                    reference_bytes
                        .get(x_offset as usize..)
                        .ok_or_else(|| corrupted("animation position x values out of bounds"))?,
                )?;

                for (i, frame) in frames.iter_mut().enumerate() {
                    frame.x = extract_animation_value(i, &x_values, self.bone.position_scale[0]);
                }
            }

            if y_offset > 0 {
                let y_values = self.read_animation_values(
                    reference_bytes
                        .get(y_offset as usize..)
                        .ok_or_else(|| corrupted("animation position y values out of bounds"))?,
                )?;

                for (i, frame) in frames.iter_mut().enumerate() {
                    frame.y = extract_animation_value(i, &y_values, self.bone.position_scale[1]);
                }
            }

            if z_offset > 0 {
                let z_values = self.read_animation_values(
                    reference_bytes
                        .get(z_offset as usize..)
                        .ok_or_else(|| corrupted("animation position z values out of bounds"))?,
                )?;

                for (i, frame) in frames.iter_mut().enumerate() {
                    frame.z = extract_animation_value(i, &z_values, self.bone.position_scale[2]);
                }
            }

            for frame in &mut frames {
                frame.x += self.bone.position[0];
                frame.y += self.bone.position[1];
                frame.z += self.bone.position[2];
            }

            data.position = AnimationData::Animated(frames);
        };

        Ok(())
    }

    fn read_animation_values(&self, mut bytes: &[u8]) -> Result<Vec<AnimationValue>> {
        let mut values = Vec::new();
        let mut total = 0;

        while total < self.frame_count {
            let value = read_animation_value(&mut bytes)?;

            if value.total() == 0 {
                break;
            }

            total += value.total() as usize;

            values.push(value);

            for _ in 0..value.valid() {
                values.push(read_animation_value(&mut bytes)?);
            }
        }

        Ok(values)
    }
}

pub fn euler_to_quat(e: Vec3) -> Quat {
    Quat::from_euler(EulerRot::ZYX, e.z, e.y, e.x)
}

fn read_animation_value(bytes: &mut &[u8]) -> Result<AnimationValue> {
    let value_bytes = bytes
        .get(..2)
        .ok_or_else(|| corrupted("animation values out of bounds"))?
        .try_into()
        .expect("slice must have correct length");

    *bytes = &bytes[2..];

    Ok(AnimationValue::from_bytes(value_bytes))
}

fn extract_animation_value(frame: usize, values: &[AnimationValue], scale: f32) -> f32 {
    let mut k = frame;
    let mut i = 0;

    loop {
        match values.get(i) {
            Some(v) if v.total() as usize > k => break,
            Some(v) if v.total() == 0 => return 0.0,
            Some(v) => {
                k -= v.total() as usize;
                i += v.valid() as usize + 1;
            }
            None => return 0.0,
        }
    }

    values
        .get(i)
        .map(|&v| {
            if v.valid() as usize > k {
                i + k + 1
            } else {
                i + v.valid() as usize
            }
        })
        .and_then(|i| values.get(i))
        .map(|&v| f32::from(v.0) * scale)
        .unwrap_or_default()
}

#[cfg(test)]
#[allow(clippy::approx_constant)]
mod tests {
    use std::{collections::BTreeMap, f32::consts::FRAC_PI_2, path::PathBuf, result};

    use approx::{assert_relative_eq, relative_eq};
    use serde::Deserialize;

    use crate::{
        fs::{DirEntryType, GamePath, ReadDir},
        steam::Libraries,
        test_utils::{read_game_file, FileSpec},
    };

    use super::*;

    fn quat_to_euler(q: Quat) -> Vec3 {
        let (z, y, x) = q.to_euler(EulerRot::ZYX);
        Vec3::new(x, y, z)
    }

    /// Tests that euler to quat conversion is identical to Blender
    #[test]
    fn euler_to_quat_conversion_blender_compatible() {
        assert_relative_eq!(
            euler_to_quat(Vec3::new(1.442_919_9, -0.457_030_3, 0.202_343_17)),
            Quat::from_xyzw(0.657_201, -0.104_246, 0.222_718, 0.712_472),
            epsilon = 0.01,
        );

        assert_relative_eq!(
            euler_to_quat(Vec3::new(1.570_796_1, 0.0, -1.570_776_7)),
            Quat::from_xyzw(0.500_005, -0.499_995, -0.499_995, 0.500_005),
            epsilon = 0.01,
        );
    }

    /// Tests that source quat to euler conversion is identical to Crowbar
    #[test]
    fn quat_to_euler_conversion_crowbar_compatible() {
        assert_relative_eq!(
            quat_to_euler(Quat::from_xyzw(0.0, 0.999_998_57, 0.0, 0.001_691_454_9)),
            Vec3::new(3.141_592_7, 0.003_382_911_4, 3.141_592_7),
        );

        assert_relative_eq!(
            quat_to_euler(Quat::from_xyzw(0.0, 0.0, 0.707_105_3, 0.707_108_26)),
            Vec3::new(0.0, 0.0, 1.570_792_1)
        );
    }

    #[test]
    fn quat_euler_conversion_consistency() {
        let original = Quat::from_xyzw(0.657_201, -0.104_246, 0.222_718, 0.712_472);
        let converted = euler_to_quat(quat_to_euler(original));

        assert_relative_eq!(original, converted);
    }

    /// Crowbar applies -90 z rotation to things by default, this undoes it for testing against crowbar
    fn uncrowbarify_quat() -> Quat {
        Quat::from_rotation_z(FRAC_PI_2)
    }

    fn assert_quats_equal(other: Quat, mut crowbar: Quat, i: usize, uncrowbarify: bool) {
        if uncrowbarify {
            crowbar = uncrowbarify_quat() * crowbar;
        }

        assert!(
            relative_eq!(other, crowbar, epsilon = 0.001)
                || relative_eq!(other, -crowbar, epsilon = 0.001),
            "rotation at frame {}: got {}, expected {}",
            i,
            other,
            crowbar
        );
    }

    fn assert_vecs_equal(other: Vec3, mut crowbar: Vec3, i: usize, uncrowbarify: bool) {
        if uncrowbarify {
            crowbar = uncrowbarify_quat() * crowbar;
        }

        assert!(
            relative_eq!(other, crowbar, epsilon = 0.001,),
            "position at frame {}: got {}, expected {}",
            i,
            other,
            crowbar
        );
    }

    #[derive(Debug, PartialEq, Deserialize)]
    pub struct MdlSpec {
        pub version: i32,
        pub name: String,
        pub flags: i32,
        pub bones: Vec<BoneSpec>,
        pub texture_paths: Vec<String>,
        pub textures: Vec<String>,
        pub body_parts: Vec<BodyPartSpec>,
        pub animations: Vec<AnimationSpec>,
    }

    impl FileSpec for MdlSpec {
        type Type = Mdl;

        fn extension() -> &'static str {
            ".mdl"
        }

        fn read(file: std::fs::File) -> Self::Type {
            Mdl::read(GameFile::Fs(file)).unwrap()
        }

        fn verify(&self, data: Mdl) {
            assert_eq!(data.version().unwrap(), self.version);
            eprintln!("  Version ok");

            let header = data.header().unwrap();

            assert_eq!(header.name().unwrap(), self.name);
            eprintln!("  Name ok");
            assert_eq!(header.flags().bits, self.flags);
            eprintln!("  Flags ok");

            for (i, bone) in header.iter_bones().unwrap().enumerate() {
                eprintln!("  Verifying bone {}", i);
                self.bones[i].verify(bone);
            }

            assert_eq!(header.texture_paths().unwrap(), self.texture_paths);
            eprintln!("  Texture paths ok");

            for (i, body_part) in header.iter_body_parts().unwrap().enumerate() {
                eprintln!("  Verifying body part {}", i);
                self.body_parts[i].verify(body_part);
            }

            for (i, animation) in header.iter_animation_descs().unwrap().enumerate() {
                eprintln!("  Verifying animation {}", i);
                self.animations[i].verify(animation);
            }

            eprintln!("  Mdl ok");
        }
    }

    #[derive(Debug, PartialEq, Deserialize)]
    pub struct BoneSpec {
        pub name: String,
        pub surface_prop: String,
        pub parent_bone_index: i32,
        pub position: [f32; 3],
        pub position_scale: [f32; 3],
        pub rotation: [f32; 3],
        pub rotation_scale: [f32; 3],
    }

    impl BoneSpec {
        fn verify(&self, bone: BoneRef) {
            assert_eq!(bone.name().unwrap(), self.name);
            assert_eq!(bone.surface_prop().unwrap().unwrap(), self.surface_prop);
            assert_eq!(bone.parent_bone_index, self.parent_bone_index);
            assert_relative_eq!(bone.position.as_ref(), self.position.as_ref());
            assert_relative_eq!(bone.position_scale.as_ref(), self.position_scale.as_ref());
            assert_relative_eq!(bone.rotation.as_ref(), self.rotation.as_ref());
            assert_relative_eq!(bone.rotation_scale.as_ref(), self.rotation_scale.as_ref());
            eprintln!("    Bone ok");
        }
    }

    #[derive(Debug, PartialEq, Eq, Deserialize)]
    pub struct BodyPartSpec {
        pub name: String,
        pub models: Vec<ModelSpec>,
    }

    impl BodyPartSpec {
        fn verify(&self, body_part: BodyPartRef) {
            assert_eq!(body_part.name().unwrap(), self.name);

            for (i, model) in body_part.iter_models().unwrap().enumerate() {
                eprintln!("    Verifying model {}", i);
                self.models[i].verify(model);
            }
            eprintln!("    Body part ok");
        }
    }

    #[derive(Debug, PartialEq, Eq, Deserialize)]
    pub struct ModelSpec {
        pub name: String,
    }

    impl ModelSpec {
        fn verify(&self, model: ModelRef) {
            assert_eq!(model.name().unwrap(), self.name);
            eprintln!("       Model ok");
        }
    }

    #[derive(Debug, PartialEq, Deserialize)]
    pub struct AnimationSpec {
        pub name: String,
        pub flags: i32,
        #[serde(default)]
        pub should_error: bool,
        #[serde(default)]
        pub sections: Vec<SectionSpec>,
        #[serde(default)]
        pub data_final: BTreeMap<u8, BoneAnimationDataSpec>,
    }

    impl AnimationSpec {
        fn verify(&self, animation: AnimationDescRef) {
            assert_eq!(animation.name().unwrap(), self.name);
            assert_eq!(animation.flags().bits, self.flags);

            let result = animation.iter_animation_sections();

            if self.should_error {
                assert!(result.is_err());
                eprintln!("    Animation errored as expected, ok");
                return;
            }

            for (i, section) in result.unwrap().unwrap().enumerate() {
                match self.sections.get(i) {
                    None => continue,
                    Some(spec) => {
                        eprintln!("    Verifying section {}", i);
                        spec.verify(section);
                    }
                };
            }

            let data = animation.data().unwrap();

            assert_eq!(data.len(), self.data_final.len());

            for (&bone, spec) in &self.data_final {
                let bone = bone as usize;
                eprintln!("    Verifying final data for bone {}", bone);
                spec.verify(
                    data.get(&bone).unwrap(),
                    animation.bones[bone].parent_bone_index < 0,
                );
            }

            eprintln!("    Animation ok");
        }
    }

    #[derive(Debug, PartialEq, Deserialize)]
    pub struct SectionSpec {
        pub data_post_calc: BTreeMap<u8, BoneAnimationDataSpec>,
    }

    impl SectionSpec {
        fn verify(&self, section: AnimationSectionRef) {
            for bone_anim in section.iter_bone_animations() {
                let bone_anim = bone_anim.unwrap();
                let bone = bone_anim.animation.bone_index;

                eprintln!("      Verifying post-calc data for bone {}", bone);
                let spec = self.data_post_calc.get(&bone).unwrap();
                let data = bone_anim.animation_data().unwrap();
                spec.verify(&data, false);
            }

            eprintln!("      Section ok");
        }
    }

    #[derive(Debug, PartialEq, Deserialize)]
    pub struct BoneAnimationDataSpec {
        position: Vec<Vec3>,
        rotation_euler: Vec<Vec3>,
    }

    impl BoneAnimationDataSpec {
        fn verify(&self, data: &BoneAnimationData, uncrowbarify: bool) {
            match &data.position {
                AnimationData::Constant(v) => {
                    assert_eq!(
                        self.position.len(),
                        1,
                        "got constant position (1 value), expected {} values",
                        self.position.len()
                    );
                    assert_vecs_equal(*v, self.position[0], 0, uncrowbarify);
                }
                AnimationData::Animated(v) => {
                    assert_eq!(
                        v.len(),
                        self.position.len(),
                        "got {} frames of position, expected {}",
                        v.len(),
                        self.position.len()
                    );
                    for (i, (a, b)) in v.iter().zip(&self.position).enumerate() {
                        assert_vecs_equal(*a, *b, i, uncrowbarify);
                    }
                }
                AnimationData::None => assert_eq!(
                    self.position.len(),
                    0,
                    "got no position, expected {} values",
                    self.position.len()
                ),
            }

            match &data.rotation {
                AnimationData::Constant(v) => {
                    assert_eq!(
                        self.rotation_euler.len(),
                        1,
                        "got constant rotation (1 value), expected {} values",
                        self.rotation_euler.len()
                    );
                    assert_quats_equal(*v, euler_to_quat(self.rotation_euler[0]), 0, uncrowbarify);
                }
                AnimationData::Animated(v) => {
                    assert_eq!(
                        v.len(),
                        self.rotation_euler.len(),
                        "got {} frames of rotation, expected {}",
                        v.len(),
                        self.rotation_euler.len()
                    );
                    for (i, (a, b)) in v.iter().zip(&self.rotation_euler).enumerate() {
                        assert_quats_equal(*a, euler_to_quat(*b), i, uncrowbarify);
                    }
                }
                AnimationData::None => assert_eq!(
                    self.rotation_euler.len(),
                    0,
                    "got no rotation, expected {} values",
                    self.rotation_euler.len()
                ),
            }
        }
    }

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
                        filesystem.read_dir(GamePath::try_from_str("models").unwrap()),
                        &mut version_counter,
                    );
                    eprintln!("mdl versions: {:?}", version_counter);
                }
                Err(err) => eprintln!("warning: failed filesystem discovery: {}", err),
            }
        }
    }

    fn recurse(readdir: ReadDir, version_counter: &mut BTreeMap<i32, usize>) {
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
                DirEntryType::Directory => recurse(entry.read_dir(), version_counter),
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

    /// Fails if CSGO is not installed on Steam
    #[test]
    #[ignore]
    fn read_animated() {
        let data = read_game_file(
            730,
            "models/props/de_inferno/hr_i/inferno_ceiling_fan/inferno_ceiling_fan.mdl",
        );

        read_mdl_bytes(&data);
    }

    // Fails if L4D2 is not installed on Steam
    #[test]
    #[ignore]
    fn read_frame_animated() {
        let data = read_game_file(550, "models/v_models/v_huntingrifle.mdl");
        read_mdl_bytes(&data);
    }

    fn get_mdl_from_bytes(bytes: &[u8]) -> Mdl {
        let bytes = maligned::aligned::<A4, _>(bytes);
        Mdl {
            bytes: bytes.to_vec(),
        }
    }

    fn read_mdl_bytes(bytes: &[u8]) {
        let mdl = get_mdl_from_bytes(bytes);
        read_mdl(&mdl);
    }

    fn read_mdl(mdl: &Mdl) {
        mdl.check_signature().unwrap();
        mdl.check_version().unwrap();
        let header = mdl.header().unwrap();
        dbg!(header.name().unwrap(),);
        dbg!(header.flags());
        for bone in header.iter_bones().unwrap() {
            dbg!(bone.name().unwrap());
            dbg!(bone.surface_prop().unwrap());
        }
        for texture in header.iter_textures().unwrap() {
            dbg!(texture.name().unwrap());
        }
        dbg!(header.texture_paths().unwrap());
        for body_part in header.iter_body_parts().unwrap() {
            dbg!(body_part.name().unwrap());

            for model in body_part.iter_models().unwrap() {
                dbg!(model.name().unwrap());
                for _ in model.iter_meshes().unwrap() {}
            }
        }
        for animation in header.iter_animation_descs().unwrap() {
            dbg!(animation.name().unwrap());
            dbg!(animation.flags());

            for _ in animation.iter_movements().unwrap() {}

            if let Some(sections) = animation.iter_animation_sections().unwrap() {
                for section in sections {
                    read_animation_section(animation, section);
                }
            } else {
                read_animation_section(animation, animation.animation_section().unwrap());
            }
        }
    }

    fn read_animation_section(animation: AnimationDescRef, section: AnimationSectionRef) {
        if animation.flags().contains(AnimationDescFlags::FRAMEANIM) {
            let frame_animation = section.frame_animation().unwrap();
            frame_animation.animation_data().unwrap();
        } else {
            for bone_anim in section.iter_bone_animations() {
                let bone_anim = bone_anim.unwrap();
                bone_anim.animation_data().unwrap();
            }
        }
    }

    #[test]
    fn verify_against_mdl_specs() {
        let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("tests")
            .join("mdl");

        MdlSpec::verify_from_path(&path);
    }
}
