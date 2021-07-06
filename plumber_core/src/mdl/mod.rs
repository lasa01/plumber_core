/// version 49
use zerocopy::{FromBytes, LayoutVerified};

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
struct Header {
    id: i32,
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
    skin_r_family_count: i32,

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

    anim_block_model: i32,

    bone_table_name_offset: i32,

    vertex_base: i32,
    offset_base: i32,

    directional_dot_product: u8,
    root_lod: u8,
    num_allowed_root_lods: u8,

    unused: u8,
    zero_frame_cache_index: i32,

    flex_controller_ui_count: i32,
    flex_controller_ui_offset: i32,

    header_2_offset: i32,
}

#[derive(Debug, PartialEq, FromBytes)]
#[repr(C)]
struct Header2 {
    src_bone_transform_count: i32,
    src_bone_transform_offset: i32,

    illum_position_attachment_index: i32,

    max_eye_deflection: f32,

    linear_bone_offset: i32,

    name_copy_offset: i32,
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
struct BoneController {
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
