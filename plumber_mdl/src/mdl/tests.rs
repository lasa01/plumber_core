#![allow(clippy::approx_constant)]

use std::{collections::BTreeMap, f32::consts::FRAC_PI_2, path::PathBuf, result};

use approx::{assert_relative_eq, relative_eq};
use serde::Deserialize;

use plumber_fs::{DirEntryType, GamePath, ReadDir, SourceAppsExt};
use plumber_steam::Libraries;
use plumber_test_utils::{read_game_file, FileSpec};

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
        "rotation at frame {i}: got {other}, expected {crowbar}"
    );
}

fn assert_vecs_equal(other: Vec3, mut crowbar: Vec3, i: usize, uncrowbarify: bool) {
    if uncrowbarify {
        crowbar = uncrowbarify_quat() * crowbar;
    }

    assert!(
        relative_eq!(other, crowbar, epsilon = 0.001,),
        "position at frame {i}: got {other}, expected {crowbar}"
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
            eprintln!("  Verifying bone {i}");
            self.bones[i].verify(bone);
        }

        assert_eq!(header.texture_paths().unwrap(), self.texture_paths);
        eprintln!("  Texture paths ok");

        for (i, body_part) in header.iter_body_parts().unwrap().enumerate() {
            eprintln!("  Verifying body part {i}");
            self.body_parts[i].verify(body_part);
        }

        for (i, animation) in header.iter_animation_descs().unwrap().enumerate() {
            eprintln!("  Verifying animation {i}");
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
            eprintln!("    Verifying model {i}");
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
                    eprintln!("    Verifying section {i}");
                    spec.verify(section);
                }
            };
        }

        let data = animation.data().unwrap();

        assert_eq!(data.len(), self.data_final.len());

        for (&bone, spec) in &self.data_final {
            let bone = bone as usize;
            eprintln!("    Verifying final data for bone {bone}");
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

            eprintln!("      Verifying post-calc data for bone {bone}");
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
                eprintln!("mdl versions: {version_counter:?}");
            }
            Err(err) => eprintln!("warning: failed filesystem discovery: {err}"),
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
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");

    MdlSpec::verify_from_path(&path);
}
