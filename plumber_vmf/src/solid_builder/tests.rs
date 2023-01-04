use std::ops::Range;

use plumber_vdf as vdf;

use super::*;
use approx::assert_relative_eq;

fn get_test_solid() -> Solid {
    let input = include_str!("test_solid.txt");
    vdf::from_str(input).unwrap()
}

fn assert_face_sorted(face: &[usize], sorted: &[usize], tag: &'static str) {
    let mut sorted = sorted.to_vec();
    assert_eq!(
        face.len(),
        sorted.len(),
        "face {} has wrong vertice amount",
        tag
    );

    for _ in 0..face.len() {
        if face.iter().zip(&sorted).all(|(&v_i, &s_i)| v_i == s_i) {
            return;
        }
        sorted.rotate_right(1);
    }

    assert_eq!(face, sorted, "face {} is not sorted", tag);
}

#[test]
fn solid_building() {
    let solid = get_test_solid();
    let mut builder = SolidBuilder::new(&solid);
    builder.intersect_sides(1e-3, 1e-3);
    builder.remove_invalid_faces();
    builder.recenter();

    let expected_vertices = vec![
        Vec3::new(-64.0, 64.0, 32.0),
        Vec3::new(-64.0, -64.0, 32.0),
        Vec3::new(64.0, 64.0, 32.0),
        Vec3::new(64.0, -64.0, 32.0),
        Vec3::new(-64.0, 64.0, -32.0),
        Vec3::new(-64.0, -64.0, -32.0),
        Vec3::new(64.0, 64.0, -32.0),
        Vec3::new(64.0, -64.0, -32.0),
    ];

    let mut is = vec![0; expected_vertices.len()];

    assert_eq!(builder.vertices.len(), expected_vertices.len());

    for (i, vertice) in builder.vertices.iter().enumerate() {
        assert!(
            expected_vertices.iter().enumerate().any(|(j, v)| {
                if relative_eq!(v, vertice, epsilon = 1e-3) {
                    is[j] = i;
                    true
                } else {
                    false
                }
            }),
            "unexpected vertice {}",
            vertice
        );
    }

    assert_relative_eq!(builder.center, Vec3::new(-896.0, 0.0, 32.0), epsilon = 1e-3);

    builder.sort_vertices();

    assert_face_sorted(
        &builder.faces[0].vertice_indices,
        &[is[1], is[3], is[2], is[0]],
        "+z",
    );
    assert_face_sorted(
        &builder.faces[1].vertice_indices,
        &[is[6], is[7], is[5], is[4]],
        "-z",
    );
    assert_face_sorted(
        &builder.faces[2].vertice_indices,
        &[is[4], is[5], is[1], is[0]],
        "-x",
    );
    assert_face_sorted(
        &builder.faces[3].vertice_indices,
        &[is[3], is[7], is[6], is[2]],
        "+x",
    );
    assert_face_sorted(
        &builder.faces[4].vertice_indices,
        &[is[2], is[6], is[4], is[0]],
        "+y",
    );
    assert_face_sorted(
        &builder.faces[5].vertice_indices,
        &[is[5], is[7], is[3], is[1]],
        "-y",
    );

    builder.build_uvs(|_| Some(MaterialInfo::new(1024, 1024, false)));

    let side = &builder.faces[0];
    let expected_uvs = side
        .vertice_indices
        .iter()
        .map(|&i| match is.iter().position(|&j| j == i).unwrap() {
            0 => Vec2::new(-0.75, -0.25),
            1 => Vec2::new(-0.75, 0.25),
            2 => Vec2::new(-0.25, -0.25),
            3 => Vec2::new(-0.25, 0.25),
            _ => unreachable!(),
        })
        .collect_vec();
    for (uv, expected_uv) in side.vertice_uvs.iter().zip(&expected_uvs) {
        assert!(
            relative_eq!(uv, expected_uv, epsilon = 1e-3),
            "got {:?}, expected {:?}",
            side.vertice_uvs,
            expected_uvs
        );
    }
}

#[allow(clippy::too_many_lines)]
fn get_test_displacement() -> Solid {
    let input = include_str!("test_displacement.txt");
    vdf::from_str(input).unwrap()
}

#[test]
#[allow(clippy::unreadable_literal, clippy::too_many_lines)]
fn displacement_building() {
    let solid = get_test_displacement();
    let mut builder = SolidBuilder::new(&solid);
    builder.intersect_sides(1e-3, 1e-3);
    builder.remove_invalid_faces();
    builder.sort_vertices();
    builder.build_uvs(|_| Some(MaterialInfo::new(1024, 1024, false)));
    builder.maybe_build_displacement().unwrap();
    builder.recenter();

    let expected_vertices = vec![
        Vec3::new(-393.922, -511.9999885559082, 334.967_16),
        Vec3::new(449.985, -511.9999885559082, -245.033_45),
        Vec3::new(-288.433_53, -511.9999885559082, 262.467_04),
        Vec3::new(-182.945_1, -511.9999885559082, 189.967),
        Vec3::new(-77.456_696, -511.9999885559082, 117.466_93),
        Vec3::new(28.031_721, -511.9999885559082, 44.966_843),
        Vec3::new(133.520_17, -511.9999885559082, -27.533_241),
        Vec3::new(239.008_54, -511.9999885559082, -100.033_31),
        Vec3::new(344.496_95, -511.9999885559082, -172.533_37),
        Vec3::new(-416.578_2, -383.99999141693115, 302.002_1),
        Vec3::new(365.024_57, -383.99999141693115, -368.652_47),
        Vec3::new(-313.921_78, -383.99999141693115, 225.381_4),
        Vec3::new(-208.813_78, -383.99999141693115, 152.327_8),
        Vec3::new(-100.112_94, -383.99999141693115, 84.501_88),
        Vec3::new(5.375_473, -383.99999141693115, 12.001_801),
        Vec3::new(105.538_18, -383.99999141693115, -68.247_26),
        Vec3::new(210.998_96, -383.99999141693115, -140.787_48),
        Vec3::new(319.008_67, -383.99999141693115, -209.619_05),
        Vec3::new(-416.578_2, -255.9999942779541, 302.002_1),
        Vec3::new(365.024_57, -255.9999942779541, -368.652_47),
        Vec3::new(-261.827_88, -255.9999942779541, 301.178_47),
        Vec3::new(-253.228_09, -255.9999942779541, 87.704_575),
        Vec3::new(-100.112_94, -255.9999942779541, 84.501_88),
        Vec3::new(5.375_473, -255.9999942779541, 12.001_801),
        Vec3::new(65.646_64, -255.9999942779541, -126.289_82),
        Vec3::new(266.795_3, -255.9999942779541, -59.603_333),
        Vec3::new(335.661_74, -255.9999942779541, -185.388_67),
        Vec3::new(-416.578_2, -127.99999713897705, 302.002_1),
        Vec3::new(365.024_57, -127.99999713897705, -368.652_47),
        Vec3::new(-330.709_9, -127.99999713897705, 200.954_53),
        Vec3::new(-266.520_08, -127.99999713897705, 68.364_64),
        Vec3::new(112.116_325, -127.99999713897705, 157.739_18),
        Vec3::new(-6.679_397, -127.99999713897705, -5.538_154_6),
        Vec3::new(126.356_415, -127.99999713897705, -37.956_535),
        Vec3::new(199.802_58, -127.99999713897705, -157.078_32),
        Vec3::new(305.004_9, -127.99999713897705, -229.994_63),
        Vec3::new(-416.578_2, 0.0, 302.002_1),
        Vec3::new(365.024_57, 7.680_7e-7, -368.652_47),
        Vec3::new(-299.066_1, 0.0, 246.996_57),
        Vec3::new(-247.022_34, 0.0, 96.734_024),
        Vec3::new(-108.196_41, 0.0, 58.017_986),
        Vec3::new(-16.196_547, 0.0, -19.385_695),
        Vec3::new(105.049_774, 0.0, -68.957_91),
        Vec3::new(208.983_15, 0.0, -143.720_52),
        Vec3::new(324.249_9, 0.0, -201.992_97),
        Vec3::new(-416.578_2, 127.99999713897705, 302.002_1),
        Vec3::new(365.024_57, 127.99999713897705, -368.652_47),
        Vec3::new(-284.319_76, 127.99999713897705, 268.452_6),
        Vec3::new(-261.952_12, 127.99999713897705, 75.011_06),
        Vec3::new(-92.201_16, 127.99999713897705, 96.013_61),
        Vec3::new(23.052_368, 127.99999713897705, 22.999_45),
        Vec3::new(105.715_79, 127.99999713897705, -67.988_86),
        Vec3::new(220.522_06, 127.99999713897705, -126.931_3),
        Vec3::new(312.896_58, 127.99999713897705, -218.512_2),
        Vec3::new(-416.578_2, 255.9999942779541, 302.002_1),
        Vec3::new(365.024_57, 255.9999942779541, -368.652_47),
        Vec3::new(-295.188_23, 255.9999942779541, 252.638_87),
        Vec3::new(-217.918_1, 255.9999942779541, 139.080_92),
        Vec3::new(-121.962_845, 255.9999942779541, 52.710_068),
        Vec3::new(-14.600_067, 255.9999942779541, -17.062_813),
        Vec3::new(140.010_83, 255.9999942779541, -91.701_13),
        Vec3::new(184.233_67, 255.9999942779541, -179.731_23),
        Vec3::new(287.460_42, 255.9999942779541, -255.522_03),
        Vec3::new(-416.578_2, 383.99999141693115, 302.002_1),
        Vec3::new(365.024_57, 383.99999141693115, -368.652_47),
        Vec3::new(-320.855_93, 383.99999141693115, 215.292_14),
        Vec3::new(-208.433_36, 383.99999141693115, 152.881_32),
        Vec3::new(-102.944_97, 383.99999141693115, 80.381_256),
        Vec3::new(-35.607_056, 383.99999141693115, -47.628_162),
        Vec3::new(108.031_89, 383.99999141693115, -64.618_91),
        Vec3::new(215.795_04, 383.99999141693115, -133.809_16),
        Vec3::new(317.049_62, 383.99999141693115, -212.469_48),
        Vec3::new(-416.578_2, 511.9999885559082, 302.002_1),
        Vec3::new(365.024_57, 511.9999885559082, -368.652_47),
        Vec3::new(-313.921_78, 511.9999885559082, 225.381_4),
        Vec3::new(-208.433_36, 511.9999885559082, 152.881_32),
        Vec3::new(-102.944_97, 511.9999885559082, 80.381_256),
        Vec3::new(2.543_457, 511.9999885559082, 7.881_172),
        Vec3::new(108.031_89, 511.9999885559082, -64.618_91),
        Vec3::new(213.520_26, 511.9999885559082, -137.118_99),
        Vec3::new(319.008_67, 511.9999885559082, -209.619_05),
    ];

    assert_eq!(builder.vertices.len(), expected_vertices.len());

    for vertice in &builder.vertices {
        assert!(
            expected_vertices
                .iter()
                .any(|v| relative_eq!(v, vertice, epsilon = 1e-3)),
            "unexpected vertice {}",
            vertice
        );
    }

    assert_relative_eq!(
        builder.center,
        Vec3::new(146.974, 0.0, 131.398),
        epsilon = 1e-3
    );

    for side in &builder.faces {
        for &i in &side.vertice_indices {
            assert!(
                builder.vertices.len() > i,
                "face has invalid vertice index {}",
                i
            );
        }
    }
}

fn create_test_face(side: &Side, plane: NdPlane) -> FaceBuilder {
    FaceBuilder {
        side,
        plane,
        vertice_indices: Vec::new(),
        vertice_uvs: Vec::new(),
        material_index: 0,
        vertice_alphas: Vec::new(),
        vertice_multiblends: None,
    }
}

#[test]
#[allow(clippy::too_many_lines)]
fn side_clipping() {
    let mut vertices = vec![
        Vec3::new(-2.0, 1.0, 0.0),
        Vec3::new(-2.0, -1.0, 0.0),
        Vec3::new(0.0, -1.0, 0.0),
        Vec3::new(2.0, 1.0, 0.0),
        Vec3::new(-2.0, -1.0, 2.0),
        Vec3::new(-2.0, 1.0, 2.0),
    ];

    let dummy_side = Side::default();

    let clipping_sides = vec![
        create_test_face(
            &dummy_side,
            NdPlane::from_point_normal(Vec3::new(-1.0, 0.0, 0.0), Vec3::new(-1.0, 0.0, 0.0)),
        ),
        create_test_face(
            &dummy_side,
            NdPlane::from_point_normal(Vec3::new(1.0, 0.0, 0.0), Vec3::new(1.0, 0.0, 0.0)),
        ),
        create_test_face(
            &dummy_side,
            NdPlane::from_point_normal(Vec3::new(0.0, -2.0, 0.0), Vec3::new(0.0, -1.0, 0.0)),
        ),
        create_test_face(
            &dummy_side,
            NdPlane::from_point_normal(Vec3::new(0.0, 2.0, 0.0), Vec3::new(0.0, 1.0, 0.0)),
        ),
    ];

    let clipped_side = FaceBuilder {
        side: &dummy_side,
        plane: NdPlane::from_point_normal(Vec3::new(2.0, 2.0, 0.0), Vec3::new(0.0, 0.0, 1.0)),
        vertice_indices: vec![0, 1, 2, 3],
        vertice_uvs: vec![Vec2::ZERO; 4],
        material_index: 0,
        vertice_alphas: Vec::new(),
        vertice_multiblends: None,
    };

    let mut clipped = Vec::new();

    clipped_side.clip_to_sides(
        Vec3::new(3.0, 2.0, 0.0),
        &mut clipped,
        &mut vertices,
        &clipping_sides,
        Vec3::new(5.0, 2.0, 0.0),
        false,
        1e-3,
    );

    assert_relative_eq!(
        &vertices[..],
        &[
            Vec3::new(-2.0, 1.0, 0.0),
            Vec3::new(-2.0, -1.0, 0.0),
            Vec3::new(0.0, -1.0, 0.0),
            Vec3::new(2.0, 1.0, 0.0),
            Vec3::new(-2.0, -1.0, 2.0),
            Vec3::new(-2.0, 1.0, 2.0),
            Vec3::new(1.0, 0.0, 0.0),
            Vec3::new(1.0, 1.0, 0.0),
        ][..],
        epsilon = 1e-3
    );

    assert_eq!(
        clipped,
        vec![FaceBuilder {
            side: &dummy_side,
            plane: NdPlane::from_point_normal(Vec3::new(2.0, 2.0, 0.0), Vec3::new(0.0, 0.0, 1.0)),
            vertice_indices: vec![0, 1, 2, 6, 7],
            vertice_uvs: vec![Vec2::ZERO; 5],
            material_index: 0,
            vertice_alphas: Vec::new(),
            vertice_multiblends: None,
        }]
    );

    let clipped_side = FaceBuilder {
        side: &dummy_side,
        plane: NdPlane::from_point_normal(Vec3::new(-2.0, 0.0, 2.0), Vec3::new(-1.0, 0.0, 0.0)),
        vertice_indices: vec![0, 1, 4, 5],
        vertice_uvs: vec![Vec2::ZERO; 4],
        material_index: 0,
        vertice_alphas: Vec::new(),
        vertice_multiblends: None,
    };

    let mut clipped = Vec::new();

    clipped_side.clone().clip_to_sides(
        Vec3::new(3.0, 2.0, 0.0),
        &mut clipped,
        &mut vertices,
        &clipping_sides,
        Vec3::new(5.0, 2.0, 0.0),
        false,
        1e-3,
    );

    assert_eq!(clipped, vec![clipped_side]);

    let clipped_side = FaceBuilder {
        side: &dummy_side,
        plane: NdPlane::from_point_normal(Vec3::new(1.0, 0.0, 0.0), Vec3::new(0.0, 0.0, 1.0)),
        vertice_indices: vec![3, 7, 6],
        vertice_uvs: vec![Vec2::ZERO; 2],
        material_index: 0,
        vertice_alphas: Vec::new(),
        vertice_multiblends: None,
    };

    let mut clipped = Vec::new();

    clipped_side.clone().clip_to_sides(
        Vec3::new(3.0, 2.0, 0.0),
        &mut clipped,
        &mut vertices,
        &clipping_sides,
        Vec3::new(5.0, 2.0, 0.0),
        false,
        1e-3,
    );

    assert_eq!(clipped, Vec::new());
}

#[test]
#[allow(clippy::too_many_lines)]
fn solid_clipping() {
    let dummy_side = Side::default();
    let dummy_solid = Solid::default();

    let clipping_solid = SolidBuilder {
        solid: &dummy_solid,
        center: Vec3::new(5.0, 2.0, 0.0),
        faces: vec![
            create_test_face(
                &dummy_side,
                NdPlane::from_point_normal(Vec3::new(-1.0, 0.0, 0.0), Vec3::new(-1.0, 0.0, 0.0)),
            ),
            create_test_face(
                &dummy_side,
                NdPlane::from_point_normal(Vec3::new(1.0, 0.0, 0.0), Vec3::new(1.0, 0.0, 0.0)),
            ),
            create_test_face(
                &dummy_side,
                NdPlane::from_point_normal(Vec3::new(0.0, -2.0, 0.0), Vec3::new(0.0, -1.0, 0.0)),
            ),
            create_test_face(
                &dummy_side,
                NdPlane::from_point_normal(Vec3::new(0.0, 2.0, 0.0), Vec3::new(0.0, 1.0, 0.0)),
            ),
            create_test_face(
                &dummy_side,
                NdPlane::from_point_normal(Vec3::new(0.0, 0.0, -1.0), Vec3::new(0.0, 0.0, -1.0)),
            ),
            create_test_face(
                &dummy_side,
                NdPlane::from_point_normal(Vec3::new(0.0, 0.0, 1.0), Vec3::new(0.0, 0.0, 1.0)),
            ),
        ],
        vertices: Vec::new(),
        materials: Vec::new(),
        is_displacement: false,
        aabb_min: Vec3::new(0.0, 0.0, 0.0),
        aabb_max: Vec3::new(0.0, 0.0, 0.0),
    };

    let mut clipped_solid = SolidBuilder {
        solid: &dummy_solid,
        center: Vec3::new(6.0, 4.0, 0.0),
        faces: vec![
            FaceBuilder {
                side: &dummy_side,
                plane: NdPlane::from_point_normal(
                    Vec3::new(-1.0, 2.0, 0.0),
                    Vec3::new(-1.0, 1.0, 0.0).normalize(),
                ),
                vertice_indices: vec![0, 1, 5, 4],
                vertice_uvs: vec![Vec2::ZERO; 4],
                material_index: 0,
                vertice_alphas: Vec::new(),
                vertice_multiblends: None,
            },
            FaceBuilder {
                side: &dummy_side,
                plane: NdPlane::from_point_normal(
                    Vec3::new(-2.0, 1.0, 0.0),
                    Vec3::new(-1.0, -1.0, 0.0).normalize(),
                ),
                vertice_indices: vec![1, 2, 6, 5],
                vertice_uvs: vec![Vec2::ZERO; 4],
                material_index: 0,
                vertice_alphas: Vec::new(),
                vertice_multiblends: None,
            },
            FaceBuilder {
                side: &dummy_side,
                plane: NdPlane::from_point_normal(
                    Vec3::new(1.0, -2.0, 0.0),
                    Vec3::new(1.0, -1.0, 0.0).normalize(),
                ),
                vertice_indices: vec![2, 3, 7, 6],
                vertice_uvs: vec![Vec2::ZERO; 4],
                material_index: 0,
                vertice_alphas: Vec::new(),
                vertice_multiblends: None,
            },
            FaceBuilder {
                side: &dummy_side,
                plane: NdPlane::from_point_normal(
                    Vec3::new(2.0, -1.0, 0.0),
                    Vec3::new(1.0, 1.0, 0.0).normalize(),
                ),
                vertice_indices: vec![3, 0, 4, 7],
                vertice_uvs: vec![Vec2::ZERO; 4],
                material_index: 0,
                vertice_alphas: Vec::new(),
                vertice_multiblends: None,
            },
            FaceBuilder {
                side: &dummy_side,
                plane: NdPlane::from_point_normal(
                    Vec3::new(0.0, 0.0, 1.0),
                    Vec3::new(0.0, 0.0, 1.0),
                ),
                vertice_indices: vec![0, 1, 2, 3],
                vertice_uvs: vec![Vec2::ZERO; 4],
                material_index: 0,
                vertice_alphas: Vec::new(),
                vertice_multiblends: None,
            },
            FaceBuilder {
                side: &dummy_side,
                plane: NdPlane::from_point_normal(
                    Vec3::new(0.0, 0.0, -1.0),
                    Vec3::new(0.0, 0.0, -1.0),
                ),
                vertice_indices: vec![7, 6, 5, 4],
                vertice_uvs: vec![Vec2::ZERO; 4],
                material_index: 0,
                vertice_alphas: Vec::new(),
                vertice_multiblends: None,
            },
        ],
        vertices: vec![
            Vec3::new(-1.0, 2.0, 1.0),
            Vec3::new(-2.0, 1.0, 1.0),
            Vec3::new(1.0, -2.0, 1.0),
            Vec3::new(2.0, -1.0, 1.0),
            Vec3::new(-1.0, 2.0, -1.0),
            Vec3::new(-2.0, 1.0, -1.0),
            Vec3::new(1.0, -2.0, -1.0),
            Vec3::new(2.0, -1.0, -1.0),
        ],
        materials: Vec::new(),
        is_displacement: false,
        aabb_min: Vec3::new(0.0, 0.0, 0.0),
        aabb_max: Vec3::new(0.0, 0.0, 0.0),
    };

    clipped_solid.clip_to_solid(&clipping_solid, false, 1e-3);

    dbg!(clipped_solid);
}

#[test]
fn uv_normalization_finite_results() {
    const UV_RANGE: Range<f32> = -1000.0..1000.0;

    let test_data = include_str!("test_solid_uv.txt");
    let solid: Solid = vdf::from_str(test_data).unwrap();

    let mut builder = SolidBuilder::new(&solid);

    builder.intersect_sides(1e-3, 1e-3);
    builder.remove_invalid_faces();
    builder.sort_vertices();
    builder.build_uvs(|_| Some(MaterialInfo::new(1024, 1024, false)));
    builder.maybe_build_displacement().unwrap();

    if !builder.is_displacement {
        builder.create_default_alphas();
    }

    for face in &builder.faces {
        for uv in &face.vertice_uvs {
            assert!(
                UV_RANGE.contains(&uv.x),
                "invalid u {} for {}",
                uv.x,
                face.side.id
            );
            assert!(
                UV_RANGE.contains(&uv.y),
                "invalid v {} for {}",
                uv.y,
                face.side.id
            );
        }
    }
}
