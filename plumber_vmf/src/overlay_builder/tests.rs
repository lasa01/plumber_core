use std::{f32::consts::FRAC_1_SQRT_2, sync::Mutex};

use plumber_vdf as vdf;
use plumber_vmt::MaterialInfo;

use super::*;
use crate::{entities::TypedEntity, vmf::Entity, vmf::Solid};

fn get_test_solids() -> Vec<Solid> {
    let input = include_str!("test_solids.txt");
    vdf::from_str(input).unwrap()
}

fn get_test_overlay() -> Entity {
    let input = include_str!("test_overlay.txt");
    vdf::from_str(input).unwrap()
}

#[test]
#[allow(clippy::too_many_lines)]
fn overlay_building() {
    let solids = get_test_solids();
    let side_faces_map = Mutex::new(SideFacesMap::new());
    for solid in &solids {
        solid
            .build_mesh(
                |_| Some(MaterialInfo::new(1024, 1024, false)),
                &side_faces_map,
                &GeometrySettings::default(),
                1.0,
            )
            .unwrap();
    }

    let side_faces_map = side_faces_map
        .into_inner()
        .expect("mutex shouldn't be poisoned");
    let entity = get_test_overlay();
    let overlay = match entity.typed() {
        TypedEntity::Overlay(o) => o,
        _ => unreachable!(),
    };
    let mut builder = OverlayBuilder::new(overlay).unwrap();
    builder.create_vertices(&side_faces_map, 1e-3).unwrap();

    let expected_vertices = vec![
        Vec3::new(-960.0, 64.0, 64.0),
        Vec3::new(-960.0, -64.0, 64.0),
        Vec3::new(-832.0, 64.0, 64.0),
        Vec3::new(-832.0, -64.0, 64.0),
        Vec3::new(-960.0, -64.0, 0.0),
        Vec3::new(-832.0, -64.0, 0.0),
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

    builder.offset_vertices().unwrap();

    let expected_vertices = vec![
        Vec3::new(-960.0, 64.0, 64.0 + 0.1),
        Vec3::new(
            -960.0,
            -64.0 - FRAC_1_SQRT_2 * 0.1,
            64.0 + FRAC_1_SQRT_2 * 0.1,
        ),
        Vec3::new(-832.0, 64.0, 64.0 + 0.1),
        Vec3::new(
            -832.0,
            -64.0 - FRAC_1_SQRT_2 * 0.1,
            64.0 + FRAC_1_SQRT_2 * 0.1,
        ),
        Vec3::new(-960.0, -64.0 - 0.1, 0.0),
        Vec3::new(-832.0, -64.0 - 0.1, 0.0),
    ];

    for vertice in &builder.vertices {
        assert!(
            expected_vertices
                .iter()
                .any(|v| relative_eq!(v, vertice, epsilon = 1e-3)),
            "unexpected vertice {}",
            vertice
        );
    }

    builder.cut_faces(1e-3, 1e-3).unwrap();
    builder.remove_vertices_outside(1e-3);
    builder.ensure_not_empty().unwrap();
    builder.create_uvs();
    builder.recenter();

    let mut is = vec![0; expected_vertices.len()];

    let expected_vertices = vec![
        Vec3::new(-35.934_752, -31.288_08, 15.476_86),
        Vec3::new(-36.673_89, 50.222_676, 15.476_852),
        Vec3::new(32.293_945, 74.929_66, 15.476_86),
        Vec3::new(37.192_076, -31.288_08, 15.476_86),
        Vec3::new(38.820_615, -31.288_08, -37.880_24),
        Vec3::new(-35.697_692, -31.288_08, -24.027_198),
    ];

    assert_eq!(builder.vertices.len(), expected_vertices.len());

    for (i, vertice) in builder.vertices.iter().enumerate() {
        assert!(
            expected_vertices.iter().enumerate().any(|(j, v)| {
                // test data has different offset algorithm, need large epsilon
                if relative_eq!(v, vertice, epsilon = 0.1) {
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

    for face in &builder.faces {
        for &i in &face.vertice_indices {
            assert!(
                builder.vertices.len() > i,
                "face has invalid vertice index {}",
                i
            );
        }
    }

    let side = &builder.faces[0];
    let expected_uvs = side
        .vertice_indices
        .iter()
        .map(|&i| match is.iter().position(|&j| j == i).unwrap() {
            0 => Vec2::new(0.0, 1.0 - 0.232_844),
            1 => Vec2::new(0.0, 0.0),
            2 => Vec2::new(1.0, 0.0),
            3 => Vec2::new(1.0, 1.0 - 0.236_098),
            _ => unreachable!(),
        })
        .collect_vec();
    for (uv, expected_uv) in side.vertice_uvs.iter().zip(&expected_uvs) {
        assert!(
            // test data has different offset algorithm, need large epsilon
            relative_eq!(uv, expected_uv, epsilon = 0.1),
            "got {:?}, expected {:?}",
            side.vertice_uvs,
            expected_uvs
        );
    }
}
