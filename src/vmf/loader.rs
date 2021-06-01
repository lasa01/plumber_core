use std::fmt::Debug;

use crate::{
    fs::{Path, PathBuf},
    vmt::loader::MaterialInfo,
};

use super::{Plane, Side, Solid};

use approx::{abs_diff_eq, relative_eq};
use float_ord::FloatOrd;
use itertools::Itertools;
use nalgebra::{geometry::Point3, Vector3};
use ndarray::{Array2, Array3, Zip};
use thiserror::Error;

const EPSILON: f64 = 1e-3;
const CUT_THRESHOLD: f64 = 1e-3;

#[derive(Debug, Error)]
pub enum SolidError {
    #[error("displacement side `{side_id}` is not valid: got {vertices} vertices, expected 4")]
    InvalidDisplacement { side_id: i32, vertices: usize },
}

#[derive(Debug)]
pub struct BuiltSolid<'a> {
    pub solid: &'a Solid,
    pub position: Point3<f64>,
    pub vertices: Vec<Point3<f64>>,
    pub sides: Vec<BuiltSide>,
    pub materials: Vec<PathBuf>,
}

#[derive(Debug)]
pub struct BuiltSide {
    pub vertice_indices: Vec<usize>,
    pub vertice_uvs: Vec<(f64, f64)>,
    pub material_index: usize,
}

/// A plane defined by a normal vector and a distance to the origin.
/// Also keeps a point on the plane for convenience.
#[derive(Debug, Clone, Copy)]
struct NdPlane {
    point: Point3<f64>,
    normal: Vector3<f64>,
    distance: f64,
}

impl NdPlane {
    fn from_plane(plane: &Plane, center: &Point3<f64>) -> Self {
        // in vmf, plane points are in cw winding order,
        // everywhere from now on, ccw winding order
        Self::from_points(
            &(plane.2 - center).into(),
            &(plane.1 - center).into(),
            &(plane.0 - center).into(),
        )
    }

    fn from_points(a: &Point3<f64>, b: &Point3<f64>, c: &Point3<f64>) -> Self {
        let normal = (b - a).cross(&(c - a)).normalize();
        let distance = -a.coords.dot(&normal);
        Self {
            point: *a,
            normal,
            distance,
        }
    }

    fn distance_to_point(&self, point: &Point3<f64>) -> f64 {
        (point - self.point).dot(&self.normal)
    }
}

struct SideBuilder<'a> {
    side: &'a Side,
    plane: NdPlane,
    vertice_indices: Vec<usize>,
    vertice_uvs: Vec<(f64, f64)>,
    material_index: usize,
}

impl<'a> Debug for SideBuilder<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SideBuilder")
            .field("plane", &self.plane)
            .field("vertice_indices", &self.vertice_indices)
            .field("vertice_uvs", &self.vertice_uvs)
            .field("material_index", &self.material_index)
            .finish()
    }
}

impl<'a> SideBuilder<'a> {
    fn new(side: &'a Side, center: Point3<f64>) -> Self {
        let plane = NdPlane::from_plane(&side.plane, &center);
        Self {
            side,
            plane,
            vertice_indices: Vec::new(),
            vertice_uvs: Vec::new(),
            material_index: 0,
        }
    }

    fn insert_vertice(&mut self, i: usize) {
        if !self.vertice_indices.contains(&i) {
            self.vertice_indices.push(i);
        }
    }

    fn disp_vertices_len(&self) -> Option<usize> {
        self.side
            .disp_info
            .as_ref()
            .map(|info| info.dimension().pow(2))
    }

    fn disp_faces_len(&self) -> Option<usize> {
        self.side
            .disp_info
            .as_ref()
            .map(|info| 2 * (info.dimension() - 1).pow(2))
    }

    fn verify_displacement(&self) -> Result<(), SolidError> {
        let side_vertices_n = self.vertice_indices.len();
        if side_vertices_n != 4 {
            return Err(SolidError::InvalidDisplacement {
                side_id: self.side.id,
                vertices: side_vertices_n,
            });
        }
        Ok(())
    }

    fn maybe_build_displacement(
        &mut self,
        old_vertices: &[Point3<f64>],
        vertices: &mut Vec<Point3<f64>>,
        sides: &mut Vec<SideBuilder<'a>>,
    ) -> Result<(), SolidError> {
        if let Some(info) = &self.side.disp_info {
            self.verify_displacement()?;

            // find out which corner vertice the displacement start position is
            let (start_i, _) = self
                .vertice_indices
                .iter()
                .enumerate()
                .min_by_key(|(_, &vertice_i)| {
                    let vertice = old_vertices[vertice_i];
                    FloatOrd(vertice.coords.metric_distance(&info.start_position))
                })
                .unwrap();

            let top_left_i = start_i;
            let top_right_i = (start_i + 3) % 4;
            let btm_right_i = (start_i + 2) % 4;
            let btm_left_i = (start_i + 1) % 4;

            let top_left_vert = old_vertices[self.vertice_indices[top_left_i]];
            let top_right_vert = old_vertices[self.vertice_indices[top_right_i]];
            let btm_right_vert = old_vertices[self.vertice_indices[btm_right_i]];
            let btm_left_vert = old_vertices[self.vertice_indices[btm_left_i]];

            let dimension = info.dimension();
            let last_i = dimension - 1;

            let mut disp_vertice_is = Array2::zeros((dimension, dimension));

            for (row_i, mut row) in disp_vertice_is.rows_mut().into_iter().enumerate() {
                let blend = row_i as f64 / last_i as f64;

                let vert_left = top_left_vert.coords.lerp(&btm_left_vert.coords, blend);
                let vert_left_i = vertices.len();
                vertices.push(vert_left.into());

                let vert_right = top_right_vert.coords.lerp(&btm_right_vert.coords, blend);
                let vert_right_i = vertices.len();
                vertices.push(vert_right.into());

                for (col_i, vert_i) in row.indexed_iter_mut() {
                    *vert_i = if col_i == 0 {
                        vert_left_i
                    } else if col_i == last_i {
                        vert_right_i
                    } else {
                        let i = vertices.len();
                        vertices.push(
                            vert_left
                                .lerp(&vert_right, col_i as f64 / last_i as f64)
                                .into(),
                        );
                        i
                    };
                }
            }

            Zip::from(&disp_vertice_is)
                .and(&info.offsets.data)
                .and(&info.normals.data)
                .and(&info.distances.data)
                .for_each(|&v_i, offset, normal, &distance| {
                    vertices[v_i] +=
                        offset + distance * normal + self.plane.normal * info.elevation;
                });

            let mut disp_uvs = Array2::default((dimension, dimension));

            for (row_i, mut row) in disp_uvs.rows_mut().into_iter().enumerate() {
                let blend = row_i as f64 / last_i as f64;

                let uv_left = lerp_uv(
                    self.vertice_uvs[top_left_i],
                    self.vertice_uvs[btm_left_i],
                    blend,
                );
                let uv_right = lerp_uv(
                    self.vertice_uvs[top_right_i],
                    self.vertice_uvs[btm_right_i],
                    blend,
                );

                for (col_i, vert_uv) in row.indexed_iter_mut() {
                    *vert_uv = lerp_uv(uv_left, uv_right, col_i as f64 / last_i as f64);
                }
            }

            let mut disp_faces = Array3::<SideBuilder>::from_shape_simple_fn(
                (dimension - 1, dimension - 1, 2),
                || SideBuilder {
                    side: self.side,
                    plane: self.plane,
                    vertice_indices: Vec::with_capacity(3),
                    vertice_uvs: Vec::with_capacity(3),
                    material_index: self.material_index,
                },
            );

            for ((r_i, c_i, face_i), face) in disp_faces.indexed_iter_mut() {
                let face_vert_is = match face_i {
                    0 if r_i % 2 == c_i % 2 => [(r_i + 1, c_i), (r_i, c_i), (r_i + 1, c_i + 1)],
                    0 => [(r_i + 1, c_i), (r_i, c_i), (r_i, c_i + 1)],
                    1 if r_i % 2 == c_i % 2 => [(r_i, c_i), (r_i, c_i + 1), (r_i + 1, c_i + 1)],
                    1 => [(r_i + 1, c_i), (r_i, c_i + 1), (r_i + 1, c_i + 1)],
                    _ => unreachable!(),
                };
                face.vertice_indices
                    .extend(face_vert_is.iter().map(|&i| disp_vertice_is[i]));
                face.vertice_uvs
                    .extend(face_vert_is.iter().map(|&i| disp_uvs[i]));
                face.material_index = self.material_index;
            }

            sides.append(&mut disp_faces.into_raw_vec())
        }
        Ok(())
    }

    fn finish(self) -> BuiltSide {
        BuiltSide {
            vertice_indices: self.vertice_indices,
            vertice_uvs: self.vertice_uvs,
            material_index: self.material_index,
        }
    }
}

fn intersect_planes(a: &NdPlane, b: &NdPlane, c: &NdPlane) -> Option<Point3<f64>> {
    let denominator = a.normal.dot(&b.normal.cross(&c.normal));
    if abs_diff_eq!(denominator, 0.0, epsilon = EPSILON) {
        return None;
    }
    Some(
        ((-a.distance * b.normal.cross(&c.normal)
            - b.distance * c.normal.cross(&a.normal)
            - c.distance * a.normal.cross(&b.normal))
            / denominator)
            .into(),
    )
}

fn polygon_center<I>(polygon: I) -> Point3<f64>
where
    I: Iterator<Item = Point3<f64>> + ExactSizeIterator,
{
    let len = polygon.len() as f64;
    (polygon.fold(Vector3::<f64>::zeros(), |a, b| a + b.coords) / len).into()
}

fn polygon_normal<I>(polygon: I) -> Vector3<f64>
where
    I: Clone + Iterator<Item = Point3<f64>> + ExactSizeIterator,
{
    let center = polygon_center(polygon.clone());
    let mut normal = Vector3::<f64>::zeros();

    for (a, b) in polygon.circular_tuple_windows() {
        normal += (a - center).cross(&(b - center));
    }

    normal.normalize()
}

struct SolidBuilder<'a> {
    solid: &'a Solid,
    center: Point3<f64>,
    sides: Vec<SideBuilder<'a>>,
    vertices: Vec<Point3<f64>>,
    materials: Vec<PathBuf>,
}

impl<'a> Debug for SolidBuilder<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SolidBuilder")
            .field("center", &self.center)
            .field("sides", &self.sides)
            .field("vertices", &self.vertices)
            .field("materials", &self.materials)
            .finish()
    }
}

impl<'a> SolidBuilder<'a> {
    fn new(solid: &'a Solid) -> Self {
        let solid_center = polygon_center(
            solid
                .sides
                .iter()
                .map(|side| (side.plane.0 + side.plane.1.coords + side.plane.2.coords) / 3.0),
        );

        Self {
            solid,
            center: solid_center,
            sides: solid
                .sides
                .iter()
                .map(|side| SideBuilder::new(side, solid_center))
                .collect(),
            vertices: Vec::new(),
            materials: Vec::new(),
        }
    }

    fn intersect_sides(&mut self) {
        for (i1, i2, i3) in (0..self.sides.len()).tuple_combinations() {
            if let Some(point) = intersect_planes(
                &self.sides[i1].plane,
                &self.sides[i2].plane,
                &self.sides[i3].plane,
            ) {
                // check if vertice is outside the brush
                if self
                    .sides
                    .iter()
                    .enumerate()
                    .filter(|(i, _)| *i != i1 && *i != i2 && *i != i3)
                    .any(|(_, builder)| builder.plane.distance_to_point(&point) > CUT_THRESHOLD)
                {
                    continue;
                }
                // check if vertice already exists
                let vertice_i = self
                    .vertices
                    .iter()
                    .enumerate()
                    .find(|(_, v)| relative_eq!(*v, &point, epsilon = EPSILON))
                    .map(|(i, _)| i)
                    .unwrap_or_else(|| {
                        // if not, create it
                        self.vertices.push(point);
                        self.vertices.len() - 1
                    });
                // add vertice index to sides
                self.sides[i1].insert_vertice(vertice_i);
                self.sides[i2].insert_vertice(vertice_i);
                self.sides[i3].insert_vertice(vertice_i);
            }
        }
    }

    fn remove_invalid_sides(&mut self) {
        self.sides
            .retain(|builder| builder.vertice_indices.len() >= 3);
    }

    fn sort_vertices(&mut self) {
        let vertices = &mut self.vertices;
        for builder in &mut self.sides {
            let center: Point3<f64> =
                polygon_center(builder.vertice_indices.iter().map(|&i| vertices[i]));

            for i in 0..builder.vertice_indices.len() - 2 {
                let vertice = &vertices[builder.vertice_indices[i]];
                let to_current: Vector3<f64> = (vertice - center).normalize();
                let filter_plane =
                    NdPlane::from_points(&vertice, &center, &(center + builder.plane.normal));

                if let Some((next_idx, _)) = builder.vertice_indices[i + 1..]
                    .iter()
                    .enumerate()
                    .filter(|(_, &vi)| filter_plane.distance_to_point(&vertices[vi]) >= 0.0)
                    .map(|(i, &vi)| {
                        let to_candidate: Vector3<f64> = (vertices[vi] - center).normalize();
                        let dot = to_current.dot(&to_candidate);
                        (i, dot)
                    })
                    // max because smaller angle -> bigger dot product
                    .max_by_key(|(_, d)| FloatOrd(*d))
                {
                    builder.vertice_indices.swap(i + 1, i + 1 + next_idx);
                }
            }

            // reverse if the normal is facing the wrong way
            if polygon_normal(builder.vertice_indices.iter().map(|i| vertices[*i]))
                .dot(&builder.plane.normal)
                < 0.0
            {
                builder.vertice_indices.reverse();
            }
        }
    }

    fn build_uvs(&mut self, mut get_material_info: impl FnMut(&Path) -> MaterialInfo) {
        let vertices = &self.vertices;
        for builder in &mut self.sides {
            let material_info = get_material_info(&builder.side.material);
            let texture_width = f64::from(material_info.width());
            let texture_height = f64::from(material_info.height());
            builder
                .vertice_uvs
                .reserve_exact(builder.vertice_indices.len());
            let u_axis = builder.side.u_axis;
            let v_axis = builder.side.v_axis;
            for &vi in &builder.vertice_indices {
                let u = vertices[vi].coords.dot(&u_axis.axis) / (texture_width * u_axis.scale)
                    + u_axis.translation / texture_width;
                let v = vertices[vi].coords.dot(&v_axis.axis) / (texture_height * v_axis.scale)
                    + v_axis.translation / texture_height;
                builder.vertice_uvs.push((u, v));
            }

            // normalize
            let mut nearest_u = f64::MAX;
            let mut nearest_v = f64::MAX;
            for (u, _) in &builder.vertice_uvs {
                if u.abs() <= 1.0 {
                    nearest_u = 0.0;
                    break;
                }
                if u.abs() < nearest_u.abs() {
                    nearest_u = *u;
                }
            }
            nearest_u = if nearest_u > 0.0 {
                nearest_u.floor()
            } else {
                nearest_u.ceil()
            };

            for (_, v) in &builder.vertice_uvs {
                if v.abs() <= 1.0 {
                    nearest_v = 0.0;
                    break;
                }
                if v.abs() < nearest_v.abs() {
                    nearest_v = *v;
                }
            }
            nearest_v = if nearest_v > 0.0 {
                nearest_v.floor()
            } else {
                nearest_v.ceil()
            };

            for (u, v) in &mut builder.vertice_uvs {
                *u -= nearest_u;
                *v -= nearest_v;
            }
        }
    }

    fn maybe_build_displacement(&mut self) -> Result<(), SolidError> {
        // calculate amount of new vertices
        let vertices_len = match self
            .sides
            .iter()
            .filter_map(SideBuilder::disp_vertices_len)
            .sum1()
        {
            Some(sum) => sum,
            None => return Ok(()), // this is not a displacement
        };

        let old_vertices = &self.vertices;
        let mut vertices: Vec<Point3<f64>> = Vec::with_capacity(vertices_len);

        let sides_len = self
            .sides
            .iter()
            .filter_map(SideBuilder::disp_faces_len)
            .sum();
        let mut sides: Vec<SideBuilder> = Vec::with_capacity(sides_len);

        for builder in &mut self.sides {
            builder.maybe_build_displacement(old_vertices, &mut vertices, &mut sides)?;
        }

        self.vertices = vertices;
        self.sides = sides;

        Ok(())
    }

    fn recenter(&mut self) {
        let center = polygon_center(self.vertices.iter().copied()).coords;
        for vertice in &mut self.vertices {
            *vertice -= center;
        }
        self.center += center;
    }

    fn finish(self) -> BuiltSolid<'a> {
        BuiltSolid {
            solid: self.solid,
            position: self.center,
            vertices: self.vertices,
            sides: self.sides.into_iter().map(SideBuilder::finish).collect(),
            materials: self.materials,
        }
    }
}

fn lerp_uv(lhs: (f64, f64), rhs: (f64, f64), t: f64) -> (f64, f64) {
    (lhs.0 * (1.0 - t) + rhs.0 * t, lhs.1 * (1.0 - t) + rhs.1 * t)
}

impl Solid {
    /// # Errors
    ///
    /// Returns `Err` if the mesh creation fails.
    pub fn build_mesh(
        &self,
        get_material_info: impl FnMut(&Path) -> MaterialInfo,
    ) -> Result<BuiltSolid, SolidError> {
        let mut builder = SolidBuilder::new(self);
        builder.intersect_sides();
        builder.remove_invalid_sides();
        builder.sort_vertices();
        builder.build_uvs(get_material_info);
        builder.maybe_build_displacement()?;
        builder.recenter();
        Ok(builder.finish())
    }
}

#[cfg(test)]
mod tests {
    use crate::vdf;

    use super::*;
    use approx::assert_relative_eq;

    #[test]
    fn plane_creation() {
        let plane_1 = NdPlane::from_points(
            &Point3::new(-1.0, 0.0, 0.0),
            &Point3::new(0.0, 3.0, 0.0),
            &Point3::new(0.0, 0.0, 2.0),
        );
        let plane_2 = NdPlane {
            point: Point3::new(-1.0, 0.0, 0.0),
            normal: Vector3::new(6.0, -2.0, -3.0).normalize(),
            distance: 0.857_142_857_142_857,
        };
        assert_relative_eq!(plane_1.normal, plane_2.normal, epsilon = EPSILON);
        assert_relative_eq!(plane_1.distance, plane_2.distance, epsilon = EPSILON);

        let distance = plane_1.distance_to_point(&Point3::new(0.0, -2.0, 0.0));
        assert_relative_eq!(distance, 1.428_571_428_571_428, epsilon = EPSILON);
    }

    #[test]
    fn plane_intersection() {
        let a = Point3::new(1.0, 2.0, -1.0);
        let b = Point3::new(3.0, -2.0, 1.0);
        let c = Point3::new(2.0, 3.0, 0.0);
        let d = Point3::new(-3.0, 2.0, 4.0);

        let plane_1 = NdPlane::from_points(&a, &b, &c);
        let plane_2 = NdPlane::from_points(&a, &c, &d);
        let plane_3 = NdPlane::from_points(&c, &b, &d);

        let intersection = intersect_planes(&plane_1, &plane_2, &plane_3).unwrap();

        assert_relative_eq!(intersection, c, epsilon = EPSILON);
    }

    #[test]
    fn center_calculation() {
        let points = vec![
            Point3::new(2.0, 0.0, 0.0),
            Point3::new(0.0, 2.0, 1.0),
            Point3::new(-2.0, 0.0, 2.0),
            Point3::new(0.0, -2.0, 1.0),
        ];

        let center = polygon_center(points.into_iter());

        assert_relative_eq!(center, Point3::new(0.0, 0.0, 1.0), epsilon = EPSILON);
    }

    #[test]
    fn normal_calculation() {
        let points = vec![
            Point3::new(2.0, 0.0, 0.0),
            Point3::new(0.0, 2.0, 1.0),
            Point3::new(-2.0, 0.0, 2.0),
            Point3::new(0.0, -2.0, 1.0),
        ];

        let normal = polygon_normal(points.into_iter());

        assert_relative_eq!(
            normal,
            Vector3::new(0.447_213_595, 0.0, 0.894_427_191),
            epsilon = EPSILON
        );
    }

    fn get_test_solid() -> Solid {
        let input = r#"
            "id" "3"
            side
            {
                "id" "7"
                "plane" "(-960 64 64) (-832 64 64) (-832 -64 64)"
                "material" "AR_DIZZY/DIZZY_FACADE_COLOR"
                "uaxis" "[1 0 0 0] 0.25"
                "vaxis" "[0 -1 0 0] 0.25"
                "rotation" "0"
                "lightmapscale" "16"
                "smoothing_groups" "0"
            }
            side
            {
                "id" "8"
                "plane" "(-960 -64 0) (-832 -64 0) (-832 64 0)"
                "material" "AR_DIZZY/DIZZY_FACADE_COLOR"
                "uaxis" "[1 0 0 0] 0.25"
                "vaxis" "[0 -1 0 0] 0.25"
                "rotation" "0"
                "lightmapscale" "16"
                "smoothing_groups" "0"
            }
            side
            {
                "id" "9"
                "plane" "(-960 64 64) (-960 -64 64) (-960 -64 0)"
                "material" "AR_DIZZY/DIZZY_FACADE_COLOR"
                "uaxis" "[0 1 0 0] 0.25"
                "vaxis" "[0 0 -1 0] 0.25"
                "rotation" "0"
                "lightmapscale" "16"
                "smoothing_groups" "0"
            }
            side
            {
                "id" "10"
                "plane" "(-832 64 0) (-832 -64 0) (-832 -64 64)"
                "material" "AR_DIZZY/DIZZY_FACADE_COLOR"
                "uaxis" "[0 1 0 0] 0.25"
                "vaxis" "[0 0 -1 0] 0.25"
                "rotation" "0"
                "lightmapscale" "16"
                "smoothing_groups" "0"
            }
            side
            {
                "id" "11"
                "plane" "(-832 64 64) (-960 64 64) (-960 64 0)"
                "material" "AR_DIZZY/DIZZY_FACADE_COLOR"
                "uaxis" "[1 0 0 0] 0.25"
                "vaxis" "[0 0 -1 0] 0.25"
                "rotation" "0"
                "lightmapscale" "16"
                "smoothing_groups" "0"
            }
            side
            {
                "id" "12"
                "plane" "(-832 -64 0) (-960 -64 0) (-960 -64 64)"
                "material" "AR_DIZZY/DIZZY_FACADE_COLOR"
                "uaxis" "[1 0 0 0] 0.25"
                "vaxis" "[0 0 -1 0] 0.25"
                "rotation" "0"
                "lightmapscale" "16"
                "smoothing_groups" "0"
            }
            editor
            {
                "color" "0 177 198"
                "visgroupshown" "1"
                "visgroupautoshown" "1"
            }
        "#;

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
        builder.intersect_sides();
        builder.remove_invalid_sides();
        builder.recenter();

        let expected_vertices = vec![
            Point3::new(-64.0, 64.0, 32.0),
            Point3::new(-64.0, -64.0, 32.0),
            Point3::new(64.0, 64.0, 32.0),
            Point3::new(64.0, -64.0, 32.0),
            Point3::new(-64.0, 64.0, -32.0),
            Point3::new(-64.0, -64.0, -32.0),
            Point3::new(64.0, 64.0, -32.0),
            Point3::new(64.0, -64.0, -32.0),
        ];

        let mut is = vec![0; expected_vertices.len()];

        assert_eq!(builder.vertices.len(), expected_vertices.len());

        for (i, vertice) in builder.vertices.iter().enumerate() {
            assert!(
                expected_vertices.iter().enumerate().any(|(j, v)| {
                    if relative_eq!(v, vertice, epsilon = EPSILON) {
                        is[i] = j;
                        true
                    } else {
                        false
                    }
                }),
                "unexpected vertice {}",
                vertice
            );
        }

        assert_relative_eq!(
            builder.center,
            Point3::new(-896.0, 0.0, 32.0),
            epsilon = EPSILON
        );

        builder.sort_vertices();

        assert_face_sorted(
            &builder.sides[0].vertice_indices,
            &[is[1], is[3], is[2], is[0]],
            "+z",
        );
        assert_face_sorted(
            &builder.sides[1].vertice_indices,
            &[is[6], is[7], is[5], is[4]],
            "-z",
        );
        assert_face_sorted(
            &builder.sides[2].vertice_indices,
            &[is[4], is[5], is[1], is[0]],
            "-x",
        );
        assert_face_sorted(
            &builder.sides[3].vertice_indices,
            &[is[3], is[7], is[6], is[2]],
            "+x",
        );
        assert_face_sorted(
            &builder.sides[4].vertice_indices,
            &[is[2], is[6], is[4], is[0]],
            "+y",
        );
        assert_face_sorted(
            &builder.sides[5].vertice_indices,
            &[is[5], is[7], is[3], is[1]],
            "-y",
        );
    }
}
