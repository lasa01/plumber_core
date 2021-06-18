use std::{collections::BTreeMap, fmt::Debug};

use crate::{
    fs::{Path, PathBuf},
    vmt::loader::MaterialInfo,
};

use super::{
    entities::{EntityParseError, Overlay, OverlayUvInfo},
    Plane, Side, Solid,
};

use approx::{abs_diff_eq, relative_eq};
use float_ord::FloatOrd;
use itertools::Itertools;
use nalgebra::{geometry::Point3, Matrix2x3, Matrix3, Point2, Vector3};
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

    fn from_point_normal(point: Point3<f64>, normal: Vector3<f64>) -> Self {
        let distance = -point.coords.dot(&normal);
        Self {
            point,
            normal,
            distance,
        }
    }

    fn distance_to_point(&self, point: &Point3<f64>) -> f64 {
        (point - self.point).dot(&self.normal)
    }

    fn intersect_line(
        &self,
        line_point: &Point3<f64>,
        line_direction: &Vector3<f64>,
    ) -> Option<Point3<f64>> {
        let line_direction = line_direction.normalize();
        if abs_diff_eq!(self.normal.dot(&line_direction), 0.0, epsilon = EPSILON) {
            return None;
        }
        let t = (self.normal.dot(&self.point.coords) - self.normal.dot(&line_point.coords))
            / self.normal.dot(&line_direction);
        Some(line_point + line_direction * t)
    }

    fn intersect(a: &NdPlane, b: &NdPlane, c: &NdPlane) -> Option<Point3<f64>> {
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

    fn sort_vertices(&mut self, vertices: &mut Vec<Point3<f64>>) {
        let center: Point3<f64> = polygon_center(self.vertice_indices.iter().map(|&i| vertices[i]));

        for i in 0..self.vertice_indices.len() - 2 {
            let vertice = &vertices[self.vertice_indices[i]];
            let to_current: Vector3<f64> = (vertice - center).normalize();
            let filter_plane =
                NdPlane::from_points(&vertice, &center, &(center + self.plane.normal));

            if let Some((next_idx, _)) = self.vertice_indices[i + 1..]
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
                self.vertice_indices.swap(i + 1, i + 1 + next_idx);
            }
        }

        // reverse if the normal is facing the wrong way
        if polygon_normal(self.vertice_indices.iter().map(|i| vertices[*i])).dot(&self.plane.normal)
            < 0.0
        {
            self.vertice_indices.reverse();
        }
    }

    fn build_uvs(
        &mut self,
        center: &Point3<f64>,
        vertices: &[Point3<f64>],
        materials: &mut Vec<PathBuf>,
        get_material_info: &mut impl FnMut(&Path) -> MaterialInfo,
    ) {
        self.material_index = materials
            .iter()
            .position(|p| p == &self.side.material)
            .unwrap_or_else(|| {
                let index = materials.len();
                materials.push(self.side.material.clone());
                index
            });

        let material_info = get_material_info(&self.side.material);
        let texture_width = f64::from(material_info.width());
        let texture_height = f64::from(material_info.height());
        self.vertice_uvs.reserve_exact(self.vertice_indices.len());
        let u_axis = self.side.u_axis;
        let v_axis = self.side.v_axis;
        for &vi in &self.vertice_indices {
            let u = (vertices[vi].coords + center.coords).dot(&u_axis.axis)
                / (texture_width * u_axis.scale)
                + u_axis.translation / texture_width;
            let v = (vertices[vi].coords + center.coords).dot(&v_axis.axis)
                / (texture_height * v_axis.scale)
                + v_axis.translation / texture_height;
            self.vertice_uvs.push((u, v));
        }

        // normalize
        let mut nearest_u = f64::MAX;
        let mut nearest_v = f64::MAX;
        for (u, _) in &self.vertice_uvs {
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

        for (_, v) in &self.vertice_uvs {
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

        for (u, v) in &mut self.vertice_uvs {
            *u -= nearest_u;
            *v -= nearest_v;
        }
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
        center: &Point3<f64>,
        old_vertices: &[Point3<f64>],
        vertices: &mut Vec<Point3<f64>>,
        sides: &mut Vec<SideBuilder<'a>>,
    ) -> Result<(), SolidError> {
        if let Some(info) = &self.side.disp_info {
            self.verify_displacement()?;

            // find out which corner vertice the displacement start position is
            let start_position = info.start_position - center.coords;

            let (start_i, _) = self
                .vertice_indices
                .iter()
                .enumerate()
                .min_by_key(|(_, &vertice_i)| {
                    let vertice = old_vertices[vertice_i];
                    FloatOrd(vertice.coords.metric_distance(&start_position))
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
            if let Some(point) = NdPlane::intersect(
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
                    .position(|v| relative_eq!(*v, &point, epsilon = EPSILON))
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
            builder.sort_vertices(vertices);
        }
    }

    fn build_uvs(&mut self, mut get_material_info: impl FnMut(&Path) -> MaterialInfo) {
        let vertices = &self.vertices;
        for builder in &mut self.sides {
            builder.build_uvs(
                &self.center,
                vertices,
                &mut self.materials,
                &mut get_material_info,
            );
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
            builder.maybe_build_displacement(
                &self.center,
                old_vertices,
                &mut vertices,
                &mut sides,
            )?;
        }

        self.vertices = vertices;
        self.sides = sides;

        Ok(())
    }

    fn side_faces_map(&self) -> SideFacesMap {
        let mut map = SideFacesMap::new();

        for builder in &self.sides {
            map.entry(builder.side.id).or_default().push(
                builder
                    .vertice_indices
                    .iter()
                    .map(|&i| self.center + self.vertices[i].coords)
                    .collect(),
            );
        }

        map
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

fn lerp(lhs: f64, rhs: f64, t: f64) -> f64 {
    lhs + (lhs - rhs) * t
}

fn lerp_uv(lhs: (f64, f64), rhs: (f64, f64), t: f64) -> (f64, f64) {
    (lerp(lhs.0, rhs.0, t), lerp(lhs.1, rhs.1, t))
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

type SideFacesMap = BTreeMap<i32, Vec<Vec<Point3<f64>>>>;

#[derive(Debug, Error)]
pub enum OverlayError {
    #[error("error parsing overlay: {0}")]
    Parse(#[from] EntityParseError),
    #[error("overlay is applied to nonexistent side id {id}")]
    SideNotFound { id: i32 },
    #[error("no geometry was imported for the sides the overlay was applied to")]
    NoGeometry,
    #[error("overlay contains invalid uv data")]
    InvalidUvData,
}

#[derive(Debug)]
pub struct BuiltOverlay<'a, 'b> {
    pub overlay: &'a Overlay<'b>,
    pub position: Point3<f64>,
    pub vertices: Vec<Point3<f64>>,
    pub faces: Vec<BuiltOverlayFace>,
    pub material: PathBuf,
}

#[derive(Debug)]
pub struct BuiltOverlayFace {
    pub vertice_indices: Vec<usize>,
    pub vertice_uvs: Vec<(f64, f64)>,
}

struct OverlayFaceBuilder {
    vertice_indices: Vec<usize>,
    vertice_uvs: Vec<(f64, f64)>,
}

impl OverlayFaceBuilder {
    fn finish(self) -> BuiltOverlayFace {
        BuiltOverlayFace {
            vertice_indices: self.vertice_indices,
            vertice_uvs: self.vertice_uvs,
        }
    }
}

struct OverlayBuilder<'a, 'b> {
    overlay: &'a Overlay<'b>,
    uv_info: OverlayUvInfo,
    origin: Point3<f64>,
    uv_to_global_matrix: Matrix3<f64>,
    global_to_uv_matrix: Matrix3<f64>,
    faces: Vec<OverlayFaceBuilder>,
    vertices: Vec<Point3<f64>>,
    uv_space_vertices: Vec<Point3<f64>>,
}

impl<'a, 'b> OverlayBuilder<'a, 'b> {
    fn new(overlay: &'a Overlay<'b>) -> Result<Self, OverlayError> {
        let uv_info = overlay.uv_info()?;

        let u_axis = uv_info.basis_u;
        let v_axis = uv_info.basis_v;
        let normal = uv_info.basis_normal;
        let origin = uv_info.basis_origin;

        let uv_to_global_matrix = Matrix3::from_row_slice(&[
            u_axis.x, v_axis.x, normal.x, u_axis.y, v_axis.y, normal.y, u_axis.z, v_axis.z,
            normal.z,
        ]);

        let global_to_uv_matrix = uv_to_global_matrix
            .try_inverse()
            .ok_or(OverlayError::InvalidUvData)?;

        Ok(Self {
            overlay,
            uv_info,
            origin,
            uv_to_global_matrix,
            global_to_uv_matrix,
            faces: Vec::new(),
            vertices: Vec::new(),
            uv_space_vertices: Vec::new(),
        })
    }

    fn create_vertices(&mut self, side_faces_map: &SideFacesMap) -> Result<(), OverlayError> {
        for side_i in self.overlay.sides()? {
            let faces = side_faces_map
                .get(&side_i)
                .ok_or(OverlayError::SideNotFound { id: side_i })?;

            for vertices in faces {
                let vertice_indices = vertices
                    .iter()
                    .map(|vertice| {
                        // check if vertice already exists
                        self.vertices
                            .iter()
                            .position(|v| relative_eq!(v, vertice, epsilon = EPSILON))
                            .unwrap_or_else(|| {
                                // if not, create it
                                self.vertices.push(*vertice);
                                self.vertices.len() - 1
                            })
                    })
                    .collect();
                self.faces.push(OverlayFaceBuilder {
                    vertice_indices,
                    vertice_uvs: Vec::new(),
                });
            }
        }

        if self.faces.is_empty() {
            return Err(OverlayError::NoGeometry);
        }

        Ok(())
    }

    fn offset_vertices(&mut self) -> Result<(), OverlayError> {
        let offset = f64::from(self.overlay.render_order()? + 1) * 0.1;
        let mut offset_directions = vec![Vector3::zeros(); self.vertices.len()];

        for builder in &self.faces {
            let normal = polygon_normal(builder.vertice_indices.iter().map(|&i| self.vertices[i]));
            for &vert_i in &builder.vertice_indices {
                offset_directions[vert_i] += normal;
            }
        }

        for (direction, vertice) in offset_directions.iter().zip(self.vertices.iter_mut()) {
            *vertice += offset * direction.normalize();
        }

        Ok(())
    }

    fn cut_faces(&mut self) -> Result<(), OverlayError> {
        let vertices = &mut self.vertices;
        let global_to_uv_matrix = &self.global_to_uv_matrix;

        let mut uv_space_vertices = vertices
            .iter()
            .map(|v| global_to_uv_matrix * v)
            .collect_vec();

        let up = Vector3::new(0.0, 0.0, 1.0);

        // cut faces partially outside uv borders
        for (side_vert_a, side_vert_b) in self.uv_info.uvs.iter().circular_tuple_windows() {
            let cut_plane_normal = up.cross(&(side_vert_a - side_vert_b));
            let cut_plane = NdPlane::from_point_normal(*side_vert_a, cut_plane_normal);

            let outside_vertice_is = uv_space_vertices
                .iter()
                .positions(|v| cut_plane.distance_to_point(v) > CUT_THRESHOLD)
                .collect_vec();

            for builder in &mut self.faces {
                let inside_i = match builder
                    .vertice_indices
                    .iter()
                    .position(|i| !outside_vertice_is.contains(i))
                {
                    Some(i) => i,
                    // this face is completely outside the border, will be removed later
                    None => continue,
                };

                // rotate the vec so that it starts with a vertice that is inside
                builder.vertice_indices.rotate_left(inside_i);

                // find the first face vertice that is outside the uv border
                let first_outside_i = match builder
                    .vertice_indices
                    .iter()
                    .position(|i| outside_vertice_is.contains(i))
                {
                    Some(i) => i,
                    // this face is completely inside the border, doesn't need to be cut
                    None => continue,
                };

                let first_line_a = uv_space_vertices[builder.vertice_indices
                    [(first_outside_i - 1) % builder.vertice_indices.len()]];
                let first_line_b = uv_space_vertices[builder.vertice_indices[first_outside_i]];

                // create new vertice on the uv border by intersecting the first edge crossing the uv border with the uv border plane
                let new_uv_space_vertice = cut_plane
                    .intersect_line(&first_line_a, &(first_line_b - first_line_a))
                    .ok_or(OverlayError::InvalidUvData)?;
                let new_vertice =
                    self.origin + self.uv_to_global_matrix * new_uv_space_vertice.coords;

                let first_new_i = vertices
                    .iter()
                    .position(|v| relative_eq!(v, &new_vertice, epsilon = EPSILON))
                    .unwrap_or_else(|| {
                        uv_space_vertices.push(new_uv_space_vertice);
                        vertices.push(new_vertice);
                        vertices.len() - 1
                    });

                // do the same for the last face vertice that is outside the border
                let last_outside_i = builder
                    .vertice_indices
                    .iter()
                    .rev()
                    .position(|i| outside_vertice_is.contains(i))
                    // the case where there are no vertices outside is handled above so unwrap cannot fail.
                    .unwrap();

                let last_line_a = uv_space_vertices
                    [builder.vertice_indices[(last_outside_i + 1) % builder.vertice_indices.len()]];
                let last_line_b = uv_space_vertices[builder.vertice_indices[last_outside_i]];

                let new_uv_space_vertice = cut_plane
                    .intersect_line(&last_line_a, &(last_line_b - last_line_a))
                    .ok_or(OverlayError::InvalidUvData)?;
                let new_vertice =
                    self.origin + self.uv_to_global_matrix * new_uv_space_vertice.coords;

                let last_new_i = vertices
                    .iter()
                    .position(|v| relative_eq!(v, &new_vertice, epsilon = EPSILON))
                    .unwrap_or_else(|| {
                        uv_space_vertices.push(new_uv_space_vertice);
                        vertices.push(new_vertice);
                        vertices.len() - 1
                    });

                // replace the face vertices that were outside the uv border with the 2 newly created ones
                builder.vertice_indices.splice(
                    first_outside_i..=last_outside_i,
                    [first_new_i, last_new_i].iter().copied(),
                );
            }
        }

        self.uv_space_vertices = uv_space_vertices;

        Ok(())
    }

    fn remove_vertices_outside(&mut self) {
        let mut vertices_to_remove = Vec::new();
        let up = Vector3::new(0.0, 0.0, 1.0);

        for (side_vert_a, side_vert_b) in self.uv_info.uvs.iter().circular_tuple_windows() {
            let cut_plane_normal = up.cross(&(side_vert_a - side_vert_b));
            let cut_plane = NdPlane::from_point_normal(*side_vert_a, cut_plane_normal);
            for (i, _) in self
                .uv_space_vertices
                .iter()
                .enumerate()
                .filter(|(_, v)| cut_plane.distance_to_point(v) > CUT_THRESHOLD)
            {
                if !vertices_to_remove.contains(&i) {
                    vertices_to_remove.push(i);
                }
            }
        }

        let mut i = 0_usize;
        let mut new_i = 0_usize;
        let faces = &mut self.faces;
        self.vertices.retain(|_| {
            if vertices_to_remove.contains(&i) {
                // remove faces referencing the vertice
                faces.retain(|f| !f.vertice_indices.contains(&i));
                // fix other faces' vertice indices
                for builder in faces.iter_mut() {
                    for f_i in &mut builder.vertice_indices {
                        if *f_i == i {
                            *f_i = new_i;
                        }
                    }
                }
                i += 1;
                false
            } else {
                i += 1;
                new_i += 1;
                true
            }
        });
    }

    fn ensure_not_empty(&self) -> Result<(), OverlayError> {
        if self.faces.is_empty() {
            return Err(OverlayError::InvalidUvData);
        }
        Ok(())
    }

    fn create_uvs(&mut self) -> Result<(), OverlayError> {
        for builder in &mut self.faces {
            // uv calculations are 2 affine transformations of triangles

            // calculate matrix for first triangle (vertices 0, 1, 2)
            let mut src_matrix_a = Matrix3::new(
                self.uv_info.uvs[0].x,
                self.uv_info.uvs[1].x,
                self.uv_info.uvs[2].x,
                self.uv_info.uvs[0].y,
                self.uv_info.uvs[1].y,
                self.uv_info.uvs[2].y,
                1.0,
                1.0,
                1.0,
            );

            let dst_matrix_a = Matrix2x3::new(
                self.uv_info.start_u,
                self.uv_info.start_u,
                self.uv_info.end_u,
                self.uv_info.end_v,
                self.uv_info.start_v,
                self.uv_info.start_v,
            );

            if !src_matrix_a.try_inverse_mut() {
                return Err(OverlayError::InvalidUvData);
            }

            let affine_matrix_a = dst_matrix_a * src_matrix_a;

            // same for second triangle (vertices 2, 3, 0)
            let mut src_matrix_b = Matrix3::new(
                self.uv_info.uvs[2].x,
                self.uv_info.uvs[3].x,
                self.uv_info.uvs[0].x,
                self.uv_info.uvs[2].y,
                self.uv_info.uvs[3].y,
                self.uv_info.uvs[0].y,
                1.0,
                1.0,
                1.0,
            );

            let dst_matrix_b = Matrix2x3::new(
                self.uv_info.end_u,
                self.uv_info.end_u,
                self.uv_info.start_u,
                self.uv_info.start_v,
                self.uv_info.end_v,
                self.uv_info.end_v,
            );

            if !src_matrix_b.try_inverse_mut() {
                return Err(OverlayError::InvalidUvData);
            }

            let affine_matrix_b = dst_matrix_b * src_matrix_b;

            for &vert_i in &builder.vertice_indices {
                let uv_vert = self.uv_space_vertices[vert_i].xy();

                // determine which triangle this vertice is inside of
                let affine_matrix = if distance_point_to_line(
                    &self.uv_info.uvs[0].xy(),
                    &self.uv_info.uvs[2].xy(),
                    &uv_vert,
                ) > 0.0
                {
                    &affine_matrix_b
                } else {
                    &affine_matrix_a
                };

                let uv = affine_matrix.remove_column(2) * uv_vert + affine_matrix.column(2);
                builder.vertice_uvs.push((uv.x, uv.y));
            }
        }
        Ok(())
    }

    fn recenter(&mut self) {
        let center = polygon_center(self.vertices.iter().copied()).coords;
        for vertice in &mut self.vertices {
            *vertice -= center;
        }
        self.origin += center;
    }

    fn finish(self) -> Result<BuiltOverlay<'a, 'b>, OverlayError> {
        let material = self.overlay.material()?.to_string().into();

        Ok(BuiltOverlay {
            overlay: self.overlay,
            position: self.origin,
            vertices: self.vertices,
            faces: self
                .faces
                .into_iter()
                .map(OverlayFaceBuilder::finish)
                .collect(),
            material,
        })
    }
}

fn distance_point_to_line(line_a: &Point2<f64>, line_b: &Point2<f64>, point: &Point2<f64>) -> f64 {
    (point.x - line_a.x) * (line_b.y - line_a.y) - (point.y - line_a.y) * (line_b.x - line_a.x)
}

impl<'a> Overlay<'a> {
    /// # Errors
    ///
    /// Returns `Err` if the mesh creation fails.
    pub fn build_mesh(&self, side_faces_map: &SideFacesMap) -> Result<BuiltOverlay, OverlayError> {
        let mut builder = OverlayBuilder::new(self)?;
        builder.create_vertices(side_faces_map)?;
        builder.offset_vertices()?;
        builder.cut_faces()?;
        builder.remove_vertices_outside();
        builder.ensure_not_empty()?;
        builder.create_uvs()?;
        builder.recenter();
        builder.finish()
    }
}

#[cfg(test)]
mod tests {
    use plumber_vdf as vdf;

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

        let intersection = NdPlane::intersect(&plane_1, &plane_2, &plane_3).unwrap();

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

        builder.build_uvs(|_| MaterialInfo::new(1024, 1024, false));

        let side = &builder.sides[0];
        let expected_uvs = side
            .vertice_indices
            .iter()
            .map(|&i| match is.iter().position(|&j| j == i).unwrap() {
                0 => (-0.75, -0.25),
                1 => (-0.75, 0.25),
                2 => (-0.25, -0.25),
                3 => (-0.25, 0.25),
                _ => unreachable!(),
            })
            .collect_vec();
        for (uv, expected_uv) in side.vertice_uvs.iter().zip(&expected_uvs) {
            assert!(
                relative_eq!(uv.0, expected_uv.0, epsilon = EPSILON),
                "got {:?}, expected {:?}",
                side.vertice_uvs,
                expected_uvs
            );
            assert!(
                relative_eq!(uv.1, expected_uv.1, epsilon = EPSILON),
                "got {:?}, expected {:?}",
                side.vertice_uvs,
                expected_uvs
            );
        }
    }

    #[allow(clippy::too_many_lines)]
    fn get_test_displacement() -> Solid {
        let input = r#"
        "id" "9616"
		side
		{
			"id" "9622"
			"plane" "(-331.905 -512 342.744) (-331.905 512 342.744) (512 512 -237.255)"
			"material" "AR_DIZZY/DIZZY_WOODEN_PLANKS_02"
			"uaxis" "[0.824159 0.0174523 -0.566426 -177.341] 0.25"
			"vaxis" "[0.0144681 -1 -0.00975883 -3.11321] 0.25"
			"rotation" "1"
			"lightmapscale" "16"
			"smoothing_groups" "0"
			dispinfo
			{
				"power" "3"
				"startposition" "[-331.91 -512 342.746]"
				"flags" "0"
				"elevation" "100"
				"subdiv" "0"
				normals
				{
					"row0" "0.566406 0 0.824126 0.566406 0 0.824126 0.566406 0 0.824126 0.566406 0 0.824126 0.566406 0 0.824126 0.566406 0 0.824126 0.566406 0 0.824126 0.566406 0 0.824126 0.566406 0 0.824126"
					"row1" "0.566406 0 0.824126 0.566406 0 0.824126 0.566406 0 0.824126 0.566406 0 0.824126 0.566406 0 0.824126 0.566406 0 0.824126 0.566406 0 0.824126 0.566406 0 0.824126 -0.566405 7.6807e-09 -0.824127"
					"row2" "0.566406 0 0.824126 0.566406 0 0.824126 -0.566406 0 -0.824126 0.566406 0 0.824126 0.566406 0 0.824126 -0.566406 0 -0.824126 0.566406 0 0.824126 0.566406 0 0.824126 -0.566405 7.6807e-09 -0.824127"
					"row3" "0.566406 0 0.824126 -0.566406 0 -0.824126 -0.566406 0 -0.824126 0.936656 0 0.350251 -0.566406 0 -0.824126 0.566406 0 0.824126 -0.566406 0 -0.824126 -0.566406 0 -0.824126 -0.566405 7.6807e-09 -0.824127"
					"row4" "0.566406 0 0.824126 0.566406 0 0.824126 -0.566406 0 -0.824126 -0.131473 0 -0.991319 -0.566406 0 -0.824126 -0.566406 0 -0.824125 -0.566406 0 -0.824126 0.566406 0 0.824126 -0.566405 7.6807e-09 -0.824127"
					"row5" "0.566406 0 0.824126 0.566406 0 0.824126 -0.566406 0 -0.824126 0.566406 0 0.824126 0.771655 0 0.636041 0.566406 0 0.824126 0.566406 0 0.824126 -0.566406 0 -0.824126 -0.566405 7.6807e-09 -0.824127"
					"row6" "0.566406 0 0.824126 0.566406 0 0.824126 -0.566406 0 -0.824126 -0.566406 0 -0.824126 -0.566406 0 -0.824126 0.83476 0 -0.550614 -0.566406 0 -0.824126 -0.566406 0 -0.824126 -0.566405 7.6807e-09 -0.824127"
					"row7" "0.566406 0 0.824126 -0.566406 0 -0.824126 0.566406 0 0.824126 0.566406 0 0.824126 -0.566406 0 -0.824126 0.566406 0 0.824126 0.566406 0 0.824126 0.566406 0 0.824126 -0.566405 7.6807e-09 -0.824127"
					"row8" "0.566406 0 0.824126 0.566406 0 0.824126 0.566406 0 0.824126 0.566406 0 0.824126 0.566406 0 0.824126 0.566406 0 0.824126 0.566406 0 0.824126 0.566406 0 0.824126 -0.566405 7.6807e-09 -0.824127"
				}
				distances
				{
					"row0" "50 50 50 50 50 50 50 50 50"
					"row1" "10 5 4.32837 10 10 0.597336 0.548607 5 100"
					"row2" "10 96.9727 74.0859 10 10 69.8319 99.058 34.4013 100"
					"row3" "10 24.6397 97.5531 232.629 11.2831 37.3523 19.2188 19.7239 100"
					"row4" "10 31.228 63.1295 18.4024 28.0858 0.264973 3.01036 14.2535 100"
					"row5" "10 57.2629 89.4883 23.9684 30.2479 0.910881 17.3618 5.79103 100"
					"row6" "10 38.0744 11.7455 28.5764 25.2672 41.7018 46.706 50.699 100"
					"row7" "10 7.24235 5 5 62.3554 5 9.01616 1.54124 100"
					"row8" "10 5 5 5 5 5 5 5 100"
				}
				offsets
				{
					"row0" "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
					"row1" "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
					"row2" "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
					"row3" "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
					"row4" "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
					"row5" "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
					"row6" "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
					"row7" "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
					"row8" "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
				}
				offset_normals
				{
					"row0" "0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127"
					"row1" "0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127"
					"row2" "0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127"
					"row3" "0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127"
					"row4" "0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127"
					"row5" "0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127"
					"row6" "0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127"
					"row7" "0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127"
					"row8" "0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127 0.566405 -8.40277e-09 0.824127"
				}
				alphas
				{
					"row0" "0 0 0 0 0 0 0 0 0"
					"row1" "0 0 0 0 0 0 0 0 0"
					"row2" "0 0 0 0 0 0 0 0 0"
					"row3" "0 0 0 0 0 0 0 0 0"
					"row4" "0 0 0 0 0 0 0 0 0"
					"row5" "0 0 0 0 0 0 0 0 0"
					"row6" "0 0 0 0 0 0 0 0 0"
					"row7" "0 0 0 0 0 0 0 0 0"
					"row8" "0 0 0 0 0 0 0 0 0"
				}
				triangle_tags
				{
					"row0" "1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0"
					"row1" "9 9 0 1 1 9 9 9 1 0 9 0 0 0 0 0"
					"row2" "0 1 0 0 9 9 0 0 9 0 9 0 0 0 0 0"
					"row3" "0 9 0 0 9 0 0 0 9 9 1 0 9 9 0 0"
					"row4" "9 9 0 0 9 9 1 1 0 9 9 9 1 9 0 0"
					"row5" "9 9 0 0 9 0 1 1 0 9 0 1 0 1 0 0"
					"row6" "1 9 0 9 9 1 9 0 9 9 0 1 1 1 0 0"
					"row7" "1 1 9 9 9 9 9 0 9 9 9 9 1 9 0 0"
				}
				allowed_verts
				{
					"10" "-1 -1 -1 -1 -1 -1 -1 -1 -1 -1"
				}
			}
		}
		side
		{
			"id" "9623"
			"plane" "(-404.406 512 237.255) (-404.406 -512 237.255) (439.499 -512 -342.744)"
			"material" "AR_DIZZY/DIZZY_WOODEN_PLANKS_02"
			"uaxis" "[0.824158 0 -0.566427 -177.341] 0.25"
			"vaxis" "[0 -1 0 0] 0.25"
			"rotation" "0"
			"lightmapscale" "16"
			"smoothing_groups" "0"
		}
		side
		{
			"id" "9624"
			"plane" "(-404.41 -512 237.256) (-404.41 512 237.256) (-331.908 512 342.747)"
			"material" "AR_DIZZY/DIZZY_WOODEN_PLANKS_02"
			"uaxis" "[0 1 0 0] 0.25"
			"vaxis" "[-0.566427 0 -0.824158 121.883] 0.25"
			"rotation" "0"
			"lightmapscale" "16"
			"smoothing_groups" "0"
		}
		side
		{
			"id" "9625"
			"plane" "(439.499 512 -342.744) (439.499 -512 -342.744) (511.997 -512 -237.257)"
			"material" "AR_DIZZY/DIZZY_WOODEN_PLANKS_02"
			"uaxis" "[0 1 0 0] 0.25"
			"vaxis" "[-0.566427 0 -0.824158 121.883] 0.25"
			"rotation" "0"
			"lightmapscale" "16"
			"smoothing_groups" "0"
		}
		side
		{
			"id" "9626"
			"plane" "(-404.404 512 237.252) (439.497 512 -342.744) (511.997 512 -237.259)"
			"material" "AR_DIZZY/DIZZY_WOODEN_PLANKS_02"
			"uaxis" "[0.824158 0 -0.566427 -177.341] 0.25"
			"vaxis" "[-0.566427 0 -0.824158 121.883] 0.25"
			"rotation" "0"
			"lightmapscale" "16"
			"smoothing_groups" "0"
		}
		side
		{
			"id" "9627"
			"plane" "(439.497 -512 -342.744) (-404.408 -512 237.256) (-331.907 -512 342.745)"
			"material" "AR_DIZZY/DIZZY_WOODEN_PLANKS_02"
			"uaxis" "[0.824158 0 -0.566427 -177.341] 0.25"
			"vaxis" "[-0.566427 0 -0.824158 121.883] 0.25"
			"rotation" "0"
			"lightmapscale" "16"
			"smoothing_groups" "0"
		}
		editor
		{
			"color" "0 188 177"
			"visgroupshown" "1"
			"visgroupautoshown" "1"
		}
        "#;

        vdf::from_str(input).unwrap()
    }

    #[test]
    #[allow(clippy::unreadable_literal, clippy::too_many_lines)]
    fn displacement_building() {
        let solid = get_test_displacement();
        let mut builder = SolidBuilder::new(&solid);
        builder.intersect_sides();
        builder.remove_invalid_sides();
        builder.sort_vertices();
        builder.build_uvs(|_| MaterialInfo::new(1024, 1024, false));
        builder.maybe_build_displacement().unwrap();
        builder.recenter();

        let expected_vertices = vec![
            Point3::new(-393.92199516296387, -511.9999885559082, 334.96716022491455),
            Point3::new(449.9849796295166, -511.9999885559082, -245.03345489501953),
            Point3::new(-288.43352794647217, -511.9999885559082, 262.46702671051025),
            Point3::new(-182.94509649276733, -511.9999885559082, 189.9669885635376),
            Point3::new(-77.45669484138489, -511.9999885559082, 117.46692657470703),
            Point3::new(28.031721711158752, -511.9999885559082, 44.966840744018555),
            Point3::new(133.52017402648926, -511.9999885559082, -27.533242106437683),
            Point3::new(239.00854587554932, -511.9999885559082, -100.03330707550049),
            Point3::new(344.4969415664673, -511.9999885559082, -172.53336906433105),
            Point3::new(-416.57819747924805, -383.99999141693115, 302.00212001800537),
            Point3::new(365.0245666503906, -383.99999141693115, -368.65246295928955),
            Point3::new(-313.92178535461426, -383.99999141693115, 225.38139820098877),
            Point3::new(-208.81378650665283, -383.99999141693115, 152.3277997970581),
            Point3::new(-100.11293888092041, -383.99999141693115, 84.5018744468689),
            Point3::new(5.375472828745842, -383.99999141693115, 12.001800537109375),
            Point3::new(105.53817749023438, -383.99999141693115, -68.24725866317749),
            Point3::new(210.99896430969238, -383.99999141693115, -140.78748226165771),
            Point3::new(319.0086603164673, -383.99999141693115, -209.61904525756836),
            Point3::new(-416.57819747924805, -255.9999942779541, 302.00212001800537),
            Point3::new(365.0245666503906, -255.9999942779541, -368.65246295928955),
            Point3::new(-261.8278741836548, -255.9999942779541, 301.1784553527832),
            Point3::new(-253.22809219360352, -255.9999942779541, 87.7045750617981),
            Point3::new(-100.11293888092041, -255.9999942779541, 84.5018744468689),
            Point3::new(5.375472828745842, -255.9999942779541, 12.001800537109375),
            Point3::new(65.64663648605347, -255.9999942779541, -126.28982067108154),
            Point3::new(266.79527759552, -255.9999942779541, -59.603333473205566),
            Point3::new(335.66174507141113, -255.9999942779541, -185.38867235183716),
            Point3::new(-416.57819747924805, -127.99999713897705, 302.00212001800537),
            Point3::new(365.0245666503906, -127.99999713897705, -368.65246295928955),
            Point3::new(-330.7098865509033, -127.99999713897705, 200.95453262329102),
            Point3::new(-266.5200710296631, -127.99999713897705, 68.36463809013367),
            Point3::new(112.11632490158081, -127.99999713897705, 157.73917436599731),
            Point3::new(-6.679397076368332, -127.99999713897705, -5.5381543934345245),
            Point3::new(126.35641098022461, -127.99999713897705, -37.95653581619263),
            Point3::new(199.80257749557495, -127.99999713897705, -157.0783257484436),
            Point3::new(305.0049066543579, -127.99999713897705, -229.99463081359863),
            Point3::new(-416.57819747924805, 0.0, 302.00212001800537),
            Point3::new(365.0245666503906, 7.680699631862353e-7, -368.65246295928955),
            Point3::new(-299.06609058380127, 0.0, 246.9965696334839),
            Point3::new(-247.02234268188477, 0.0, 96.73402309417725),
            Point3::new(-108.19641351699829, 0.0, 58.01798701286316),
            Point3::new(-16.1965474486351, 0.0, -19.385695457458496),
            Point3::new(105.04977703094482, 0.0, -68.95790696144104),
            Point3::new(208.98315906524658, 0.0, -143.7205195426941),
            Point3::new(324.2499113082886, 0.0, -201.99296474456787),
            Point3::new(-416.57819747924805, 127.99999713897705, 302.00212001800537),
            Point3::new(365.0245666503906, 127.99999713897705, -368.65246295928955),
            Point3::new(-284.31975841522217, 127.99999713897705, 268.4526205062866),
            Point3::new(-261.9521141052246, 127.99999713897705, 75.01106262207031),
            Point3::new(-92.20115542411804, 127.99999713897705, 96.01361155509949),
            Point3::new(23.052367568016052, 127.99999713897705, 22.99945056438446),
            Point3::new(105.71578741073608, 127.99999713897705, -67.98886060714722),
            Point3::new(220.52206993103027, 127.99999713897705, -126.93129777908325),
            Point3::new(312.89656162261963, 127.99999713897705, -218.5122013092041),
            Point3::new(-416.57819747924805, 255.9999942779541, 302.00212001800537),
            Point3::new(365.0245666503906, 255.9999942779541, -368.65246295928955),
            Point3::new(-295.18823623657227, 255.9999942779541, 252.6388645172119),
            Point3::new(-217.91810989379883, 255.9999942779541, 139.0809178352356),
            Point3::new(-121.96284532546997, 255.9999942779541, 52.710068225860596),
            Point3::new(-14.600066840648651, 255.9999942779541, -17.06281304359436),
            Point3::new(140.01083374023438, 255.9999942779541, -91.70113205909729),
            Point3::new(184.2336654663086, 255.9999942779541, -179.73122596740723),
            Point3::new(287.46042251586914, 255.9999942779541, -255.5220365524292),
            Point3::new(-416.57819747924805, 383.99999141693115, 302.00212001800537),
            Point3::new(365.0245666503906, 383.99999141693115, -368.65246295928955),
            Point3::new(-320.8559274673462, 383.99999141693115, 215.2921438217163),
            Point3::new(-208.43336582183838, 383.99999141693115, 152.88132429122925),
            Point3::new(-102.94497013092041, 383.99999141693115, 80.3812563419342),
            Point3::new(-35.60705482959747, 383.99999141693115, -47.6281613111496),
            Point3::new(108.03189277648926, 383.99999141693115, -64.61890935897827),
            Point3::new(215.79504013061523, 383.99999141693115, -133.80916118621826),
            Point3::new(317.04962253570557, 383.99999141693115, -212.469482421875),
            Point3::new(-416.57819747924805, 511.9999885559082, 302.00212001800537),
            Point3::new(365.0245666503906, 511.9999885559082, -368.65246295928955),
            Point3::new(-313.92178535461426, 511.9999885559082, 225.38139820098877),
            Point3::new(-208.43336582183838, 511.9999885559082, 152.88132429122925),
            Point3::new(-102.94497013092041, 511.9999885559082, 80.3812563419342),
            Point3::new(2.5434570387005806, 511.9999885559082, 7.881172001361847),
            Point3::new(108.03189277648926, 511.9999885559082, -64.61890935897827),
            Point3::new(213.52026462554932, 511.9999885559082, -137.1189832687378),
            Point3::new(319.0086603164673, 511.9999885559082, -209.61904525756836),
        ];

        assert_eq!(builder.vertices.len(), expected_vertices.len());

        for vertice in &builder.vertices {
            assert!(
                expected_vertices
                    .iter()
                    .any(|v| relative_eq!(v, vertice, epsilon = EPSILON)),
                "unexpected vertice {}",
                vertice
            );
        }

        assert_relative_eq!(
            builder.center,
            Point3::new(146.974, 0.0, 131.398),
            epsilon = EPSILON
        )
    }
}
