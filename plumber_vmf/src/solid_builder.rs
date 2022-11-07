#![allow(clippy::cast_possible_truncation, clippy::cast_precision_loss)]

use std::{fmt::Debug, mem, ptr, sync::Mutex};

use approx::relative_eq;
use glam::{Vec2, Vec3};
use itertools::{izip, Itertools};
use log::warn;
use ndarray::{Array2, Array3, Zip};
use thiserror::Error;

use plumber_fs::{GamePathBuf, PathBuf};
use plumber_vmt::MaterialInfo;

use crate::{
    builder_utils::{
        closest_point_on_edge_to_point, distance_point_to_edge, point_is_inside_polygon,
        polygon_center, polygon_intersection, sort_polygon, CompatibleSpace, GeometrySettings,
        LocalSpace, NdPlane, PointClassification, PolygonClassification, PolygonIntersection,
        Space,
    },
    entities::SkyCamera,
    overlay_builder::SideFacesMap,
    sky_detector,
    vmf::{Entity, Side, Solid, World},
};

#[cfg(test)]
mod tests;

#[derive(Debug, Error, Clone, Hash, PartialEq, Eq)]
pub enum SolidError {
    #[error("displacement side `{side_id}` is not valid: got {vertices} vertices, expected 4")]
    InvalidDisplacement { side_id: i32, vertices: usize },
}

#[derive(Debug)]
pub struct BuiltSolid {
    pub id: i32,
    pub position: Vec3,
    pub scale: f32,
    pub vertices: Vec<Vec3>,
    pub faces: Vec<SolidFace>,
    pub materials: Vec<SolidMaterial>,
}

#[derive(Debug, Clone)]
pub struct SolidMaterial {
    pub name: GamePathBuf,
    pub info: MaterialInfo,
}

impl PartialEq for SolidMaterial {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

#[derive(Debug)]
pub struct SolidFace {
    pub vertice_indices: Vec<usize>,
    pub vertice_uvs: Vec<Vec2>,
    pub material_index: usize,
    pub vertice_alphas: Vec<f32>,
}

#[derive(Clone)]
pub(crate) struct FaceBuilder<'a> {
    side: &'a Side,
    plane: NdPlane,
    vertice_indices: Vec<usize>,
    vertice_uvs: Vec<Vec2>,
    material_index: usize,
    vertice_alphas: Vec<f32>,
    aabb_min: Vec3,
    aabb_max: Vec3,
}

impl<'a> PartialEq for FaceBuilder<'a> {
    fn eq(&self, other: &Self) -> bool {
        // compare side references instead of contents
        ptr::eq(self.side, other.side)
            && self.plane == other.plane
            && self.vertice_indices == other.vertice_indices
            && self.vertice_uvs == other.vertice_uvs
            && self.material_index == other.material_index
    }
}

impl<'a> Debug for FaceBuilder<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SideBuilder")
            .field("plane", &self.plane)
            .field("vertice_indices", &self.vertice_indices)
            .field("vertice_uvs", &self.vertice_uvs)
            .field("material_index", &self.material_index)
            .finish()
    }
}

impl<'a> FaceBuilder<'a> {
    fn new(side: &'a Side, center: Vec3) -> Self {
        let plane = NdPlane::from_plane(&side.plane, center);
        Self {
            side,
            plane,
            vertice_indices: Vec::new(),
            vertice_uvs: Vec::new(),
            material_index: 0,
            vertice_alphas: Vec::new(),
            aabb_min: Vec3::ZERO,
            aabb_max: Vec3::ZERO,
        }
    }

    fn new_split(&self) -> Self {
        Self {
            side: self.side,
            plane: self.plane,
            vertice_indices: Vec::new(),
            vertice_uvs: Vec::new(),
            material_index: self.material_index,
            vertice_alphas: Vec::new(),
            aabb_min: Vec3::ZERO,
            aabb_max: Vec3::ZERO,
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

    pub fn iter_vertices<'b>(
        &'b self,
        solid: &'b SolidBuilder,
        space: impl Space + 'b,
    ) -> impl Iterator<Item = Vec3> + ExactSizeIterator + Clone + 'b {
        self.vertice_indices
            .iter()
            .map(move |&i| space.transform_into(solid, solid.vertices[i]))
    }

    pub fn plane(&self, solid: &SolidBuilder, space: impl Space) -> NdPlane {
        self.plane.in_space(solid, space)
    }

    pub fn plane_normal(&self) -> Vec3 {
        self.plane.normal
    }

    fn sort_vertices(&mut self, vertices: &[Vec3]) {
        sort_polygon(&mut self.vertice_indices, self.plane.normal, |&i| {
            vertices[i]
        });
    }

    fn build_uvs(
        &mut self,
        center: Vec3,
        vertices: &[Vec3],
        materials: &mut Vec<SolidMaterial>,
        get_material_info: &mut impl FnMut(&PathBuf) -> Option<MaterialInfo>,
    ) {
        let mut material_path = GamePathBuf::from("materials");
        material_path.push(&self.side.material);

        let (material_index, material) = if let Some(r) = materials
            .iter()
            .enumerate()
            .find(|(_, p)| p.name == material_path)
        {
            r
        } else {
            let index = materials.len();
            let material_path = PathBuf::from(material_path);
            let info = get_material_info(&material_path).unwrap_or_default();
            let material = SolidMaterial {
                name: if let PathBuf::Game(path) = material_path {
                    path
                } else {
                    unreachable!()
                },
                info,
            };
            materials.push(material);
            (index, materials.last().unwrap())
        };

        self.material_index = material_index;

        let texture_width = material.info.width() as f32;
        let texture_height = material.info.height() as f32;
        self.vertice_uvs.reserve_exact(self.vertice_indices.len());
        let u_axis = self.side.u_axis;
        let v_axis = self.side.v_axis;
        for &vi in &self.vertice_indices {
            let u = (vertices[vi] + center).dot(u_axis.axis)
                / (texture_width * u_axis.scale as f32)
                + u_axis.translation as f32 / texture_width;
            let v = (vertices[vi] + center).dot(v_axis.axis)
                / (texture_height * v_axis.scale as f32)
                + v_axis.translation as f32 / texture_height;
            self.vertice_uvs.push(Vec2::new(u, v));
        }

        // normalize
        let mut nearest_u = f32::MAX;
        let mut nearest_v = f32::MAX;

        for uv in &self.vertice_uvs {
            if uv.x.abs() <= 1.0 {
                nearest_u = 0.0;
                break;
            }
            if uv.x.abs() < nearest_u.abs() {
                nearest_u = uv.x;
            }
        }

        for uv in &self.vertice_uvs {
            if uv.y.abs() <= 1.0 {
                nearest_v = 0.0;
                break;
            }
            if uv.y.abs() < nearest_v.abs() {
                nearest_v = uv.y;
            }
        }

        nearest_u = if nearest_u > 0.0 {
            nearest_u.floor()
        } else {
            nearest_u.ceil()
        };

        nearest_v = if nearest_v > 0.0 {
            nearest_v.floor()
        } else {
            nearest_v.ceil()
        };

        for uv in &mut self.vertice_uvs {
            uv.x -= nearest_u;
            uv.y -= nearest_v;
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
        center: Vec3,
        old_vertices: &[Vec3],
        vertices: &mut Vec<Vec3>,
        faces: &mut Vec<FaceBuilder<'a>>,
    ) -> Result<(), SolidError> {
        if let Some(info) = &self.side.disp_info {
            self.verify_displacement()?;

            // find out which corner vertice the displacement start position is
            let start_position = info.start_position - center;

            let (start_i, _) = self
                .vertice_indices
                .iter()
                .enumerate()
                .min_by(|(_, &vertice_i_a), (_, &vertice_i_b)| {
                    let vertice_a = old_vertices[vertice_i_a];
                    let vertice_b = old_vertices[vertice_i_b];

                    vertice_a
                        .distance(start_position)
                        .total_cmp(&vertice_b.distance(start_position))
                })
                .expect("vertice_indices shouldn't be empty");

            let top_left_i = start_i;
            let top_right_i = (start_i + 1) % 4;
            let btm_right_i = (start_i + 2) % 4;
            let btm_left_i = (start_i + 3) % 4;

            let top_left_vert = old_vertices[self.vertice_indices[top_left_i]];
            let top_right_vert = old_vertices[self.vertice_indices[top_right_i]];
            let btm_right_vert = old_vertices[self.vertice_indices[btm_right_i]];
            let btm_left_vert = old_vertices[self.vertice_indices[btm_left_i]];

            let dimension = info.dimension();
            let last_i = dimension - 1;

            let mut disp_vertice_is = Array2::zeros((dimension, dimension));

            for (row_i, mut row) in disp_vertice_is.rows_mut().into_iter().enumerate() {
                let blend = row_i as f32 / last_i as f32;

                let vert_left = top_left_vert.lerp(btm_left_vert, blend);
                let vert_left_i = vertices.len();
                vertices.push(vert_left);

                let vert_right = top_right_vert.lerp(btm_right_vert, blend);
                let vert_right_i = vertices.len();
                vertices.push(vert_right);

                for (col_i, vert_i) in row.indexed_iter_mut() {
                    *vert_i = if col_i == 0 {
                        vert_left_i
                    } else if col_i == last_i {
                        vert_right_i
                    } else {
                        let i = vertices.len();
                        vertices.push(vert_left.lerp(vert_right, col_i as f32 / last_i as f32));
                        i
                    };
                }
            }

            Zip::from(&disp_vertice_is)
                .and(&info.offsets.data)
                .and(&info.normals.data)
                .and(&info.distances.data)
                .for_each(|&v_i, &offset, &normal, &distance| {
                    vertices[v_i] += offset
                        + distance as f32 * normal
                        + self.plane.normal * info.elevation as f32;
                });

            let mut disp_uvs = Array2::default((dimension, dimension));

            for (row_i, mut row) in disp_uvs.rows_mut().into_iter().enumerate() {
                let blend = row_i as f32 / last_i as f32;

                let uv_left =
                    self.vertice_uvs[top_left_i].lerp(self.vertice_uvs[btm_left_i], blend);
                let uv_right =
                    self.vertice_uvs[top_right_i].lerp(self.vertice_uvs[btm_right_i], blend);

                for (col_i, vert_uv) in row.indexed_iter_mut() {
                    *vert_uv = uv_left.lerp(uv_right, col_i as f32 / last_i as f32);
                }
            }

            let mut disp_faces = Array3::<FaceBuilder>::from_shape_simple_fn(
                (dimension - 1, dimension - 1, 2),
                || FaceBuilder {
                    side: self.side,
                    plane: self.plane,
                    vertice_indices: Vec::with_capacity(3),
                    vertice_uvs: Vec::with_capacity(3),
                    material_index: self.material_index,
                    vertice_alphas: Vec::new(),
                    aabb_min: Vec3::ZERO,
                    aabb_max: Vec3::ZERO,
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

                face.vertice_alphas = face_vert_is
                    .iter()
                    .map(|&i| info.alphas.data[i] as f32)
                    .collect();
            }

            faces.append(&mut disp_faces.into_raw_vec());
        }
        Ok(())
    }

    fn create_default_alphas(&mut self) {
        self.vertice_alphas = vec![0.0; self.vertice_indices.len()];
    }

    #[allow(clippy::too_many_arguments)]
    fn clip_to_sides(
        self,
        self_center: Vec3,
        clipped_sides: &mut Vec<Self>,
        vertices: &mut Vec<Vec3>,
        sides: &[FaceBuilder],
        sides_center: Vec3,
        clip_on_plane: bool,
        epsilon: f32,
    ) -> bool {
        // solids are convex, so if self is in front of any side it's outside the solid and doesn't need clipping
        if sides.iter().any(|side| {
            side.plane.classify_polygon(
                self.vertice_indices
                    .iter()
                    .map(|&i| vertices[i] + self_center - sides_center),
                epsilon,
            ) == PolygonClassification::Front
        }) {
            clipped_sides.push(self);
            return false;
        }

        let Some((side, remaining_sides)) = sides.split_first() else {
            clipped_sides.push(self);
            return false;
        };

        match side.plane.classify_polygon(
            self.vertice_indices
                .iter()
                .map(|&i| vertices[i] + self_center - sides_center),
            epsilon,
        ) {
            PolygonClassification::Front => unreachable!("checked earlier"),
            PolygonClassification::Back => {
                if remaining_sides.is_empty() {
                    return true;
                }
                self.clip_to_sides(
                    self_center,
                    clipped_sides,
                    vertices,
                    remaining_sides,
                    sides_center,
                    clip_on_plane,
                    epsilon,
                )
            }
            PolygonClassification::OnPlane => {
                if !clip_on_plane
                    && relative_eq!(self.plane.normal, side.plane.normal, epsilon = epsilon)
                {
                    clipped_sides.push(self);
                    return false;
                }

                if remaining_sides.is_empty() {
                    return true;
                }

                self.clip_to_sides(
                    self_center,
                    clipped_sides,
                    vertices,
                    remaining_sides,
                    sides_center,
                    clip_on_plane,
                    epsilon,
                )
            }
            PolygonClassification::Spanning => {
                let (front, back) =
                    self.split(self_center, vertices, &side.plane, sides_center, epsilon);

                if front.vertice_indices.len() > 2 {
                    clipped_sides.push(front);
                }

                if remaining_sides.is_empty() {
                    return true;
                }

                let back_clipped = back.clone().clip_to_sides(
                    self_center,
                    clipped_sides,
                    vertices,
                    remaining_sides,
                    sides_center,
                    clip_on_plane,
                    epsilon,
                );

                if back_clipped {
                    true
                } else {
                    *clipped_sides
                        .last_mut()
                        .expect("clip_to_sides should push to clipped_sides if it returns false") =
                        self;
                    false
                }
            }
        }
    }

    fn split(
        &self,
        self_center: Vec3,
        vertices: &mut Vec<Vec3>,
        plane: &NdPlane,
        plane_center: Vec3,
        epsilon: f32,
    ) -> (Self, Self) {
        let vertice_positions = self
            .vertice_indices
            .iter()
            .map(|&i| plane.classify_point(vertices[i] + self_center - plane_center, epsilon))
            .collect_vec();

        let mut front = self.new_split();
        let mut back = self.new_split();

        // iterate pairs of vertices that form edges
        for ((&i, &uv, pos), (&i_next, &uv_next, pos_next)) in
            izip!(&self.vertice_indices, &self.vertice_uvs, vertice_positions)
                .circular_tuple_windows()
        {
            let vert = vertices[i] + self_center - plane_center;
            let vert_next = vertices[i_next] + self_center - plane_center;

            match pos {
                PointClassification::Front => {
                    front.vertice_indices.push(i);
                    front.vertice_uvs.push(uv);
                }
                PointClassification::Back => {
                    back.vertice_indices.push(i);
                    back.vertice_uvs.push(uv);
                }
                PointClassification::OnPlane => {
                    front.vertice_indices.push(i);
                    front.vertice_uvs.push(uv);
                    back.vertice_indices.push(i);
                    back.vertice_uvs.push(uv);
                }
            }

            if (pos == PointClassification::OnPlane) ^ (pos_next == PointClassification::OnPlane) {
                // only one of the points is on the split plane
                // a new vertex doesn't need to be created
                continue;
            }
            if pos == pos_next {
                // if the edge's vertices have the same position, a new vertex isn't needed either
                continue;
            }

            // otherwise split the edge, creating a new vertex
            let (new_vert, factor) =
                if let Some(v) = plane.intersect_line_with_factor(vert, vert_next, epsilon) {
                    v
                } else {
                    continue;
                };

            let new_vert = new_vert + plane_center - self_center;
            // check if the new vertex already exists
            let new_i = vertices
                .iter()
                .position(|v| relative_eq!(*v, &new_vert, epsilon = epsilon))
                .unwrap_or_else(|| {
                    // if not, create it
                    vertices.push(new_vert);
                    vertices.len() - 1
                });
            front.vertice_indices.push(new_i);
            back.vertice_indices.push(new_i);

            // calculate uvs
            let new_uv = uv.lerp(uv_next, factor);
            front.vertice_uvs.push(new_uv);
            back.vertice_uvs.push(new_uv);
        }

        (front, back)
    }

    /// The more oriented this face is towards a point, the larger a number is returned.
    /// Point should be in the same space as the vertices.
    fn angular_closeness_to_point(&self, vertices: &[Vec3], point: Vec3) -> f32 {
        let face_vector = self.plane.normal;
        let face_center = polygon_center(self.vertice_indices.iter().map(|&i| vertices[i]));

        let ref_vector = (point - face_center).normalize();

        // bigger dot product = smaller angle between vectors
        ref_vector.dot(face_vector)
    }

    /// Calculates the AABB. Vertices should be in solid's local space.
    fn calculate_aabb(&mut self, vertices: &[Vec3]) {
        let mut min = Vec3::ZERO;
        let mut max = Vec3::ZERO;

        for &i in &self.vertice_indices {
            let vertice = vertices[i];

            min = min.min(vertice);
            max = max.max(vertice);
        }

        self.aabb_min = min;
        self.aabb_max = max;
    }

    /// Returns the AABB in global space.
    /// The AABB must be calculated beforehand.
    pub fn global_aabb(&self, solid: &SolidBuilder) -> (Vec3, Vec3) {
        (self.aabb_min + solid.center, self.aabb_max + solid.center)
    }

    /// Returns whether self collides with another face.
    pub fn collides_with(
        &self,
        self_solid: &SolidBuilder,
        other: &Self,
        other_solid: &SolidBuilder,
        epsilon: f32,
    ) -> bool {
        if self.vertice_indices.len() < 2 || other.vertice_indices.len() < 2 {
            return false;
        }

        let self_vertices = self.iter_vertices(self_solid, CompatibleSpace(other_solid));
        let other_vertices = other.iter_vertices(other_solid, CompatibleSpace(self_solid));

        // Not colliding if all points of the other face are in front or back of self,
        // or the other way around.
        if let PolygonClassification::Front | PolygonClassification::Back = self
            .plane(self_solid, CompatibleSpace(other_solid))
            .classify_polygon(other_vertices.clone(), epsilon)
        {
            return false;
        }

        if let PolygonClassification::Front | PolygonClassification::Back = other
            .plane(other_solid, CompatibleSpace(self_solid))
            .classify_polygon(self_vertices.clone(), epsilon)
        {
            return false;
        }

        // Otherwise, check all edge normals for SAT collision detection

        let self_normals = self_vertices
            .clone()
            .circular_tuple_windows()
            .map(|(a, b)| (b - a).cross(self.plane.normal));

        let other_normals = other_vertices
            .clone()
            .circular_tuple_windows()
            .map(|(a, b)| (b - a).cross(other.plane.normal));

        for edge_normal in self_normals.chain(other_normals) {
            if relative_eq!(edge_normal, Vec3::ZERO, epsilon = epsilon) {
                continue;
            }

            let (self_min, self_max) = self_vertices
                .clone()
                .map(|v| v.dot(edge_normal))
                .minmax_by(f32::total_cmp)
                .into_option()
                .expect("should have vertices, checked above");

            let (other_min, other_max) = other_vertices
                .clone()
                .map(|v| v.dot(edge_normal))
                .minmax_by(f32::total_cmp)
                .into_option()
                .expect("should have vertices, checked above");

            // if ranges don't overlap, separating axis found => no collision
            if self_min.max(other_min) > self_max.min(other_max) + epsilon {
                return false;
            }
        }

        // No separating axis found => must collide
        true
    }

    /// Returns whether the point is inside self, assuming that it is on the same plane as self.
    /// The space must be same as the space the point is in.
    fn point_is_inside(
        &self,
        solid: &SolidBuilder,
        space: impl Space,
        point: Vec3,
        epsilon: f32,
    ) -> bool {
        point_is_inside_polygon(
            point,
            self.iter_vertices(solid, space).circular_tuple_windows(),
            self.plane.normal,
            epsilon,
        )
    }

    /// Returns whether a point collides with self.
    /// The space must be same as the space the point is in.
    pub fn collides_with_point(
        &self,
        solid: &SolidBuilder,
        space: impl Space,
        point: Vec3,
        epsilon: f32,
    ) -> bool {
        if let PointClassification::OnPlane =
            self.plane(solid, space).classify_point(point, epsilon)
        {
            self.point_is_inside(solid, space, point, epsilon)
        } else {
            false
        }
    }

    /// Returns the intersection of a face and an edge.
    /// The space must be same as the space the edge is in.
    /// Returns the point of intersection, if it exists.
    /// Returns None if the edge and the face do not intersect or intersect along the edge.
    pub fn intersection_with_edge(
        &self,
        solid: &SolidBuilder,
        space: impl Space,
        edge: [Vec3; 2],
        epsilon: f32,
    ) -> Option<Vec3> {
        let (point, factor) = self
            .plane(solid, space)
            .intersect_line_with_factor(edge[0], edge[1], epsilon)?;

        if factor > -epsilon
            && factor < 1.0 + epsilon
            && self.point_is_inside(solid, space, point, epsilon)
        {
            Some(point)
        } else {
            None
        }
    }

    /// Returns the intersection of 2 faces.
    ///
    /// The space combinations must result in the vertices being in the same space in the end.
    /// The returned intersection is in the specified space.
    pub fn intersection(
        &self,
        self_solid: &SolidBuilder,
        self_space: impl Space,
        other: &Self,
        other_solid: &SolidBuilder,
        other_space: impl Space,
        epsilon: f32,
    ) -> PolygonIntersection {
        polygon_intersection(
            self.iter_vertices(self_solid, self_space),
            self.plane(self_solid, self_space),
            other.iter_vertices(other_solid, other_space),
            other.plane(other_solid, other_space),
            epsilon,
        )
    }

    /// Returns the distance between self and a point.
    /// The space must be same as the space the point is in.
    pub fn distance_to_point(&self, solid: &SolidBuilder, space: impl Space, point: Vec3) -> f32 {
        let mut edge_planes = self
            .iter_vertices(solid, space)
            .circular_tuple_windows()
            .map(|(v1, v2)| {
                let edge_vector = v2 - v1;
                let edge_normal = edge_vector.cross(self.plane.normal);

                NdPlane::from_point_normal(v1, edge_normal)
            });

        let is_closest_point_inside_face =
            edge_planes.all(|plane| plane.distance_to_point(point) < 0.0);

        if is_closest_point_inside_face {
            self.plane(solid, space).distance_to_point(point)
        } else {
            self.iter_vertices(solid, space)
                .circular_tuple_windows()
                .map(|(a, b)| distance_point_to_edge(a, b, point))
                .min_by(f32::total_cmp)
                .unwrap_or(f32::INFINITY)
        }
    }

    /// Returns the closest point on self to another point, and the distance between them.
    /// The space must be same as the space the point is in.
    pub fn closest_point_to_point(
        &self,
        solid: &SolidBuilder,
        space: impl Space,
        point: Vec3,
    ) -> (Vec3, f32) {
        let mut edge_planes = self
            .iter_vertices(solid, space)
            .clone()
            .circular_tuple_windows()
            .map(|(v1, v2)| {
                let edge_vector = v2 - v1;
                let edge_normal = edge_vector.cross(self.plane.normal);

                NdPlane::from_point_normal(v1, edge_normal)
            });

        let is_closest_point_inside_face =
            edge_planes.all(|plane| plane.distance_to_point(point) < 0.0);

        if is_closest_point_inside_face {
            let self_plane = self.plane(solid, space);
            let distance = self_plane.distance_to_point(point);
            let point_on_plane = point - distance * self_plane.normal;

            (point_on_plane, distance)
        } else {
            self.iter_vertices(solid, space)
                .circular_tuple_windows()
                .map(|(a, b)| closest_point_on_edge_to_point(a, b, point))
                .min_by(|(_, a_dist), (_, b_dist)| f32::total_cmp(a_dist, b_dist))
                .unwrap_or((Vec3::NAN, f32::INFINITY))
        }
    }

    fn finish(self) -> SolidFace {
        SolidFace {
            vertice_indices: self.vertice_indices,
            vertice_uvs: self.vertice_uvs,
            material_index: self.material_index,
            vertice_alphas: self.vertice_alphas,
        }
    }
}

#[derive(Clone, PartialEq)]
pub(crate) struct SolidBuilder<'a> {
    solid: &'a Solid,
    center: Vec3,
    faces: Vec<FaceBuilder<'a>>,
    vertices: Vec<Vec3>,
    materials: Vec<SolidMaterial>,
    is_displacement: bool,
    aabb_min: Vec3,
    aabb_max: Vec3,
}

impl<'a> Debug for SolidBuilder<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SolidBuilder")
            .field("center", &self.center)
            .field("sides", &self.faces)
            .field("vertices", &self.vertices)
            .field("materials", &self.materials)
            .field("is_displacement", &self.is_displacement)
            .finish_non_exhaustive()
    }
}

impl<'a> SolidBuilder<'a> {
    fn new(solid: &'a Solid) -> Self {
        let solid_center = polygon_center(
            solid
                .sides
                .iter()
                .map(|side| (side.plane.0 + side.plane.1 + side.plane.2) / 3.0),
        );

        Self {
            solid,
            center: solid_center,
            faces: solid
                .sides
                .iter()
                .map(|side| FaceBuilder::new(side, solid_center))
                .collect(),
            vertices: Vec::new(),
            materials: Vec::new(),
            is_displacement: false,
            aabb_min: Vec3::new(0.0, 0.0, 0.0),
            aabb_max: Vec3::new(0.0, 0.0, 0.0),
        }
    }

    fn intersect_sides(&mut self, epsilon: f32, cut_threshold: f32) {
        for (i1, i2, i3) in (0..self.faces.len()).tuple_combinations() {
            if let Some(point) = NdPlane::intersect(
                &self.faces[i1].plane,
                &self.faces[i2].plane,
                &self.faces[i3].plane,
                epsilon,
            ) {
                // check if vertice is outside the brush
                if self
                    .faces
                    .iter()
                    .enumerate()
                    .filter(|&(i, _)| i != i1 && i != i2 && i != i3)
                    .any(|(_, builder)| builder.plane.distance_to_point(point) > cut_threshold)
                {
                    continue;
                }
                // check if vertice already exists
                let vertice_i = self
                    .vertices
                    .iter()
                    .position(|v| relative_eq!(*v, &point, epsilon = epsilon))
                    .unwrap_or_else(|| {
                        // if not, create it
                        self.vertices.push(point);
                        self.vertices.len() - 1
                    });
                // add vertice index to sides
                self.faces[i1].insert_vertice(vertice_i);
                self.faces[i2].insert_vertice(vertice_i);
                self.faces[i3].insert_vertice(vertice_i);
            }
        }
    }

    fn remove_invalid_faces(&mut self) {
        self.faces
            .retain(|builder| builder.vertice_indices.len() >= 3);
    }

    fn sort_vertices(&mut self) {
        let vertices = &mut self.vertices;
        for builder in &mut self.faces {
            builder.sort_vertices(vertices);
        }
    }

    fn build_uvs(&mut self, mut get_material_info: impl FnMut(&PathBuf) -> Option<MaterialInfo>) {
        let vertices = &self.vertices;
        for builder in &mut self.faces {
            builder.build_uvs(
                self.center,
                vertices,
                &mut self.materials,
                &mut get_material_info,
            );
        }
    }

    fn maybe_build_displacement(&mut self) -> Result<(), SolidError> {
        // calculate amount of new vertices
        let Some(vertices_len) = self
            .faces
            .iter()
            .filter_map(FaceBuilder::disp_vertices_len)
            .sum1()
        else {
            return Ok(()); // this is not a displacement
        };

        let old_vertices = &self.vertices;
        let mut vertices: Vec<Vec3> = Vec::with_capacity(vertices_len);

        let faces_len = self
            .faces
            .iter()
            .filter_map(FaceBuilder::disp_faces_len)
            .sum();
        let mut faces: Vec<FaceBuilder> = Vec::with_capacity(faces_len);

        for builder in &mut self.faces {
            builder.maybe_build_displacement(
                self.center,
                old_vertices,
                &mut vertices,
                &mut faces,
            )?;
        }

        self.vertices = vertices;
        self.faces = faces;

        self.is_displacement = true;

        Ok(())
    }

    fn create_default_alphas(&mut self) {
        for builder in &mut self.faces {
            builder.create_default_alphas();
        }
    }

    fn extend_side_faces_map(&self, map: &Mutex<SideFacesMap>) {
        let mut map = map.lock().expect("mutex shouldn't be poisoned");
        for builder in &self.faces {
            map.entry(builder.side.id).or_default().push(
                builder
                    .vertice_indices
                    .iter()
                    .map(|&i| self.center + self.vertices[i])
                    .collect(),
            );
        }
    }

    fn build(
        &mut self,
        get_material_info: impl FnMut(&PathBuf) -> Option<MaterialInfo>,
        side_faces_map: &Mutex<SideFacesMap>,
        settings: &GeometrySettings,
    ) -> Result<(), SolidError> {
        self.intersect_sides(settings.epsilon, settings.cut_threshold);
        self.remove_invalid_faces();
        self.sort_vertices();
        self.build_uvs(get_material_info);
        self.maybe_build_displacement()?;
        self.extend_side_faces_map(side_faces_map);

        if !self.is_displacement {
            self.create_default_alphas();
        }

        Ok(())
    }

    fn calculate_aabb(&mut self) {
        let mut min = Vec3::ZERO;
        let mut max = Vec3::ZERO;

        for &vertice in &self.vertices {
            min = min.min(vertice);
            max = max.max(vertice);
        }

        self.aabb_min = min;
        self.aabb_max = max;

        for face in &mut self.faces {
            face.calculate_aabb(&self.vertices);
        }
    }

    fn aabb_intersects(&self, other: &Self) -> bool {
        self.aabb_min.x < other.aabb_max.x
            && other.aabb_min.x < self.aabb_max.x
            && self.aabb_min.y < other.aabb_max.y
            && other.aabb_min.y < self.aabb_max.y
            && self.aabb_min.z < other.aabb_max.z
            && other.aabb_min.z < self.aabb_max.z
    }

    fn clip_to_solid(&mut self, solid: &Self, clip_on_plane: bool, epsilon: f32) {
        let faces = mem::take(&mut self.faces);

        for builder in faces {
            builder.clip_to_sides(
                self.center,
                &mut self.faces,
                &mut self.vertices,
                &solid.faces,
                solid.center,
                clip_on_plane,
                epsilon,
            );
        }

        // remove unused vertices
        let mut vertices_to_retain = vec![false; self.vertices.len()];

        for builder in &self.faces {
            for &vertice_index in &builder.vertice_indices {
                vertices_to_retain[vertice_index] = true;
            }
        }

        let mut retain_iter = vertices_to_retain.iter();
        self.vertices.retain(|_| *retain_iter.next().unwrap());

        let mut current_correction = 0_usize;
        let vertice_index_correction = vertices_to_retain
            .iter()
            .map(|retain| {
                if !retain {
                    current_correction += 1;
                }
                current_correction
            })
            .collect_vec();

        for builder in &mut self.faces {
            for vertice_index in &mut builder.vertice_indices {
                *vertice_index -= vertice_index_correction[*vertice_index];
            }
        }
    }

    fn recenter(&mut self) {
        let center = polygon_center(self.vertices.iter().copied());
        for vertice in &mut self.vertices {
            *vertice -= center;
        }
        self.center += center;
    }

    fn is_nodraw(&self) -> bool {
        self.materials.iter().all(|mat| mat.info.no_draw())
    }

    /// Returns the AABB in global space.
    pub fn global_aabb(&self) -> (Vec3, Vec3) {
        (self.aabb_min + self.center, self.aabb_max + self.center)
    }

    /// Get the closest face to a point, if there are faces. Point should be in global space.
    pub fn closest_face_to_point(&self, mut point: Vec3) -> Option<&FaceBuilder> {
        point -= self.center;

        self.faces.iter().max_by(|&a, &b| {
            a.angular_closeness_to_point(&self.vertices, point)
                .total_cmp(&b.angular_closeness_to_point(&self.vertices, point))
        })
    }

    /// Returns the shortest distance from self to a point in global space.
    pub fn distance_to_point(&self, mut point: Vec3) -> f32 {
        let closest_face = if let Some(f) = self.closest_face_to_point(point) {
            f
        } else {
            return f32::INFINITY;
        };

        point -= self.center;

        closest_face.distance_to_point(self, LocalSpace, point)
    }

    /// Returns whether a point in global space is inside the solid.
    pub fn point_is_inside(&self, mut point: Vec3) -> bool {
        point -= self.center;

        self.faces
            .iter()
            .all(|f| f.plane.distance_to_point(point) <= 0.0)
    }

    pub fn is_displacement(&self) -> bool {
        self.is_displacement
    }

    /// Returns the vertices in local space.
    pub fn vertices(&self) -> &[Vec3] {
        &self.vertices
    }

    pub fn faces(&self) -> &[FaceBuilder<'a>] {
        &self.faces
    }

    /// Returns the solid's position in global space.
    pub fn position(&self) -> Vec3 {
        self.center
    }

    fn finish(self, scale: f32) -> BuiltSolid {
        BuiltSolid {
            id: self.solid.id,
            position: self.center * scale,
            scale,
            vertices: self.vertices,
            faces: self.faces.into_iter().map(FaceBuilder::finish).collect(),
            materials: self.materials,
        }
    }
}

impl Solid {
    /// # Errors
    ///
    /// Returns `Err` if the mesh creation fails.
    pub fn build_mesh(
        &self,
        get_material_info: impl FnMut(&PathBuf) -> Option<MaterialInfo>,
        side_faces_map: &Mutex<SideFacesMap>,
        settings: &GeometrySettings,
        scale: f32,
    ) -> Result<BuiltSolid, SolidError> {
        let mut builder = SolidBuilder::new(self);
        builder.build(get_material_info, side_faces_map, settings)?;
        builder.recenter();
        Ok(builder.finish(scale))
    }
}

#[derive(Debug)]
pub struct MergedSolids {
    pub vertices: Vec<Vec3>,
    pub faces: Vec<SolidFace>,
    pub materials: Vec<SolidMaterial>,
    pub scale: f32,
}

impl MergedSolids {
    fn merge(
        mut solids: Vec<SolidBuilder>,
        epsilon: f32,
        optimize: bool,
        scale: f32,
    ) -> Option<Self> {
        let merge_solids: Vec<SolidBuilder>;

        if optimize {
            // calculate aabbs
            for solid in &mut solids {
                solid.calculate_aabb();
            }

            let mut clipped_solids = solids.clone();

            // clip solids
            for (i, clip_solid) in clipped_solids.iter_mut().enumerate() {
                if clip_solid.is_displacement {
                    continue;
                }

                let mut clip_on_plane = false;

                for (j, solid) in solids.iter().enumerate() {
                    // make sure solids are not clipped against themselves
                    if i == j {
                        clip_on_plane = true;
                    } else if clip_solid.aabb_intersects(solid) {
                        clip_solid.clip_to_solid(solid, clip_on_plane, epsilon);
                    }
                }
            }

            merge_solids = clipped_solids;
        } else {
            merge_solids = solids;
        }

        if merge_solids.is_empty() {
            return None;
        }

        // merge solids
        let mut vertices = Vec::new();
        let mut materials = Vec::new();

        let mut faces = Vec::with_capacity(merge_solids.iter().map(|s| s.faces.len()).sum());

        let mut displacements = Vec::new();

        for solid in merge_solids {
            if solid.is_displacement {
                displacements.push(solid);
                continue;
            }

            let center = solid.center;

            let vertice_i_map = solid
                .vertices
                .into_iter()
                .map(|vertice| {
                    let vertice = vertice + center;
                    vertices
                        .iter()
                        .position(|v| relative_eq!(v, &vertice, epsilon = epsilon))
                        .unwrap_or_else(|| {
                            // add the vertice if it doesn't exist already
                            vertices.push(vertice);
                            vertices.len() - 1
                        })
                })
                .collect_vec();

            let material_i_map = solid
                .materials
                .into_iter()
                .map(|mat| {
                    materials.iter().position(|m| m == &mat).unwrap_or_else(|| {
                        // add the material if it doesn't exist already
                        materials.push(mat);
                        materials.len() - 1
                    })
                })
                .collect_vec();

            for mut face in solid.faces {
                // fix indices
                face.material_index = material_i_map[face.material_index];

                for vertice_index in &mut face.vertice_indices {
                    *vertice_index = vertice_i_map[*vertice_index];
                }

                faces.push(face.finish());
            }
        }

        // merge displacement last without checking for existing vertices
        for solid in displacements {
            let center = solid.center;
            let vertice_i_offset = vertices.len();

            vertices.extend(solid.vertices.into_iter().map(|v| v + center));

            let material_i_map = solid
                .materials
                .into_iter()
                .map(|mat| {
                    materials.iter().position(|m| m == &mat).unwrap_or_else(|| {
                        // add the material if it doesn't exist already
                        materials.push(mat);
                        materials.len() - 1
                    })
                })
                .collect_vec();

            for mut face in solid.faces {
                // fix indices
                face.material_index = material_i_map[face.material_index];

                for vertice_index in &mut face.vertice_indices {
                    *vertice_index += vertice_i_offset;
                }

                faces.push(face.finish());
            }
        }

        Some(Self {
            vertices,
            faces,
            materials,
            scale,
        })
    }
}

#[derive(Debug)]
pub struct BuiltBrushEntity<'a> {
    pub id: i32,
    pub class_name: &'a str,
    pub merged_solids: Option<MergedSolids>,
    pub solids: Vec<BuiltSolid>,
}

impl<'a> BuiltBrushEntity<'a> {
    fn new(
        solids: &[Solid],
        id: i32,
        class_name: &'a str,
        mut get_material_info: impl FnMut(&PathBuf) -> Option<MaterialInfo>,
        side_faces_map: &Mutex<SideFacesMap>,
        settings: &GeometrySettings,
        scale: f32,
        sky_camera: Option<SkyCamera>,
    ) -> Self {
        let mut solid_builders = Vec::with_capacity(solids.len());

        for solid in solids {
            let mut builder = SolidBuilder::new(solid);

            if let Err(err) = builder.build(&mut get_material_info, side_faces_map, settings) {
                warn!("brush `{}`: {}", id, err);
                continue;
            }

            solid_builders.push(builder);
        }

        if let Some(sky_camera) = sky_camera {
            for solid in &mut solid_builders {
                solid.calculate_aabb();
            }

            sky_detector::detect(&solid_builders, sky_camera);
        }

        if !settings.invisible_solids.import() {
            solid_builders.retain(|s| !s.is_nodraw());
        }

        if settings.merge_solids.merge() {
            BuiltBrushEntity {
                id,
                class_name,
                merged_solids: MergedSolids::merge(
                    solid_builders,
                    settings.epsilon,
                    settings.merge_solids.optimize(),
                    scale,
                ),
                solids: Vec::new(),
            }
        } else {
            BuiltBrushEntity {
                id,
                class_name,
                merged_solids: None,
                solids: solid_builders
                    .into_iter()
                    .map(|s| s.finish(scale))
                    .collect(),
            }
        }
    }
}

impl Entity {
    /// # Errors
    ///
    /// Returns `Err` if the building fails.
    pub fn build_brush(
        &self,
        get_material_info: impl FnMut(&PathBuf) -> Option<MaterialInfo>,
        side_faces_map: &Mutex<SideFacesMap>,
        settings: &GeometrySettings,
        scale: f32,
    ) -> BuiltBrushEntity {
        BuiltBrushEntity::new(
            &self.solids,
            self.id,
            &self.class_name,
            get_material_info,
            side_faces_map,
            settings,
            scale,
            None,
        )
    }
}

impl World {
    /// # Errors
    ///
    /// Returns `Err` if the building fails.
    pub fn build_brush(
        &self,
        get_material_info: impl FnMut(&PathBuf) -> Option<MaterialInfo>,
        side_faces_map: &Mutex<SideFacesMap>,
        settings: &GeometrySettings,
        scale: f32,
    ) -> BuiltBrushEntity {
        BuiltBrushEntity::new(
            &self.solids,
            self.id,
            &self.class_name,
            get_material_info,
            side_faces_map,
            settings,
            scale,
            None,
        )
    }
}
