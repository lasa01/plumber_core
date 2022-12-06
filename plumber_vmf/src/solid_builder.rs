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

use super::{
    builder_utils::{
        polygon_center, polygon_normal, GeometrySettings, NdPlane, PointClassification,
        PolygonClassification,
    },
    overlay_builder::SideFacesMap,
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
struct FaceBuilder<'a> {
    side: &'a Side,
    plane: NdPlane,
    vertice_indices: Vec<usize>,
    vertice_uvs: Vec<Vec2>,
    material_index: usize,
    vertice_alphas: Vec<f32>,
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

    fn sort_vertices(&mut self, vertices: &[Vec3]) {
        let center = polygon_center(self.vertice_indices.iter().map(|&i| vertices[i]));

        for i in 0..self.vertice_indices.len() - 2 {
            let vertice = vertices[self.vertice_indices[i]];
            let to_current = (vertice - center).normalize();
            let filter_plane = NdPlane::from_points(vertice, center, center + self.plane.normal);

            if let Some((next_idx, _)) = self.vertice_indices[i + 1..]
                .iter()
                .enumerate()
                .filter(|(_, &vi)| filter_plane.distance_to_point(vertices[vi]) >= 0.0)
                .map(|(i, &vi)| {
                    let to_candidate = (vertices[vi] - center).normalize();
                    let dot = to_current.dot(to_candidate);
                    (i, dot)
                })
                // max because smaller angle -> bigger dot product
                .max_by(|(_, a), (_, b)| a.total_cmp(b))
            {
                self.vertice_indices.swap(i + 1, i + 1 + next_idx);
            }
        }

        // reverse if the normal is facing the wrong way
        if polygon_normal(self.vertice_indices.iter().map(|i| vertices[*i])).dot(self.plane.normal)
            < 0.0
        {
            self.vertice_indices.reverse();
        }
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
                + u_axis.translation as f32 / texture_width as f32;
            let v = (vertices[vi] + center).dot(v_axis.axis)
                / (texture_height * v_axis.scale as f32)
                + v_axis.translation as f32 / texture_height as f32;
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
struct SolidBuilder<'a> {
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
        let mut min = self
            .vertices
            .first()
            .copied()
            .unwrap_or_else(|| Vec3::new(0.0, 0.0, 0.0));
        let mut max = min;

        for &vertice in &self.vertices {
            min = min.min(vertice);
            max = max.max(vertice);
        }

        self.aabb_min = min;
        self.aabb_max = max;
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
    ) -> Self {
        if settings.merge_solids.merge() {
            let mut mergable_solids = Vec::new();

            for solid in solids {
                let mut builder = SolidBuilder::new(solid);
                if let Err(err) = builder.build(&mut get_material_info, side_faces_map, settings) {
                    warn!("brush `{}`: {}", id, err);
                    continue;
                }

                if !settings.invisible_solids.import() && builder.is_nodraw() {
                    continue;
                }

                mergable_solids.push(builder);
            }

            BuiltBrushEntity {
                id,
                class_name,
                merged_solids: MergedSolids::merge(
                    mergable_solids,
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
                solids: solids
                    .iter()
                    .filter_map(|solid| {
                        let mut builder = SolidBuilder::new(solid);
                        if let Err(err) =
                            builder.build(&mut get_material_info, side_faces_map, settings)
                        {
                            warn!("brush `{}`: {}", id, err);
                            return None;
                        }
                        if settings.invisible_solids.import() || !builder.is_nodraw() {
                            Some(builder.finish(scale))
                        } else {
                            None
                        }
                    })
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
        )
    }
}
