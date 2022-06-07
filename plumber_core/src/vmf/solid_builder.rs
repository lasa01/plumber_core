use std::{fmt::Debug, mem, ptr, sync::Mutex};

use crate::{
    fs::{GamePathBuf, PathBuf},
    vmf::builder_utils::PointClassification,
    vmt::loader::{MaterialInfo, MaterialLoadError},
};

use super::{
    builder_utils::{
        polygon_center, polygon_normal, GeometrySettings, NdPlane, PolygonClassification,
    },
    overlay_builder::SideFacesMap,
    Entity, Side, Solid, World,
};

use approx::relative_eq;
use float_ord::FloatOrd;
use glam::{Vec2, Vec3};
use itertools::{izip, Itertools};
use log::warn;
use ndarray::{Array2, Array3, Zip};
use thiserror::Error;

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
                .max_by_key(|(_, d)| FloatOrd(*d))
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
        get_material_info: &mut impl FnMut(&PathBuf) -> Result<MaterialInfo, MaterialLoadError>,
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
                .min_by_key(|(_, &vertice_i)| {
                    let vertice = old_vertices[vertice_i];
                    FloatOrd(vertice.distance(start_position))
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

        let (side, remaining_sides) = if let Some(r) = sides.split_first() {
            r
        } else {
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

    fn build_uvs(
        &mut self,
        mut get_material_info: impl FnMut(&PathBuf) -> Result<MaterialInfo, MaterialLoadError>,
    ) {
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
        let vertices_len = match self
            .faces
            .iter()
            .filter_map(FaceBuilder::disp_vertices_len)
            .sum1()
        {
            Some(sum) => sum,
            None => return Ok(()), // this is not a displacement
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
        get_material_info: impl FnMut(&PathBuf) -> Result<MaterialInfo, MaterialLoadError>,
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
        get_material_info: impl FnMut(&PathBuf) -> Result<MaterialInfo, MaterialLoadError>,
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
        mut get_material_info: impl FnMut(&PathBuf) -> Result<MaterialInfo, MaterialLoadError>,
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
        get_material_info: impl FnMut(&PathBuf) -> Result<MaterialInfo, MaterialLoadError>,
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
        get_material_info: impl FnMut(&PathBuf) -> Result<MaterialInfo, MaterialLoadError>,
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

#[cfg(test)]
mod tests {
    use std::ops::Range;

    use plumber_vdf as vdf;

    use super::*;
    use approx::assert_relative_eq;

    fn get_test_solid() -> Solid {
        let input = r#"
            "id" "3"
            side
            {
                "id" "7"
                "plane" "(-960 64 64) (-832 64 64) (-832 -64 64)"
                "material" "DE_TEST/GRID"
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
                "material" "DE_TEST/GRID"
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
                "material" "DE_TEST/GRID"
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
                "material" "DE_TEST/GRID"
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
                "material" "DE_TEST/GRID"
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
                "material" "DE_TEST/GRID"
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

        builder.build_uvs(|_| Ok(MaterialInfo::new(1024, 1024, false)));

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
        let input = r#"
        "id" "9616"
		side
		{
			"id" "9622"
			"plane" "(-331.905 -512 342.744) (-331.905 512 342.744) (512 512 -237.255)"
			"material" "DE_TEST/GRID"
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
			"material" "DE_TEST/GRID"
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
			"material" "DE_TEST/GRID"
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
			"material" "DE_TEST/GRID"
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
			"material" "DE_TEST/GRID"
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
			"material" "DE_TEST/GRID"
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
        builder.intersect_sides(1e-3, 1e-3);
        builder.remove_invalid_faces();
        builder.sort_vertices();
        builder.build_uvs(|_| Ok(MaterialInfo::new(1024, 1024, false)));
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
            FaceBuilder {
                side: &dummy_side,
                plane: NdPlane::from_point_normal(
                    Vec3::new(-1.0, 0.0, 0.0),
                    Vec3::new(-1.0, 0.0, 0.0),
                ),
                vertice_indices: Vec::new(),
                vertice_uvs: Vec::new(),
                material_index: 0,
                vertice_alphas: Vec::new(),
            },
            FaceBuilder {
                side: &dummy_side,
                plane: NdPlane::from_point_normal(
                    Vec3::new(1.0, 0.0, 0.0),
                    Vec3::new(1.0, 0.0, 0.0),
                ),
                vertice_indices: Vec::new(),
                vertice_uvs: Vec::new(),
                material_index: 0,
                vertice_alphas: Vec::new(),
            },
            FaceBuilder {
                side: &dummy_side,
                plane: NdPlane::from_point_normal(
                    Vec3::new(0.0, -2.0, 0.0),
                    Vec3::new(0.0, -1.0, 0.0),
                ),
                vertice_indices: Vec::new(),
                vertice_uvs: Vec::new(),
                material_index: 0,
                vertice_alphas: Vec::new(),
            },
            FaceBuilder {
                side: &dummy_side,
                plane: NdPlane::from_point_normal(
                    Vec3::new(0.0, 2.0, 0.0),
                    Vec3::new(0.0, 1.0, 0.0),
                ),
                vertice_indices: Vec::new(),
                vertice_uvs: Vec::new(),
                material_index: 0,
                vertice_alphas: Vec::new(),
            },
        ];

        let clipped_side = FaceBuilder {
            side: &dummy_side,
            plane: NdPlane::from_point_normal(Vec3::new(2.0, 2.0, 0.0), Vec3::new(0.0, 0.0, 1.0)),
            vertice_indices: vec![0, 1, 2, 3],
            vertice_uvs: vec![Vec2::ZERO; 4],
            material_index: 0,
            vertice_alphas: Vec::new(),
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
                plane: NdPlane::from_point_normal(
                    Vec3::new(2.0, 2.0, 0.0),
                    Vec3::new(0.0, 0.0, 1.0)
                ),
                vertice_indices: vec![0, 1, 2, 6, 7],
                vertice_uvs: vec![Vec2::ZERO; 5],
                material_index: 0,
                vertice_alphas: Vec::new(),
            }]
        );

        let clipped_side = FaceBuilder {
            side: &dummy_side,
            plane: NdPlane::from_point_normal(Vec3::new(-2.0, 0.0, 2.0), Vec3::new(-1.0, 0.0, 0.0)),
            vertice_indices: vec![0, 1, 4, 5],
            vertice_uvs: vec![Vec2::ZERO; 4],
            material_index: 0,
            vertice_alphas: Vec::new(),
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
                FaceBuilder {
                    side: &dummy_side,
                    plane: NdPlane::from_point_normal(
                        Vec3::new(-1.0, 0.0, 0.0),
                        Vec3::new(-1.0, 0.0, 0.0),
                    ),
                    vertice_indices: Vec::new(),
                    vertice_uvs: Vec::new(),
                    material_index: 0,
                    vertice_alphas: Vec::new(),
                },
                FaceBuilder {
                    side: &dummy_side,
                    plane: NdPlane::from_point_normal(
                        Vec3::new(1.0, 0.0, 0.0),
                        Vec3::new(1.0, 0.0, 0.0),
                    ),
                    vertice_indices: Vec::new(),
                    vertice_uvs: Vec::new(),
                    material_index: 0,
                    vertice_alphas: Vec::new(),
                },
                FaceBuilder {
                    side: &dummy_side,
                    plane: NdPlane::from_point_normal(
                        Vec3::new(0.0, -2.0, 0.0),
                        Vec3::new(0.0, -1.0, 0.0),
                    ),
                    vertice_indices: Vec::new(),
                    vertice_uvs: Vec::new(),
                    material_index: 0,
                    vertice_alphas: Vec::new(),
                },
                FaceBuilder {
                    side: &dummy_side,
                    plane: NdPlane::from_point_normal(
                        Vec3::new(0.0, 2.0, 0.0),
                        Vec3::new(0.0, 1.0, 0.0),
                    ),
                    vertice_indices: Vec::new(),
                    vertice_uvs: Vec::new(),
                    material_index: 0,
                    vertice_alphas: Vec::new(),
                },
                FaceBuilder {
                    side: &dummy_side,
                    plane: NdPlane::from_point_normal(
                        Vec3::new(0.0, 0.0, -1.0),
                        Vec3::new(0.0, 0.0, -1.0),
                    ),
                    vertice_indices: Vec::new(),
                    vertice_uvs: Vec::new(),
                    material_index: 0,
                    vertice_alphas: Vec::new(),
                },
                FaceBuilder {
                    side: &dummy_side,
                    plane: NdPlane::from_point_normal(
                        Vec3::new(0.0, 0.0, 1.0),
                        Vec3::new(0.0, 0.0, 1.0),
                    ),
                    vertice_indices: Vec::new(),
                    vertice_uvs: Vec::new(),
                    material_index: 0,
                    vertice_alphas: Vec::new(),
                },
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

        let test_data = r#"
            "id" "13354"
            side
            {
                "id" "13355"
                "plane" "(0 -24 112) (0 -24 104) (0 152 104)"
                "smoothing_groups" "0"
                "material" "tools/toolsnodraw"
                "uaxis" "[0 -1 0 0] 0.25"
                "vaxis" "[0 0 -1 0] 0.25"
                "lightmapscale" "16"
            }
            side
            {
                "id" "13356"
                "plane" "(88 152 112) (88 152 104) (88 -24 104)"
                "smoothing_groups" "0"
                "material" "tools/toolsnodraw"
                "uaxis" "[0 1 -0 0] 0.25"
                "vaxis" "[0 -0 -1 0] 0.25"
                "lightmapscale" "16"
            }
            side
            {
                "id" "13357"
                "plane" "(88 -24 112) (88 -24 104) (0 -24 104)"
                "smoothing_groups" "0"
                "material" "tools/toolsnodraw"
                "uaxis" "[1 0 0 0] 0.25"
                "vaxis" "[0 0 -1 0] 0.25"
                "lightmapscale" "16"
            }
            side
            {
                "id" "13358"
                "plane" "(0 152 112) (0 152 104) (88 152 104)"
                "smoothing_groups" "0"
                "material" "tools/toolsnodraw"
                "uaxis" "[-1 0 0 0] 0.25"
                "vaxis" "[-0 -0 -1 0] 0.25"
                "lightmapscale" "16"
            }
            side
            {
                "id" "13359"
                "plane" "(0 152 104) (0 -24 104) (88 -24 104)"
                "smoothing_groups" "0"
                "material" "tools/toolsnodraw"
                "uaxis" "[-1 0 0 0] 0.25"
                "vaxis" "[0 -1 0 0] 0.25"
                "lightmapscale" "16"
            }
            side
            {
                "id" "13360"
                "plane" "(0 -24 112) (0 152 112) (88 152 112)"
                "smoothing_groups" "0"
                "material" "de_mirage/base/de_mirage_mid_ver1_diffuse"
                "uaxis" "[1 0 0 0] 5"
                "vaxis" "[0 -1 0 0] 5"
                "lightmapscale" "16"
            }
        "#;

        let solid: Solid = vdf::from_str(test_data).unwrap();

        let mut builder = SolidBuilder::new(&solid);

        builder.intersect_sides(1e-3, 1e-3);
        builder.remove_invalid_faces();
        builder.sort_vertices();
        builder.build_uvs(|_| Ok(MaterialInfo::new(1024, 1024, false)));
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
}
