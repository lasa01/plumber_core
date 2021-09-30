use std::{fmt::Debug, mem, ptr, sync::Mutex};

use crate::{
    fs::{Path, PathBuf},
    vmf::builder_utils::PointClassification,
    vmt::loader::{MaterialInfo, MaterialLoadError},
};

use super::{
    builder_utils::{
        lerp_uv, polygon_center, polygon_normal, GeometrySettings, NdPlane, PolygonClassification,
    },
    overlay_builder::SideFacesMap,
    Entity, Side, Solid, World,
};

use approx::relative_eq;
use float_ord::FloatOrd;
use itertools::{izip, Itertools};
use nalgebra::{geometry::Point3, Vector3};
use ndarray::{Array2, Array3, Zip};
use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum SolidError {
    #[error("displacement side `{side_id}` is not valid: got {vertices} vertices, expected 4")]
    InvalidDisplacement { side_id: i32, vertices: usize },
}

#[derive(Debug)]
pub struct BuiltSolid {
    pub id: i32,
    pub position: Point3<f64>,
    pub vertices: Vec<Point3<f64>>,
    pub faces: Vec<Face>,
    pub materials: Vec<SolidMaterial>,
}

#[derive(Debug, Clone)]
pub struct SolidMaterial {
    pub name: PathBuf,
    pub info: MaterialInfo,
}

impl PartialEq for SolidMaterial {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

#[derive(Debug)]
pub struct Face {
    pub vertice_indices: Vec<usize>,
    pub vertice_uvs: Vec<[f64; 2]>,
    pub material_index: usize,
}

#[derive(Clone)]
struct FaceBuilder<'a> {
    side: &'a Side,
    plane: NdPlane,
    vertice_indices: Vec<usize>,
    vertice_uvs: Vec<[f64; 2]>,
    material_index: usize,
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

    fn new_split(&self) -> Self {
        Self {
            side: self.side,
            plane: self.plane,
            vertice_indices: Vec::new(),
            vertice_uvs: Vec::new(),
            material_index: self.material_index,
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
                NdPlane::from_points(vertice, &center, &(center + self.plane.normal.as_ref()));

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
        materials: &mut Vec<SolidMaterial>,
        get_material_info: &mut impl FnMut(&Path) -> Result<MaterialInfo, MaterialLoadError>,
    ) {
        let mut material_path = PathBuf::from("materials");
        material_path.push(&self.side.material);

        let (material_index, material) = if let Some(r) = materials
            .iter()
            .enumerate()
            .find(|(_, p)| p.name == material_path)
        {
            r
        } else {
            let index = materials.len();
            let info = get_material_info(&material_path).unwrap_or_default();
            let material = SolidMaterial {
                name: material_path,
                info,
            };
            materials.push(material);
            (index, materials.last().unwrap())
        };

        self.material_index = material_index;

        let texture_width = f64::from(material.info.width());
        let texture_height = f64::from(material.info.height());
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
            self.vertice_uvs.push([u, v]);
        }

        // normalize
        let mut nearest_u = f64::MAX;
        let mut nearest_v = f64::MAX;
        for [u, _] in &self.vertice_uvs {
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

        for [_, v] in &self.vertice_uvs {
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

        for [u, v] in &mut self.vertice_uvs {
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
        faces: &mut Vec<FaceBuilder<'a>>,
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
                        offset + distance * normal + self.plane.normal.as_ref() * info.elevation;
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

            let mut disp_faces = Array3::<FaceBuilder>::from_shape_simple_fn(
                (dimension - 1, dimension - 1, 2),
                || FaceBuilder {
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

            faces.append(&mut disp_faces.into_raw_vec());
        }
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn clip_to_sides(
        self,
        self_center: Point3<f64>,
        clipped_sides: &mut Vec<Self>,
        vertices: &mut Vec<Point3<f64>>,
        sides: &[FaceBuilder],
        sides_center: Point3<f64>,
        clip_on_plane: bool,
        epsilon: f64,
    ) -> bool {
        // solids are convex, so if self is in front of any side it's outside the solid and doesn't need clipping
        if sides.iter().any(|side| {
            side.plane.classify_polygon(
                self.vertice_indices
                    .iter()
                    .map(|&i| vertices[i] + self_center.coords - sides_center.coords),
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
                .map(|&i| vertices[i] + self_center.coords - sides_center.coords),
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
        self_center: Point3<f64>,
        vertices: &mut Vec<Point3<f64>>,
        plane: &NdPlane,
        plane_center: Point3<f64>,
        epsilon: f64,
    ) -> (Self, Self) {
        let vertice_positions = self
            .vertice_indices
            .iter()
            .map(|&i| {
                plane.classify_point(
                    &(vertices[i] + self_center.coords - plane_center.coords),
                    epsilon,
                )
            })
            .collect_vec();

        let mut front = self.new_split();
        let mut back = self.new_split();

        // iterate pairs of vertices that form edges
        for ((&i, &uv, pos), (&i_next, &uv_next, pos_next)) in
            izip!(&self.vertice_indices, &self.vertice_uvs, vertice_positions)
                .circular_tuple_windows()
        {
            let vert = vertices[i] + self_center.coords - plane_center.coords;
            let vert_next = vertices[i_next] + self_center.coords - plane_center.coords;

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
                if let Some(v) = plane.intersect_line_with_factor(&vert, &vert_next, epsilon) {
                    v
                } else {
                    continue;
                };

            let new_vert = new_vert + plane_center.coords - self_center.coords;
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
            let new_uv = lerp_uv(uv, uv_next, factor);
            front.vertice_uvs.push(new_uv);
            back.vertice_uvs.push(new_uv);
        }

        (front, back)
    }

    fn finish(self) -> Face {
        Face {
            vertice_indices: self.vertice_indices,
            vertice_uvs: self.vertice_uvs,
            material_index: self.material_index,
        }
    }
}

#[derive(Clone, PartialEq)]
struct SolidBuilder<'a> {
    solid: &'a Solid,
    center: Point3<f64>,
    faces: Vec<FaceBuilder<'a>>,
    vertices: Vec<Point3<f64>>,
    materials: Vec<SolidMaterial>,
    is_displacement: bool,
    aabb_min: Point3<f64>,
    aabb_max: Point3<f64>,
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
                .map(|side| (side.plane.0 + side.plane.1.coords + side.plane.2.coords) / 3.0),
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
            aabb_min: Point3::new(0.0, 0.0, 0.0),
            aabb_max: Point3::new(0.0, 0.0, 0.0),
        }
    }

    fn intersect_sides(&mut self, epsilon: f64, cut_threshold: f64) {
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
                    .any(|(_, builder)| builder.plane.distance_to_point(&point) > cut_threshold)
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
        mut get_material_info: impl FnMut(&Path) -> Result<MaterialInfo, MaterialLoadError>,
    ) {
        let vertices = &self.vertices;
        for builder in &mut self.faces {
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
            .faces
            .iter()
            .filter_map(FaceBuilder::disp_vertices_len)
            .sum1()
        {
            Some(sum) => sum,
            None => return Ok(()), // this is not a displacement
        };

        let old_vertices = &self.vertices;
        let mut vertices: Vec<Point3<f64>> = Vec::with_capacity(vertices_len);

        let faces_len = self
            .faces
            .iter()
            .filter_map(FaceBuilder::disp_faces_len)
            .sum();
        let mut faces: Vec<FaceBuilder> = Vec::with_capacity(faces_len);

        for builder in &mut self.faces {
            builder.maybe_build_displacement(
                &self.center,
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

    fn extend_side_faces_map(&self, map: &Mutex<SideFacesMap>) {
        let mut map = map.lock().expect("mutex shouldn't be poisoned");
        for builder in &self.faces {
            map.entry(builder.side.id).or_default().push(
                builder
                    .vertice_indices
                    .iter()
                    .map(|&i| self.center + self.vertices[i].coords)
                    .collect(),
            );
        }
    }

    fn build(
        &mut self,
        get_material_info: impl FnMut(&Path) -> Result<MaterialInfo, MaterialLoadError>,
        side_faces_map: &Mutex<SideFacesMap>,
        settings: &GeometrySettings,
    ) -> Result<(), SolidError> {
        self.intersect_sides(settings.epsilon, settings.cut_threshold);
        self.remove_invalid_faces();
        self.sort_vertices();
        self.build_uvs(get_material_info);
        self.maybe_build_displacement()?;
        self.extend_side_faces_map(side_faces_map);

        Ok(())
    }

    fn calculate_aabb(&mut self) {
        let mut min = self
            .vertices
            .first()
            .copied()
            .unwrap_or_else(|| Point3::new(0.0, 0.0, 0.0));
        let mut max = min;

        for vertice in &self.vertices {
            min.coords = min.coords.inf(&vertice.coords);
            max.coords = max.coords.sup(&vertice.coords);
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

    fn clip_to_solid(&mut self, solid: &Self, clip_on_plane: bool, epsilon: f64) {
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
        let center = polygon_center(self.vertices.iter().copied()).coords;
        for vertice in &mut self.vertices {
            *vertice -= center;
        }
        self.center += center;
    }

    fn is_nodraw(&self) -> bool {
        self.materials.iter().all(|mat| mat.info.no_draw())
    }

    fn finish(self) -> BuiltSolid {
        BuiltSolid {
            id: self.solid.id,
            position: self.center,
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
        get_material_info: impl FnMut(&Path) -> Result<MaterialInfo, MaterialLoadError>,
        side_faces_map: &Mutex<SideFacesMap>,
        settings: &GeometrySettings,
    ) -> Result<BuiltSolid, SolidError> {
        let mut builder = SolidBuilder::new(self);
        builder.build(get_material_info, side_faces_map, settings)?;
        builder.recenter();
        Ok(builder.finish())
    }
}

#[derive(Debug)]
pub struct MergedSolids {
    pub vertices: Vec<Point3<f64>>,
    pub faces: Vec<Face>,
    pub materials: Vec<SolidMaterial>,
}

impl MergedSolids {
    fn merge(mut solids: Vec<SolidBuilder>, epsilon: f64, optimize: bool) -> Self {
        let merge_solids: Vec<SolidBuilder>;

        if optimize {
            // calculate aabbs
            for solid in &mut solids {
                solid.calculate_aabb();
            }

            let mut clipped_solids = solids.clone();

            // clip solids
            for (i, clip_solid) in clipped_solids.iter_mut().enumerate() {
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

        // merge solids
        let mut vertices = Vec::new();
        let mut materials = Vec::new();

        let mut faces = Vec::with_capacity(merge_solids.iter().map(|s| s.faces.len()).sum());

        for clipped_solid in merge_solids {
            let center = clipped_solid.center.coords;

            let vertice_i_map = clipped_solid
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

            let material_i_map = clipped_solid
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

            for mut face in clipped_solid.faces {
                // fix indices
                face.material_index = material_i_map[face.material_index];

                for vertice_index in &mut face.vertice_indices {
                    *vertice_index = vertice_i_map[*vertice_index];
                }

                faces.push(face.finish());
            }
        }

        Self {
            vertices,
            faces,
            materials,
        }
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
        mut get_material_info: impl FnMut(&Path) -> Result<MaterialInfo, MaterialLoadError>,
        side_faces_map: &Mutex<SideFacesMap>,
        settings: &GeometrySettings,
    ) -> Result<Self, SolidError> {
        if settings.merge_solids.merge() {
            let mut mergable_solids = Vec::new();
            let mut displacements = Vec::new();

            solids
                .iter()
                .filter_map(|solid| {
                    let mut builder = SolidBuilder::new(solid);
                    if let Err(err) =
                        builder.build(&mut get_material_info, side_faces_map, settings)
                    {
                        return Some(Err(err));
                    }
                    if settings.invisible_solids.import() || !builder.is_nodraw() {
                        Some(Ok(builder))
                    } else {
                        None
                    }
                })
                .try_for_each(|r| {
                    r.map(|mut b| {
                        if b.is_displacement {
                            b.recenter();
                            displacements.push(b.finish());
                        } else {
                            mergable_solids.push(b);
                        }
                    })
                })?;

            Ok(BuiltBrushEntity {
                id,
                class_name,
                merged_solids: Some(MergedSolids::merge(
                    mergable_solids,
                    settings.epsilon,
                    settings.merge_solids.optimize(),
                )),
                solids: displacements,
            })
        } else {
            Ok(BuiltBrushEntity {
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
                            return Some(Err(err));
                        }
                        if settings.invisible_solids.import() || !builder.is_nodraw() {
                            Some(Ok(builder.finish()))
                        } else {
                            None
                        }
                    })
                    .try_collect()?,
            })
        }
    }
}

impl Entity {
    /// # Errors
    ///
    /// Returns `Err` if the building fails.
    pub fn build_brush(
        &self,
        get_material_info: impl FnMut(&Path) -> Result<MaterialInfo, MaterialLoadError>,
        side_faces_map: &Mutex<SideFacesMap>,
        settings: &GeometrySettings,
    ) -> Result<BuiltBrushEntity, SolidError> {
        BuiltBrushEntity::new(
            &self.solids,
            self.id,
            &self.class_name,
            get_material_info,
            side_faces_map,
            settings,
        )
    }
}

impl World {
    /// # Errors
    ///
    /// Returns `Err` if the building fails.
    pub fn build_brush(
        &self,
        get_material_info: impl FnMut(&Path) -> Result<MaterialInfo, MaterialLoadError>,
        side_faces_map: &Mutex<SideFacesMap>,
        settings: &GeometrySettings,
    ) -> Result<BuiltBrushEntity, SolidError> {
        BuiltBrushEntity::new(
            &self.solids,
            self.id,
            &self.class_name,
            get_material_info,
            side_faces_map,
            settings,
        )
    }
}

#[cfg(test)]
mod tests {
    use nalgebra::Unit;
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
        builder.intersect_sides(1e-3, 1e-3);
        builder.remove_invalid_faces();
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

        assert_relative_eq!(
            builder.center,
            Point3::new(-896.0, 0.0, 32.0),
            epsilon = 1e-3
        );

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
                0 => (-0.75, -0.25),
                1 => (-0.75, 0.25),
                2 => (-0.25, -0.25),
                3 => (-0.25, 0.25),
                _ => unreachable!(),
            })
            .collect_vec();
        for (uv, expected_uv) in side.vertice_uvs.iter().zip(&expected_uvs) {
            assert!(
                relative_eq!(uv[0], expected_uv.0, epsilon = 1e-3),
                "got {:?}, expected {:?}",
                side.vertice_uvs,
                expected_uvs
            );
            assert!(
                relative_eq!(uv[1], expected_uv.1, epsilon = 1e-3),
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
        builder.intersect_sides(1e-3, 1e-3);
        builder.remove_invalid_faces();
        builder.sort_vertices();
        builder.build_uvs(|_| Ok(MaterialInfo::new(1024, 1024, false)));
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
                    .any(|v| relative_eq!(v, vertice, epsilon = 1e-3)),
                "unexpected vertice {}",
                vertice
            );
        }

        assert_relative_eq!(
            builder.center,
            Point3::new(146.974, 0.0, 131.398),
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
            Point3::new(-2.0, 1.0, 0.0),
            Point3::new(-2.0, -1.0, 0.0),
            Point3::new(0.0, -1.0, 0.0),
            Point3::new(2.0, 1.0, 0.0),
            Point3::new(-2.0, -1.0, 2.0),
            Point3::new(-2.0, 1.0, 2.0),
        ];

        let dummy_side = Side::default();

        let clipping_sides = vec![
            FaceBuilder {
                side: &dummy_side,
                plane: NdPlane::from_point_normal(
                    Point3::new(-1.0, 0.0, 0.0),
                    Unit::new_normalize(Vector3::new(-1.0, 0.0, 0.0)),
                ),
                vertice_indices: Vec::new(),
                vertice_uvs: Vec::new(),
                material_index: 0,
            },
            FaceBuilder {
                side: &dummy_side,
                plane: NdPlane::from_point_normal(
                    Point3::new(1.0, 0.0, 0.0),
                    Unit::new_normalize(Vector3::new(1.0, 0.0, 0.0)),
                ),
                vertice_indices: Vec::new(),
                vertice_uvs: Vec::new(),
                material_index: 0,
            },
            FaceBuilder {
                side: &dummy_side,
                plane: NdPlane::from_point_normal(
                    Point3::new(0.0, -2.0, 0.0),
                    Unit::new_normalize(Vector3::new(0.0, -1.0, 0.0)),
                ),
                vertice_indices: Vec::new(),
                vertice_uvs: Vec::new(),
                material_index: 0,
            },
            FaceBuilder {
                side: &dummy_side,
                plane: NdPlane::from_point_normal(
                    Point3::new(0.0, 2.0, 0.0),
                    Unit::new_normalize(Vector3::new(0.0, 1.0, 0.0)),
                ),
                vertice_indices: Vec::new(),
                vertice_uvs: Vec::new(),
                material_index: 0,
            },
        ];

        let clipped_side = FaceBuilder {
            side: &dummy_side,
            plane: NdPlane::from_point_normal(
                Point3::new(2.0, 2.0, 0.0),
                Unit::new_normalize(Vector3::new(0.0, 0.0, 1.0)),
            ),
            vertice_indices: vec![0, 1, 2, 3],
            vertice_uvs: vec![[0.0, 0.0]; 4],
            material_index: 0,
        };

        let mut clipped = Vec::new();

        clipped_side.clip_to_sides(
            Point3::new(3.0, 2.0, 0.0),
            &mut clipped,
            &mut vertices,
            &clipping_sides,
            Point3::new(5.0, 2.0, 0.0),
            false,
            1e-3,
        );

        assert_relative_eq!(
            &vertices[..],
            &[
                Point3::new(-2.0, 1.0, 0.0),
                Point3::new(-2.0, -1.0, 0.0),
                Point3::new(0.0, -1.0, 0.0),
                Point3::new(2.0, 1.0, 0.0),
                Point3::new(-2.0, -1.0, 2.0),
                Point3::new(-2.0, 1.0, 2.0),
                Point3::new(1.0, 0.0, 0.0),
                Point3::new(1.0, 1.0, 0.0),
            ][..],
            epsilon = 1e-3
        );

        assert_eq!(
            clipped,
            vec![FaceBuilder {
                side: &dummy_side,
                plane: NdPlane::from_point_normal(
                    Point3::new(2.0, 2.0, 0.0),
                    Unit::new_normalize(Vector3::new(0.0, 0.0, 1.0))
                ),
                vertice_indices: vec![0, 1, 2, 6, 7],
                vertice_uvs: vec![[0.0, 0.0]; 5],
                material_index: 0,
            }]
        );

        let clipped_side = FaceBuilder {
            side: &dummy_side,
            plane: NdPlane::from_point_normal(
                Point3::new(-2.0, 0.0, 2.0),
                Unit::new_normalize(Vector3::new(-1.0, 0.0, 0.0)),
            ),
            vertice_indices: vec![0, 1, 4, 5],
            vertice_uvs: vec![[0.0, 0.0]; 4],
            material_index: 0,
        };

        let mut clipped = Vec::new();

        clipped_side.clone().clip_to_sides(
            Point3::new(3.0, 2.0, 0.0),
            &mut clipped,
            &mut vertices,
            &clipping_sides,
            Point3::new(5.0, 2.0, 0.0),
            false,
            1e-3,
        );

        assert_eq!(clipped, vec![clipped_side]);

        let clipped_side = FaceBuilder {
            side: &dummy_side,
            plane: NdPlane::from_point_normal(
                Point3::new(1.0, 0.0, 0.0),
                Unit::new_normalize(Vector3::new(0.0, 0.0, 1.0)),
            ),
            vertice_indices: vec![3, 7, 6],
            vertice_uvs: vec![[0.0, 0.0]; 2],
            material_index: 0,
        };

        let mut clipped = Vec::new();

        clipped_side.clone().clip_to_sides(
            Point3::new(3.0, 2.0, 0.0),
            &mut clipped,
            &mut vertices,
            &clipping_sides,
            Point3::new(5.0, 2.0, 0.0),
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
            center: Point3::new(5.0, 2.0, 0.0),
            faces: vec![
                FaceBuilder {
                    side: &dummy_side,
                    plane: NdPlane::from_point_normal(
                        Point3::new(-1.0, 0.0, 0.0),
                        Unit::new_normalize(Vector3::new(-1.0, 0.0, 0.0)),
                    ),
                    vertice_indices: Vec::new(),
                    vertice_uvs: Vec::new(),
                    material_index: 0,
                },
                FaceBuilder {
                    side: &dummy_side,
                    plane: NdPlane::from_point_normal(
                        Point3::new(1.0, 0.0, 0.0),
                        Unit::new_normalize(Vector3::new(1.0, 0.0, 0.0)),
                    ),
                    vertice_indices: Vec::new(),
                    vertice_uvs: Vec::new(),
                    material_index: 0,
                },
                FaceBuilder {
                    side: &dummy_side,
                    plane: NdPlane::from_point_normal(
                        Point3::new(0.0, -2.0, 0.0),
                        Unit::new_normalize(Vector3::new(0.0, -1.0, 0.0)),
                    ),
                    vertice_indices: Vec::new(),
                    vertice_uvs: Vec::new(),
                    material_index: 0,
                },
                FaceBuilder {
                    side: &dummy_side,
                    plane: NdPlane::from_point_normal(
                        Point3::new(0.0, 2.0, 0.0),
                        Unit::new_normalize(Vector3::new(0.0, 1.0, 0.0)),
                    ),
                    vertice_indices: Vec::new(),
                    vertice_uvs: Vec::new(),
                    material_index: 0,
                },
                FaceBuilder {
                    side: &dummy_side,
                    plane: NdPlane::from_point_normal(
                        Point3::new(0.0, 0.0, -1.0),
                        Unit::new_normalize(Vector3::new(0.0, 0.0, -1.0)),
                    ),
                    vertice_indices: Vec::new(),
                    vertice_uvs: Vec::new(),
                    material_index: 0,
                },
                FaceBuilder {
                    side: &dummy_side,
                    plane: NdPlane::from_point_normal(
                        Point3::new(0.0, 0.0, 1.0),
                        Unit::new_normalize(Vector3::new(0.0, 0.0, 1.0)),
                    ),
                    vertice_indices: Vec::new(),
                    vertice_uvs: Vec::new(),
                    material_index: 0,
                },
            ],
            vertices: Vec::new(),
            materials: Vec::new(),
            is_displacement: false,
            aabb_min: Point3::new(0.0, 0.0, 0.0),
            aabb_max: Point3::new(0.0, 0.0, 0.0),
        };

        let mut clipped_solid = SolidBuilder {
            solid: &dummy_solid,
            center: Point3::new(6.0, 4.0, 0.0),
            faces: vec![
                FaceBuilder {
                    side: &dummy_side,
                    plane: NdPlane::from_point_normal(
                        Point3::new(-1.0, 2.0, 0.0),
                        Unit::new_normalize(Vector3::new(-1.0, 1.0, 0.0)),
                    ),
                    vertice_indices: vec![0, 1, 5, 4],
                    vertice_uvs: vec![[0.0, 0.0]; 4],
                    material_index: 0,
                },
                FaceBuilder {
                    side: &dummy_side,
                    plane: NdPlane::from_point_normal(
                        Point3::new(-2.0, 1.0, 0.0),
                        Unit::new_normalize(Vector3::new(-1.0, -1.0, 0.0)),
                    ),
                    vertice_indices: vec![1, 2, 6, 5],
                    vertice_uvs: vec![[0.0, 0.0]; 4],
                    material_index: 0,
                },
                FaceBuilder {
                    side: &dummy_side,
                    plane: NdPlane::from_point_normal(
                        Point3::new(1.0, -2.0, 0.0),
                        Unit::new_normalize(Vector3::new(1.0, -1.0, 0.0)),
                    ),
                    vertice_indices: vec![2, 3, 7, 6],
                    vertice_uvs: vec![[0.0, 0.0]; 4],
                    material_index: 0,
                },
                FaceBuilder {
                    side: &dummy_side,
                    plane: NdPlane::from_point_normal(
                        Point3::new(2.0, -1.0, 0.0),
                        Unit::new_normalize(Vector3::new(1.0, 1.0, 0.0)),
                    ),
                    vertice_indices: vec![3, 0, 4, 7],
                    vertice_uvs: vec![[0.0, 0.0]; 4],
                    material_index: 0,
                },
                FaceBuilder {
                    side: &dummy_side,
                    plane: NdPlane::from_point_normal(
                        Point3::new(0.0, 0.0, 1.0),
                        Unit::new_normalize(Vector3::new(0.0, 0.0, 1.0)),
                    ),
                    vertice_indices: vec![0, 1, 2, 3],
                    vertice_uvs: vec![[0.0, 0.0]; 4],
                    material_index: 0,
                },
                FaceBuilder {
                    side: &dummy_side,
                    plane: NdPlane::from_point_normal(
                        Point3::new(0.0, 0.0, -1.0),
                        Unit::new_normalize(Vector3::new(0.0, 0.0, -1.0)),
                    ),
                    vertice_indices: vec![7, 6, 5, 4],
                    vertice_uvs: vec![[0.0, 0.0]; 4],
                    material_index: 0,
                },
            ],
            vertices: vec![
                Point3::new(-1.0, 2.0, 1.0),
                Point3::new(-2.0, 1.0, 1.0),
                Point3::new(1.0, -2.0, 1.0),
                Point3::new(2.0, -1.0, 1.0),
                Point3::new(-1.0, 2.0, -1.0),
                Point3::new(-2.0, 1.0, -1.0),
                Point3::new(1.0, -2.0, -1.0),
                Point3::new(2.0, -1.0, -1.0),
            ],
            materials: Vec::new(),
            is_displacement: false,
            aabb_min: Point3::new(0.0, 0.0, 0.0),
            aabb_max: Point3::new(0.0, 0.0, 0.0),
        };

        clipped_solid.clip_to_solid(&clipping_solid, false, 1e-3);

        dbg!(clipped_solid);
    }
}
