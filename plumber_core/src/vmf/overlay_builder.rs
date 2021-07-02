use std::{collections::BTreeMap, fmt::Debug, sync::Mutex};

use crate::fs::PathBuf;

use super::{
    builder_utils::{polygon_center, polygon_normal, NdPlane, CUT_THRESHOLD, EPSILON},
    entities::{EntityParseError, Overlay, OverlayUvInfo},
};

use approx::relative_eq;
use itertools::Itertools;
use nalgebra::{geometry::Point3, Matrix2x3, Matrix3, Point2, Unit, Vector3};
use thiserror::Error;

pub(crate) type SideFacesMap = BTreeMap<i32, Vec<Vec<Point3<f64>>>>;

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
pub struct BuiltOverlay<'a> {
    pub overlay: Overlay<'a>,
    pub position: Point3<f64>,
    pub vertices: Vec<Point3<f64>>,
    pub faces: Vec<BuiltOverlayFace>,
    pub material: PathBuf,
}

#[derive(Debug)]
pub struct BuiltOverlayFace {
    pub vertice_indices: Vec<usize>,
    pub vertice_uvs: Vec<[f64; 2]>,
}

struct OverlayFaceBuilder {
    vertice_indices: Vec<usize>,
    vertice_uvs: Vec<[f64; 2]>,
}

impl OverlayFaceBuilder {
    fn finish(self) -> BuiltOverlayFace {
        BuiltOverlayFace {
            vertice_indices: self.vertice_indices,
            vertice_uvs: self.vertice_uvs,
        }
    }
}

struct OverlayBuilder<'a> {
    overlay: Overlay<'a>,
    uv_info: OverlayUvInfo,
    origin: Point3<f64>,
    uv_to_global_matrix: Matrix3<f64>,
    global_to_uv_matrix: Matrix3<f64>,
    faces: Vec<OverlayFaceBuilder>,
    vertices: Vec<Point3<f64>>,
    uv_space_vertices: Vec<Point3<f64>>,
}

impl<'a> OverlayBuilder<'a> {
    fn new(overlay: Overlay<'a>) -> Result<Self, OverlayError> {
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

    fn create_vertices(
        &mut self,
        side_faces_map: &Mutex<SideFacesMap>,
    ) -> Result<(), OverlayError> {
        for side_i in self.overlay.sides()? {
            let side_faces_map = side_faces_map.lock().expect("mutex shouldn't be poisoned");
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
        let origin = &self.origin;

        let mut uv_space_vertices = vertices
            .iter()
            .map(|v| Point3::from(global_to_uv_matrix * (v - origin)))
            .collect_vec();

        let up = Vector3::new(0.0, 0.0, 1.0);

        // cut faces partially outside uv borders
        for (side_vert_a, side_vert_b) in self.uv_info.uvs.iter().circular_tuple_windows() {
            let cut_plane_normal = Unit::new_normalize(up.cross(&(side_vert_b - side_vert_a)));
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
                    .rposition(|i| outside_vertice_is.contains(i))
                    .expect("the case where there are no vertices outside is handled above");

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
                    IntoIterator::into_iter([first_new_i, last_new_i]),
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
            let cut_plane_normal = Unit::new_normalize(up.cross(&(side_vert_b - side_vert_a)));
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
                i += 1;
                false
            } else {
                // fix other faces' vertice indices
                for builder in faces.iter_mut() {
                    for f_i in &mut builder.vertice_indices {
                        if *f_i == i {
                            *f_i = new_i;
                        }
                    }
                }
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
                builder.vertice_uvs.push([uv.x, uv.y]);
            }
        }
        Ok(())
    }

    fn recenter(&mut self) {
        let center = polygon_center(self.vertices.iter().copied()).coords;
        for vertice in &mut self.vertices {
            *vertice -= center;
        }
        // vertices are global space before this point
        self.origin = center.into();
    }

    fn finish(self) -> Result<BuiltOverlay<'a>, OverlayError> {
        let material = self.overlay.material()?;

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
    pub fn build_mesh(
        self,
        side_faces_map: &Mutex<SideFacesMap>,
    ) -> Result<BuiltOverlay<'a>, OverlayError> {
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
