use std::{collections::BTreeMap, fmt::Debug};

use approx::relative_eq;
use glam::{Mat3, Vec2, Vec3, Vec3Swizzles};
use itertools::Itertools;
use thiserror::Error;

use plumber_fs::GamePathBuf;

use super::{
    builder_utils::{
        affine_matrix, affine_transform_point, is_point_left_of_line, polygon_center,
        polygon_normal, GeometrySettings, NdPlane,
    },
    entities::{EntityParseError, Overlay, OverlayUvInfo},
};

#[cfg(test)]
mod tests;

pub type SideFacesMap = BTreeMap<i32, Vec<Vec<Vec3>>>;

#[derive(Debug, Error, Clone, Hash, PartialEq, Eq)]
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
    pub position: Vec3,
    pub scale: f32,
    pub vertices: Vec<Vec3>,
    pub faces: Vec<BuiltOverlayFace>,
    pub material: GamePathBuf,
}

#[derive(Debug)]
pub struct BuiltOverlayFace {
    pub vertice_indices: Vec<usize>,
    pub vertice_uvs: Vec<Vec2>,
}

#[derive(Debug)]
struct OverlayFaceBuilder {
    vertice_indices: Vec<usize>,
    vertice_uvs: Vec<Vec2>,
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
    origin: Vec3,
    uv_to_global_matrix: Mat3,
    global_to_uv_matrix: Mat3,
    faces: Vec<OverlayFaceBuilder>,
    vertices: Vec<Vec3>,
    uv_space_vertices: Vec<Vec3>,
}

impl<'a> OverlayBuilder<'a> {
    fn new(overlay: Overlay<'a>) -> Result<Self, OverlayError> {
        let uv_info = overlay.uv_info()?;

        let u_axis = uv_info.basis_u;
        let v_axis = uv_info.basis_v;
        let normal = uv_info.basis_normal;
        let origin = uv_info.basis_origin;

        let uv_to_global_matrix = Mat3::from_cols(u_axis, v_axis, normal);

        let global_to_uv_matrix = uv_to_global_matrix.inverse();

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
        side_faces_map: &SideFacesMap,
        epsilon: f32,
    ) -> Result<(), OverlayError> {
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
                            .position(|v| relative_eq!(v, vertice, epsilon = epsilon))
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
        let offset = f32::from(self.overlay.render_order()? + 1) * 0.1;
        let mut offset_directions = vec![Vec3::ZERO; self.vertices.len()];

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

    fn cut_faces(&mut self, epsilon: f32, cut_threshold: f32) -> Result<(), OverlayError> {
        let vertices = &mut self.vertices;
        let global_to_uv_matrix = self.global_to_uv_matrix;
        let origin = self.origin;

        let mut uv_space_vertices = vertices
            .iter()
            .map(|&v| global_to_uv_matrix * (v - origin))
            .collect_vec();

        let up = Vec3::new(0.0, 0.0, 1.0);

        // cut faces partially outside uv borders
        for (&side_vert_a, &side_vert_b) in self.uv_info.uvs.iter().circular_tuple_windows() {
            let cut_plane_normal = up.cross(side_vert_b - side_vert_a).normalize();
            let cut_plane = NdPlane::from_point_normal(side_vert_a, cut_plane_normal);

            let outside_vertice_is = uv_space_vertices
                .iter()
                .positions(|&v| cut_plane.distance_to_point(v) > cut_threshold)
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
                    .intersect_line(first_line_a, first_line_b - first_line_a, epsilon)
                    .ok_or(OverlayError::InvalidUvData)?;
                let new_vertice = self.origin + self.uv_to_global_matrix * new_uv_space_vertice;

                let first_new_i = vertices
                    .iter()
                    .position(|v| relative_eq!(v, &new_vertice, epsilon = epsilon))
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
                    .intersect_line(last_line_a, last_line_b - last_line_a, epsilon)
                    .ok_or(OverlayError::InvalidUvData)?;
                let new_vertice = self.origin + self.uv_to_global_matrix * new_uv_space_vertice;

                let last_new_i = vertices
                    .iter()
                    .position(|v| relative_eq!(v, &new_vertice, epsilon = epsilon))
                    .unwrap_or_else(|| {
                        uv_space_vertices.push(new_uv_space_vertice);
                        vertices.push(new_vertice);
                        vertices.len() - 1
                    });

                // replace the face vertices that were outside the uv border with the 2 newly created ones
                builder
                    .vertice_indices
                    .splice(first_outside_i..=last_outside_i, [first_new_i, last_new_i]);
            }
        }

        self.uv_space_vertices = uv_space_vertices;

        Ok(())
    }

    fn remove_vertices_outside(&mut self, cut_threshold: f32) {
        let mut vertices_to_remove = Vec::new();

        for (&side_vert_a, &side_vert_b) in self.uv_info.uvs.iter().circular_tuple_windows() {
            let cut_plane_normal = Vec3::Z.cross(side_vert_b - side_vert_a).normalize();
            let cut_plane = NdPlane::from_point_normal(side_vert_a, cut_plane_normal);
            for (i, _) in self
                .uv_space_vertices
                .iter()
                .enumerate()
                .filter(|(_, &v)| cut_plane.distance_to_point(v) > cut_threshold)
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

        i = 0;
        self.uv_space_vertices.retain(|_| {
            let retain = !vertices_to_remove.contains(&i);
            i += 1;
            retain
        });
    }

    fn cleanup_faces(&mut self) {
        self.faces.retain_mut(|face| {
            // Make sure there are no sequential duplicate vertice indices in a face (which would be an invalid face)

            let Some(previous_vertice_index) = face.vertice_indices.last() else {
                return false;
            };
            let mut previous_vertice_index = *previous_vertice_index;

            face.vertice_indices.retain(|&i| {
                let retain = i != previous_vertice_index;

                previous_vertice_index = i;

                retain
            });

            // And also remove the whole face if there are less than 3 vertices after the cleanup
            face.vertice_indices.len() >= 3
        });
    }

    fn ensure_not_empty(&self) -> Result<(), OverlayError> {
        if self.faces.is_empty() {
            return Err(OverlayError::InvalidUvData);
        }
        Ok(())
    }

    fn create_uvs(&mut self) {
        for builder in &mut self.faces {
            // uv calculations are 2 affine transformations of triangles

            // calculate matrix for first triangle (uv points 0, 1, 2)
            let affine_matrix_a = affine_matrix(
                [
                    self.uv_info.uvs[0].xy(),
                    self.uv_info.uvs[1].xy(),
                    self.uv_info.uvs[2].xy(),
                ],
                [
                    Vec2::new(self.uv_info.start_u, self.uv_info.start_v),
                    Vec2::new(self.uv_info.start_u, self.uv_info.end_v),
                    Vec2::new(self.uv_info.end_u, self.uv_info.end_v),
                ],
            );

            // same for second triangle (uv points 2, 3, 0)
            let affine_matrix_b = affine_matrix(
                [
                    self.uv_info.uvs[2].xy(),
                    self.uv_info.uvs[3].xy(),
                    self.uv_info.uvs[0].xy(),
                ],
                [
                    Vec2::new(self.uv_info.end_u, self.uv_info.end_v),
                    Vec2::new(self.uv_info.end_u, self.uv_info.start_v),
                    Vec2::new(self.uv_info.start_u, self.uv_info.start_v),
                ],
            );

            for &vert_i in &builder.vertice_indices {
                let uv_vert = self.uv_space_vertices[vert_i].xy();

                // determine which triangle this vertice is inside of
                let affine_matrix = if is_point_left_of_line(
                    self.uv_info.uvs[0].xy(),
                    self.uv_info.uvs[2].xy(),
                    uv_vert,
                ) {
                    affine_matrix_a
                } else {
                    affine_matrix_b
                };

                let uv = affine_transform_point(affine_matrix, uv_vert);
                builder.vertice_uvs.push(uv);
            }
        }
    }

    fn recenter(&mut self) {
        let center = polygon_center(self.vertices.iter().copied());
        for vertice in &mut self.vertices {
            *vertice -= center;
        }
        // vertices are global space before this point
        self.origin = center;
    }

    fn finish(self, scale: f32) -> Result<BuiltOverlay<'a>, OverlayError> {
        let mut material = GamePathBuf::from("materials");
        let overlay_material = self.overlay.material()?;
        material.push(&overlay_material);

        Ok(BuiltOverlay {
            overlay: self.overlay,
            position: self.origin * scale,
            scale,
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

impl<'a> Overlay<'a> {
    /// # Errors
    ///
    /// Returns `Err` if the mesh creation fails.
    pub fn build_mesh(
        self,
        side_faces_map: &SideFacesMap,
        settings: &GeometrySettings,
        scale: f32,
    ) -> Result<BuiltOverlay<'a>, OverlayError> {
        let mut builder = OverlayBuilder::new(self)?;
        builder.create_vertices(side_faces_map, settings.epsilon)?;
        builder.offset_vertices()?;
        builder.cut_faces(settings.epsilon, settings.cut_threshold)?;
        builder.remove_vertices_outside(settings.cut_threshold);
        builder.cleanup_faces();
        builder.ensure_not_empty()?;

        builder.create_uvs();
        builder.recenter();
        builder.finish(scale)
    }
}
