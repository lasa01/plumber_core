use super::{Plane, Side, Solid};

use approx::{abs_diff_eq, relative_eq};
use float_ord::FloatOrd;
use itertools::Itertools;
use nalgebra::{geometry::Point3, Vector3};

const EPSILON: f64 = 1e-6;
const CUT_THRESHOLD: f64 = 1e-4;

pub struct BuiltSolid<'a> {
    pub solid: &'a Solid,
    pub vertices: Vec<Point3<f64>>,
}

/// A plane defined by a normal vector and a distance to the origin.
/// Also keeps a point on the plane for convenience.
struct NdPlane {
    point: Point3<f64>,
    normal: Vector3<f64>,
    distance: f64,
}

impl NdPlane {
    fn from_plane(plane: &Plane) -> Self {
        Self::from_points(&plane.0, &plane.1, &plane.2)
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
}

impl<'a> SideBuilder<'a> {
    fn new(side: &'a Side) -> Self {
        let plane = NdPlane::from_plane(&side.plane);
        Self {
            side,
            plane,
            vertice_indices: Vec::new(),
            vertice_uvs: Vec::new(),
        }
    }

    fn insert_vertice(&mut self, i: usize) {
        if !self.vertice_indices.contains(&i) {
            self.vertice_indices.push(i);
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

fn polygon_center<'a, I>(polygon: I) -> Point3<f64>
where
    I: Iterator<Item = &'a Point3<f64>> + ExactSizeIterator,
{
    let len = polygon.len() as f64;
    (polygon.fold(Vector3::<f64>::zeros(), |a, b| a + b.coords) / len).into()
}

fn polygon_normal<'a, I>(polygon: I) -> Vector3<f64>
where
    I: Clone + Iterator<Item = &'a Point3<f64>> + ExactSizeIterator,
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
    sides: Vec<SideBuilder<'a>>,
    vertices: Vec<Point3<f64>>,
}

impl<'a> SolidBuilder<'a> {
    fn new(solid: &'a Solid) -> Self {
        Self {
            solid,
            sides: solid
                .sides
                .iter()
                .map(|side| SideBuilder::new(side))
                .collect(),
            vertices: Vec::new(),
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
                    .any(|(_, side)| side.plane.distance_to_point(&point) > CUT_THRESHOLD)
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
        self.sides.retain(|side| side.vertice_indices.len() >= 3);
    }

    fn sort_vertices(&mut self) {
        let vertices = &mut self.vertices;
        for side in &mut self.sides {
            let center: Point3<f64> =
                polygon_center(side.vertice_indices.iter().map(|&i| &vertices[i]));

            for i in 0..side.vertice_indices.len() - 2 {
                let vertice = &vertices[side.vertice_indices[i]];
                let to_current: Vector3<f64> = (vertice - center).normalize();
                let filter_plane =
                    NdPlane::from_points(&vertice, &center, &(center + side.plane.normal));

                if let Some((next_idx, _)) = side.vertice_indices[i + 1..]
                    .iter()
                    .enumerate()
                    .filter(|(_, &vi)| filter_plane.distance_to_point(&vertices[vi]) >= 0.0)
                    .map(|(i, &vi)| {
                        let to_candidate: Vector3<f64> = (vertices[vi] - center).normalize();
                        let angle = to_current.dot(&to_candidate);
                        (i, angle)
                    })
                    .min_by_key(|(_, d)| FloatOrd(*d))
                {
                    side.vertice_indices.swap(i + 1, next_idx);
                }
            }

            // reverse if the normal is facing the wrong way
            if polygon_normal(side.vertice_indices.iter().map(|i| &vertices[*i]))
                .dot(&side.plane.normal)
                < 0.0
            {
                side.vertice_indices.reverse();
            }
        }
    }

    fn build_uvs(&mut self, texture_width: f64, texture_height: f64) {
        let vertices = &self.vertices;
        for side in &mut self.sides {
            side.vertice_uvs.reserve_exact(side.vertice_indices.len());
            let u_axis = side.side.u_axis;
            let v_axis = side.side.v_axis;
            for &vi in &side.vertice_indices {
                let u = vertices[vi].coords.dot(&u_axis.axis) / (texture_width * u_axis.scale)
                    + u_axis.translation / texture_width;
                let v = vertices[vi].coords.dot(&v_axis.axis) / (texture_height * v_axis.scale)
                    + v_axis.translation / texture_height;
                side.vertice_uvs.push((u, v));
            }

            // normalize
            let mut nearest_u = f64::MAX;
            let mut nearest_v = f64::MAX;
            for (u, _) in &side.vertice_uvs {
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

            for (_, v) in &side.vertice_uvs {
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

            for (u, v) in &mut side.vertice_uvs {
                *u -= nearest_u;
                *v -= nearest_v;
            }
        }
    }

    fn finish(self) -> BuiltSolid<'a> {
        BuiltSolid {
            solid: self.solid,
            vertices: self.vertices,
        }
    }
}

impl Solid {
    #[must_use]
    pub fn build_mesh(&self) -> BuiltSolid {
        let mut builder = SolidBuilder::new(self);
        builder.intersect_sides();
        builder.remove_invalid_sides();
        builder.sort_vertices();
        builder.build_uvs(1024.0, 1024.0);
        builder.finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use approx::assert_relative_eq;

    #[test]
    fn test_planes() {
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
}
