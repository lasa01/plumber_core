use std::fmt::Debug;

use super::Plane;

use approx::abs_diff_eq;
use itertools::Itertools;
use nalgebra::{geometry::Point3, Vector3};

pub(crate) const EPSILON: f64 = 1e-3;
pub(crate) const CUT_THRESHOLD: f64 = 1e-3;

/// A plane defined by a normal vector and a distance to the origin.
/// Also keeps a point on the plane for convenience.
#[derive(Debug, Clone, Copy)]
pub(crate) struct NdPlane {
    pub point: Point3<f64>,
    pub normal: Vector3<f64>,
    pub distance: f64,
}

impl NdPlane {
    pub fn from_plane(plane: &Plane, center: &Point3<f64>) -> Self {
        // in vmf, plane points are in cw winding order,
        // everywhere from now on, ccw winding order
        Self::from_points(
            &(plane.2 - center).into(),
            &(plane.1 - center).into(),
            &(plane.0 - center).into(),
        )
    }

    pub fn from_points(a: &Point3<f64>, b: &Point3<f64>, c: &Point3<f64>) -> Self {
        let normal = (b - a).cross(&(c - a)).normalize();
        let distance = -a.coords.dot(&normal);
        Self {
            point: *a,
            normal,
            distance,
        }
    }

    pub fn from_point_normal(point: Point3<f64>, normal: Vector3<f64>) -> Self {
        let distance = -point.coords.dot(&normal);
        Self {
            point,
            normal,
            distance,
        }
    }

    pub fn distance_to_point(&self, point: &Point3<f64>) -> f64 {
        (point - self.point).dot(&self.normal)
    }

    pub fn intersect_line(
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

    pub fn intersect(a: &NdPlane, b: &NdPlane, c: &NdPlane) -> Option<Point3<f64>> {
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

pub(crate) fn polygon_center<I>(polygon: I) -> Point3<f64>
where
    I: Iterator<Item = Point3<f64>> + ExactSizeIterator,
{
    let len = polygon.len() as f64;
    (polygon.fold(Vector3::<f64>::zeros(), |a, b| a + b.coords) / len).into()
}

pub(crate) fn polygon_normal<I>(polygon: I) -> Vector3<f64>
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

pub(crate) fn lerp(lhs: f64, rhs: f64, t: f64) -> f64 {
    lhs + (lhs - rhs) * t
}

pub(crate) fn lerp_uv(lhs: [f64; 2], rhs: [f64; 2], t: f64) -> [f64; 2] {
    [lerp(lhs[0], rhs[0], t), lerp(lhs[1], rhs[1], t)]
}
