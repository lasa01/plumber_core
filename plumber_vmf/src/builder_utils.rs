use std::fmt::Debug;

use approx::abs_diff_eq;
use glam::{Affine2, Mat3, Vec2, Vec3};
use itertools::Itertools;

use crate::types::Plane;

#[derive(Debug, Clone, Copy)]
pub enum MergeSolids {
    Separate,
    Merge,
    MergeAndOptimize,
}

impl MergeSolids {
    #[must_use]
    pub fn merge(self) -> bool {
        matches!(self, Self::Merge | Self::MergeAndOptimize)
    }

    #[must_use]
    pub fn optimize(self) -> bool {
        matches!(self, Self::MergeAndOptimize)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum InvisibleSolids {
    Import,
    Skip,
}

impl InvisibleSolids {
    #[must_use]
    pub fn skip(self) -> bool {
        matches!(self, Self::Skip)
    }

    #[must_use]
    pub fn import(self) -> bool {
        matches!(self, Self::Import)
    }
}

#[derive(Debug, Clone, Copy)]
#[non_exhaustive]
pub struct GeometrySettings {
    pub epsilon: f32,
    pub cut_threshold: f32,
    pub merge_solids: MergeSolids,
    pub invisible_solids: InvisibleSolids,
}

impl GeometrySettings {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn epsilon(&mut self, epsilon: f32) {
        self.epsilon = epsilon;
    }

    pub fn cut_threshold(&mut self, cut_threshold: f32) {
        self.cut_threshold = cut_threshold;
    }

    pub fn merge_solids(&mut self, merge: MergeSolids) {
        self.merge_solids = merge;
    }

    pub fn invisible_solids(&mut self, invisible: InvisibleSolids) {
        self.invisible_solids = invisible;
    }
}

impl Default for GeometrySettings {
    fn default() -> Self {
        Self {
            epsilon: 1e-3,
            cut_threshold: 1e-3,
            merge_solids: MergeSolids::Merge,
            invisible_solids: InvisibleSolids::Skip,
        }
    }
}

/// A plane defined by a normal vector and a distance to the origin.
/// Also keeps a point on the plane for convenience.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct NdPlane {
    pub point: Vec3,
    pub normal: Vec3,
    pub distance: f32,
}

impl NdPlane {
    pub fn from_plane(plane: &Plane, center: Vec3) -> Self {
        // in vmf, plane points are in cw winding order,
        // everywhere from now on, ccw winding order
        Self::from_points(plane.2 - center, plane.1 - center, plane.0 - center)
    }

    pub fn from_points(a: Vec3, b: Vec3, c: Vec3) -> Self {
        let normal = (b - a).cross(c - a).normalize();
        let distance = -a.dot(normal);
        Self {
            point: a,
            normal,
            distance,
        }
    }

    /// `normal` must be normalized.
    pub fn from_point_normal(point: Vec3, normal: Vec3) -> Self {
        let distance = -point.dot(normal);
        Self {
            point,
            normal,
            distance,
        }
    }

    pub fn distance_to_point(&self, point: Vec3) -> f32 {
        (point - self.point).dot(self.normal)
    }

    pub fn classify_point(&self, point: Vec3, epsilon: f32) -> PointClassification {
        let distance = self.distance_to_point(point);

        if distance > epsilon {
            PointClassification::Front
        } else if distance < -epsilon {
            PointClassification::Back
        } else {
            PointClassification::OnPlane
        }
    }

    pub fn intersect_line_with_factor(
        &self,
        line_1: Vec3,
        line_2: Vec3,
        epsilon: f32,
    ) -> Option<(Vec3, f32)> {
        let line_direction = (line_2 - line_1).normalize();
        if abs_diff_eq!(self.normal.dot(line_direction), 0.0, epsilon = epsilon) {
            return None;
        }
        let t = (self.normal.dot(self.point) - self.normal.dot(line_1))
            / self.normal.dot(line_direction);

        let point = line_1 + line_direction * t;
        let factor = t / line_1.distance(line_2);

        Some((point, factor))
    }

    pub fn intersect_line(
        &self,
        line_point: Vec3,
        line_direction: Vec3,
        epsilon: f32,
    ) -> Option<Vec3> {
        let line_direction = line_direction.normalize();
        if abs_diff_eq!(self.normal.dot(line_direction), 0.0, epsilon = epsilon) {
            return None;
        }
        let t = (self.normal.dot(self.point) - self.normal.dot(line_point))
            / self.normal.dot(line_direction);
        Some(line_point + line_direction * t)
    }

    pub fn intersect(a: &NdPlane, b: &NdPlane, c: &NdPlane, epsilon: f32) -> Option<Vec3> {
        let denominator = a.normal.dot(b.normal.cross(c.normal));
        if abs_diff_eq!(denominator, 0.0, epsilon = epsilon) {
            return None;
        }
        Some(
            (-a.distance * b.normal.cross(c.normal)
                - b.distance * c.normal.cross(a.normal)
                - c.distance * a.normal.cross(b.normal))
                / denominator,
        )
    }

    pub fn classify_polygon<I>(&self, polygon: I, epsilon: f32) -> PolygonClassification
    where
        I: Iterator<Item = Vec3> + ExactSizeIterator,
    {
        let points_len = polygon.len();
        let mut points_front = 0_usize;
        let mut points_back = 0_usize;
        let mut points_on_plane = 0_usize;

        for point in polygon {
            match self.classify_point(point, epsilon) {
                PointClassification::Front => points_front += 1,
                PointClassification::Back => points_back += 1,
                PointClassification::OnPlane => points_on_plane += 1,
            }
        }

        if points_front == points_len {
            return PolygonClassification::Front;
        }
        if points_back == points_len {
            return PolygonClassification::Back;
        }
        if points_on_plane == points_len {
            return PolygonClassification::OnPlane;
        }

        PolygonClassification::Spanning
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PointClassification {
    Front,
    Back,
    OnPlane,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PolygonClassification {
    Front,
    Back,
    OnPlane,
    Spanning,
}

#[allow(clippy::cast_precision_loss)]
pub(crate) fn polygon_center<I>(polygon: I) -> Vec3
where
    I: Iterator<Item = Vec3> + ExactSizeIterator,
{
    let len = polygon.len() as f32;
    polygon.fold(Vec3::ZERO, |a, b| a + b) / len
}

pub(crate) fn polygon_normal<I>(polygon: I) -> Vec3
where
    I: Clone + Iterator<Item = Vec3> + ExactSizeIterator,
{
    let center = polygon_center(polygon.clone());
    let mut normal = Vec3::ZERO;

    for (a, b) in polygon.circular_tuple_windows() {
        normal += (a - center).cross(b - center);
    }

    normal.normalize()
}

pub(crate) fn affine_matrix(src_points: [Vec2; 3], dst_points: [Vec2; 3]) -> Affine2 {
    let src_matrix = Mat3::from_cols_array(&[
        src_points[0].x,
        src_points[0].y,
        1.0,
        src_points[1].x,
        src_points[1].y,
        1.0,
        src_points[2].x,
        src_points[2].y,
        1.0,
    ])
    .inverse();

    let dst_matrix = Mat3::from_cols_array(&[
        dst_points[0].x,
        dst_points[0].y,
        1.0,
        dst_points[1].x,
        dst_points[1].y,
        1.0,
        dst_points[2].x,
        dst_points[2].y,
        1.0,
    ]);

    Affine2::from_mat3(dst_matrix * src_matrix)
}

pub(crate) fn affine_transform_point(matrix: Affine2, point: Vec2) -> Vec2 {
    matrix.transform_point2(point)
}

pub(crate) fn is_point_left_of_line(line_a: Vec2, line_b: Vec2, point: Vec2) -> bool {
    (point.x - line_a.x) * (line_b.y - line_a.y) - (point.y - line_a.y) * (line_b.x - line_a.x)
        < 0.0
}

#[cfg(test)]
mod tests {
    use super::*;
    use approx::assert_relative_eq;

    #[test]
    fn plane_creation() {
        let plane_1 = NdPlane::from_points(
            Vec3::new(-1.0, 0.0, 0.0),
            Vec3::new(0.0, 3.0, 0.0),
            Vec3::new(0.0, 0.0, 2.0),
        );
        let plane_2 = NdPlane {
            point: Vec3::new(-1.0, 0.0, 0.0),
            normal: Vec3::new(6.0, -2.0, -3.0).normalize(),
            distance: 0.857_142,
        };
        assert_relative_eq!(plane_1.normal, plane_2.normal, epsilon = 1e-3);
        assert_relative_eq!(plane_1.distance, plane_2.distance, epsilon = 1e-3);

        let distance = plane_1.distance_to_point(Vec3::new(0.0, -2.0, 0.0));
        assert_relative_eq!(distance, 1.428_571, epsilon = 1e-3);
    }

    #[test]
    fn plane_intersection() {
        let a = Vec3::new(1.0, 2.0, -1.0);
        let b = Vec3::new(3.0, -2.0, 1.0);
        let c = Vec3::new(2.0, 3.0, 0.0);
        let d = Vec3::new(-3.0, 2.0, 4.0);

        let plane_1 = NdPlane::from_points(a, b, c);
        let plane_2 = NdPlane::from_points(a, c, d);
        let plane_3 = NdPlane::from_points(c, b, d);

        let intersection = NdPlane::intersect(&plane_1, &plane_2, &plane_3, 1e-3).unwrap();

        assert_relative_eq!(intersection, c, epsilon = 1e-3);
    }

    #[test]
    fn center_calculation() {
        let points = vec![
            Vec3::new(2.0, 0.0, 0.0),
            Vec3::new(0.0, 2.0, 1.0),
            Vec3::new(-2.0, 0.0, 2.0),
            Vec3::new(0.0, -2.0, 1.0),
        ];

        let center = polygon_center(points.into_iter());

        assert_relative_eq!(center, Vec3::new(0.0, 0.0, 1.0), epsilon = 1e-3);
    }

    #[test]
    fn normal_calculation() {
        let points = vec![
            Vec3::new(2.0, 0.0, 0.0),
            Vec3::new(0.0, 2.0, 1.0),
            Vec3::new(-2.0, 0.0, 2.0),
            Vec3::new(0.0, -2.0, 1.0),
        ];

        let normal = polygon_normal(points.into_iter());

        assert_relative_eq!(normal, Vec3::new(0.447_213, 0.0, 0.894_427), epsilon = 1e-3);
    }

    #[test]
    fn plane_line_intersection() {
        let plane = NdPlane::from_points(
            Vec3::new(0.0, -3.0, 0.0),
            Vec3::new(0.0, 0.0, 3.0),
            Vec3::new(2.0, 0.0, 0.0),
        );

        assert_relative_eq!(
            plane
                .intersect_line(Vec3::new(-1.0, 0.0, 0.0), Vec3::new(1.0, 0.0, 2.0), 1e-3)
                .unwrap(),
            Vec3::new(0.285_714, 0.0, 2.571_428),
            epsilon = 1e-3,
        );

        let (point, factor) = plane
            .intersect_line_with_factor(Vec3::new(2.0, 0.0, 1.0), Vec3::new(0.0, 0.0, 1.0), 1e-3)
            .unwrap();

        assert_relative_eq!(point, Vec3::new(4.0 / 3.0, 0.0, 1.0), epsilon = 1e-3,);

        assert_relative_eq!(factor, 1.0 / 3.0, epsilon = 1e-3,);

        let plane_2 =
            NdPlane::from_point_normal(Vec3::new(-1.0, 0.0, 0.0), Vec3::new(-1.0, 0.0, 0.0));

        assert_relative_eq!(
            plane_2
                .intersect_line_with_factor(
                    Vec3::new(-2.0, -1.0, 0.0),
                    Vec3::new(0.0, 1.0, 0.0),
                    1e-3
                )
                .unwrap()
                .1,
            0.5,
            epsilon = 1e-3,
        );

        assert_relative_eq!(
            plane_2
                .intersect_line_with_factor(
                    Vec3::new(0.0, 1.0, 0.0),
                    Vec3::new(-4.0, 1.0, 0.0),
                    1e-3
                )
                .unwrap()
                .1,
            0.25,
            epsilon = 1e-3,
        );
    }

    #[test]
    fn affine_transformation() {
        let affine_matrix = affine_matrix(
            [
                Vec2::new(-35.3834, -48.1264),
                Vec2::new(-36.3596, 41.648),
                Vec2::new(32.6082, 62.2508),
            ],
            [
                Vec2::new(0.0, 1.0),
                Vec2::new(0.0, 0.0),
                Vec2::new(1.0, 0.0),
            ],
        );

        assert_relative_eq!(
            affine_transform_point(affine_matrix, Vec2::new(-35.3834, -48.1264)),
            Vec2::new(0.0, 1.0),
        );
    }

    #[test]
    fn point_line_side() {
        let line_a = Vec2::new(-2.0, -1.0);
        let line_b = Vec2::new(1.0, 0.0);
        let point = Vec2::new(-1.0, 1.0);

        assert!(is_point_left_of_line(line_a, line_b, point));

        let point = Vec2::new(2.0, 0.0);

        assert!(!is_point_left_of_line(line_a, line_b, point));
    }

    #[test]
    fn polygon_classification() {
        let plane = NdPlane::from_points(
            Vec3::new(0.0, -3.0, 0.0),
            Vec3::new(0.0, 0.0, 3.0),
            Vec3::new(2.0, 0.0, 0.0),
        );

        let polygon = [
            Vec3::new(0.0, -3.0, 0.0),
            Vec3::new(0.0, 0.0, 3.0),
            Vec3::new(2.0, 0.0, 0.0),
        ];

        assert_eq!(
            plane.classify_polygon(polygon.into_iter(), 1e-3),
            PolygonClassification::OnPlane,
        );

        let polygon_2 = [
            Vec3::new(-1.0, -2.0, 0.0),
            Vec3::new(-1.0, 3.0, 0.0),
            Vec3::new(2.0, 2.0, 0.0),
            Vec3::new(1.0, 1.0, 0.0),
        ];

        assert_eq!(
            plane.classify_polygon(polygon_2.into_iter(), 1e-3),
            PolygonClassification::Front,
        );

        let polygon_3 = [
            Vec3::new(-1.0, -2.0, 100.0),
            Vec3::new(-1.0, 3.0, 100.0),
            Vec3::new(2.0, 2.0, 100.0),
            Vec3::new(1.0, 1.0, 100.0),
        ];

        assert_eq!(
            plane.classify_polygon(polygon_3.into_iter(), 1e-3),
            PolygonClassification::Back,
        );

        let polygon_4 = [
            Vec3::new(-10.0, -20.0, 0.0),
            Vec3::new(-10.0, 30.0, 0.0),
            Vec3::new(20.0, 20.0, 0.0),
            Vec3::new(10.0, 10.0, 0.0),
        ];

        assert_eq!(
            plane.classify_polygon(polygon_4.into_iter(), 1e-3),
            PolygonClassification::Spanning,
        );
    }
}
