use std::fmt::Debug;

use super::Plane;

use approx::abs_diff_eq;
use itertools::Itertools;
use nalgebra::{geometry::Point3, Matrix2x3, Matrix3, Point2, Unit, Vector3};

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
pub struct GeometrySettings {
    pub(crate) epsilon: f64,
    pub(crate) cut_threshold: f64,
    pub(crate) merge_solids: MergeSolids,
    pub(crate) invisible_solids: InvisibleSolids,
}

impl GeometrySettings {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn epsilon(&mut self, epsilon: f64) {
        self.epsilon = epsilon;
    }

    pub fn cut_threshold(&mut self, cut_threshold: f64) {
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
    pub point: Point3<f64>,
    pub normal: Unit<Vector3<f64>>,
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
        let normal = Unit::new_normalize((b - a).cross(&(c - a)));
        let distance = -a.coords.dot(&normal);
        Self {
            point: *a,
            normal,
            distance,
        }
    }

    pub fn from_point_normal(point: Point3<f64>, normal: Unit<Vector3<f64>>) -> Self {
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

    pub fn classify_point(&self, point: &Point3<f64>, epsilon: f64) -> PointClassification {
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
        line_1: &Point3<f64>,
        line_2: &Point3<f64>,
        epsilon: f64,
    ) -> Option<(Point3<f64>, f64)> {
        let line_direction = (line_2 - line_1).normalize();
        if abs_diff_eq!(self.normal.dot(&line_direction), 0.0, epsilon = epsilon) {
            return None;
        }
        let t = (self.normal.dot(&self.point.coords) - self.normal.dot(&line_1.coords))
            / self.normal.dot(&line_direction);

        let point = line_1 + line_direction * t;
        let factor = t / (line_2 - line_1).magnitude();

        Some((point, factor))
    }

    pub fn intersect_line(
        &self,
        line_point: &Point3<f64>,
        line_direction: &Vector3<f64>,
        epsilon: f64,
    ) -> Option<Point3<f64>> {
        let line_direction = line_direction.normalize();
        if abs_diff_eq!(self.normal.dot(&line_direction), 0.0, epsilon = epsilon) {
            return None;
        }
        let t = (self.normal.dot(&self.point.coords) - self.normal.dot(&line_point.coords))
            / self.normal.dot(&line_direction);
        Some(line_point + line_direction * t)
    }

    pub fn intersect(a: &NdPlane, b: &NdPlane, c: &NdPlane, epsilon: f64) -> Option<Point3<f64>> {
        let denominator = a.normal.dot(&b.normal.cross(&c.normal));
        if abs_diff_eq!(denominator, 0.0, epsilon = epsilon) {
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

    pub fn classify_polygon<I>(&self, polygon: I, epsilon: f64) -> PolygonClassification
    where
        I: Iterator<Item = Point3<f64>> + ExactSizeIterator,
    {
        let points_len = polygon.len();
        let mut points_front = 0_usize;
        let mut points_back = 0_usize;
        let mut points_on_plane = 0_usize;

        for point in polygon {
            match self.classify_point(&point, epsilon) {
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PointClassification {
    Front,
    Back,
    OnPlane,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PolygonClassification {
    Front,
    Back,
    OnPlane,
    Spanning,
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
    lhs * (1.0 - t) + rhs * t
}

pub(crate) fn lerp_uv(lhs: [f64; 2], rhs: [f64; 2], t: f64) -> [f64; 2] {
    [lerp(lhs[0], rhs[0], t), lerp(lhs[1], rhs[1], t)]
}

pub(crate) fn affine_matrix(
    src_points: [Point2<f64>; 3],
    dst_points: [Point2<f64>; 3],
) -> Option<Matrix2x3<f64>> {
    let src_matrix = Matrix3::new(
        src_points[0].x,
        src_points[1].x,
        src_points[2].x,
        src_points[0].y,
        src_points[1].y,
        src_points[2].y,
        1.0,
        1.0,
        1.0,
    )
    .try_inverse()?;

    let dst_matrix = Matrix2x3::new(
        dst_points[0].x,
        dst_points[1].x,
        dst_points[2].x,
        dst_points[0].y,
        dst_points[1].y,
        dst_points[2].y,
    );

    Some(dst_matrix * src_matrix)
}

pub(crate) fn affine_transform_point(matrix: &Matrix2x3<f64>, point: Point2<f64>) -> Point2<f64> {
    matrix.remove_column(2) * point + matrix.column(2)
}

pub(crate) fn is_point_left_of_line(
    line_a: &Point2<f64>,
    line_b: &Point2<f64>,
    point: &Point2<f64>,
) -> bool {
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
            &Point3::new(-1.0, 0.0, 0.0),
            &Point3::new(0.0, 3.0, 0.0),
            &Point3::new(0.0, 0.0, 2.0),
        );
        let plane_2 = NdPlane {
            point: Point3::new(-1.0, 0.0, 0.0),
            normal: Unit::new_normalize(Vector3::new(6.0, -2.0, -3.0)),
            distance: 0.857_142_857_142_857,
        };
        assert_relative_eq!(plane_1.normal, plane_2.normal, epsilon = 1e-3);
        assert_relative_eq!(plane_1.distance, plane_2.distance, epsilon = 1e-3);

        let distance = plane_1.distance_to_point(&Point3::new(0.0, -2.0, 0.0));
        assert_relative_eq!(distance, 1.428_571_428_571_428, epsilon = 1e-3);
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

        let intersection = NdPlane::intersect(&plane_1, &plane_2, &plane_3, 1e-3).unwrap();

        assert_relative_eq!(intersection, c, epsilon = 1e-3);
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

        assert_relative_eq!(center, Point3::new(0.0, 0.0, 1.0), epsilon = 1e-3);
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
            epsilon = 1e-3
        );
    }

    #[test]
    fn lerping() {
        assert_relative_eq!(5.0, lerp(-5.0, 10.0, 2.0 / 3.0));
    }

    #[test]
    fn plane_line_intersection() {
        let plane = NdPlane::from_points(
            &Point3::new(0.0, -3.0, 0.0),
            &Point3::new(0.0, 0.0, 3.0),
            &Point3::new(2.0, 0.0, 0.0),
        );

        assert_relative_eq!(
            plane
                .intersect_line(
                    &Point3::new(-1.0, 0.0, 0.0),
                    &Vector3::new(1.0, 0.0, 2.0),
                    1e-3
                )
                .unwrap(),
            Point3::new(0.285_714_285_7, 0.0, 2.571_428_571_4),
            epsilon = 1e-3,
        );

        let (point, factor) = plane
            .intersect_line_with_factor(
                &Point3::new(2.0, 0.0, 1.0),
                &Point3::new(0.0, 0.0, 1.0),
                1e-3,
            )
            .unwrap();

        assert_relative_eq!(point, Point3::new(4.0 / 3.0, 0.0, 1.0), epsilon = 1e-3,);

        assert_relative_eq!(factor, 1.0 / 3.0, epsilon = 1e-3,);

        let plane_2 = NdPlane::from_point_normal(
            Point3::new(-1.0, 0.0, 0.0),
            Unit::new_normalize(Vector3::new(-1.0, 0.0, 0.0)),
        );

        assert_relative_eq!(
            plane_2
                .intersect_line_with_factor(
                    &Point3::new(-2.0, -1.0, 0.0),
                    &Point3::new(0.0, 1.0, 0.0),
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
                    &Point3::new(0.0, 1.0, 0.0),
                    &Point3::new(-4.0, 1.0, 0.0),
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
                Point2::new(-35.3834, -48.1264),
                Point2::new(-36.3596, 41.648),
                Point2::new(32.6082, 62.2508),
            ],
            [
                Point2::new(0.0, 1.0),
                Point2::new(0.0, 0.0),
                Point2::new(1.0, 0.0),
            ],
        )
        .unwrap();

        assert_relative_eq!(
            affine_transform_point(&affine_matrix, Point2::new(-35.3834, -48.1264)),
            Point2::new(0.0, 1.0),
        );
    }

    #[test]
    fn point_line_side() {
        let line_a = Point2::new(-2.0, -1.0);
        let line_b = Point2::new(1.0, 0.0);
        let point = Point2::new(-1.0, 1.0);

        assert!(is_point_left_of_line(&line_a, &line_b, &point));

        let point = Point2::new(2.0, 0.0);

        assert!(!is_point_left_of_line(&line_a, &line_b, &point));
    }

    #[test]
    fn polygon_classification() {
        let plane = NdPlane::from_points(
            &Point3::new(0.0, -3.0, 0.0),
            &Point3::new(0.0, 0.0, 3.0),
            &Point3::new(2.0, 0.0, 0.0),
        );

        let polygon = [
            Point3::new(0.0, -3.0, 0.0),
            Point3::new(0.0, 0.0, 3.0),
            Point3::new(2.0, 0.0, 0.0),
        ];

        assert_eq!(
            plane.classify_polygon(polygon.into_iter(), 1e-3),
            PolygonClassification::OnPlane,
        );

        let polygon_2 = [
            Point3::new(-1.0, -2.0, 0.0),
            Point3::new(-1.0, 3.0, 0.0),
            Point3::new(2.0, 2.0, 0.0),
            Point3::new(1.0, 1.0, 0.0),
        ];

        assert_eq!(
            plane.classify_polygon(polygon_2.into_iter(), 1e-3),
            PolygonClassification::Front,
        );

        let polygon_3 = [
            Point3::new(-1.0, -2.0, 100.0),
            Point3::new(-1.0, 3.0, 100.0),
            Point3::new(2.0, 2.0, 100.0),
            Point3::new(1.0, 1.0, 100.0),
        ];

        assert_eq!(
            plane.classify_polygon(polygon_3.into_iter(), 1e-3),
            PolygonClassification::Back,
        );

        let polygon_4 = [
            Point3::new(-10.0, -20.0, 0.0),
            Point3::new(-10.0, 30.0, 0.0),
            Point3::new(20.0, 20.0, 0.0),
            Point3::new(10.0, 10.0, 0.0),
        ];

        assert_eq!(
            plane.classify_polygon(polygon_4.into_iter(), 1e-3),
            PolygonClassification::Spanning,
        );
    }
}
