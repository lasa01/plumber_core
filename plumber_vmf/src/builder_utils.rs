use std::{f32::consts::PI, fmt::Debug, mem::swap};

use approx::{abs_diff_eq, relative_eq};
use glam::{Affine2, Mat3, Vec2, Vec3};
use itertools::Itertools;

use crate::{solid_builder::SolidBuilder, types::Plane};

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
    pub overlays: bool,
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

    pub fn overlays(&mut self, overlays: bool) {
        self.overlays = overlays;
    }
}

impl Default for GeometrySettings {
    fn default() -> Self {
        Self {
            epsilon: 1e-3,
            cut_threshold: 1e-3,
            merge_solids: MergeSolids::Merge,
            invisible_solids: InvisibleSolids::Skip,
            overlays: true,
        }
    }
}

pub(crate) trait Space: Clone + Copy {
    fn transform_into(&self, solid: &SolidBuilder, vertice: Vec3) -> Vec3;
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct LocalSpace;

impl Space for LocalSpace {
    fn transform_into(&self, _solid: &SolidBuilder, vertice: Vec3) -> Vec3 {
        vertice
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct GlobalSpace;

impl Space for GlobalSpace {
    fn transform_into(&self, solid: &SolidBuilder, vertice: Vec3) -> Vec3 {
        vertice + solid.position()
    }
}

/// The space of a different solid.
#[derive(Debug, Clone, Copy)]
pub(crate) struct OtherSpace<'a>(pub &'a SolidBuilder<'a>);

impl<'a> Space for OtherSpace<'a> {
    fn transform_into(&self, solid: &SolidBuilder, vertice: Vec3) -> Vec3 {
        vertice + solid.position() - self.0.position()
    }
}

/// An "average" space between two solids.
/// Can be used for collision checking etc., using this with 2 different solids
/// will result in the returned vertices being in the same space.
#[derive(Debug, Clone, Copy)]
pub(crate) struct CompatibleSpace<'a>(pub &'a SolidBuilder<'a>);

impl<'a> Space for CompatibleSpace<'a> {
    fn transform_into(&self, solid: &SolidBuilder, vertice: Vec3) -> Vec3 {
        vertice + solid.position() / 2.0 - self.0.position() / 2.0
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

    pub fn in_space(&self, solid: &SolidBuilder, space: impl Space) -> Self {
        let point = space.transform_into(solid, self.point);
        let distance = -point.dot(self.normal);

        Self {
            point,
            normal: self.normal,
            distance,
        }
    }

    pub fn distance_to_point(&self, point: Vec3) -> f32 {
        (point - self.point).dot(self.normal)
    }

    pub fn distance_to_origin(&self) -> f32 {
        self.point.dot(self.normal).abs()
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

pub(crate) fn distance_point_to_edge(edge_a: Vec3, edge_b: Vec3, point: Vec3) -> f32 {
    let ab = edge_b - edge_a;
    let ap = point - edge_a;

    if ap.dot(ab) <= 0.0 {
        return ap.length();
    }

    let bp = point - edge_b;

    if bp.dot(ab) >= 0.0 {
        return bp.length();
    }

    ab.cross(ap).length() / ab.length()
}

pub(crate) fn distance_point_to_edge2(edge_a: Vec2, edge_b: Vec2, point: Vec2) -> f32 {
    let ab_l2 = edge_a.distance_squared(edge_b);

    if ab_l2 == 0.0 {
        return point.distance(edge_a);
    }

    let t = ((point - edge_a).dot(edge_b - edge_a) / ab_l2).clamp(0.0, 1.0);

    let proj = edge_a + t * (edge_b - edge_a);

    point.distance(proj)
}

pub(crate) fn closest_point_on_edge_to_point(
    edge_a: Vec3,
    edge_b: Vec3,
    point: Vec3,
) -> (Vec3, f32) {
    let ab = edge_b - edge_a;
    let ap = point - edge_a;

    if ap.dot(ab) <= 0.0 {
        return (edge_a, ap.length());
    }

    let bp = point - edge_b;

    if bp.dot(ab) >= 0.0 {
        return (edge_b, bp.length());
    }

    let closest = edge_a + ap.project_onto(ab);
    let distance = (closest - point).length();

    (closest, distance)
}

/// Right-handed signed angle between vectors, based on a normal vector.
/// Returns an angle between [-PI, PI] radians.
pub(crate) fn signed_angle(n: Vec3, a: Vec3, b: Vec3) -> f32 {
    f32::atan2(a.cross(b).dot(n), a.dot(b))
}

/// Returns difference between angles, taking into account possible wrapping around.
/// All angles are in radians, returns an angle between [-PI, PI] radians.
pub(crate) fn angle_difference(a: f32, b: f32) -> f32 {
    let diff = (b - a) % (2.0 * PI);

    if diff >= PI {
        diff - 2.0 * PI
    } else if diff < -PI {
        diff + 2.0 * PI
    } else {
        diff
    }
}

pub(crate) fn sort_polygon<T>(polygon: &mut [T], normal: Vec3, get_vert: impl Fn(&T) -> Vec3) {
    let center = polygon_center(polygon.iter().map(&get_vert));

    for i in 0..polygon.len() - 2 {
        let vertice = get_vert(&polygon[i]);
        let to_current = (vertice - center).normalize();
        let filter_plane = NdPlane::from_points(vertice, center, center + normal);

        if let Some((next_idx, _)) = polygon[i + 1..]
            .iter()
            .enumerate()
            .filter(|(_, vi)| filter_plane.distance_to_point(get_vert(vi)) >= 0.0)
            .map(|(i, vi)| {
                let to_candidate = (get_vert(vi) - center).normalize();
                let dot = to_current.dot(to_candidate);
                (i, dot)
            })
            // max because smaller angle -> bigger dot product
            .max_by(|(_, a), (_, b)| a.total_cmp(b))
        {
            polygon.swap(i + 1, i + 1 + next_idx);
        }
    }

    // reverse if the normal is facing the wrong way
    if polygon_normal(polygon.iter().map(&get_vert)).dot(normal) < 0.0 {
        polygon.reverse();
    }
}

/// Returns whether a point is inside a polygon, assuming that the point and the polygon are on the same plane.
/// The edges must be in CCW winding order.
/// Only works for convex polygons.
pub(crate) fn point_is_inside_polygon(
    point: Vec3,
    edges: impl Iterator<Item = (Vec3, Vec3)>,
    polygon_normal: Vec3,
    epsilon: f32,
) -> bool {
    for (a, b) in edges {
        if relative_eq!(point, a, epsilon = epsilon) {
            return true;
        }

        let edge_vector = b - a;
        let comparison_vector = point - a;

        let rotation = signed_angle(polygon_normal, edge_vector, comparison_vector);

        // Signed rotation must be positive if the vertice is inside,
        // Since the vertices are in ccw (right handed) winding order
        if !relative_eq!(rotation, 0.0, epsilon = epsilon) && !rotation.is_sign_positive() {
            // this vertice is outside
            return false;
        }
    }

    true
}

/// Returns whether a point is inside a polygon in 2D.
/// The edges must be in sorted order, winding order doesn't matter.
/// Works for concave and convex polygons.
/// Points near the edges are considered inside the polygon.
// https://stackoverflow.com/a/43896965
pub(crate) fn point_is_inside_polygon2(
    point: Vec2,
    edges: impl Iterator<Item = (Vec2, Vec2)>,
    epsilon: f32,
) -> bool {
    let mut inside = false;

    for (edge_a, edge_b) in edges {
        // Check if we are on the edge, and consider us inside the polygon in that case.
        if relative_eq!(
            distance_point_to_edge2(edge_a, edge_b, point),
            0.0,
            epsilon = epsilon
        ) {
            return true;
        }

        // First check if the line crosses the horizontal line at point.y in either direction.
        if (edge_a.y <= point.y) && (edge_b.y > point.y)
            || (edge_b.y <= point.y) && (edge_a.y > point.y)
        {
            // If so, get the point where it crosses that line. This is a simple solution
            // to a linear equation. Note that we can't get a division by zero here -
            // if edge_b.y == edge_a.y then the above if will be false.
            let cross =
                (edge_b.x - edge_a.x) * (point.y - edge_a.y) / (edge_b.y - edge_a.y) + edge_a.x;

            // Finally check if it crosses to the left of our test point.
            if cross < point.x {
                inside = !inside;
            }
        }
    }

    inside
}

/// Projects a 3D point onto a 2D coordinate system, returning a 2D point.
/// The coordinate system is specified with 2 ortonormal vectors.
pub fn project_point_to_2d(point: Vec3, basis_x: Vec3, basis_y: Vec3) -> Vec2 {
    Vec2::new(basis_x.dot(point), basis_y.dot(point))
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum PolygonIntersection {
    /// The polygons do not collide / intersect.
    None,
    /// The polygons collide at a single point.
    Point(Vec3),
    /// The polygons collide at an edge.
    Edge([Vec3; 2]),
    /// The polygons are coplanar and collide at a .
    Polygon(Vec<Vec3>),
}

/// Determines whether a point is inside the edge of a polygon,
/// ie. on the side of the edge which would be inside the polygon.
/// The edge must be a part of a convex polygon and in CCW winding order relative to the normal.
/// Points on the edge or close to the edge (based on epsilon) are considered to be inside the edge.
fn is_point_inside_edge(
    normal: Vec3,
    edge_start: Vec3,
    edge_direction: Vec3,
    point: Vec3,
    epsilon: f32,
) -> bool {
    relative_eq!(point, edge_start, epsilon = epsilon)
        || signed_angle(normal, edge_direction, point - edge_start) > -epsilon
}

/// Sutherland–Hodgman algorithm, modified for 3d.
/// The normals of the polygons must match (which depend on winding order).
fn coplanar_polygon_intersection(
    a_vertices: Vec<Vec3>,
    b_vertices: impl Iterator<Item = Vec3> + Clone + ExactSizeIterator,
    plane: NdPlane,
    epsilon: f32,
) -> Vec<Vec3> {
    let mut output_polygon = a_vertices;
    let mut input_polygon = Vec::with_capacity(output_polygon.len());

    // Clip a with each of b's edges
    for (edge_start, edge_end) in b_vertices.circular_tuple_windows() {
        // Input of the next iteration is the output of the last iteration
        swap(&mut output_polygon, &mut input_polygon);
        // For the output of this iteration, we can reuse the previous Vec
        output_polygon.clear();

        let edge_direction = edge_end - edge_start;
        let cut_plane_normal = plane.normal.cross(edge_direction).normalize();
        let cut_plane = NdPlane::from_point_normal(edge_start, cut_plane_normal);

        for (&prev, &current) in input_polygon.iter().circular_tuple_windows() {
            let Some(intersection) = cut_plane.intersect_line(prev, current - prev, epsilon) else {
                if is_point_inside_edge(plane.normal, edge_start, edge_direction, current, epsilon) {
                    output_polygon.push(current);
                }

                continue;
            };

            if is_point_inside_edge(plane.normal, edge_start, edge_direction, current, epsilon) {
                if !is_point_inside_edge(plane.normal, edge_start, edge_direction, prev, epsilon)
                    && !relative_eq!(intersection, current, epsilon = epsilon)
                {
                    output_polygon.push(intersection);
                }

                output_polygon.push(current);
            } else if is_point_inside_edge(plane.normal, edge_start, edge_direction, prev, epsilon)
                && !relative_eq!(intersection, prev, epsilon = epsilon)
            {
                output_polygon.push(intersection);
            }
        }
    }

    output_polygon
}

/// Only works for convex polygons.
/// The returned intersection is in the winding order of the first polygon (in case of a polygon intersection).
pub(crate) fn polygon_intersection(
    a_vertices: impl Iterator<Item = Vec3> + Clone + ExactSizeIterator,
    a_plane: NdPlane,
    b_vertices: impl Iterator<Item = Vec3> + Clone + ExactSizeIterator,
    b_plane: NdPlane,
    epsilon: f32,
) -> PolygonIntersection {
    let dot = a_plane.normal.dot(b_plane.normal);

    // Check if polygons are coplanar
    if relative_eq!(dot, 1.0, epsilon = epsilon) || relative_eq!(dot, -1.0, epsilon = epsilon) {
        // If distances to the origin are same => the planes are coincident
        if relative_eq!(
            a_plane.distance_to_origin(),
            b_plane.distance_to_origin(),
            epsilon = epsilon
        ) {
            let mut b_vertices = b_vertices.collect_vec();

            // If the polygon normals are opposite, need to flip it for one of the polygons
            if dot < 0.0 {
                b_vertices.reverse();
            }

            let intersection =
                coplanar_polygon_intersection(b_vertices, a_vertices, a_plane, epsilon);

            if intersection.is_empty() {
                return PolygonIntersection::None;
            }

            if let Ok::<[Vec3; 1], _>([a]) = intersection.as_slice().try_into() {
                return PolygonIntersection::Point(a);
            }

            if let Ok::<[Vec3; 2], _>([a, b]) = intersection.as_slice().try_into() {
                return PolygonIntersection::Edge([a, b]);
            }

            return PolygonIntersection::Polygon(intersection);
        }

        return PolygonIntersection::None;
    }

    let mut first_intersection = None;

    // Faces are not coplanar, find b's edges' intersections with a's plane and check if the intersection points are inside a
    for (b_a, b_b) in b_vertices.circular_tuple_windows() {
        if let Some((point, factor)) = a_plane.intersect_line_with_factor(b_a, b_b, epsilon) {
            if factor + epsilon < 0.0 || factor - epsilon > 1.0 {
                // No intersection
                continue;
            }

            if point_is_inside_polygon(
                point,
                a_vertices.clone().circular_tuple_windows(),
                a_plane.normal,
                epsilon,
            ) {
                if let Some(first_intersection) = first_intersection {
                    // Found the second intersection point => intersection is an edge, unless the points are equal
                    if relative_eq!(point, first_intersection, epsilon = epsilon) {
                        return PolygonIntersection::Point(point);
                    }

                    return PolygonIntersection::Edge([first_intersection, point]);
                }

                first_intersection = Some(point);
            }
        }
    }

    if let Some(intersection) = first_intersection {
        // Only one intersection point found
        PolygonIntersection::Point(intersection)
    } else {
        PolygonIntersection::None
    }
}

pub(crate) trait SliceExt<T> {
    /// Split a slice into three parts: all elements before index,
    /// the element at the index, and all elements after the index.
    fn split_off_mut(&mut self, index: usize) -> (&mut [T], &mut T, &mut [T]);
}

impl<T> SliceExt<T> for [T] {
    fn split_off_mut(&mut self, index: usize) -> (&mut [T], &mut T, &mut [T]) {
        let self_len = self.len();

        let (before, after) = self.split_at_mut(index);

        let Some((mid, after)) = after.split_first_mut() else {
            panic!("index out of bounds: the len is {} but the index is {}", self_len, index);
        };

        (before, mid, after)
    }
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

    #[test]
    #[allow(clippy::approx_constant)]
    fn point_to_edge_distance() {
        let edge_a = Vec3::new(1.0, 0.0, 0.0);
        let edge_b = Vec3::new(2.0, -1.0, -1.0);

        let point = Vec3::new(2.0, 0.0, 0.0);
        assert_relative_eq!(distance_point_to_edge(edge_a, edge_b, point), 0.816_496_58);

        let point = Vec3::new(0.0, 0.0, 1.0);
        assert_relative_eq!(distance_point_to_edge(edge_a, edge_b, point), 1.414_213_5);

        let point = Vec3::new(2.0, -3.0, -1.0);
        assert_relative_eq!(distance_point_to_edge(edge_a, edge_b, point), 2.0);
    }

    #[test]
    #[allow(clippy::approx_constant)]
    fn on_edge_to_point_closest_point() {
        let edge_a = Vec3::new(1.0, 0.0, 0.0);
        let edge_b = Vec3::new(2.0, -1.0, -1.0);

        let point = Vec3::new(2.0, 0.0, 0.0);
        let (closest, distance) = closest_point_on_edge_to_point(edge_a, edge_b, point);
        assert_relative_eq!(
            closest,
            Vec3::new(1.333_333_4, -0.333_333_34, -0.333_333_34)
        );
        assert_relative_eq!(distance, 0.816_496_58);

        let point = Vec3::new(0.0, 0.0, 1.0);
        let (closest, distance) = closest_point_on_edge_to_point(edge_a, edge_b, point);
        assert_relative_eq!(closest, edge_a);
        assert_relative_eq!(distance, 1.414_213_5);

        let point = Vec3::new(2.0, -3.0, -1.0);
        let (closest, distance) = closest_point_on_edge_to_point(edge_a, edge_b, point);
        assert_relative_eq!(closest, edge_b);
        assert_relative_eq!(distance, 2.0);
    }

    #[test]
    fn angle_signed() {
        let normal = Vec3::Z;

        assert_relative_eq!(
            signed_angle(normal, Vec3::Y, Vec3::X),
            -90.0_f32.to_radians(),
        );

        assert_relative_eq!(
            signed_angle(normal, Vec3::X, Vec3::Y),
            90.0_f32.to_radians(),
        );
    }

    #[test]
    fn difference_angle() {
        assert_relative_eq!(angle_difference(PI, 3.0 * PI / 2.0), PI / 2.0);
        assert_relative_eq!(angle_difference(PI, -PI), 0.0);
        assert_relative_eq!(angle_difference(-PI, PI), 0.0);
        assert_relative_eq!(
            angle_difference(5.0 * PI / 6.0, -5.0 * PI / 6.0),
            PI / 3.0,
            epsilon = 0.001
        );
    }

    #[test]
    fn is_point_inside_polygon2() {
        let polygon = [
            Vec2::new(-2.0, 1.0),
            Vec2::new(-1.0, 2.0),
            Vec2::new(2.0, 1.0),
            Vec2::new(3.0, -4.0),
            Vec2::new(-1.0, 1.0),
        ];

        assert!(point_is_inside_polygon2(
            Vec2::ONE,
            polygon.into_iter().circular_tuple_windows(),
            0.001,
        ));

        assert!(!point_is_inside_polygon2(
            Vec2::new(1.0, 2.0),
            polygon.into_iter().circular_tuple_windows(),
            0.001,
        ));

        for point in polygon {
            assert!(point_is_inside_polygon2(
                point,
                polygon.into_iter().circular_tuple_windows(),
                0.001,
            ));
        }
    }

    fn create_test_plane(vertices: &[Vec3]) -> NdPlane {
        NdPlane::from_points(vertices[0], vertices[1], vertices[2])
    }

    #[test]
    fn intersection_polygon() {
        let face_vertices = vec![
            Vec3::new(-2.0, 1.0, 0.0),
            Vec3::new(-2.0, 0.0, 0.0),
            Vec3::new(0.0, 0.0, 0.0),
            Vec3::new(-1.0, 1.0, 0.0),
        ];

        let nonintersecting_vertices = vec![
            Vec3::new(-2.0, 1.0, 1.0),
            Vec3::new(-2.0, 0.0, 1.0),
            Vec3::new(0.0, 0.0, 1.0),
            Vec3::new(-1.0, 1.0, 1.0),
        ];

        assert_eq!(
            polygon_intersection(
                face_vertices.iter().copied(),
                create_test_plane(&face_vertices),
                nonintersecting_vertices.iter().copied(),
                create_test_plane(&nonintersecting_vertices),
                0.001,
            ),
            PolygonIntersection::None,
        );

        let point_vertices = vec![
            Vec3::new(0.0, 1.0, 0.0),
            Vec3::new(-1.0, 1.0, 0.0),
            Vec3::new(0.0, 1.0, 1.0),
        ];

        assert_eq!(
            polygon_intersection(
                face_vertices.iter().copied(),
                create_test_plane(&face_vertices),
                point_vertices.iter().copied(),
                create_test_plane(&point_vertices),
                0.001,
            ),
            PolygonIntersection::Point(Vec3::new(-1.0, 1.0, 0.0)),
        );

        let edge_vertices = vec![
            Vec3::new(-1.0, 0.0, -1.0),
            Vec3::new(-2.0, 1.0, -1.0),
            Vec3::new(-2.0, 1.0, 1.0),
            Vec3::new(-1.0, 0.0, 1.0),
        ];

        let edge_intersection = polygon_intersection(
            face_vertices.iter().copied(),
            create_test_plane(&face_vertices),
            edge_vertices.iter().copied(),
            create_test_plane(&edge_vertices),
            0.001,
        );

        if let PolygonIntersection::Edge(edge) = edge_intersection {
            assert!(edge
                .iter()
                .any(|&v| relative_eq!(v, Vec3::new(-1.0, 0.0, 0.0))));
            assert!(edge
                .iter()
                .any(|&v| relative_eq!(v, Vec3::new(-2.0, 1.0, 0.0))));
        } else {
            panic!("Bad intersection: {:?}, expected edge", edge_intersection);
        }

        let mut coplanar_vertices = vec![
            Vec3::new(-1.5, 0.5, 0.0),
            Vec3::new(-1.5, -2.0, 0.0),
            Vec3::new(0.0, -2.0, 0.0),
            Vec3::new(0.0, 0.0, 0.0),
        ];

        let expected_polygon = PolygonIntersection::Polygon(vec![
            Vec3::new(-1.5, 0.0, 0.0),
            Vec3::new(0.0, 0.0, 0.0),
            Vec3::new(-1.5, 0.5, 0.0),
        ]);

        assert_eq!(
            polygon_intersection(
                face_vertices.iter().copied(),
                create_test_plane(&face_vertices),
                coplanar_vertices.iter().copied(),
                create_test_plane(&coplanar_vertices),
                0.001,
            ),
            expected_polygon
        );

        coplanar_vertices.reverse();

        assert_eq!(
            polygon_intersection(
                face_vertices.iter().copied(),
                create_test_plane(&face_vertices),
                coplanar_vertices.iter().copied(),
                create_test_plane(&coplanar_vertices),
                0.001,
            ),
            expected_polygon
        );

        let coplanar_vertices2 = vec![
            Vec3::new(-1.5, 0.0, 0.0),
            Vec3::new(-1.5, -2.0, 0.0),
            Vec3::new(0.0, -2.0, 0.0),
            Vec3::new(0.0, 0.0, 0.0),
        ];

        assert_eq!(
            polygon_intersection(
                face_vertices.iter().copied(),
                create_test_plane(&face_vertices),
                coplanar_vertices2.iter().copied(),
                create_test_plane(&coplanar_vertices2),
                0.001,
            ),
            PolygonIntersection::Edge([Vec3::new(0.0, 0.0, 0.0), Vec3::new(-1.5, 0.0, 0.0)])
        );

        let coplanar_vertices3 = vec![
            Vec3::new(-1.5, -1.0, 0.0),
            Vec3::new(-1.5, -2.0, 0.0),
            Vec3::new(0.0, -2.0, 0.0),
            Vec3::new(0.0, -0.0001, 0.0),
        ];

        assert_eq!(
            polygon_intersection(
                face_vertices.iter().copied(),
                create_test_plane(&face_vertices),
                coplanar_vertices3.iter().copied(),
                create_test_plane(&coplanar_vertices3),
                0.001,
            ),
            PolygonIntersection::Point(Vec3::new(0.0, -0.0001, 0.0))
        );
    }
}
