use std::{cmp::Ordering, collections::BTreeSet, iter, ptr};

use glam::Vec3;
use itertools::Itertools;
use rstar::{PointDistance, RTree, RTreeObject, AABB};
use thiserror::Error;

use crate::{
    builder_utils::{
        angle_difference, point_is_inside_polygon2, polygon_intersection, project_point_to_2d,
        signed_angle, GlobalSpace, LocalSpace, OtherSpace, PolygonIntersection,
    },
    entities::{EntityParseError, PointEntity, SkyCamera},
    solid_builder::{FaceBuilder, SolidBuilder},
};

impl<'a, 'b> RTreeObject for &'a SolidBuilder<'b> {
    type Envelope = AABB<[f32; 3]>;

    fn envelope(&self) -> Self::Envelope {
        let (min, max) = self.global_aabb();

        AABB::from_corners(min.to_array(), max.to_array())
    }
}

impl<'a, 'b> PointDistance for &'a SolidBuilder<'b> {
    fn distance_2(&self, point: &[f32; 3]) -> f32 {
        let distance = self.distance_to_point((*point).into());

        distance * distance
    }

    fn contains_point(&self, point: &[f32; 3]) -> bool {
        self.point_is_inside((*point).into())
    }
}

#[derive(Debug, Error)]
pub enum SkyDetectError {
    #[error("error parsing sky camera: {0}")]
    EntityParse(#[from] EntityParseError),
}

struct SolidTree<'a> {
    r_tree: RTree<&'a SolidBuilder<'a>>,
}

impl<'a> SolidTree<'a> {
    fn new(solids: &'a [SolidBuilder<'a>]) -> Self {
        let solids = solids
            .iter()
            .filter(|solid| !solid.is_displacement())
            .by_ref()
            .collect();

        Self {
            r_tree: RTree::bulk_load(solids),
        }
    }

    fn nearest_solids_iter(&self, point: Vec3) -> impl Iterator<Item = &'a SolidBuilder<'a>> + '_ {
        self.r_tree
            .nearest_neighbor_iter(&point.to_array())
            .copied()
    }

    /// Returns a subset of all faces that might collide with the specified faces,
    /// to avoid doing more expensive accurate collision checking for all faces.
    fn maybe_colliding_faces<'b>(
        &'b self,
        face: &'b CollidingFace,
    ) -> impl Iterator<Item = CollidingFace<'a>> + 'b {
        let (min, max) = face.face.global_aabb(face.solid);
        let aabb = AABB::from_corners(min.to_array(), max.to_array());

        self.r_tree
            .locate_in_envelope_intersecting(&aabb)
            .copied()
            .flat_map(|solid| {
                solid
                    .faces()
                    .iter()
                    .map(|face| CollidingFace { solid, face })
            })
    }

    /// Returns faces that actually collide with the specified face.
    fn colliding_faces<'b>(
        &'b self,
        face: &'b CollidingFace,
    ) -> impl Iterator<Item = CollidingFace<'a>> + 'b {
        self.maybe_colliding_faces(face).filter(|other| {
            other
                .face
                .collides_with(other.solid, face.face, face.solid, 0.001)
        })
    }
}

#[derive(Debug, Clone, Copy)]
struct CollidingFace<'a> {
    solid: &'a SolidBuilder<'a>,
    face: &'a FaceBuilder<'a>,
}

#[derive(Debug, Clone, Copy)]
struct VisitedFace<'a> {
    solid: &'a SolidBuilder<'a>,
    face: &'a FaceBuilder<'a>,
}

impl<'a> From<CollidingFace<'a>> for VisitedFace<'a> {
    fn from(f: CollidingFace<'a>) -> Self {
        Self {
            solid: f.solid,
            face: f.face,
        }
    }
}

impl<'a, 'b> From<&'b FaceToVisit<'a>> for VisitedFace<'a> {
    fn from(f: &'b FaceToVisit<'a>) -> Self {
        Self {
            solid: f.solid,
            face: f.face,
        }
    }
}

impl<'a, 'b> From<&'b FaceCollision<'a>> for VisitedFace<'a> {
    fn from(f: &'b FaceCollision<'a>) -> Self {
        Self {
            solid: f.solid,
            face: f.face,
        }
    }
}

impl<'a> PartialOrd for VisitedFace<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'a> Ord for VisitedFace<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
        (self.face as *const _ as usize).cmp(&(other.face as *const _ as usize))
    }
}

impl<'a> PartialEq for VisitedFace<'a> {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.face, other.face)
    }
}

impl<'a> Eq for VisitedFace<'a> {}

#[derive(Debug)]
struct FaceToVisit<'a> {
    face: &'a FaceBuilder<'a>,
    solid: &'a SolidBuilder<'a>,
    initial_point: Vec3,
}

impl<'a, 'b> From<&'b FaceToVisit<'a>> for CollidingFace<'a> {
    fn from(val: &'b FaceToVisit<'a>) -> Self {
        CollidingFace {
            solid: val.solid,
            face: val.face,
        }
    }
}

pub(crate) fn detect(solids: &[SolidBuilder], sky_camera: SkyCamera) -> Result<(), SkyDetectError> {
    let solid_tree = SolidTree::new(solids);

    let sky_camera_point = sky_camera.origin()?;

    for solid in solid_tree.nearest_solids_iter(sky_camera_point) {
        if let Some(face) = solid.closest_face_to_point(sky_camera_point) {
            let (initial_point, _) =
                face.closest_point_to_point(solid, GlobalSpace, sky_camera_point);

            detect_continuous_face_surface(
                FaceToVisit {
                    face,
                    solid,
                    initial_point,
                },
                &solid_tree,
            );
        }
    }

    Ok(())
}

#[derive(Debug, Clone, Copy, Default)]
enum DfsStatus {
    /// Search has not reached this node
    #[default]
    New,
    /// Search is in progress for this node
    InProgress,
    /// Search is finished for this node
    Finished,
}

#[derive(Debug, Clone)]
struct FaceCollision<'a> {
    face: &'a FaceBuilder<'a>,
    solid: &'a SolidBuilder<'a>,
    intersecting_edge: [Vec3; 2],
    edge_angles: [f32; 2],
}

impl<'a> FaceCollision<'a> {
    fn intersection_middle(&self) -> Vec3 {
        (self.intersecting_edge[0] + self.intersecting_edge[1]) / 2.0
    }
}

#[derive(Debug, Clone)]
struct DfsSource {
    index: usize,
    /// The start point of this face in the loop.
    /// Also equal to the end point of the previous face.
    /// The end point cannot be stored on the previous face,
    /// since there can be multiple next faces with different end points for each.
    start_point: Vec3,
}

#[derive(Debug, Clone, Default)]
struct DfsState {
    status: DfsStatus,
    total_length: f32,
    source: Option<DfsSource>,
}

fn detect_continuous_face_surface(initial_face: FaceToVisit, solid_tree: &SolidTree) {
    let mut visited_faces = BTreeSet::new();

    let mut faces_to_visit = vec![initial_face];

    while let Some(current_face) = faces_to_visit.pop() {
        if !visited_faces.insert(VisitedFace::from(&current_face)) {
            continue;
        }

        let maybe_colliding_faces: Vec<_> = solid_tree
            .maybe_colliding_faces(&CollidingFace::from(&current_face))
            .collect();

        let normal = current_face.face.plane_normal();
        let angle_reference = normal.any_orthonormal_vector();

        // Figure out the edges from the face collisions,
        // faces which collide with the reference face at a single point or at a polygon
        // can be ignored since they cannot form face loops
        // Also calculate the face angles around the reference point

        let mut face_collisions: Vec<_> = maybe_colliding_faces
            .iter()
            .filter_map(|f| {
                match current_face.face.intersection(
                    current_face.solid,
                    LocalSpace,
                    f.face,
                    f.solid,
                    OtherSpace(current_face.solid),
                    0.001,
                ) {
                    PolygonIntersection::Edge(mut intersecting_edge) => {
                        let mut edge_angles = intersecting_edge.map(|p| {
                            signed_angle(normal, angle_reference, p - current_face.initial_point)
                        });

                        // Make sure the edge is in correct order
                        if angle_difference(edge_angles[0], edge_angles[1]) > 0.0 {
                            edge_angles.swap(0, 1);
                            intersecting_edge.swap(0, 1);
                        }

                        Some(FaceCollision {
                            face: f.face,
                            solid: f.solid,
                            intersecting_edge,
                            edge_angles,
                        })
                    }
                    _ => None,
                }
            })
            .collect();

        face_collisions.sort_unstable_by(|a, b| {
            f32::total_cmp(
                &a.face.distance_to_point(
                    a.solid,
                    OtherSpace(current_face.solid),
                    current_face.initial_point,
                ),
                &b.face.distance_to_point(
                    a.solid,
                    OtherSpace(current_face.solid),
                    current_face.initial_point,
                ),
            )
        });

        let mut search_states: Vec<_> = iter::repeat(DfsState::default())
            .take(face_collisions.len())
            .collect();

        // Try to find a closed loop as close as possible to the initial point
        for closest_index in 0..face_collisions.len() {
            let closest_state = &search_states[closest_index];

            if let DfsStatus::New = closest_state.status {
                if let Some(candidate) = detect_face_loop(
                    &current_face,
                    &face_collisions,
                    &mut search_states,
                    closest_index,
                ) {
                    for face in iter_face_loop_faces(candidate, &face_collisions, &search_states) {
                        faces_to_visit.push(face);
                    }
                }
            }
        }

        // TODO: extend faces to visit
    }
}

struct LoopCandidate {
    distance: f32,
    last_index: usize,
}

fn detect_face_loop<'a>(
    reference_face: &FaceToVisit,
    all_faces: &[FaceCollision<'a>],
    search_states: &mut [DfsState],
    start_index: usize,
) -> Option<LoopCandidate> {
    let mut best_candidate: Option<LoopCandidate> = None;
    // Point on the starting face from which the current path we're going through started
    let mut current_path_start_point = Vec3::ZERO;

    let mut to_visit = vec![start_index];
    search_states[start_index].total_length = 0.0;

    while let Some(current_index) = to_visit.pop() {
        let current = &all_faces[current_index];
        let current_state = &mut search_states[current_index];

        let DfsStatus::New = current_state.status else {
            current_state.status = DfsStatus::Finished;
            continue;
        };

        current_state.status = DfsStatus::InProgress;
        to_visit.push(current_index);

        // Iterate through all the faces except the current face
        let other_indices = (0..all_faces.len()).filter(|&i| i != current_index);

        for other_index in other_indices {
            let other = &all_faces[other_index];

            let intersection_point = match other.face.intersection(
                other.solid,
                OtherSpace(reference_face.solid),
                current.face,
                current.solid,
                OtherSpace(reference_face.solid),
                0.001,
            ) {
                // Filter to only faces colliding with the next face
                PolygonIntersection::None => continue,
                // Need to also check if the collision happens on the reference face,
                // not interested in this if it doesn't
                PolygonIntersection::Point(point) => {
                    if reference_face.face.collides_with_point(
                        reference_face.solid,
                        LocalSpace,
                        point,
                        0.001,
                    ) {
                        point
                    } else {
                        continue;
                    }
                }
                PolygonIntersection::Edge(edge) => {
                    let intersection_with_edge = reference_face.face.intersection_with_edge(
                        reference_face.solid,
                        LocalSpace,
                        edge,
                        0.001,
                    );

                    if let Some(point) = intersection_with_edge {
                        point
                    } else {
                        continue;
                    }
                }
                // If the face is coplanar with the other face,
                // need ensure that the intersection intersects with the reference face
                // and select a point on that intersection
                PolygonIntersection::Polygon(intersection) => {
                    match polygon_intersection(
                        reference_face
                            .face
                            .iter_vertices(reference_face.solid, LocalSpace),
                        reference_face.face.plane(reference_face.solid, LocalSpace),
                        intersection.iter().copied(),
                        // Since the original intersection was a polygon intersection,
                        // the plane for the intersection is equal to current and other's planes
                        current
                            .face
                            .plane(current.solid, OtherSpace(reference_face.solid)),
                        0.001,
                    ) {
                        // Single collision point between all 3 faces, select that
                        PolygonIntersection::Point(point) => point,
                        // There is an edge in which the 3 faces collide, select the center point
                        PolygonIntersection::Edge([a, b]) => (a + b) / 2.0,
                        // A polygon intersection shouldn't be possible here, ignore the possibility
                        // If the collision doesn't happen on the reference face, this cannot contribute to the loop either
                        PolygonIntersection::Polygon(_) | PolygonIntersection::None => continue,
                    }
                }
            };

            let other_state = &search_states[other_index];

            match other_state.status {
                DfsStatus::New => {
                    let distance_to_other =
                        if let Some(source) = &search_states[current_index].source {
                            source.start_point.distance(intersection_point)
                        } else {
                            // For the direct descendants of the root node (= collides with starting face)
                            // store the distance to the middle of the root node and correct the distance
                            // when we know which portion of the first node (= face) should be taken into account
                            // Also store the start point for the correction later
                            current_path_start_point = intersection_point;

                            current.intersection_middle().distance(intersection_point)
                        };

                    search_states[other_index].total_length =
                        search_states[current_index].total_length + distance_to_other;

                    search_states[other_index].source = Some(DfsSource {
                        index: current_index,
                        start_point: intersection_point,
                    });

                    to_visit.push(other_index);
                }
                DfsStatus::InProgress => {
                    // Found a loop, we are only interested in it if it ends in the initial face,
                    // since otherwise it cannot encompass the reference point
                    if other_index != start_index {
                        continue;
                    }

                    let Some(current_source) = &search_states[current_index].source else {
                        // If we don't know where the source data of the next is, it means that next is the start index,
                        // and we can't form a loop with only one face
                        continue;
                    };

                    let distance_to_other = current_source.start_point.distance(intersection_point);
                    let mut distance =
                        search_states[current_index].total_length + distance_to_other;

                    // Correct the distance now that we know the start/end point of the loop
                    let start_middle = all_faces[start_index].intersection_middle();
                    if current_path_start_point.distance(intersection_point)
                        > current_path_start_point.distance(start_middle)
                    {
                        distance += start_middle.distance(intersection_point);
                    } else {
                        distance -= start_middle.distance(intersection_point);
                    }

                    if let Some(best) = &best_candidate {
                        if best.distance < distance {
                            continue;
                        }
                    }

                    // Iterates the starting edge of the loop and the starting point
                    let first = iter::once(intersection_point);
                    // Iterates all other edges and their starting points from the last to the first
                    let others = iter::successors(
                        Some((current_index, current_source.start_point)),
                        |&(i, _)| {
                            search_states[i].source.as_ref().map(|s| {
                                (
                                    s.index,
                                    search_states[s.index]
                                        .source
                                        .as_ref()
                                        // If this is None, it means that this is the starting edge,
                                        // and that the starting point is the intersection point we found
                                        .map_or(intersection_point, |s| s.start_point),
                                )
                            })
                        },
                    )
                    .map(|(_i, v)| v);
                    // Windows of the combined iterator iterates over every edge in the loop
                    let edge_iter = first.chain(others).tuple_windows();

                    // Check if the found loop encompasses the reference face's initial point

                    // Convert points onto 2d points on the reference face for point in polygon check
                    let (basis_x, basis_y) =
                        reference_face.face.plane_normal().any_orthonormal_pair();

                    let initial_point2 =
                        project_point_to_2d(reference_face.initial_point, basis_x, basis_y);

                    if !point_is_inside_polygon2(
                        initial_point2,
                        edge_iter.map(|(a, b)| {
                            (
                                project_point_to_2d(a, basis_x, basis_y),
                                project_point_to_2d(b, basis_x, basis_y),
                            )
                        }),
                        0.001,
                    ) {
                        continue;
                    }

                    best_candidate = Some(LoopCandidate {
                        distance,
                        last_index: current_index,
                    });
                }
                _ => {}
            }
        }
    }

    best_candidate
}

fn iter_face_loop_faces<'i, 'f>(
    loop_candidate: LoopCandidate,
    all_faces: &'i [FaceCollision<'f>],
    search_states: &'i [DfsState],
) -> impl Iterator<Item = FaceToVisit<'f>> + 'i {
    let face_indices = iter::successors(Some(loop_candidate.last_index), |&i| {
        search_states[i].source.as_ref().map(|s| s.index)
    });

    face_indices.map(|i| {
        let face = &all_faces[i];

        FaceToVisit {
            face: face.face,
            solid: face.solid,
            initial_point: todo!(),
        }
    })
}
