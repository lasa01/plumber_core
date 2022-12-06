mod builder_utils;
pub mod entities;
mod overlay_builder;
mod solid_builder;
mod types;
pub mod vmf;

pub mod builder {
    pub use super::builder_utils::{GeometrySettings, InvisibleSolids, MergeSolids};
    pub use super::overlay_builder::{BuiltOverlay, BuiltOverlayFace, OverlayError};
    pub use super::solid_builder::{
        BuiltBrushEntity, BuiltSolid, MergedSolids, SolidError, SolidFace,
    };
}

use plumber_vdf as vdf;

use vmf::Vmf;

/// # Errors
///
/// Returns `Err` if the deserialization fails.
pub fn from_bytes(input: &[u8]) -> vdf::Result<Vmf> {
    Vmf::from_bytes(input)
}

/// # Errors
///
/// Returns `Err` if the serialization fails.
pub fn to_string(vmf: &Vmf) -> vdf::Result<String> {
    vmf.to_string()
}
