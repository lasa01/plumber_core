#![deny(clippy::all, clippy::pedantic, clippy::cargo)]
// conflicts with serde's convention of to_string etc. methods
#![allow(clippy::should_implement_trait)]
// causes too long living borrows
#![allow(clippy::map_unwrap_or)]
// this is intentional
#![allow(clippy::cast_precision_loss)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::cast_sign_loss)]

#[cfg(feature = "fs")]
pub mod fs;
mod parsers;
#[cfg(feature = "steam")]
pub mod steam;
pub mod vdf;
#[cfg(feature = "vmf")]
pub mod vmf;
#[cfg(feature = "vpk")]
pub mod vpk;
