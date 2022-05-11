#![deny(clippy::all, clippy::pedantic, clippy::cargo)]
// this is intentional
#![allow(clippy::cast_precision_loss)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::cast_sign_loss)]
// causes too long living borrows
#![allow(clippy::option_if_let_else)]
// approx triggers this
#![allow(clippy::manual_assert)]

pub mod asset;
pub mod fs;
pub mod model;
mod parsers;
pub mod steam;
pub mod vmf;
pub mod vmt;

pub use plumber_uncased as uncased;
pub use plumber_vdf as vdf;
pub use plumber_vpk as vpk;

#[cfg(test)]
mod test_utils;
