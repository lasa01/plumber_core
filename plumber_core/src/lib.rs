#![deny(clippy::all, clippy::pedantic, clippy::cargo)]
// this is intentional
#![allow(clippy::cast_precision_loss)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::cast_sign_loss)]

mod binary_utils;
#[cfg(feature = "fs")]
pub mod fs;
#[cfg(feature = "model")]
pub mod model;
mod parsers;
#[cfg(feature = "steam")]
pub mod steam;
#[cfg(feature = "vmf")]
pub mod vmf;
#[cfg(feature = "vmt")]
pub mod vmt;
