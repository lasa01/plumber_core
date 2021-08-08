#![deny(clippy::all, clippy::pedantic, clippy::cargo)]
// this is intentional
#![allow(clippy::cast_precision_loss)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::cast_sign_loss)]
// causes too long living borrows
#![allow(clippy::option_if_let_else)]

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
