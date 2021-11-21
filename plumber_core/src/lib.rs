#![deny(clippy::all, clippy::pedantic, clippy::cargo)]
// this is intentional
#![allow(clippy::cast_precision_loss)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::cast_sign_loss)]
// causes too long living borrows
#![allow(clippy::option_if_let_else)]

pub mod asset;
pub mod fs;
pub mod model;
mod parsers;
pub mod steam;
pub mod vmf;
pub mod vmt;
