#![deny(clippy::all, clippy::pedantic, clippy::cargo)]
#![allow(clippy::should_implement_trait)]

mod parsers;
pub mod vdf;
#[cfg(feature = "vmf")]
pub mod vmf;
