#![deny(clippy::all, clippy::pedantic, clippy::cargo)]
// conflicts with serde's convention of to_string etc. methods
#![allow(clippy::should_implement_trait)]
// causes too long living borrows
#![allow(clippy::clippy::map_unwrap_or)]
// this is intentional
#![allow(clippy::clippy::cast_precision_loss)]

mod parsers;
pub mod vdf;
#[cfg(feature = "vmf")]
pub mod vmf;
