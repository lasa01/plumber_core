#![warn(clippy::all, clippy::pedantic, clippy::multiple_crate_versions)]
// conflicts with serde's convention of to_string etc. methods
#![allow(clippy::should_implement_trait)]

mod de;
mod error;
mod escape;
pub mod nom_utils;
mod parsers;
mod ser;
mod value;

pub use de::{escaped_from_bytes, escaped_from_str, from_bytes, from_str, Deserializer};
pub use error::{Error, Result};
pub use ser::{escaped_to_string, to_string, Serializer};
pub use value::Value;
