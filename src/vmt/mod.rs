pub mod loader;

use crate::{
    fs::{self, PathBuf},
    vdf,
};

use std::{collections::BTreeMap, fmt, io};

use serde::{
    de::{self, MapAccess, Visitor},
    ser::SerializeMap,
    Deserialize, Deserializer, Serialize, Serializer,
};
use serde_derive::{Deserialize, Serialize};
use thiserror::Error;
use uncased::Uncased;

type UncasedString = Uncased<'static>;

/// # Errors
///
/// Returns `Err` if the deserialization fails.
pub fn from_str(input: &str) -> vdf::Result<Vmt> {
    Vmt::from_str(input)
}

/// # Errors
///
/// Returns `Err` if the serialization fails.
pub fn to_string(vmt: &Vmt) -> vdf::Result<String> {
    vmt.to_string()
}

#[derive(Debug, Clone, PartialEq)]
enum StringOrPatch {
    Patch,
    String(String),
}

impl<'de> Deserialize<'de> for StringOrPatch {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct PatchOrStringVisitor;

        impl<'de> Visitor<'de> for PatchOrStringVisitor {
            type Value = StringOrPatch;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a string")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                if v.eq_ignore_ascii_case("patch") {
                    Ok(StringOrPatch::Patch)
                } else {
                    Ok(StringOrPatch::String(v.into()))
                }
            }

            fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                if v.eq_ignore_ascii_case("patch") {
                    Ok(StringOrPatch::Patch)
                } else {
                    Ok(StringOrPatch::String(v))
                }
            }
        }

        deserializer.deserialize_string(PatchOrStringVisitor)
    }
}

#[derive(Debug, Clone)]
enum ShaderOrPatch {
    Shader(Shader),
    Patch(Patch),
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct Patch {
    include: PathBuf,
    insert: BTreeMap<UncasedString, String>,
}

#[derive(Debug, Clone)]
pub struct Shader {
    pub shader: UncasedString,
    pub parameters: BTreeMap<UncasedString, String>,
}

impl Shader {}

#[derive(Debug, Error)]
pub enum ShaderResolveError {
    #[error("io error: {0}")]
    Io(#[from] io::Error),
    #[error("error deserializing included material: {0}")]
    Deserialization(#[from] vdf::Error),
    #[error("included material cannot be a patch material")]
    RecursivePatch,
}

#[derive(Debug, Clone)]
pub struct Vmt {
    shader: ShaderOrPatch,
}

impl Vmt {
    /// # Errors
    ///
    /// Returns `Err` if the deserialization fails.
    pub fn from_str(input: &str) -> vdf::Result<Self> {
        vdf::from_str(input)
    }

    /// # Errors
    ///
    /// Returns `Err` if the serialization fails.
    pub fn to_string(&self) -> vdf::Result<String> {
        vdf::to_string(self)
    }

    /// Resolve the shader.
    /// If this is a patch material, applies the patch.
    ///
    /// # Errors
    ///
    /// If this is a patch material,
    /// returns `Err` if the included material can't be found or parsed,
    /// or if the included material is also a patch material.
    pub fn resolve_shader(
        self,
        filesystem: &fs::OpenFileSystem,
    ) -> Result<Shader, ShaderResolveError> {
        match self.shader {
            ShaderOrPatch::Shader(shader) => Ok(shader),
            ShaderOrPatch::Patch(mut patch) => {
                let base_contents = filesystem.read_to_string(&patch.include)?;
                let base_vmt = Self::from_str(&base_contents)?;
                let mut base_shader = base_vmt
                    .into_shader()
                    .ok_or(ShaderResolveError::RecursivePatch)?;
                base_shader.parameters.append(&mut patch.insert);
                Ok(base_shader)
            }
        }
    }

    /// Convert the material into the inner shader.
    /// Returns `None` if this is a patch material.
    #[must_use]
    pub fn into_shader(self) -> Option<Shader> {
        if let ShaderOrPatch::Shader(shader) = self.shader {
            Some(shader)
        } else {
            None
        }
    }
}

impl<'de> Deserialize<'de> for Vmt {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct VmtVisitor;

        impl<'de> Visitor<'de> for VmtVisitor {
            type Value = Vmt;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a vmt file")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let shader_or_patch: StringOrPatch = map
                    .next_key()?
                    .ok_or_else(|| de::Error::invalid_length(0, &"a shader"))?;
                let shader = match shader_or_patch {
                    StringOrPatch::Patch => ShaderOrPatch::Patch(map.next_value()?),
                    StringOrPatch::String(shader) => {
                        let parameters = map.next_value()?;
                        ShaderOrPatch::Shader(Shader {
                            shader: shader.into(),
                            parameters,
                        })
                    }
                };
                Ok(Vmt { shader })
            }
        }

        deserializer.deserialize_map(VmtVisitor)
    }
}

impl Serialize for Vmt {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(1))?;
        match &self.shader {
            ShaderOrPatch::Shader(shader) => {
                map.serialize_entry(&shader.shader, &shader.parameters)
            }
            ShaderOrPatch::Patch(patch) => map.serialize_entry("patch", patch),
        }?;
        map.end()
    }
}
