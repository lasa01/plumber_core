pub mod loader;

use crate::fs::{self, Path, PathBuf};

use plumber_vdf as vdf;
use vdf::Value;

use std::{collections::BTreeMap, fmt, io};

use serde::{
    de::{self, IgnoredAny, MapAccess, Visitor},
    ser::SerializeMap,
    Deserialize, Deserializer, Serialize, Serializer,
};
use thiserror::Error;
use uncased::Uncased;

type UncasedString = Uncased<'static>;

/// # Errors
///
/// Returns `Err` if the deserialization fails.
pub fn from_bytes(input: &[u8]) -> vdf::Result<Vmt> {
    Vmt::from_bytes(input)
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
    #[serde(default, alias = "replace")]
    insert: BTreeMap<UncasedString, String>,
}

#[derive(Debug, Clone, PartialEq)]
enum StringOrProxies {
    Proxies,
    String(String),
}

impl<'de> Deserialize<'de> for StringOrProxies {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct ProxiesOrStringVisitor;

        impl<'de> Visitor<'de> for ProxiesOrStringVisitor {
            type Value = StringOrProxies;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a string")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                if v.eq_ignore_ascii_case("proxies") {
                    Ok(StringOrProxies::Proxies)
                } else {
                    Ok(StringOrProxies::String(v.into()))
                }
            }

            fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                if v.eq_ignore_ascii_case("patch") {
                    Ok(StringOrProxies::Proxies)
                } else {
                    Ok(StringOrProxies::String(v))
                }
            }
        }

        deserializer.deserialize_string(ProxiesOrStringVisitor)
    }
}

struct Parameters {
    parameters: BTreeMap<UncasedString, String>,
    proxies: BTreeMap<UncasedString, Value>,
}

impl<'de> Deserialize<'de> for Parameters {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct ParametersVisitor;

        impl<'de> Visitor<'de> for ParametersVisitor {
            type Value = Parameters;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("shader parameters")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let mut parameters = BTreeMap::new();
                let mut proxies = BTreeMap::new();

                while let Some(key) = map.next_key()? {
                    match key {
                        StringOrProxies::Proxies => {
                            proxies.append(&mut map.next_value()?);
                        }
                        StringOrProxies::String(key) => {
                            if let Ok(value) = map.next_value() {
                                parameters.insert(key.into(), value);
                            } else {
                                map.next_value::<IgnoredAny>()?;
                            }
                        }
                    }
                }

                Ok(Parameters {
                    parameters,
                    proxies,
                })
            }
        }

        deserializer.deserialize_map(ParametersVisitor)
    }
}

#[derive(Debug, Clone)]
pub struct Shader {
    pub shader: UncasedString,
    pub parameters: BTreeMap<UncasedString, String>,
    pub proxies: BTreeMap<UncasedString, Value>,
}

impl Shader {}

#[derive(Debug, Clone, Error)]
pub enum ShaderResolveError {
    #[error("io error reading `{path}`: {error}")]
    Io { path: String, error: String },
    #[error("error deserializing included material: {0}")]
    Deserialization(#[from] vdf::Error),
    #[error("included material cannot be a patch material")]
    RecursivePatch,
}

impl ShaderResolveError {
    fn from_io(err: &io::Error, path: &Path) -> Self {
        Self::Io {
            path: path.as_str().to_string(),
            error: err.to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Vmt {
    shader: ShaderOrPatch,
}

impl Vmt {
    /// # Errors
    ///
    /// Returns `Err` if the deserialization fails.
    pub fn from_bytes(input: &[u8]) -> vdf::Result<Self> {
        vdf::from_bytes(input)
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
        file_system: &fs::OpenFileSystem,
    ) -> Result<Shader, ShaderResolveError> {
        match self.shader {
            ShaderOrPatch::Shader(shader) => Ok(shader),
            ShaderOrPatch::Patch(mut patch) => {
                let base_contents = file_system
                    .read(&patch.include)
                    .map_err(|err| ShaderResolveError::from_io(&err, &patch.include))?;
                let base_vmt = Self::from_bytes(&base_contents)?;
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
                        let parameters: Parameters = map.next_value()?;
                        ShaderOrPatch::Shader(Shader {
                            shader: shader.into(),
                            parameters: parameters.parameters,
                            proxies: parameters.proxies,
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

#[cfg(all(test, feature = "fs", feature = "steam"))]
mod tests {
    use super::*;
    use crate::fs::{DirEntryType, Path, ReadDir};
    use crate::steam::Libraries;

    fn is_vmt_file(filename: &str) -> bool {
        filename
            .rsplit('.')
            .next()
            .map(|ext| ext.eq_ignore_ascii_case("vmt"))
            == Some(true)
    }

    /// Fails if steam is not installed
    /// Tests parsing of all materials in all source games currently installed.
    /// Takes some time.
    #[test]
    #[ignore]
    fn parse_discovered_materials() {
        let libraries = Libraries::discover().unwrap();
        for result in libraries.apps().source().filesystems() {
            match result {
                Ok(filesystem) => {
                    eprintln!("reading from filesystem: {}", filesystem.name);
                    let filesystem = filesystem.open().unwrap();
                    recurse(filesystem.read_dir(Path::try_from_str("materials").unwrap()));
                }
                Err(err) => eprintln!("warning: failed filesystem discovery: {}", err),
            }
        }
    }

    fn recurse(readdir: ReadDir) {
        for entry in readdir.map(Result::unwrap) {
            let name = entry.name();
            match entry.entry_type() {
                DirEntryType::File => {
                    // this is an invalid material
                    if name == "cerbrus_galil.vmt" {
                        continue;
                    }
                    if is_vmt_file(name.as_str()) {
                        let vmt_contents = entry
                            .read()
                            .unwrap_or_else(|err| panic!("{}:\n{}", entry.path().as_str(), err));

                        from_bytes(&vmt_contents).unwrap_or_else(|err| {
                            panic!(
                                "{}:\n{}\n\n{}",
                                entry.path().as_str(),
                                err,
                                String::from_utf8_lossy(&vmt_contents)
                            )
                        });
                    }
                }
                DirEntryType::Directory => recurse(entry.read_dir()),
            }
        }
    }
}
