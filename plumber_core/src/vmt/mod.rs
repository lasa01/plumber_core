pub mod loader;

use crate::{
    fs::{self, GamePath, GamePathBuf, PathBuf},
    parsers::{braced, bracketed, space_separated},
};

use log::warn;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::multispace0,
    sequence::{preceded, tuple},
    IResult,
};
use plumber_vdf as vdf;
use rgb::RGB;
use vdf::Value;

use std::{collections::BTreeMap, fmt, io};

use plumber_uncased::{AsUncased, UncasedString};
use serde::{
    de::{self, IgnoredAny, MapAccess, Visitor},
    ser::SerializeMap,
    Deserialize, Deserializer, Serialize, Serializer,
};
use thiserror::Error;

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
    include: GamePathBuf,
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

#[derive(Debug, Clone, Error, Hash, PartialEq, Eq)]
#[error("parameter `{parameter}` is not a valid {kind}: `{value}`")]
pub struct ParameterError {
    parameter: &'static str,
    kind: &'static str,
    value: String,
}

pub trait ParameterType: Sized {
    /// Type name to use for error messages.
    const TYPE_NAME: &'static str;

    /// Returns Some if parsing the parameter is successful, None if not.
    fn parse(s: &str) -> Option<Self>;
}

impl ParameterType for bool {
    const TYPE_NAME: &'static str = "boolean";

    fn parse(s: &str) -> Option<Self> {
        match s.trim() {
            "0" => Some(false),
            "1" => Some(true),
            _ => None,
        }
    }
}

macro_rules! impl_parameter_type_from_str {
    ($ty:ty, $type_name:expr) => {
        impl ParameterType for $ty {
            const TYPE_NAME: &'static str = $type_name;

            fn parse(s: &str) -> Option<Self> {
                s.trim().parse().ok()
            }
        }
    };
}

impl_parameter_type_from_str!(u8, "integer (u8)");
impl_parameter_type_from_str!(i8, "integer (i8)");
impl_parameter_type_from_str!(u16, "integer (u16)");
impl_parameter_type_from_str!(i16, "integer (i16)");
impl_parameter_type_from_str!(u32, "integer (u32)");
impl_parameter_type_from_str!(i32, "integer (i32)");
impl_parameter_type_from_str!(u64, "integer (u64)");
impl_parameter_type_from_str!(i64, "integer (i64)");
impl_parameter_type_from_str!(usize, "integer (usize)");
impl_parameter_type_from_str!(isize, "integer (isize)");

impl_parameter_type_from_str!(f32, "float");
impl_parameter_type_from_str!(f64, "float");

impl ParameterType for glam::Vec2 {
    const TYPE_NAME: &'static str = "vector2";

    fn parse(s: &str) -> Option<Self> {
        fn vec_parser(input: &str) -> IResult<&str, (&str, &str)> {
            tuple((space_separated, space_separated))(input)
        }

        let (x, y) = alt((bracketed(vec_parser), vec_parser))(s).ok()?.1;

        let x = x.parse().ok()?;
        let y = y.parse().ok()?;

        Some(glam::Vec2::new(x, y))
    }
}

impl ParameterType for glam::Vec3 {
    const TYPE_NAME: &'static str = "vector3";

    fn parse(s: &str) -> Option<Self> {
        fn vec_parser(input: &str) -> IResult<&str, (&str, &str, &str)> {
            tuple((space_separated, space_separated, space_separated))(input)
        }

        let (x, y, z) = alt((bracketed(vec_parser), vec_parser))(s).ok()?.1;

        let x = x.parse().ok()?;
        let y = y.parse().ok()?;
        let z = z.parse().ok()?;

        Some(glam::Vec3::new(x, y, z))
    }
}

impl ParameterType for rgb::RGB<f32> {
    const TYPE_NAME: &'static str = "color";

    fn parse(s: &str) -> Option<Self> {
        fn integer_color_parser(input: &str) -> IResult<&str, (&str, &str, &str)> {
            braced(tuple((space_separated, space_separated, space_separated)))(input)
        }

        if let Ok((_, (r, g, b))) = integer_color_parser(s) {
            let r = r.parse::<f32>().ok()? / 255.0;
            let g = g.parse::<f32>().ok()? / 255.0;
            let b = b.parse::<f32>().ok()? / 255.0;

            return Some(RGB::new(r, g, b));
        }

        glam::Vec3::parse(s).map(|v| RGB::new(v.x, v.y, v.z))
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Transform {
    pub center: glam::Vec2,
    pub scale: glam::Vec2,
    pub rotate: f32,
    pub translate: glam::Vec2,
}

impl Default for Transform {
    fn default() -> Self {
        Self {
            center: glam::Vec2::new(0.5, 0.5),
            scale: glam::Vec2::ONE,
            rotate: 0.0,
            translate: glam::Vec2::ZERO,
        }
    }
}

impl ParameterType for Transform {
    const TYPE_NAME: &'static str = "transform";

    fn parse(s: &str) -> Option<Self> {
        type TransformResult<'a> = (
            (&'a str, &'a str),
            (&'a str, &'a str),
            &'a str,
            (&'a str, &'a str),
        );

        fn transform_parser(input: &str) -> IResult<&str, TransformResult> {
            tuple((
                preceded(
                    tuple((tag("center"), multispace0)),
                    tuple((space_separated, space_separated)),
                ),
                preceded(
                    tuple((multispace0, tag("scale"), multispace0)),
                    tuple((space_separated, space_separated)),
                ),
                preceded(
                    tuple((multispace0, tag("rotate"), multispace0)),
                    space_separated,
                ),
                preceded(
                    tuple((multispace0, tag("translate"), multispace0)),
                    tuple((space_separated, space_separated)),
                ),
            ))(input)
        }

        let (center, scale, rotate, translate) = transform_parser(s).ok()?.1;

        let x = center.0.parse().ok()?;
        let y = center.1.parse().ok()?;
        let center = glam::Vec2::new(x, y);

        let x = scale.0.parse().ok()?;
        let y = scale.1.parse().ok()?;
        let scale = glam::Vec2::new(x, y);

        let rotate = rotate.parse().ok()?;

        let x = translate.0.parse().ok()?;
        let y = translate.1.parse().ok()?;
        let translate = glam::Vec2::new(x, y);

        Some(Self {
            center,
            scale,
            rotate,
            translate,
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TexturePath(pub GamePathBuf);

impl TexturePath {
    #[must_use]
    pub fn absolute_path(&self) -> GamePathBuf {
        path_to_absolute(&self.0)
    }
}

impl ParameterType for TexturePath {
    const TYPE_NAME: &'static str = "texture";

    fn parse(s: &str) -> Option<Self> {
        Some(Self(GamePathBuf::from(s)))
    }
}

impl Shader {
    /// # Errors
    ///
    /// Returns `Err` if the parameter is not valid.
    pub fn extract_param<T: ParameterType>(
        &self,
        parameter: &'static str,
    ) -> Result<Option<T>, ParameterError> {
        let value = if let Some(value) = self.parameters.get(parameter.as_uncased()) {
            value
        } else {
            return Ok(None);
        };

        if let Some(res) = T::parse(value) {
            Ok(Some(res))
        } else {
            Err(ParameterError {
                parameter,
                kind: T::TYPE_NAME,
                value: value.clone(),
            })
        }
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter is not valid.
    pub fn extract_param_or_default<T: ParameterType + Default>(
        &self,
        parameter: &'static str,
    ) -> Result<T, ParameterError> {
        self.extract_param(parameter).map(Option::unwrap_or_default)
    }

    /// Extracts a parameter, or returns the default value if the
    /// parameter doesn't exist. Returns the default value and logs a warning if the
    /// parameter is invalid.
    #[must_use]
    pub fn extract_param_infallible<T: ParameterType + Default>(
        &self,
        parameter: &'static str,
        material_name: &PathBuf,
    ) -> T {
        match self.extract_param(parameter) {
            Ok(Some(res)) => res,
            Ok(None) => T::default(),
            Err(err) => {
                warn!("material `{}`: {}", material_name, err);
                T::default()
            }
        }
    }
}

/// Converts a texture/material path into an absolute version,
/// ie. one that starts with `materials/`.
#[must_use]
pub fn path_to_absolute(path: &GamePathBuf) -> GamePathBuf {
    GamePath::try_from_str("materials")
        .expect("cannot fail")
        .join(path)
}

#[derive(Debug, Clone, Error, Hash, PartialEq, Eq)]
pub enum ShaderResolveError {
    #[error("io error reading `{path}`: {error}")]
    Io { path: String, error: String },
    #[error("error deserializing included material: {0}")]
    Deserialization(#[from] vdf::Error),
    #[error("included material cannot be a patch material")]
    RecursivePatch,
}

impl ShaderResolveError {
    fn from_io(err: &io::Error, path: &GamePath) -> Self {
        Self::Io {
            path: path.to_string(),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fs::{DirEntryType, ReadDir};
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
                    recurse(filesystem.read_dir(GamePath::try_from_str("materials").unwrap()));
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

                        if let Err(err) = from_bytes(&vmt_contents) {
                            panic!(
                                "{}:\n{}\n\n{}",
                                entry.path().as_str(),
                                err,
                                String::from_utf8_lossy(&vmt_contents)
                            );
                        }
                    }
                }
                DirEntryType::Directory => recurse(entry.read_dir()),
            }
        }
    }
}
