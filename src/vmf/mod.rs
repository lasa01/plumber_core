use crate::{
    parsers::{bracketed, parenthesed, space_separated},
    types::{BracketedVector2, BracketedVector3, Vector3},
    vdf,
};

use std::{
    collections::BTreeMap,
    fmt::{self, Display, Write},
    str::FromStr,
};

use itertools::Itertools;
use ndarray::Array2;
use nom::sequence::tuple;
use serde::{
    de::{self, DeserializeSeed, MapAccess, Visitor},
    ser::{SerializeMap, SerializeStruct},
    Deserialize, Deserializer, Serialize, Serializer,
};
use serde_derive::{Deserialize, Serialize};
use uncased::Uncased;

/// # Errors
///
/// Will return `Err` if the deserialization fails.
pub fn from_str(input: &str) -> vdf::Result<Vmf> {
    Vmf::from_str(input)
}

/// # Errors
///
/// Will return `Err` if the serialization fails.
pub fn to_string(vmf: &Vmf) -> vdf::Result<String> {
    vmf.to_string()
}

#[derive(Debug, Deserialize, Serialize, PartialEq)]
pub struct Vmf {
    #[serde(rename = "versioninfo")]
    version_info: VersionInfo,
    #[serde(rename = "visgroups")]
    vis_groups: VisGroups,
    #[serde(rename = "viewsettings")]
    view_settings: ViewSettings,
    world: World,
    #[serde(default, rename = "entity", skip_serializing_if = "Vec::is_empty")]
    entities: Vec<Entity>,
}

impl Vmf {
    /// # Errors
    ///
    /// Will return `Err` if the deserialization fails.
    pub fn from_str(input: &str) -> vdf::Result<Self> {
        vdf::from_str(input)
    }

    /// # Errors
    ///
    /// Will return `Err` if the serialization fails.
    pub fn to_string(&self) -> vdf::Result<String> {
        vdf::to_string(self)
    }
}

#[derive(Debug, Deserialize, Serialize, PartialEq)]
#[serde(default, expecting = "class versioninfo")]
pub struct VersionInfo {
    #[serde(rename = "editorversion")]
    editor_version: i32,
    #[serde(rename = "editorbuild")]
    editor_build: i32,
    #[serde(rename = "mapversion")]
    map_version: i32,
    #[serde(rename = "formatversion")]
    format_version: i32,
    prefab: bool,
}

impl Default for VersionInfo {
    fn default() -> Self {
        Self {
            editor_version: 400,
            editor_build: 0,
            map_version: 0,
            format_version: 100,
            prefab: false,
        }
    }
}

#[derive(Debug, Deserialize, Serialize, PartialEq)]
#[serde(expecting = "class visgroups")]
pub struct VisGroups {
    #[serde(default, rename = "visgroup", skip_serializing_if = "Vec::is_empty")]
    vis_groups: Vec<VisGroup>,
}

#[derive(Debug, Deserialize, Serialize, PartialEq)]
#[serde(expecting = "class visgroup")]
pub struct VisGroup {
    name: String,
    #[serde(rename = "visgroupid")]
    vis_group_id: i32,
    color: Rgb,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Rgb {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

impl<'de> Deserialize<'de> for Rgb {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct RgbVisitor;

        impl<'de> Visitor<'de> for RgbVisitor {
            type Value = Rgb;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("an rgb string")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                match tuple((space_separated, space_separated, space_separated))(v) {
                    Ok((_, (r, g, b))) => {
                        let r = r.parse().map_err(|_| {
                            de::Error::invalid_value(
                                de::Unexpected::Str(r),
                                &"an int between 0 and 255",
                            )
                        })?;
                        let g = g.parse().map_err(|_| {
                            de::Error::invalid_value(
                                de::Unexpected::Str(g),
                                &"an int between 0 and 255",
                            )
                        })?;
                        let b = b.parse().map_err(|_| {
                            de::Error::invalid_value(
                                de::Unexpected::Str(b),
                                &"an int between 0 and 255",
                            )
                        })?;
                        Ok(Rgb { r, g, b })
                    }
                    Err(..) => Err(de::Error::invalid_value(de::Unexpected::Str(v), &Self)),
                }
            }
        }

        deserializer.deserialize_str(RgbVisitor)
    }
}

impl Serialize for Rgb {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.collect_str(self)
    }
}

impl Display for Rgb {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.r, self.g, self.b)
    }
}

#[allow(clippy::struct_excessive_bools)]
#[derive(Debug, Deserialize, Serialize, PartialEq)]
#[serde(default, expecting = "class viewsettings")]
pub struct ViewSettings {
    #[serde(rename = "bSnapToGrid")]
    snap_to_grid: bool,
    #[serde(rename = "bShowGrid")]
    show_grid: bool,
    #[serde(rename = "bShowLogicalGrid")]
    show_logical_grid: bool,
    #[serde(rename = "nGridSpacing")]
    grid_spacing: i32,
    #[serde(rename = "bShow3DGrid")]
    show_3d_grid: bool,
}

impl Default for ViewSettings {
    fn default() -> Self {
        Self {
            snap_to_grid: true,
            show_grid: true,
            show_logical_grid: false,
            grid_spacing: 64,
            show_3d_grid: false,
        }
    }
}

#[derive(Debug, Deserialize, Serialize, PartialEq)]
#[serde(expecting = "class world")]
pub struct World {
    id: i32,
    #[serde(rename = "mapversion")]
    map_version: i32,
    #[serde(rename = "classname")]
    class_name: String,
    #[serde(rename = "skyname")]
    sky_name: String,
    #[serde(flatten)]
    properties: BTreeMap<Uncased<'static>, String>,
    #[serde(default, rename = "solid", skip_serializing_if = "Vec::is_empty")]
    solids: Vec<Solid>,
}

#[derive(Debug, Deserialize, Serialize, PartialEq)]
#[serde(expecting = "class solid")]
pub struct Solid {
    id: i32,
    #[serde(rename = "side", skip_serializing_if = "Vec::is_empty")]
    sides: Vec<Side>,
    editor: Editor,
}

#[derive(Debug, Deserialize, Serialize, PartialEq)]
#[serde(expecting = "class side")]
pub struct Side {
    id: i32,
    plane: Plane,
    material: String,
    #[serde(rename = "uaxis")]
    u_axis: UvAxis,
    #[serde(rename = "vaxis")]
    v_axis: UvAxis,
    rotation: f64,
    #[serde(rename = "lightmapscale")]
    light_map_scale: i32,
    smoothing_groups: i32,
    #[serde(rename = "dispinfo", skip_serializing_if = "Option::is_none")]
    disp_info: Option<DispInfo>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Plane(pub Vector3, pub Vector3, pub Vector3);

impl<'de> Deserialize<'de> for Plane {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct PlaneVisitor;

        impl<'de> Visitor<'de> for PlaneVisitor {
            type Value = Plane;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a plane string")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                match tuple((
                    parenthesed(tuple((space_separated, space_separated, space_separated))),
                    parenthesed(tuple((space_separated, space_separated, space_separated))),
                    parenthesed(tuple((space_separated, space_separated, space_separated))),
                ))(v)
                {
                    Ok((_, ((x0, y0, z0), (x1, y1, z1), (x2, y2, z2)))) => {
                        let x0 = x0.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(x0), &"a float")
                        })?;
                        let y0 = y0.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(y0), &"a float")
                        })?;
                        let z0 = z0.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(z0), &"a float")
                        })?;
                        let x1 = x1.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(x1), &"a float")
                        })?;
                        let y1 = y1.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(y1), &"a float")
                        })?;
                        let z1 = z1.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(z1), &"a float")
                        })?;
                        let x2 = x2.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(x2), &"a float")
                        })?;
                        let y2 = y2.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(y2), &"a float")
                        })?;
                        let z2 = z2.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(z2), &"a float")
                        })?;
                        Ok(Plane(
                            Vector3 {
                                x: x0,
                                y: y0,
                                z: z0,
                            },
                            Vector3 {
                                x: x1,
                                y: y1,
                                z: z1,
                            },
                            Vector3 {
                                x: x2,
                                y: y2,
                                z: z2,
                            },
                        ))
                    }
                    Err(..) => Err(de::Error::invalid_value(de::Unexpected::Str(v), &Self)),
                }
            }
        }

        deserializer.deserialize_str(PlaneVisitor)
    }
}

impl Serialize for Plane {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.collect_str(self)
    }
}

impl Display for Plane {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "({} {} {}) ({} {} {}) ({} {} {})",
            self.0.x,
            self.0.y,
            self.0.z,
            self.1.x,
            self.1.y,
            self.1.z,
            self.2.x,
            self.2.y,
            self.2.z,
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct UvAxis {
    pub axis: Vector3,
    pub translation: f64,
    pub scale: f64,
}

impl<'de> Deserialize<'de> for UvAxis {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct UvAxisVisitor;

        impl<'de> Visitor<'de> for UvAxisVisitor {
            type Value = UvAxis;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("an uv axis string")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                match tuple((
                    bracketed(tuple((
                        space_separated,
                        space_separated,
                        space_separated,
                        space_separated,
                    ))),
                    space_separated,
                ))(v)
                {
                    Ok((_, ((x, y, z, translation), scale))) => {
                        let x = x.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(x), &"a float")
                        })?;
                        let y = y.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(y), &"a float")
                        })?;
                        let z = z.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(z), &"a float")
                        })?;
                        let translation = translation.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(translation), &"a float")
                        })?;
                        let scale = scale.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(scale), &"a float")
                        })?;
                        Ok(UvAxis {
                            axis: Vector3 { x, y, z },
                            translation,
                            scale,
                        })
                    }
                    Err(..) => Err(de::Error::invalid_value(de::Unexpected::Str(v), &Self)),
                }
            }
        }

        deserializer.deserialize_str(UvAxisVisitor)
    }
}

impl Serialize for UvAxis {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.collect_str(self)
    }
}

impl Display for UvAxis {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[{} {} {} {}] {}",
            self.axis.x, self.axis.y, self.axis.z, self.translation, self.scale,
        )
    }
}

#[derive(Debug, Serialize, PartialEq)]
pub struct DispInfo {
    power: u8,
    #[serde(rename = "startposition")]
    start_position: BracketedVector3,
    elevation: f64,
    subdiv: bool,
    normals: Vector3DispData,
    distances: NumDispData<f64>,
    offsets: Vector3DispData,
    offset_normals: Vector3DispData,
    alphas: NumDispData<u8>,
    triangle_tags: NumDispData<u8>,
    allowed_verts: AllowedVerts,
}

impl DispInfo {
    fn calculate_dimension(power: u8) -> usize {
        2_usize.pow(power.into()) + 1
    }

    #[must_use]
    pub fn dimension(&self) -> usize {
        Self::calculate_dimension(self.power)
    }
}

#[allow(clippy::too_many_lines)]
impl<'de> Deserialize<'de> for DispInfo {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(field_identifier, rename_all = "lowercase")]
        enum Field {
            Power,
            StartPosition,
            Elevation,
            Subdiv,
            Normals,
            Distances,
            Offsets,
            #[serde(rename = "offset_normals")]
            OffsetNormals,
            Alphas,
            #[serde(rename = "triangle_tags")]
            TriangleTags,
            #[serde(rename = "allowed_verts")]
            AllowedVerts,
            #[serde(other)]
            Other,
        }

        struct DispInfoVisitor;

        impl<'de> Visitor<'de> for DispInfoVisitor {
            type Value = DispInfo;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("class dispinfo")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let mut power = None;
                let mut dimension = None;
                let mut start_position = None;
                let mut elevation = None;
                let mut subdiv = None;
                let mut normals = None;
                let mut distances = None;
                let mut offsets = None;
                let mut offset_normals = None;
                let mut alphas = None;
                let mut triangle_tags = None;
                let mut allowed_verts = None;
                while let Some(key) = map.next_key()? {
                    match key {
                        Field::Power => {
                            if power.is_some() {
                                return Err(de::Error::duplicate_field("power"));
                            }
                            let value = map.next_value()?;
                            power = Some(value);
                            dimension = Some(DispInfo::calculate_dimension(value));
                        }
                        Field::StartPosition => {
                            if start_position.is_some() {
                                return Err(de::Error::duplicate_field("startposition"));
                            }
                            start_position = Some(map.next_value()?);
                        }
                        Field::Elevation => {
                            if elevation.is_some() {
                                return Err(de::Error::duplicate_field("elevation"));
                            }
                            elevation = Some(map.next_value()?);
                        }
                        Field::Subdiv => {
                            if subdiv.is_some() {
                                return Err(de::Error::duplicate_field("subdiv"));
                            }
                            subdiv = Some(map.next_value()?);
                        }
                        Field::Normals => {
                            if normals.is_some() {
                                return Err(de::Error::duplicate_field("normals"));
                            }
                            match dimension {
                                Some(dimension) => {
                                    normals =
                                        Some(map.next_value_seed(Vector3DispData::new(dimension))?);
                                }
                                None => return Err(de::Error::missing_field("power")),
                            }
                        }
                        Field::Distances => {
                            if distances.is_some() {
                                return Err(de::Error::duplicate_field("distances"));
                            }
                            match dimension {
                                Some(dimension) => {
                                    distances = Some(map.next_value_seed(NumDispData::new((
                                        dimension, dimension,
                                    )))?);
                                }
                                None => return Err(de::Error::missing_field("power")),
                            }
                        }
                        Field::Offsets => {
                            if offsets.is_some() {
                                return Err(de::Error::duplicate_field("offsets"));
                            }
                            match dimension {
                                Some(dimension) => {
                                    offsets =
                                        Some(map.next_value_seed(Vector3DispData::new(dimension))?);
                                }
                                None => return Err(de::Error::missing_field("power")),
                            }
                        }
                        Field::OffsetNormals => {
                            if offset_normals.is_some() {
                                return Err(de::Error::duplicate_field("offset_normals"));
                            }
                            match dimension {
                                Some(dimension) => {
                                    offset_normals =
                                        Some(map.next_value_seed(Vector3DispData::new(dimension))?);
                                }
                                None => return Err(de::Error::missing_field("power")),
                            }
                        }
                        Field::Alphas => {
                            if alphas.is_some() {
                                return Err(de::Error::duplicate_field("alphas"));
                            }
                            match dimension {
                                Some(dimension) => {
                                    alphas = Some(map.next_value_seed(NumDispData::new((
                                        dimension, dimension,
                                    )))?);
                                }
                                None => return Err(de::Error::missing_field("power")),
                            }
                        }
                        Field::TriangleTags => {
                            if triangle_tags.is_some() {
                                return Err(de::Error::duplicate_field("triangle_tags"));
                            }
                            match power {
                                Some(power) => {
                                    let dimension = 2_usize.pow(power.into());
                                    triangle_tags = Some(map.next_value_seed(NumDispData::new(
                                        (dimension, 2 * dimension),
                                    ))?);
                                }
                                None => return Err(de::Error::missing_field("power")),
                            }
                        }
                        Field::AllowedVerts => {
                            if allowed_verts.is_some() {
                                return Err(de::Error::duplicate_field("allowed_verts"));
                            }
                            allowed_verts = Some(map.next_value()?);
                        }
                        Field::Other => {
                            map.next_value::<de::IgnoredAny>()?;
                        }
                    }
                }
                let power = power.ok_or_else(|| de::Error::missing_field("power"))?;
                let start_position =
                    start_position.ok_or_else(|| de::Error::missing_field("startposition"))?;
                let elevation = elevation.ok_or_else(|| de::Error::missing_field("elevation"))?;
                let subdiv = subdiv.ok_or_else(|| de::Error::missing_field("subdiv"))?;
                let normals = normals.ok_or_else(|| de::Error::missing_field("normals"))?;
                let distances = distances.ok_or_else(|| de::Error::missing_field("distances"))?;
                let offsets = offsets.ok_or_else(|| de::Error::missing_field("offsets"))?;
                let offset_normals =
                    offset_normals.ok_or_else(|| de::Error::missing_field("offset_normals"))?;
                let alphas = alphas.ok_or_else(|| de::Error::missing_field("alphas"))?;
                let triangle_tags =
                    triangle_tags.ok_or_else(|| de::Error::missing_field("triangle_tags"))?;
                let allowed_verts =
                    allowed_verts.ok_or_else(|| de::Error::missing_field("allowed_verts"))?;
                Ok(DispInfo {
                    power,
                    start_position,
                    elevation,
                    subdiv,
                    normals,
                    distances,
                    offsets,
                    offset_normals,
                    alphas,
                    triangle_tags,
                    allowed_verts,
                })
            }
        }

        deserializer.deserialize_struct(
            "DispInfo",
            &[
                "power",
                "startposition",
                "elevation",
                "subdiv",
                "normals",
                "distances",
                "offsets",
                "offset_normals",
                "alphas",
                "triangle_tags",
            ],
            DispInfoVisitor,
        )
    }
}

struct RowKey(usize);

impl Display for RowKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "row{}", self.0)
    }
}

impl Serialize for RowKey {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.collect_str(self)
    }
}

#[derive(Debug, PartialEq)]
pub struct Vector3DispData {
    data: Array2<Vector3>,
}

impl Vector3DispData {
    #[must_use]
    pub fn new(dimension: usize) -> Self {
        Self {
            data: Array2::default((dimension, dimension)),
        }
    }
}

impl<'de> DeserializeSeed<'de> for Vector3DispData {
    type Value = Vector3DispData;

    fn deserialize<D>(mut self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct DispDataVisitor<'a>(&'a mut Vector3DispData);

        impl<'de, 'a> Visitor<'de> for DispDataVisitor<'a> {
            type Value = ();

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a dispinfo data class")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let dimension = self.0.data.dim();
                while let Some(key) = map.next_key::<&str>()? {
                    if !key.starts_with("row") {
                        return Err(de::Error::unknown_field(key, &["row[n]"]));
                    }
                    let row_index: usize = key[3..]
                        .parse()
                        .map_err(|_| de::Error::unknown_field(key, &["row[n]"]))?;
                    if row_index >= dimension.0 {
                        return Err(de::Error::custom("row out of bounds"));
                    }
                    let value: &str = map.next_value()?;
                    for (column_index, (x, y, z)) in
                        value.split_ascii_whitespace().tuples().enumerate()
                    {
                        if column_index >= dimension.1 {
                            return Err(de::Error::custom("column out of bounds"));
                        }
                        let item = &mut self.0.data[(row_index, column_index)];
                        item.x = x.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(x), &"a float")
                        })?;
                        item.y = y.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(y), &"a float")
                        })?;
                        item.z = z.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(z), &"a float")
                        })?;
                    }
                }
                Ok(())
            }
        }

        deserializer.deserialize_map(DispDataVisitor(&mut self))?;
        Ok(self)
    }
}

impl Serialize for Vector3DispData {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let dimension = self.data.dim();
        let mut map = serializer.serialize_map(Some(dimension.0))?;
        for (index, row) in self.data.outer_iter().enumerate() {
            let mut value = String::new();
            for vec in row.iter() {
                write!(value, "{} {} {} ", vec.x, vec.y, vec.z).unwrap();
            }
            map.serialize_entry(&RowKey(index), &value)?;
        }
        map.end()
    }
}

#[derive(Debug, PartialEq)]
pub struct NumDispData<T>
where
    T: Default + FromStr + Display,
{
    data: Array2<T>,
}

impl<T> NumDispData<T>
where
    T: Default + FromStr + Display,
{
    #[must_use]
    pub fn new(shape: (usize, usize)) -> Self {
        Self {
            data: Array2::default(shape),
        }
    }
}

impl<'de, T> DeserializeSeed<'de> for NumDispData<T>
where
    T: Default + FromStr + Display,
{
    type Value = NumDispData<T>;

    fn deserialize<D>(mut self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct DispDataVisitor<'a, T>(&'a mut NumDispData<T>)
        where
            T: Default + FromStr + Display;

        impl<'de, 'a, T> Visitor<'de> for DispDataVisitor<'a, T>
        where
            T: Default + FromStr + Display,
        {
            type Value = ();

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a dispinfo data class")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let dimension = self.0.data.dim();
                while let Some(key) = map.next_key::<&str>()? {
                    if !key.starts_with("row") {
                        return Err(de::Error::unknown_field(key, &["row[n]"]));
                    }
                    let row_index: usize = key[3..]
                        .parse()
                        .map_err(|_| de::Error::unknown_field(key, &["row[n]"]))?;
                    if row_index >= dimension.0 {
                        return Err(de::Error::custom("row out of bounds"));
                    }
                    let value: &str = map.next_value()?;
                    for (column_index, num) in value.split_ascii_whitespace().enumerate() {
                        if column_index >= dimension.1 {
                            return Err(de::Error::custom("column out of bounds"));
                        }
                        self.0.data[(row_index, column_index)] = num.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(num), &"a number")
                        })?;
                    }
                }
                Ok(())
            }
        }

        deserializer.deserialize_map(DispDataVisitor(&mut self))?;
        Ok(self)
    }
}

impl<T> Serialize for NumDispData<T>
where
    T: Default + FromStr + Display,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let dimension = self.data.dim();
        let mut map = serializer.serialize_map(Some(dimension.0))?;
        for (index, row) in self.data.outer_iter().enumerate() {
            let mut value = String::new();
            for num in row.iter() {
                write!(value, "{} ", num).unwrap();
            }
            map.serialize_entry(&RowKey(index), &value)?;
        }
        map.end()
    }
}

struct AllowedVertsInner([i32; 10]);

impl Display for AllowedVertsInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {} {} {} {} {} {} {} {} {}",
            self.0[0],
            self.0[1],
            self.0[2],
            self.0[3],
            self.0[4],
            self.0[5],
            self.0[6],
            self.0[7],
            self.0[8],
            self.0[9],
        )
    }
}

impl Serialize for AllowedVertsInner {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.collect_str(self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct AllowedVerts([i32; 10]);

impl<'de> Deserialize<'de> for AllowedVerts {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(field_identifier)]
        enum Field {
            #[serde(rename = "10")]
            Field,
        }

        struct AllowedVertsVisitor;

        impl<'de> Visitor<'de> for AllowedVertsVisitor {
            type Value = AllowedVerts;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("class allowed_verts")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let mut field: Option<[i32; 10]> = None;
                while map.next_key::<Field>()?.is_some() {
                    if field.is_some() {
                        return Err(de::Error::duplicate_field("10"));
                    }
                    let value: &str = map.next_value()?;
                    let mut data = [0; 10];
                    for (idx, num) in value.split_ascii_whitespace().enumerate() {
                        if idx > 9 {
                            return Err(de::Error::invalid_value(
                                de::Unexpected::Str(value),
                                &"10 numbers",
                            ));
                        }
                        data[idx] = num.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(num), &"an int")
                        })?;
                    }
                    field = Some(data);
                }
                let field = field.ok_or_else(|| de::Error::missing_field("10"))?;
                Ok(AllowedVerts(field))
            }
        }

        deserializer.deserialize_struct("AllowedVerts", &["10"], AllowedVertsVisitor)
    }
}

impl Serialize for AllowedVerts {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut str = serializer.serialize_struct("AllowedVerts", 1)?;
        str.serialize_field("10", &AllowedVertsInner(self.0))?;
        str.end()
    }
}

impl Default for AllowedVerts {
    fn default() -> Self {
        Self([-1; 10])
    }
}

#[derive(Debug, Deserialize, Serialize, PartialEq)]
#[serde(expecting = "class editor")]
pub struct Editor {
    color: Rgb,
    #[serde(default, rename = "groupid")]
    group_id: i32,
    #[serde(default, rename = "visgroupshown")]
    vis_group_shown: bool,
    #[serde(default, rename = "visgroupautoshown")]
    vis_group_auto_shown: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    comments: Option<String>,
    #[serde(rename = "logicalpos", skip_serializing_if = "Option::is_none")]
    logical_pos: Option<BracketedVector2>,
}

#[derive(Debug, Serialize, PartialEq)]
pub struct Entity {
    id: i32,
    #[serde(rename = "classname")]
    class_name: String,
    #[serde(rename = "spawnflags")]
    spawn_flags: i32,
    #[serde(flatten)]
    properties: BTreeMap<Uncased<'static>, String>,
    connections: BTreeMap<Uncased<'static>, String>,
    #[serde(rename = "solid", skip_serializing_if = "Vec::is_empty")]
    solids: Vec<Solid>,
    #[serde(skip_serializing_if = "Option::is_none")]
    editor: Option<Editor>,
}

impl<'de> Deserialize<'de> for Entity {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct EntityVisitor;

        impl<'de> Visitor<'de> for EntityVisitor {
            type Value = Entity;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("class entity")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let mut id = None;
                let mut class_name = None;
                let mut spawn_flags = None;
                let mut properties = BTreeMap::new();
                let mut connections = None;
                let mut solids = None;
                let mut editor = None;

                while let Some(key) = map.next_key()? {
                    match key {
                        "id" => {
                            if id.is_some() {
                                return Err(de::Error::duplicate_field("id"));
                            }
                            id = Some(map.next_value()?);
                        }
                        "classname" => {
                            if class_name.is_some() {
                                return Err(de::Error::duplicate_field("classname"));
                            }
                            class_name = Some(map.next_value()?);
                        }
                        "spawnflags" => {
                            if spawn_flags.is_some() {
                                return Err(de::Error::duplicate_field("spawnflags"));
                            }
                            spawn_flags = Some(map.next_value()?);
                        }
                        "connections" => {
                            if connections.is_some() {
                                return Err(de::Error::duplicate_field("connections"));
                            }
                            connections = Some(map.next_value()?);
                        }
                        "editor" => {
                            if editor.is_some() {
                                return Err(de::Error::duplicate_field("editor"));
                            }
                            editor = Some(map.next_value()?);
                        }
                        "solid" => {
                            if solids.is_some() {
                                return Err(de::Error::duplicate_field("solids"));
                            }
                            if let Ok(res) = map.next_value() {
                                solids = Some(res);
                            } else {
                                properties.insert("solid".into(), map.next_value()?);
                            }
                        }
                        other => {
                            properties.insert(other.to_owned().into(), map.next_value()?);
                        }
                    }
                }

                let id = id.unwrap_or_default();
                let class_name = class_name.ok_or_else(|| de::Error::missing_field("classname"))?;
                let spawn_flags = spawn_flags.unwrap_or_default();
                let connections = connections.unwrap_or_default();
                let solids = solids.unwrap_or_default();
                Ok(Entity {
                    id,
                    class_name,
                    spawn_flags,
                    properties,
                    connections,
                    solids,
                    editor,
                })
            }
        }

        deserializer.deserialize_map(EntityVisitor)
    }
}
