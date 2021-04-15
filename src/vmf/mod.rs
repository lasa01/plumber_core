use crate::{
    parsers::{bracketed, parenthesed, space_separated},
    types::{BracketedVector2, BracketedVector3, Vector3},
    vdf,
};

use std::{collections::HashMap, fmt};

use nom::sequence::tuple;
use serde::{
    de::{self, MapAccess, Visitor},
    Deserialize, Deserializer, Serialize, Serializer,
};
use serde_derive::{Deserialize, Serialize};

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

#[derive(Debug, Deserialize, Serialize)]
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

#[derive(Debug, Deserialize, Serialize)]
#[serde(default)]
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

#[derive(Debug, Deserialize, Serialize)]
pub struct VisGroups {
    #[serde(default, rename = "visgroup", skip_serializing_if = "Vec::is_empty")]
    vis_groups: Vec<VisGroup>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct VisGroup {
    name: String,
    #[serde(rename = "visgroupid")]
    vis_group_id: i32,
    color: Rgb,
}

#[derive(Debug, Clone, Copy)]
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
                formatter.write_str("rgb")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                match tuple((space_separated, space_separated, space_separated))(v) {
                    Ok((_, (r, g, b))) => {
                        let r = r.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(r), &"int (0-255)")
                        })?;
                        let g = g.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(g), &"int (0-255)")
                        })?;
                        let b = b.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(b), &"int (0-255)")
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
        serializer.serialize_str(&format!("{} {} {}", self.r, self.g, self.b))
    }
}

#[allow(clippy::struct_excessive_bools)]
#[derive(Debug, Deserialize, Serialize)]
#[serde(default)]
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

#[derive(Debug, Deserialize, Serialize)]
pub struct World {
    id: i32,
    #[serde(rename = "mapversion")]
    map_version: i32,
    #[serde(rename = "classname")]
    class_name: String,
    #[serde(rename = "skyname")]
    sky_name: String,
    #[serde(default, rename = "solid", skip_serializing_if = "Vec::is_empty")]
    solids: Vec<Solid>,
    #[serde(flatten)]
    properties: HashMap<String, String>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct Solid {
    id: i32,
    #[serde(rename = "side", skip_serializing_if = "Vec::is_empty")]
    sides: Vec<Side>,
    editor: Editor,
}

#[derive(Debug, Deserialize, Serialize)]
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

#[derive(Debug, Clone, Copy)]
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
                formatter.write_str("plane")
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
                            de::Error::invalid_value(de::Unexpected::Str(x0), &"float")
                        })?;
                        let y0 = y0.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(y0), &"float")
                        })?;
                        let z0 = z0.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(z0), &"float")
                        })?;
                        let x1 = x1.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(x1), &"float")
                        })?;
                        let y1 = y1.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(y1), &"float")
                        })?;
                        let z1 = z1.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(z1), &"float")
                        })?;
                        let x2 = x2.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(x2), &"float")
                        })?;
                        let y2 = y2.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(y2), &"float")
                        })?;
                        let z2 = z2.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(z2), &"float")
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
        serializer.serialize_str(&format!(
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
        ))
    }
}

#[derive(Debug, Clone, Copy)]
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
                formatter.write_str("uv axis")
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
                            de::Error::invalid_value(de::Unexpected::Str(x), &"float")
                        })?;
                        let y = y.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(y), &"float")
                        })?;
                        let z = z.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(z), &"float")
                        })?;
                        let translation = translation.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(translation), &"float")
                        })?;
                        let scale = scale.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(scale), &"float")
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
        serializer.serialize_str(&format!(
            "[{} {} {} {}] {}",
            self.axis.x, self.axis.y, self.axis.z, self.translation, self.scale,
        ))
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct DispInfo {
    power: u8,
    #[serde(rename = "startposition")]
    start_position: BracketedVector3,
    elevation: f64,
    subdiv: bool,
    normals: HashMap<String, String>,
    distances: HashMap<String, String>,
    offsets: HashMap<String, String>,
    offset_normals: HashMap<String, String>,
    alphas: HashMap<String, String>,
    triangle_tags: HashMap<String, String>,
    //TODO: allowed_verts
}

#[derive(Debug, Deserialize, Serialize)]
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

#[derive(Debug, Serialize)]
pub struct Entity {
    id: i32,
    #[serde(rename = "classname")]
    class_name: String,
    #[serde(rename = "spawnflags")]
    spawn_flags: i32,
    #[serde(flatten)]
    properties: HashMap<String, String>,
    connections: HashMap<String, String>,
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
                formatter.write_str("struct Entity")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let mut id = None;
                let mut class_name = None;
                let mut spawn_flags = None;
                let mut properties = HashMap::new();
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
                            properties.insert(other.into(), map.next_value()?);
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
