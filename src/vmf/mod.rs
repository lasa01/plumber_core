use crate::{types::{BracketedVec2, BracketedVec3, Plane, Rgb, UvAxis}, vdf};

use std::{collections::HashMap, fmt};

use serde::{Deserialize, de::{self, Visitor, MapAccess}};
use serde_derive::{Deserialize, Serialize};

/// # Errors
///
/// Will return `Err` if the deserialization fails.
pub fn from_str(input: &str) -> vdf::Result<Vmf> {
    vdf::from_str(input)
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
    #[serde(default, rename = "entity")]
    entities: Vec<Entity>,
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
    #[serde(default, rename = "visgroup")]
    vis_groups: Vec<VisGroup>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct VisGroup {
    name: String,
    #[serde(rename = "visgroupid")]
    vis_group_id: i32,
    color: Rgb,
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
    #[serde(default, rename = "solid")]
    solids: Vec<Solid>,
    #[serde(flatten)]
    properties: HashMap<String, String>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct Solid {
    id: i32,
    #[serde(rename = "side")]
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
    #[serde(rename = "dispinfo")]
    disp_info: Option<DispInfo>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct DispInfo {
    power: u8,
    #[serde(rename = "startposition")]
    start_position: BracketedVec3,
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
    comments: Option<String>,
    #[serde(rename = "logicalpos")]
    logical_pos: Option<BracketedVec2>,
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
    #[serde(rename = "solid")]
    solids: Vec<Solid>,
    editor: Option<Editor>,
}

impl<'de> Deserialize<'de> for Entity {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>
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
                    id, class_name, spawn_flags, properties, connections, solids, editor,
                })
            }
        }
        
        deserializer.deserialize_map(EntityVisitor)
    }
}
