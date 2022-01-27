use std::str::FromStr;

use glam::Vec3;
use itertools::Itertools;
use plumber_vpk::PathBuf;
use rgb::RGB8;
use thiserror::Error;
use uncased::AsUncased;

use super::Entity;

#[derive(Debug, Clone, Copy)]
#[non_exhaustive]
pub enum TypedEntity<'a> {
    Light(Light<'a>),
    SpotLight(SpotLight<'a>),
    EnvLight(EnvLight<'a>),
    SkyCamera(SkyCamera<'a>),
    Overlay(Overlay<'a>),
    Prop(Prop<'a>),
    Unknown(Unknown<'a>),
}

impl Entity {
    #[must_use]
    pub fn typed(&self) -> TypedEntity {
        match self.class_name.as_str() {
            "light" => TypedEntity::Light(Light::new(self)),
            "light_spot" => TypedEntity::SpotLight(SpotLight::new(self)),
            "light_environment" => TypedEntity::EnvLight(EnvLight::new(self)),
            "sky_camera" => TypedEntity::SkyCamera(SkyCamera::new(self)),
            "info_overlay" => TypedEntity::Overlay(Overlay::new(self)),
            "prop_static"
            | "prop_detail"
            | "prop_ragdoll"
            | "prop_door_rotating"
            | "prop_dynamic"
            | "prop_dynamic_override"
            | "prop_physics"
            | "prop_physics_multiplayer"
            | "prop_physics_override" => TypedEntity::Prop(Prop::new(self)),
            _ => TypedEntity::Unknown(Unknown::new(self)),
        }
    }
}

#[derive(Debug, Error, Clone, Hash, PartialEq, Eq)]
pub enum EntityParseError {
    #[error("parameter `{0}` is missing")]
    MissingParameter(&'static str),
    #[error("parameter `{parameter}` is invalid: {reason}")]
    InvalidParameterValue {
        parameter: &'static str,
        reason: &'static str,
    },
}

pub trait BaseEntity {
    #[must_use]
    fn entity(&self) -> &Entity;

    /// # Errors
    ///
    /// Returns `Err` if the parameter can't be parsed.
    fn parse_parameter<P>(
        &self,
        parameter: &'static str,
        error_message: &'static str,
    ) -> Result<Option<P>, EntityParseError>
    where
        P: FromStr,
    {
        let value = self.entity().properties.get(parameter.as_uncased());

        match value {
            Some(value) => value
                .parse()
                .map_err(|_| EntityParseError::InvalidParameterValue {
                    parameter,
                    reason: error_message,
                })
                .map(Some),
            None => Ok(None),
        }
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter can't be parsed.
    fn parse_float_parameter(
        &self,
        parameter: &'static str,
    ) -> Result<Option<f32>, EntityParseError> {
        self.parse_parameter(parameter, "not a valid float")
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter can't be parsed.
    fn parse_int_parameter(
        &self,
        parameter: &'static str,
    ) -> Result<Option<i32>, EntityParseError> {
        self.parse_parameter(parameter, "not a valid int")
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter can't be parsed.
    fn parse_bool_parameter(
        &self,
        parameter: &'static str,
    ) -> Result<Option<bool>, EntityParseError> {
        let value = self.entity().properties.get(parameter.as_uncased());

        match value {
            Some(value) => match value.as_str() {
                "0" => Ok(Some(false)),
                "1" => Ok(Some(true)),
                _ => Err(EntityParseError::InvalidParameterValue {
                    parameter,
                    reason: "not a valid bool (0 or 1)",
                }),
            },
            None => Ok(None),
        }
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter can't be parsed.
    fn parse_color_parameter(
        &self,
        parameter: &'static str,
    ) -> Result<Option<RGB8>, EntityParseError> {
        if let Some(value) = self.entity().properties.get(parameter.as_uncased()) {
            let (r, g, b) = value
                .split_ascii_whitespace()
                .map(|s| {
                    s.parse::<u8>()
                        .map_err(|_| EntityParseError::InvalidParameterValue {
                            parameter,
                            reason: "contains an invalid integer (0-255)",
                        })
                })
                .next_tuple()
                .ok_or(EntityParseError::InvalidParameterValue {
                    parameter,
                    reason: "contains less than 3 values",
                })?;

            Ok(Some(RGB8 {
                r: r?,
                g: g?,
                b: b?,
            }))
        } else {
            Ok(None)
        }
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter can't be parsed.
    fn parse_vector3_parameter(
        &self,
        parameter: &'static str,
    ) -> Result<Option<Vec3>, EntityParseError> {
        if let Some(value) = self.entity().properties.get(parameter.as_uncased()) {
            let (x, y, z) = value
                .split_ascii_whitespace()
                .map(|s| {
                    s.parse::<f32>()
                        .map_err(|_| EntityParseError::InvalidParameterValue {
                            parameter,
                            reason: "contains an invalid float",
                        })
                })
                .next_tuple()
                .ok_or(EntityParseError::InvalidParameterValue {
                    parameter,
                    reason: "contains less than 3 values",
                })?;

            Ok(Some(Vec3::new(x?, y?, z?)))
        } else {
            Ok(None)
        }
    }
}

pub trait PointEntity: BaseEntity {
    /// # Errors
    ///
    /// Returns `Err` if the parameter `origin` is missing or can't be parsed.
    fn origin(&self) -> Result<Vec3, EntityParseError> {
        self.parse_vector3_parameter("origin")
            .map(Option::unwrap_or_default)
    }
}

pub trait AngledEntity: BaseEntity {
    /// Returns the entity rotation in pitch, yaw, roll order (YZX).
    ///
    /// # Errors
    ///
    /// Returns `Err` if the parameter `angles` can't be parsed.
    fn angles(&self) -> Result<Vec3, EntityParseError> {
        self.parse_vector3_parameter("angles")
            .map(Option::unwrap_or_default)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Unknown<'a> {
    entity: &'a Entity,
}

impl<'a> Unknown<'a> {
    #[must_use]
    pub fn new(entity: &'a Entity) -> Self {
        Self { entity }
    }
}

impl<'a> BaseEntity for Unknown<'a> {
    fn entity(&self) -> &Entity {
        self.entity
    }
}

impl<'a> PointEntity for Unknown<'a> {}
impl<'a> AngledEntity for Unknown<'a> {}

pub trait LightEntity: PointEntity {
    /// # Errors
    ///
    /// Returns `Err` if the parameter can't be parsed.
    fn parse_color_brightness(
        &self,
        parameter: &'static str,
    ) -> Result<(RGB8, f32), EntityParseError> {
        let light = self
            .entity()
            .properties
            .get(parameter.as_uncased())
            .ok_or(EntityParseError::MissingParameter(parameter))?;

        let mut split = light.split_ascii_whitespace();

        let mut next_color = || {
            let value = split
                .next()
                .ok_or(EntityParseError::InvalidParameterValue {
                    parameter,
                    reason: "contains less than 3 values",
                })?;

            value
                .parse::<u8>()
                .map_err(|_| EntityParseError::InvalidParameterValue {
                    parameter,
                    reason: "contains an invalid integer (0-255)",
                })
        };
        let color = RGB8 {
            r: (next_color)()?,
            g: (next_color)()?,
            b: (next_color)()?,
        };

        let brightness = split.next().map_or(Ok(0.0), |s| {
            s.parse::<f32>()
                .map_err(|_| EntityParseError::InvalidParameterValue {
                    parameter,
                    reason: "4th value is not a valid float",
                })
        })?;

        Ok((color, brightness))
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter can't be parsed.
    fn parse_hdr_color_brightness(
        &self,
        parameter: &'static str,
    ) -> Result<Option<(RGB8, f32)>, EntityParseError> {
        if let Some(light) = self.entity().properties.get(parameter.as_uncased()) {
            let mut split = light.split_ascii_whitespace();

            let mut next_color = || {
                let value = split
                    .next()
                    .ok_or(EntityParseError::InvalidParameterValue {
                        parameter,
                        reason: "contains less than 3 integers",
                    })?;

                if value == "-1" {
                    Ok(None)
                } else {
                    value
                        .parse::<u8>()
                        .map_err(|_| EntityParseError::InvalidParameterValue {
                            parameter,
                            reason: "contains an invalid integer (-1-255)",
                        })
                        .map(Some)
                }
            };

            if let (Some(r), Some(g), Some(b)) = ((next_color)()?, (next_color)()?, (next_color)()?)
            {
                let color = RGB8 { r, g, b };

                let brightness = split.next().map_or(Ok(0.0), |s| {
                    s.parse::<f32>()
                        .map_err(|_| EntityParseError::InvalidParameterValue {
                            parameter,
                            reason: "4th value is not a valid float",
                        })
                })?;

                return Ok(Some((color, brightness)));
            }
        }
        Ok(None)
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter `_light` is missing or can't be parsed.
    fn color_brightness(&self) -> Result<(RGB8, f32), EntityParseError> {
        self.parse_color_brightness("_light")
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter `_lightHDR` can't be parsed.
    fn hdr_color_brightness(&self) -> Result<Option<(RGB8, f32)>, EntityParseError> {
        self.parse_hdr_color_brightness("_lightHDR")
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter `_lightscaleHDR` can't be parsed.
    fn hdr_scale(&self) -> Result<f32, EntityParseError> {
        self.parse_float_parameter("_lightscaleHDR")
            .map(|o| o.unwrap_or(1.0))
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter `style` can't be parsed.
    fn style(&self) -> Result<Option<i32>, EntityParseError> {
        self.parse_int_parameter("style")
    }

    fn pattern(&self) -> Option<&str> {
        self.entity()
            .properties
            .get("pattern".as_uncased())
            .map(String::as_str)
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter `_constant_attn` can't be parsed.
    fn constant_attenuation(&self) -> Result<Option<f32>, EntityParseError> {
        self.parse_float_parameter("_constant_attn")
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter `_linear_attn` can't be parsed.
    fn linear_attenuation(&self) -> Result<Option<f32>, EntityParseError> {
        self.parse_float_parameter("_linear_attn")
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter `_quadratic_attn` can't be parsed.
    fn quadratic_attenuation(&self) -> Result<Option<f32>, EntityParseError> {
        self.parse_float_parameter("_quadratic_attn")
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter `_fifty_percent_distance` can't be parsed.
    fn fifty_percent_distance(&self) -> Result<Option<f32>, EntityParseError> {
        self.parse_float_parameter("_fifty_percent_distance")
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter `_zero_percent_distance` can't be parsed.
    fn zero_percent_distance(&self) -> Result<Option<f32>, EntityParseError> {
        self.parse_float_parameter("_zero_percent_distance")
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Light<'a> {
    entity: &'a Entity,
}

impl<'a> Light<'a> {
    #[must_use]
    pub fn new(entity: &'a Entity) -> Self {
        Self { entity }
    }
}

impl<'a> BaseEntity for Light<'a> {
    fn entity(&self) -> &Entity {
        self.entity
    }
}

impl<'a> PointEntity for Light<'a> {}
impl<'a> LightEntity for Light<'a> {}

#[derive(Debug, Clone, Copy)]
pub struct SpotLight<'a> {
    entity: &'a Entity,
}

impl<'a> BaseEntity for SpotLight<'a> {
    fn entity(&self) -> &Entity {
        self.entity
    }
}

impl<'a> PointEntity for SpotLight<'a> {}
impl<'a> AngledEntity for SpotLight<'a> {}
impl<'a> LightEntity for SpotLight<'a> {}

impl<'a> SpotLight<'a> {
    #[must_use]
    pub fn new(entity: &'a Entity) -> Self {
        Self { entity }
    }

    pub fn target(&self) -> Option<&str> {
        self.entity
            .properties
            .get("target".as_uncased())
            .map(String::as_str)
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter `_inner_cone` is missing or can't be parsed.
    pub fn inner_cone(&self) -> Result<f32, EntityParseError> {
        self.parse_float_parameter("_inner_cone")
            .and_then(|o| o.ok_or(EntityParseError::MissingParameter("_inner_cone")))
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter `_cone` is missing or can't be parsed.
    pub fn outer_cone(&self) -> Result<f32, EntityParseError> {
        self.parse_float_parameter("_cone")
            .and_then(|o| o.ok_or(EntityParseError::MissingParameter("_cone")))
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter `_exponent` is missing or can't be parsed.
    pub fn exponent(&self) -> Result<f32, EntityParseError> {
        self.parse_float_parameter("_exponent")
            .and_then(|o| o.ok_or(EntityParseError::MissingParameter("_exponent")))
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter `_distance` is missing or can't be parsed.
    pub fn distance(&self) -> Result<f32, EntityParseError> {
        self.parse_float_parameter("_distance")
            .and_then(|o| o.ok_or(EntityParseError::MissingParameter("_distance")))
    }

    /// Returns the entity rotation in pitch, yaw, roll order (YZX).
    ///
    /// Spotlights have an extra parameter that overrides the pitch value in angles.
    /// You should use this method to take that into account.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the parameter `angles` or `pitch` doesn't exist or can't be parsed.
    pub fn angles(&self) -> Result<Vec3, EntityParseError> {
        let mut original = AngledEntity::angles(self)?;
        if let Some(pitch) = self.parse_float_parameter("pitch")? {
            original.x = -pitch;
        }
        Ok(original)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct EnvLight<'a> {
    entity: &'a Entity,
}

impl<'a> BaseEntity for EnvLight<'a> {
    fn entity(&self) -> &Entity {
        self.entity
    }
}

impl<'a> PointEntity for EnvLight<'a> {}
impl<'a> AngledEntity for EnvLight<'a> {}
impl<'a> LightEntity for EnvLight<'a> {}

impl<'a> EnvLight<'a> {
    #[must_use]
    pub fn new(entity: &'a Entity) -> Self {
        Self { entity }
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter `_ambient` is missing or can't be parsed.
    pub fn ambient_color_brightness(&self) -> Result<(RGB8, f32), EntityParseError> {
        self.parse_color_brightness("_ambient")
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter `_ambientHDR` can't be parsed.
    pub fn ambient_hdr_color_brightness(&self) -> Result<Option<(RGB8, f32)>, EntityParseError> {
        self.parse_hdr_color_brightness("_ambientHDR")
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter `_AmbientScaleHDR` can't be parsed.
    pub fn ambient_hdr_scale(&self) -> Result<f32, EntityParseError> {
        self.parse_float_parameter("_AmbientScaleHDR")
            .map(|o| o.unwrap_or(1.0))
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter `_SunSpreadAngle` can't be parsed.
    pub fn sun_spread_angle(&self) -> Result<f32, EntityParseError> {
        self.parse_float_parameter("_SunSpreadAngle")
            .map(Option::unwrap_or_default)
    }

    /// Returns the entity rotation in pitch, yaw, roll order (YZX).
    ///
    /// Spotlights have an extra parameter that overrides the pitch value in angles.
    /// You should use this method to take that into account.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the parameter `angles` or `pitch` doesn't exist or can't be parsed.
    pub fn angles(&self) -> Result<Vec3, EntityParseError> {
        let mut original = AngledEntity::angles(self)?;
        if let Some(pitch) = self.parse_float_parameter("pitch")? {
            original.x = -pitch;
        }
        Ok(original)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SkyCamera<'a> {
    entity: &'a Entity,
}

impl<'a> BaseEntity for SkyCamera<'a> {
    fn entity(&self) -> &Entity {
        self.entity
    }
}

impl<'a> PointEntity for SkyCamera<'a> {}
impl<'a> AngledEntity for SkyCamera<'a> {}

impl<'a> SkyCamera<'a> {
    #[must_use]
    pub fn new(entity: &'a Entity) -> Self {
        Self { entity }
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter `scale` can't be parsed.
    pub fn scale(&self) -> Result<f32, EntityParseError> {
        self.parse_float_parameter("scale")
            .map(|o| o.unwrap_or(16.0))
    }

    /// # Errors
    ///
    /// Returns `Err` if a fog related parameter can't be parsed.
    pub fn fog(&self) -> Result<Option<SkyCameraFogSettings>, EntityParseError> {
        let enabled = self.parse_bool_parameter("fogenable")?.unwrap_or_default();
        if !enabled {
            return Ok(None);
        }

        let blend = self.parse_float_parameter("fogblend")?.unwrap_or(0.0);
        let color = self.parse_color_parameter("fogcolor")?.unwrap_or_default();
        let color_2 = self.parse_color_parameter("fogcolor2")?.unwrap_or_default();
        let direction = self.parse_vector3_parameter("fogdir")?;
        let start = self.parse_float_parameter("fogstart")?;
        let end = self.parse_float_parameter("fogend")?;
        let max_density = self.parse_float_parameter("fogmaxdensity")?;

        Ok(Some(SkyCameraFogSettings {
            blend,
            color,
            color_2,
            direction,
            start,
            end,
            max_density,
        }))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SkyCameraFogSettings {
    pub blend: f32,
    pub color: RGB8,
    pub color_2: RGB8,
    pub direction: Option<Vec3>,
    pub start: Option<f32>,
    pub end: Option<f32>,
    pub max_density: Option<f32>,
}

#[derive(Debug, Clone, Copy)]
pub struct Prop<'a> {
    entity: &'a Entity,
}

impl<'a> BaseEntity for Prop<'a> {
    fn entity(&self) -> &Entity {
        self.entity
    }
}

impl<'a> PointEntity for Prop<'a> {}
impl<'a> AngledEntity for Prop<'a> {}

impl<'a> Prop<'a> {
    #[must_use]
    pub fn new(entity: &'a Entity) -> Self {
        Self { entity }
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter `modelscale` or `uniformscale` can't be parsed.
    pub fn scale(&self) -> Result<f32, EntityParseError> {
        if let Some(scale) = self.parse_float_parameter("modelscale")? {
            return Ok(scale);
        }
        if let Some(scale) = self.parse_float_parameter("uniformscale")? {
            return Ok(scale);
        }
        Ok(1.0)
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter `model` doesn't exist.
    pub fn model(&self) -> Result<&str, EntityParseError> {
        self.entity
            .properties
            .get("model".as_uncased())
            .map(String::as_str)
            .ok_or(EntityParseError::MissingParameter("model"))
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter `model` doesn't exist.
    pub fn model_path(&self) -> Result<PathBuf, EntityParseError> {
        self.model().map(PathBuf::from)
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter `skin` can't be parsed.
    pub fn skin(&self) -> Result<i32, EntityParseError> {
        self.parse_int_parameter("skin")
            .map(Option::unwrap_or_default)
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter `rendercolor` can't be parsed.
    pub fn render_color(&self) -> Result<RGB8, EntityParseError> {
        self.parse_color_parameter("rendercolor").map(|o| {
            o.unwrap_or(RGB8 {
                r: 255,
                g: 255,
                b: 255,
            })
        })
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter `renderamt` can't be parsed.
    pub fn render_amt(&self) -> Result<u8, EntityParseError> {
        self.parse_parameter("renderamt", "not a valid int (0-255)")
            .map(|o| o.unwrap_or(255))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Overlay<'a> {
    entity: &'a Entity,
}

impl<'a> BaseEntity for Overlay<'a> {
    fn entity(&self) -> &Entity {
        self.entity
    }
}

impl<'a> PointEntity for Overlay<'a> {}

impl<'a> Overlay<'a> {
    #[must_use]
    pub fn new(entity: &'a Entity) -> Self {
        Self { entity }
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter `material` doesn't exist.
    pub fn material(&self) -> Result<PathBuf, EntityParseError> {
        self.entity
            .properties
            .get("material".as_uncased())
            .map(|s| PathBuf::from(s.clone()))
            .ok_or(EntityParseError::MissingParameter("material"))
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter `sides` can't be parsed.
    pub fn sides(&self) -> Result<Vec<i32>, EntityParseError> {
        self.entity
            .properties
            .get("sides".as_uncased())
            .map_or_else(
                || Ok(Vec::new()),
                |s| {
                    s.split_ascii_whitespace()
                        .map(str::parse)
                        .try_collect()
                        .map_err(|_| EntityParseError::InvalidParameterValue {
                            parameter: "sides",
                            reason: "contains an invalid int",
                        })
                },
            )
    }

    /// # Errors
    ///
    /// Returns `Err` if the parameter `RenderOrder` can't be parsed.
    pub fn render_order(&self) -> Result<u8, EntityParseError> {
        self.parse_parameter("RenderOrder", "not a valid int (0-255)")
            .map(Option::unwrap_or_default)
    }

    /// # Errors
    ///
    /// Returns `Err` if a parameter can't be parsed or is missing.
    pub fn uv_info(&self) -> Result<OverlayUvInfo, EntityParseError> {
        let start_u = self
            .parse_float_parameter("StartU")?
            .ok_or(EntityParseError::MissingParameter("StartU"))?;
        let start_v = self
            .parse_float_parameter("StartV")?
            .ok_or(EntityParseError::MissingParameter("StartV"))?;
        let end_u = self
            .parse_float_parameter("EndU")?
            .ok_or(EntityParseError::MissingParameter("EndU"))?;
        let end_v = self
            .parse_float_parameter("EndV")?
            .ok_or(EntityParseError::MissingParameter("EndV"))?;

        let basis_origin = self
            .parse_vector3_parameter("BasisOrigin")?
            .ok_or(EntityParseError::MissingParameter("BasisOrigin"))?;
        let basis_u = self
            .parse_vector3_parameter("BasisU")?
            .ok_or(EntityParseError::MissingParameter("BasisU"))?;
        let basis_v = self
            .parse_vector3_parameter("BasisV")?
            .ok_or(EntityParseError::MissingParameter("BasisV"))?;
        let basis_normal = self
            .parse_vector3_parameter("BasisNormal")?
            .ok_or(EntityParseError::MissingParameter("BasisNormal"))?;

        let uv_0 = self
            .parse_vector3_parameter("uv0")?
            .ok_or(EntityParseError::MissingParameter("uv0"))?;
        let uv_1 = self
            .parse_vector3_parameter("uv1")?
            .ok_or(EntityParseError::MissingParameter("uv1"))?;
        let uv_2 = self
            .parse_vector3_parameter("uv2")?
            .ok_or(EntityParseError::MissingParameter("uv2"))?;
        let uv_3 = self
            .parse_vector3_parameter("uv3")?
            .ok_or(EntityParseError::MissingParameter("uv3"))?;

        Ok(OverlayUvInfo {
            start_u,
            start_v,
            end_u,
            end_v,
            basis_origin,
            basis_u,
            basis_v,
            basis_normal,
            uvs: [uv_0, uv_1, uv_2, uv_3],
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct OverlayUvInfo {
    pub start_u: f32,
    pub start_v: f32,
    pub end_u: f32,
    pub end_v: f32,
    pub basis_origin: Vec3,
    pub basis_u: Vec3,
    pub basis_v: Vec3,
    pub basis_normal: Vec3,
    pub uvs: [Vec3; 4],
}
