use std::{fmt, ops::{Deref, DerefMut}};

use nom::{IResult, bytes::complete::is_not, character::complete::{multispace0, char}, sequence::{preceded, tuple, delimited}};
use serde::{Deserialize, Deserializer, Serialize, Serializer, de::Visitor, de};

fn space_separated(input: &str) -> IResult<&str, &str> {
    preceded(multispace0, is_not(" \t\r\n])}"))(input)
}

#[derive(Debug, Clone, Copy)]
pub struct Vec2 {
    pub x: f64,
    pub y: f64,
}

impl<'de> Deserialize<'de> for Vec2 {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>
    {
        struct Vec2Visitor;

        impl<'de> Visitor<'de> for Vec2Visitor {
            type Value = Vec2;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("vec2")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                match tuple((space_separated, space_separated))(v) {
                    Ok((_, (x, y))) => {
                        let x = x.parse().map_err(|_| de::Error::invalid_value(de::Unexpected::Str(x), &"float"))?;
                        let y = y.parse().map_err(|_| de::Error::invalid_value(de::Unexpected::Str(y), &"float"))?;
                        Ok(Vec2 { x, y})
                    }
                    Err(..) => Err(de::Error::invalid_value(de::Unexpected::Str(v), &Self))
                }
            }
        }

        deserializer.deserialize_str(Vec2Visitor)
    }
}

impl Serialize for Vec2 {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer
    {
        serializer.serialize_str(&format!("{} {}", self.x, self.y))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Vec3 {
    pub x: f64,
    pub y: f64,
    pub z: f64,
}

impl<'de> Deserialize<'de> for Vec3 {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>
    {
        struct Vec3Visitor;

        impl<'de> Visitor<'de> for Vec3Visitor {
            type Value = Vec3;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("vec3")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                match tuple((space_separated, space_separated, space_separated))(v) {
                    Ok((_, (x, y, z))) => {
                        let x = x.parse().map_err(|_| de::Error::invalid_value(de::Unexpected::Str(x), &"float"))?;
                        let y = y.parse().map_err(|_| de::Error::invalid_value(de::Unexpected::Str(y), &"float"))?;
                        let z = z.parse().map_err(|_| de::Error::invalid_value(de::Unexpected::Str(z), &"float"))?;
                        Ok(Vec3 { x, y, z })
                    }
                    Err(..) => Err(de::Error::invalid_value(de::Unexpected::Str(v), &Self))
                }
            }
        }

        deserializer.deserialize_str(Vec3Visitor)
    }
}

impl Serialize for Vec3 {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer
    {
        serializer.serialize_str(&format!("{} {} {}", self.x, self.y, self.z))
    }
}

fn bracketed<'a, O>(parser: impl FnMut(&'a str) -> IResult<&'a str, O>) -> impl FnMut(&'a str) -> IResult<&'a str, O> {
    preceded(multispace0, delimited(char('['), parser, char(']')))
}

#[derive(Debug, Clone, Copy)]
pub struct BracketedVec2(Vec2);

impl Deref for BracketedVec2 {
    type Target = Vec2;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for BracketedVec2 {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'de> Deserialize<'de> for BracketedVec2 {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>
    {
        struct SquareBracketedVec2Visitor;

        impl<'de> Visitor<'de> for SquareBracketedVec2Visitor {
            type Value = BracketedVec2;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("square bracketed vec2")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                match bracketed(tuple((space_separated, space_separated)))(v) {
                    Ok((_, (x, y))) => {
                        let x = x.parse().map_err(|_| de::Error::invalid_value(de::Unexpected::Str(x), &"float"))?;
                        let y = y.parse().map_err(|_| de::Error::invalid_value(de::Unexpected::Str(y), &"float"))?;
                        Ok(BracketedVec2(Vec2 { x, y }))
                    }
                    Err(..) => Err(de::Error::invalid_value(de::Unexpected::Str(v), &Self))
                }
            }
        }

        deserializer.deserialize_str(SquareBracketedVec2Visitor)
    }
}

impl Serialize for BracketedVec2 {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer
    {
        serializer.serialize_str(&format!("[{} {}]", self.x, self.y))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BracketedVec3(Vec3);

impl Deref for BracketedVec3 {
    type Target = Vec3;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for BracketedVec3 {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'de> Deserialize<'de> for BracketedVec3 {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>
    {
        struct SquareBracketedVec3Visitor;

        impl<'de> Visitor<'de> for SquareBracketedVec3Visitor {
            type Value = BracketedVec3;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("square bracketed vec3")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                match bracketed(tuple((space_separated, space_separated, space_separated)))(v) {
                    Ok((_, (x, y, z))) => {
                        let x = x.parse().map_err(|_| de::Error::invalid_value(de::Unexpected::Str(x), &"float"))?;
                        let y = y.parse().map_err(|_| de::Error::invalid_value(de::Unexpected::Str(y), &"float"))?;
                        let z = z.parse().map_err(|_| de::Error::invalid_value(de::Unexpected::Str(z), &"float"))?;
                        Ok(BracketedVec3(Vec3 { x, y, z }))
                    }
                    Err(..) => Err(de::Error::invalid_value(de::Unexpected::Str(v), &Self))
                }
            }
        }

        deserializer.deserialize_str(SquareBracketedVec3Visitor)
    }
}

impl Serialize for BracketedVec3 {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer
    {
        serializer.serialize_str(&format!("[{} {} {}]", self.x, self.y, self.z))
    }
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
        D: Deserializer<'de>
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
                        let r = r.parse().map_err(|_| de::Error::invalid_value(de::Unexpected::Str(r), &"int (0-255)"))?;
                        let g = g.parse().map_err(|_| de::Error::invalid_value(de::Unexpected::Str(g), &"int (0-255)"))?;
                        let b = b.parse().map_err(|_| de::Error::invalid_value(de::Unexpected::Str(b), &"int (0-255)"))?;
                        Ok(Rgb { r, g, b })
                    }
                    Err(..) => Err(de::Error::invalid_value(de::Unexpected::Str(v), &Self))
                }
            }
        }

        deserializer.deserialize_str(RgbVisitor)
    }
}

impl Serialize for Rgb {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer
    {
        serializer.serialize_str(&format!("{} {} {}", self.r, self.g, self.b))
    }
}

fn parenthesed<'a, O>(parser: impl FnMut(&'a str) -> IResult<&'a str, O>) -> impl FnMut(&'a str) -> IResult<&'a str, O> {
    preceded(multispace0, delimited(char('('), parser, char(')')))
}

#[derive(Debug, Clone, Copy)]
pub struct Plane(pub Vec3, pub Vec3, pub Vec3);

impl<'de> Deserialize<'de> for Plane {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>
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
                ))(v) {
                    Ok((_, ((x0, y0, z0), (x1, y1, z1), (x2, y2, z2)))) => {
                        let x0 = x0.parse().map_err(|_| de::Error::invalid_value(de::Unexpected::Str(x0), &"float"))?;
                        let y0 = y0.parse().map_err(|_| de::Error::invalid_value(de::Unexpected::Str(y0), &"float"))?;
                        let z0 = z0.parse().map_err(|_| de::Error::invalid_value(de::Unexpected::Str(z0), &"float"))?;
                        let x1 = x1.parse().map_err(|_| de::Error::invalid_value(de::Unexpected::Str(x1), &"float"))?;
                        let y1 = y1.parse().map_err(|_| de::Error::invalid_value(de::Unexpected::Str(y1), &"float"))?;
                        let z1 = z1.parse().map_err(|_| de::Error::invalid_value(de::Unexpected::Str(z1), &"float"))?;
                        let x2 = x2.parse().map_err(|_| de::Error::invalid_value(de::Unexpected::Str(x2), &"float"))?;
                        let y2 = y2.parse().map_err(|_| de::Error::invalid_value(de::Unexpected::Str(y2), &"float"))?;
                        let z2 = z2.parse().map_err(|_| de::Error::invalid_value(de::Unexpected::Str(z2), &"float"))?;
                        Ok(Plane(
                            Vec3 {x: x0, y: y0, z: z0},
                            Vec3 {x: x1, y: y1, z: z1},
                            Vec3 {x: x2, y: y2, z: z2},
                        ))
                    },
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
        S: Serializer
    {
        serializer.serialize_str(&format!(
            "({} {} {}) ({} {} {}) ({} {} {})",
            self.0.x, self.0.y, self.0.z,
            self.1.x, self.1.y, self.1.z,
            self.2.x, self.2.y, self.2.z,
        ))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct UvAxis {
    pub axis: Vec3,
    pub translation: f64,
    pub scale: f64,
}

impl<'de> Deserialize<'de> for UvAxis {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>
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
                    bracketed(tuple((space_separated, space_separated, space_separated, space_separated))),
                    space_separated,
                ))(v) {
                    Ok((_, ((x, y, z, translation), scale))) => {
                        let x = x.parse().map_err(|_| de::Error::invalid_value(de::Unexpected::Str(x), &"float"))?;
                        let y = y.parse().map_err(|_| de::Error::invalid_value(de::Unexpected::Str(y), &"float"))?;
                        let z = z.parse().map_err(|_| de::Error::invalid_value(de::Unexpected::Str(z), &"float"))?;
                        let translation = translation.parse().map_err(|_| de::Error::invalid_value(de::Unexpected::Str(translation), &"float"))?;
                        let scale = scale.parse().map_err(|_| de::Error::invalid_value(de::Unexpected::Str(scale), &"float"))?;
                        Ok(UvAxis { axis: Vec3 { x, y, z }, translation, scale })
                    }
                    Err(..) => Err(de::Error::invalid_value(de::Unexpected::Str(v), &Self))
                }
            }
        }

        deserializer.deserialize_str(UvAxisVisitor)
    }
}

impl Serialize for UvAxis {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer
    {
        serializer.serialize_str(&format!(
            "[{} {} {} {}] {}",
            self.axis.x, self.axis.y, self.axis.z, self.translation, self.scale,
        ))
    }
}
