use crate::parsers::{space_separated, bracketed};

use std::{fmt, ops::{Deref, DerefMut}};

use nom::sequence::tuple;
use serde::{Deserialize, Deserializer, Serialize, Serializer, de::Visitor, de};

#[derive(Debug, Clone, Copy)]
pub struct Vector2 {
    pub x: f64,
    pub y: f64,
}

impl<'de> Deserialize<'de> for Vector2 {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>
    {
        struct Vec2Visitor;

        impl<'de> Visitor<'de> for Vec2Visitor {
            type Value = Vector2;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("vector2")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                match tuple((space_separated, space_separated))(v) {
                    Ok((_, (x, y))) => {
                        let x = x.parse().map_err(|_| de::Error::invalid_value(de::Unexpected::Str(x), &"float"))?;
                        let y = y.parse().map_err(|_| de::Error::invalid_value(de::Unexpected::Str(y), &"float"))?;
                        Ok(Vector2 { x, y})
                    }
                    Err(..) => Err(de::Error::invalid_value(de::Unexpected::Str(v), &Self))
                }
            }
        }

        deserializer.deserialize_str(Vec2Visitor)
    }
}

impl Serialize for Vector2 {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer
    {
        serializer.serialize_str(&format!("{} {}", self.x, self.y))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Vector3 {
    pub x: f64,
    pub y: f64,
    pub z: f64,
}

impl<'de> Deserialize<'de> for Vector3 {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>
    {
        struct Vec3Visitor;

        impl<'de> Visitor<'de> for Vec3Visitor {
            type Value = Vector3;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("vector3")
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
                        Ok(Vector3 { x, y, z })
                    }
                    Err(..) => Err(de::Error::invalid_value(de::Unexpected::Str(v), &Self))
                }
            }
        }

        deserializer.deserialize_str(Vec3Visitor)
    }
}

impl Serialize for Vector3 {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer
    {
        serializer.serialize_str(&format!("{} {} {}", self.x, self.y, self.z))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BracketedVector2(Vector2);

impl Deref for BracketedVector2 {
    type Target = Vector2;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for BracketedVector2 {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'de> Deserialize<'de> for BracketedVector2 {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>
    {
        struct SquareBracketedVec2Visitor;

        impl<'de> Visitor<'de> for SquareBracketedVec2Visitor {
            type Value = BracketedVector2;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("square bracketed vector2")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                match bracketed(tuple((space_separated, space_separated)))(v) {
                    Ok((_, (x, y))) => {
                        let x = x.parse().map_err(|_| de::Error::invalid_value(de::Unexpected::Str(x), &"float"))?;
                        let y = y.parse().map_err(|_| de::Error::invalid_value(de::Unexpected::Str(y), &"float"))?;
                        Ok(BracketedVector2(Vector2 { x, y }))
                    }
                    Err(..) => Err(de::Error::invalid_value(de::Unexpected::Str(v), &Self))
                }
            }
        }

        deserializer.deserialize_str(SquareBracketedVec2Visitor)
    }
}

impl Serialize for BracketedVector2 {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer
    {
        serializer.serialize_str(&format!("[{} {}]", self.x, self.y))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BracketedVector3(Vector3);

impl Deref for BracketedVector3 {
    type Target = Vector3;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for BracketedVector3 {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'de> Deserialize<'de> for BracketedVector3 {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>
    {
        struct SquareBracketedVec3Visitor;

        impl<'de> Visitor<'de> for SquareBracketedVec3Visitor {
            type Value = BracketedVector3;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("square bracketed vector3")
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
                        Ok(BracketedVector3(Vector3 { x, y, z }))
                    }
                    Err(..) => Err(de::Error::invalid_value(de::Unexpected::Str(v), &Self))
                }
            }
        }

        deserializer.deserialize_str(SquareBracketedVec3Visitor)
    }
}

impl Serialize for BracketedVector3 {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer
    {
        serializer.serialize_str(&format!("[{} {} {}]", self.x, self.y, self.z))
    }
}
