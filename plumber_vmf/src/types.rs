#![allow(clippy::wildcard_imports)]

use std::fmt::{self, Display};

use glam::{Vec2, Vec3};
use nom::sequence::tuple;
use rgb::RGB8;
use serde::{
    de::{self, Visitor},
    Deserialize, Deserializer, Serialize, Serializer,
};

use plumber_vdf::nom_utils::{bracketed, parenthesed, space_separated};

pub mod color {
    use super::*;

    #[allow(clippy::trivially_copy_pass_by_ref)]
    pub fn serialize<S>(color: &RGB8, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        Color::serialize(&Color(*color), serializer)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<RGB8, D::Error>
    where
        D: Deserializer<'de>,
    {
        Color::deserialize(deserializer).map(|color| color.0)
    }
}

pub struct Color(pub RGB8);

impl<'de> Deserialize<'de> for Color {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct RgbVisitor;

        impl<'de> Visitor<'de> for RgbVisitor {
            type Value = Color;

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
                        Ok(Color(RGB8 { r, g, b }))
                    }
                    Err(..) => Err(de::Error::invalid_value(de::Unexpected::Str(v), &Self)),
                }
            }
        }

        deserializer.deserialize_str(RgbVisitor)
    }
}

impl Serialize for Color {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.collect_str(self)
    }
}

impl Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.0.r, self.0.g, self.0.b)
    }
}

pub struct Vector2(pub Vec2);

impl<'de> Deserialize<'de> for Vector2 {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
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
                        let x = x.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(x), &"float")
                        })?;
                        let y = y.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(y), &"float")
                        })?;
                        Ok(Vector2(Vec2::new(x, y)))
                    }
                    Err(..) => Err(de::Error::invalid_value(de::Unexpected::Str(v), &Self)),
                }
            }
        }

        deserializer.deserialize_str(Vec2Visitor)
    }
}

impl Serialize for Vector2 {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.collect_str(self)
    }
}

impl Display for Vector2 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.0.x, self.0.y)
    }
}

pub struct Vector3(pub Vec3);

impl<'de> Deserialize<'de> for Vector3 {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
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
                        let x = x.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(x), &"float")
                        })?;
                        let y = y.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(y), &"float")
                        })?;
                        let z = z.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(z), &"float")
                        })?;
                        Ok(Vector3(Vec3::new(x, y, z)))
                    }
                    Err(..) => Err(de::Error::invalid_value(de::Unexpected::Str(v), &Self)),
                }
            }
        }

        deserializer.deserialize_str(Vec3Visitor)
    }
}

impl Serialize for Vector3 {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.collect_str(self)
    }
}

impl Display for Vector3 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.0.x, self.0.y, self.0.z)
    }
}

pub mod bracketed_vector2 {
    use super::*;

    pub mod option {
        use super::*;

        pub fn serialize<S>(vector: &Option<Vec2>, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            Option::<BracketedVector2>::serialize(&vector.map(BracketedVector2), serializer)
        }

        pub fn deserialize<'de, D>(deserializer: D) -> Result<Option<Vec2>, D::Error>
        where
            D: Deserializer<'de>,
        {
            Option::<BracketedVector2>::deserialize(deserializer)
                .map(|option| option.map(|vector| vector.0))
        }
    }
}

pub struct BracketedVector2(pub Vec2);

impl<'de> Deserialize<'de> for BracketedVector2 {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
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
                        let x = x.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(x), &"float")
                        })?;
                        let y = y.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(y), &"float")
                        })?;
                        Ok(BracketedVector2(Vec2::new(x, y)))
                    }
                    Err(..) => Err(de::Error::invalid_value(de::Unexpected::Str(v), &Self)),
                }
            }
        }

        deserializer.deserialize_str(SquareBracketedVec2Visitor)
    }
}

impl Serialize for BracketedVector2 {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.collect_str(self)
    }
}

impl Display for BracketedVector2 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{} {}]", self.0.x, self.0.y)
    }
}

pub mod bracketed_vector3 {
    use super::*;

    pub fn serialize<S>(vector: &Vec3, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        BracketedVector3::serialize(&BracketedVector3(*vector), serializer)
    }
}

pub struct BracketedVector3(pub Vec3);

impl<'de> Deserialize<'de> for BracketedVector3 {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
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
                        let x = x.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(x), &"float")
                        })?;
                        let y = y.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(y), &"float")
                        })?;
                        let z = z.parse().map_err(|_| {
                            de::Error::invalid_value(de::Unexpected::Str(z), &"float")
                        })?;
                        Ok(BracketedVector3(Vec3::new(x, y, z)))
                    }
                    Err(..) => Err(de::Error::invalid_value(de::Unexpected::Str(v), &Self)),
                }
            }
        }

        deserializer.deserialize_str(SquareBracketedVec3Visitor)
    }
}

impl Serialize for BracketedVector3 {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.collect_str(self)
    }
}

impl Display for BracketedVector3 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{} {} {}]", self.0.x, self.0.y, self.0.z)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Plane(pub Vec3, pub Vec3, pub Vec3);

impl Default for Plane {
    fn default() -> Self {
        Self(
            Vec3::new(0.0, 0.0, 0.0),
            Vec3::new(0.0, 0.0, 0.0),
            Vec3::new(0.0, 0.0, 0.0),
        )
    }
}

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
                            Vec3::new(x0, y0, z0),
                            Vec3::new(x1, y1, z1),
                            Vec3::new(x2, y2, z2),
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

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct UvAxis {
    pub axis: Vec3,
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
                            axis: Vec3::new(x, y, z),
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
