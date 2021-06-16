use std::collections::BTreeMap;

use serde::{
    de::{self, MapAccess, Visitor},
    Deserialize, Serialize,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    String(String),
    Class(BTreeMap<String, Value>),
}

impl From<BTreeMap<String, Value>> for Value {
    fn from(v: BTreeMap<String, Value>) -> Self {
        Self::Class(v)
    }
}

impl From<String> for Value {
    fn from(v: String) -> Self {
        Self::String(v)
    }
}

impl PartialEq<String> for Value {
    fn eq(&self, other: &String) -> bool {
        if let Self::String(string) = self {
            string == other
        } else {
            false
        }
    }
}

impl Value {
    /// Returns `true` if the value is a string.
    #[must_use]
    pub fn is_string(&self) -> bool {
        matches!(self, Self::String(..))
    }

    /// Returns `true` if the value is a class.
    #[must_use]
    pub fn is_class(&self) -> bool {
        matches!(self, Self::Class(..))
    }

    #[must_use]
    pub fn as_string(&self) -> Option<&String> {
        if let Self::String(v) = self {
            Some(v)
        } else {
            None
        }
    }

    #[must_use]
    pub fn as_class(&self) -> Option<&BTreeMap<String, Value>> {
        if let Self::Class(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

impl<'de> Deserialize<'de> for Value {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct ValueVisitor;

        impl<'de> Visitor<'de> for ValueVisitor {
            type Value = Value;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("any valid vdf value")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let mut values = BTreeMap::new();
                while let Some((key, value)) = map.next_entry()? {
                    values.insert(key, value);
                }
                Ok(Value::Class(values))
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(Value::String(v.into()))
            }

            fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(Value::String(v))
            }
        }

        deserializer.deserialize_any(ValueVisitor)
    }
}

impl Serialize for Value {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            Value::String(str) => str.serialize(serializer),
            Value::Class(cls) => cls.serialize(serializer),
        }
    }
}
