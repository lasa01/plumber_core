use std::{borrow::Cow, marker::PhantomData, result, str::FromStr};

use nom::{IResult, Offset};
use serde::{
    de::{
        self, value::BorrowedStrDeserializer, EnumAccess, IntoDeserializer, MapAccess, SeqAccess,
        VariantAccess,
    },
    Deserialize,
};

use super::{
    error::{Error, Position, Reason, Result},
    parsers,
};

/// # Errors
///
/// Will return `Err` if the deserialization fails.
pub fn from_str<'de, T>(mut input: &'de str) -> Result<T>
where
    T: Deserialize<'de>,
{
    let mut deserializer = Deserializer::from_str(&mut input);
    let t = T::deserialize(&mut deserializer).map_err(|err| err.with_position(&deserializer))?;
    Ok(t)
}

/// # Errors
///
/// Will return `Err` if the deserialization fails.
pub fn escaped_from_str<'de, T>(mut input: &'de str) -> Result<T>
where
    T: Deserialize<'de>,
{
    let mut deserializer = Deserializer::escaped_from_str(&mut input);
    let t = T::deserialize(&mut deserializer).map_err(|err| err.with_position(&deserializer))?;
    Ok(t)
}

pub struct Deserializer<'a, 'de> {
    inner: DeserializerImpl<'a, 'de, Root>,
}

impl<'a, 'de> Deserializer<'a, 'de> {
    pub fn from_str(input: &'a mut &'de str) -> Self {
        Self {
            inner: DeserializerImpl::<'_, '_, Root>::new(input, false),
        }
    }

    pub fn escaped_from_str(input: &'a mut &'de str) -> Self {
        Self {
            inner: DeserializerImpl::<'_, '_, Root>::new(input, true),
        }
    }

    #[must_use]
    pub fn get_position(&self) -> Position {
        self.inner.get_position()
    }
}

impl<'de_ref, 'a, 'de> serde::Deserializer<'de> for &'de_ref mut Deserializer<'a, 'de> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.inner.deserialize_any(visitor)
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.inner.deserialize_bool(visitor)
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.inner.deserialize_i8(visitor)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.inner.deserialize_i16(visitor)
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.inner.deserialize_i32(visitor)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.inner.deserialize_i64(visitor)
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.inner.deserialize_u8(visitor)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.inner.deserialize_u16(visitor)
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.inner.deserialize_u32(visitor)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.inner.deserialize_u64(visitor)
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.inner.deserialize_f32(visitor)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.inner.deserialize_f64(visitor)
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.inner.deserialize_char(visitor)
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.inner.deserialize_str(visitor)
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.inner.deserialize_string(visitor)
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.inner.deserialize_bytes(visitor)
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.inner.deserialize_byte_buf(visitor)
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.inner.deserialize_option(visitor)
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.inner.deserialize_unit(visitor)
    }

    fn deserialize_unit_struct<V>(self, name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.inner.deserialize_unit_struct(name, visitor)
    }

    fn deserialize_newtype_struct<V>(self, name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.inner.deserialize_newtype_struct(name, visitor)
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.inner.deserialize_seq(visitor)
    }

    fn deserialize_tuple<V>(self, len: usize, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.inner.deserialize_tuple(len, visitor)
    }

    fn deserialize_tuple_struct<V>(
        self,
        name: &'static str,
        len: usize,
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.inner.deserialize_tuple_struct(name, len, visitor)
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.inner.deserialize_map(visitor)
    }

    fn deserialize_struct<V>(
        self,
        name: &'static str,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.inner.deserialize_struct(name, fields, visitor)
    }

    fn deserialize_enum<V>(
        self,
        name: &'static str,
        variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.inner.deserialize_enum(name, variants, visitor)
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.inner.deserialize_identifier(visitor)
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.inner.deserialize_ignored_any(visitor)
    }
}

struct DeserializerImpl<'lvl, 'de: 'lvl, T: DeserializerLevel<'lvl, 'de>> {
    original_input: &'de str,
    input: &'lvl mut &'de str,
    last_key: Option<Cow<'de, str>>,
    escaped: bool,
    marker: PhantomData<T>,
}

impl<'lvl, 'de, T> DeserializerImpl<'lvl, 'de, T>
where
    T: DeserializerLevel<'lvl, 'de>,
{
    fn new(input: &'lvl mut &'de str, escaped: bool) -> Self {
        Self {
            original_input: <&str>::clone(input),
            input,
            last_key: None,
            marker: PhantomData,
            escaped,
        }
    }

    fn inner_level<'a, L>(&'a mut self) -> DeserializerImpl<'a, 'de, L>
    where
        L: DeserializerLevel<'a, 'de>,
    {
        DeserializerImpl {
            original_input: self.original_input,
            input: self.input,
            last_key: None,
            marker: PhantomData,
            escaped: self.escaped,
        }
    }

    fn get_position(&self) -> Position {
        let offset = self.original_input.offset(self.input);
        let prefix = &self.original_input.as_bytes()[..offset];
        let line = bytecount::count(prefix, b'\n') + 1;
        let column = prefix.iter().rev().position(|&b| b == b'\n').unwrap_or(0) + 1;
        Position { line, column }
    }

    fn parse<O, P>(
        &mut self,
        mut parser: P,
    ) -> result::Result<O, nom::Err<nom::error::Error<&'de str>>>
    where
        P: FnMut(&'de str) -> IResult<&'de str, O>,
    {
        let (rem, out) = parser(self.input)?;
        *self.input = rem;
        Ok(out)
    }

    fn peek_char(&mut self) -> Result<char> {
        self.parse(parsers::peeked_char)
            .map_err(|_| Error::new(Reason::UnexpectedEof))
    }

    fn parse_value(&mut self) -> Result<&'de str> {
        self.parse(parsers::any_value)
            .map_err(|_| Error::new(Reason::ExpectedValue))
    }

    fn parse_escaped_value(&mut self) -> Result<Cow<'de, str>> {
        let value = self
            .parse(parsers::any_escaped_value)
            .map_err(|_| Error::new(Reason::ExpectedValue))?;
        Ok(parsers::maybe_escape_str(value))
    }

    fn parse_empty_token(&mut self) -> Result<()> {
        self.parse(parsers::empty_token)
            .map_err(|_| Error::new(Reason::ExpectedEmptyValue))?;
        Ok(())
    }

    fn parse_int<I>(&mut self) -> Result<I>
    where
        I: FromStr,
    {
        let value = self.parse_value()?;
        value.parse().map_err(|_| Error::new(Reason::InvalidInt))
    }

    fn parse_float<F>(&mut self) -> Result<F>
    where
        F: FromStr,
    {
        let value = self.parse_value()?;
        value.parse().map_err(|_| Error::new(Reason::InvalidFloat))
    }

    fn parse_key(&mut self) -> Result<&'de str> {
        self.parse(parsers::any_key)
            .map_err(|_| Error::new(Reason::ExpectedValue))
    }

    fn parse_escaped_key(&mut self) -> Result<Cow<'de, str>> {
        let key = self
            .parse(parsers::any_escaped_key)
            .map_err(|_| Error::new(Reason::ExpectedValue))?;
        Ok(parsers::maybe_escape_str(key))
    }

    fn parse_block_sep(&mut self) -> Result<()> {
        self.parse(parsers::block_sep)
            .map_err(|_| Error::new(Reason::ExpectedNewline))
    }

    fn parse_block_start(&mut self) -> Result<()> {
        self.parse(parsers::block_start)
            .map_err(|_| Error::new(Reason::ExpectedOpeningBracket))
    }

    fn parse_block_end(&mut self) -> Result<()> {
        self.parse(parsers::block_end)
            .map_err(|_| Error::new(Reason::ExpectedClosingBracket))
    }

    fn peeked_seq_end(&mut self) -> bool {
        self.parse(parsers::peeked_seq_end).is_ok()
    }

    fn parsed_block_sep_and_token(&mut self, token: &str) -> bool {
        self.parse(parsers::block_sep_and_token(token)).is_ok()
    }

    fn parsed_eof(&mut self) -> bool {
        self.parse(parsers::eof).is_ok()
    }
}

trait DeserializerLevel<'lvl, 'de>: Sized {
    fn deserialize_any<V>(
        visitor: V,
        deserializer: &mut DeserializerImpl<'lvl, 'de, Self>,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>;

    fn deserialize_seq<V>(
        visitor: V,
        deserializer: &mut DeserializerImpl<'lvl, 'de, Self>,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>;

    fn deserialize_map<V>(
        visitor: V,
        deserializer: &mut DeserializerImpl<'lvl, 'de, Self>,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>;

    fn deserialize_enum<V>(
        name: &'static str,
        variants: &'static [&'static str],
        visitor: V,
        deserializer: &mut DeserializerImpl<'lvl, 'de, Self>,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>;
}

impl<'de_ref, 'lvl, 'de, T> de::Deserializer<'de> for &'de_ref mut DeserializerImpl<'lvl, 'de, T>
where
    T: DeserializerLevel<'lvl, 'de>,
{
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        T::deserialize_any(visitor, &mut *self)
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match self.parse_value()? {
            "0" => visitor.visit_bool(false),
            "1" => visitor.visit_bool(true),
            _ => Err(Error::new(Reason::InvalidBool)),
        }
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_i8(self.parse_int()?)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_i16(self.parse_int()?)
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_i32(self.parse_int()?)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_i64(self.parse_int()?)
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_u8(self.parse_int()?)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_u16(self.parse_int()?)
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_u32(self.parse_int()?)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_u64(self.parse_int()?)
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_f32(self.parse_float()?)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_f64(self.parse_float()?)
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if self.escaped {
            match self.parse_escaped_value()? {
                Cow::Borrowed(str) => visitor.visit_borrowed_str(str),
                Cow::Owned(string) => visitor.visit_string(string),
            }
        } else {
            visitor.visit_borrowed_str(self.parse_value()?)
        }
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_bytes<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!("vdf cannot deserialize bytes")
    }

    fn deserialize_byte_buf<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!("vdf cannot deserialize bytes")
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Ok(..) = self.parse_empty_token() {
            visitor.visit_none()
        } else {
            visitor.visit_some(self)
        }
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.parse_empty_token()?;
        visitor.visit_unit()
    }

    fn deserialize_unit_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_unit(visitor)
    }

    fn deserialize_newtype_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        T::deserialize_seq(visitor, &mut *self)
    }

    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        T::deserialize_map(visitor, &mut *self)
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_map(visitor)
    }

    fn deserialize_enum<V>(
        self,
        name: &'static str,
        variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        T::deserialize_enum(name, variants, visitor, &mut *self)
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_any(visitor)
    }
}

pub struct Root;

impl<'lvl, 'de> DeserializerLevel<'lvl, 'de> for Root {
    fn deserialize_any<V>(
        visitor: V,
        deserializer: &mut DeserializerImpl<'lvl, 'de, Self>,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        Self::deserialize_map(visitor, deserializer)
    }

    fn deserialize_seq<V>(
        visitor: V,
        deserializer: &mut DeserializerImpl<'lvl, 'de, Self>,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_seq(RootAccess::new(&mut deserializer.inner_level::<Value>()))
    }

    fn deserialize_map<V>(
        visitor: V,
        deserializer: &mut DeserializerImpl<'lvl, 'de, Self>,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_map(RootAccess::new(&mut deserializer.inner_level::<Value>()))
    }

    fn deserialize_enum<V>(
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
        deserializer: &mut DeserializerImpl<'lvl, 'de, Self>,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_enum(RootAccess::new(&mut deserializer.inner_level::<Value>()))
    }
}

struct RootAccess<'de_ref, 'lvl, 'de> {
    deserializer: &'de_ref mut DeserializerImpl<'lvl, 'de, Value>,
    first: bool,
}

impl<'de_ref, 'lvl, 'de> RootAccess<'de_ref, 'lvl, 'de> {
    fn new(deserializer: &'de_ref mut DeserializerImpl<'lvl, 'de, Value>) -> Self {
        Self {
            deserializer,
            first: true,
        }
    }
}

impl<'de_ref, 'lvl, 'de> SeqAccess<'de> for RootAccess<'de_ref, 'lvl, 'de> {
    type Error = Error;

    fn next_element_seed<S>(&mut self, seed: S) -> Result<Option<S::Value>>
    where
        S: de::DeserializeSeed<'de>,
    {
        if self.deserializer.parsed_eof() {
            return Ok(None);
        }
        if !self.first {
            self.deserializer.parse_block_sep()?;
        }
        self.first = false;
        if self.deserializer.escaped {
            self.deserializer.parse_escaped_key()?;
        } else {
            self.deserializer.parse_key()?;
        }
        seed.deserialize(&mut *self.deserializer).map(Some)
    }
}

impl<'de_ref, 'lvl, 'de> MapAccess<'de> for RootAccess<'de_ref, 'lvl, 'de> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: de::DeserializeSeed<'de>,
    {
        if self.deserializer.parsed_eof() {
            return Ok(None);
        }
        if !self.first {
            self.deserializer.parse_block_sep()?;
        }
        self.first = false;
        let (key, value) = if self.deserializer.escaped {
            let key = self.deserializer.parse_escaped_key()?;
            let value = seed.deserialize(key.as_ref().into_deserializer())?;
            (key, value)
        } else {
            let key = self.deserializer.parse_key()?;
            (
                Cow::Borrowed(key),
                seed.deserialize(BorrowedStrDeserializer::new(key))?,
            )
        };
        self.deserializer.last_key = Some(key);
        Ok(Some(value))
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: de::DeserializeSeed<'de>,
    {
        seed.deserialize(&mut *self.deserializer)
    }
}

impl<'de_ref, 'lvl, 'de> EnumAccess<'de> for RootAccess<'de_ref, 'lvl, 'de> {
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant)>
    where
        V: de::DeserializeSeed<'de>,
    {
        let (key, value) = if self.deserializer.escaped {
            let key = self.deserializer.parse_escaped_key()?;
            let value = seed.deserialize(key.as_ref().into_deserializer())?;
            (key, value)
        } else {
            let key = self.deserializer.parse_key()?;
            (
                Cow::Borrowed(key),
                seed.deserialize(BorrowedStrDeserializer::new(key))?,
            )
        };
        self.deserializer.last_key = Some(key);
        Ok((value, self))
    }
}

impl<'de_ref, 'lvl, 'de> VariantAccess<'de> for RootAccess<'de_ref, 'lvl, 'de> {
    type Error = Error;

    fn unit_variant(self) -> Result<()> {
        self.deserializer.parse_empty_token()
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value>
    where
        T: de::DeserializeSeed<'de>,
    {
        seed.deserialize(self.deserializer)
    }

    fn tuple_variant<V>(self, _len: usize, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        de::Deserializer::deserialize_seq(self.deserializer, visitor)
    }

    fn struct_variant<V>(self, _fields: &'static [&'static str], visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        de::Deserializer::deserialize_map(self.deserializer, visitor)
    }
}

struct Value;

impl<'lvl, 'de> DeserializerLevel<'lvl, 'de> for Value {
    fn deserialize_any<V>(
        visitor: V,
        deserializer: &mut DeserializerImpl<'lvl, 'de, Self>,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match deserializer.peek_char()? {
            '{' => Self::deserialize_map(visitor, deserializer),
            _ => de::Deserializer::deserialize_str(deserializer, visitor),
        }
    }

    fn deserialize_seq<V>(
        visitor: V,
        deserializer: &mut DeserializerImpl<'lvl, 'de, Self>,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        let element_key = deserializer
            .last_key
            .take()
            .ok_or_else(|| Error::new(Reason::SequenceUnknownKey))?;
        visitor.visit_seq(SeqValueAccess::new(deserializer, element_key))
    }

    fn deserialize_map<V>(
        visitor: V,
        deserializer: &mut DeserializerImpl<'lvl, 'de, Self>,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        deserializer.parse_block_start()?;
        visitor.visit_map(ValueAccess::new(deserializer))
    }

    fn deserialize_enum<V>(
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
        deserializer: &mut DeserializerImpl<'lvl, 'de, Self>,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match deserializer.peek_char()? {
            '{' => {
                deserializer.parse_block_start()?;
                let value = visitor.visit_enum(ValueAccess::new(deserializer))?;
                deserializer.parse_block_end()?;
                Ok(value)
            }
            _ => visitor.visit_enum(BorrowedStrDeserializer::new(deserializer.parse_value()?)),
        }
    }
}

struct ValueAccess<'de_ref, 'lvl, 'de> {
    deserializer: &'de_ref mut DeserializerImpl<'lvl, 'de, Value>,
    first: bool,
}

impl<'de_ref, 'lvl, 'de> ValueAccess<'de_ref, 'lvl, 'de> {
    fn new(deserializer: &'de_ref mut DeserializerImpl<'lvl, 'de, Value>) -> Self {
        Self {
            deserializer,
            first: true,
        }
    }
}

impl<'de_ref, 'lvl, 'de> MapAccess<'de> for ValueAccess<'de_ref, 'lvl, 'de> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: de::DeserializeSeed<'de>,
    {
        if let Ok(..) = self.deserializer.parse_block_end() {
            return Ok(None);
        }
        if !self.first {
            self.deserializer.parse_block_sep()?;
        }
        self.first = false;
        let (key, value) = if self.deserializer.escaped {
            let key = self.deserializer.parse_escaped_key()?;
            let value = seed.deserialize(key.as_ref().into_deserializer())?;
            (key, value)
        } else {
            let key = self.deserializer.parse_key()?;
            (
                Cow::Borrowed(key),
                seed.deserialize(BorrowedStrDeserializer::new(key))?,
            )
        };
        self.deserializer.last_key = Some(key);
        Ok(Some(value))
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: de::DeserializeSeed<'de>,
    {
        seed.deserialize(&mut *self.deserializer)
    }
}

impl<'de_ref, 'lvl, 'de> EnumAccess<'de> for ValueAccess<'de_ref, 'lvl, 'de> {
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant)>
    where
        V: de::DeserializeSeed<'de>,
    {
        let (key, value) = if self.deserializer.escaped {
            let key = self.deserializer.parse_escaped_key()?;
            let value = seed.deserialize(key.as_ref().into_deserializer())?;
            (key, value)
        } else {
            let key = self.deserializer.parse_key()?;
            (
                Cow::Borrowed(key),
                seed.deserialize(BorrowedStrDeserializer::new(key))?,
            )
        };
        self.deserializer.last_key = Some(key);
        Ok((value, self))
    }
}

impl<'de_ref, 'lvl, 'de> VariantAccess<'de> for ValueAccess<'de_ref, 'lvl, 'de> {
    type Error = Error;

    fn unit_variant(self) -> Result<()> {
        self.deserializer.parse_empty_token()
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value>
    where
        T: de::DeserializeSeed<'de>,
    {
        seed.deserialize(self.deserializer)
    }

    fn tuple_variant<V>(self, _len: usize, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        de::Deserializer::deserialize_seq(self.deserializer, visitor)
    }

    fn struct_variant<V>(self, _fields: &'static [&'static str], visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        de::Deserializer::deserialize_map(self.deserializer, visitor)
    }
}

struct SeqValueAccess<'de_ref, 'lvl, 'de> {
    deserializer: &'de_ref mut DeserializerImpl<'lvl, 'de, Value>,
    first: bool,
    element_key: Cow<'de, str>,
}

impl<'de_ref, 'lvl, 'de> SeqValueAccess<'de_ref, 'lvl, 'de> {
    fn new(
        deserializer: &'de_ref mut DeserializerImpl<'lvl, 'de, Value>,
        element_key: Cow<'de, str>,
    ) -> Self {
        Self {
            deserializer,
            first: true,
            element_key,
        }
    }
}

impl<'de_ref, 'lvl, 'de> SeqAccess<'de> for SeqValueAccess<'de_ref, 'lvl, 'de> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: de::DeserializeSeed<'de>,
    {
        if self.deserializer.peeked_seq_end() {
            return Ok(None);
        }
        if !self.first
            && !self
                .deserializer
                .parsed_block_sep_and_token(self.element_key.as_ref())
        {
            return Ok(None);
        }
        self.first = false;
        seed.deserialize(&mut *self.deserializer).map(Some)
    }
}
