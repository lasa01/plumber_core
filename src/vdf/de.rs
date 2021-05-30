use std::{
    borrow::Cow,
    result,
    str::{self, FromStr},
};

use nom::{IResult, Offset};
use serde::{
    de::{
        self,
        value::{BorrowedStrDeserializer, StrDeserializer},
        EnumAccess, IntoDeserializer, MapAccess, SeqAccess, VariantAccess,
    },
    Deserialize,
};

use super::{
    error::{Error, Position, Reason, Result},
    escape::maybe_unescape_str,
    parsers,
};

/// # Errors
///
/// Returns `Err` if the deserialization fails.
pub fn from_str<'de, T>(input: &'de str) -> Result<T>
where
    T: Deserialize<'de>,
{
    let mut deserializer = Deserializer::from_str(input);
    let t = T::deserialize(&mut deserializer).map_err(|err| err.with_position(&deserializer))?;
    Ok(t)
}

/// # Errors
///
/// Returns `Err` if the deserialization fails.
pub fn from_bytes<'de, T>(input: &'de [u8]) -> Result<T>
where
    T: Deserialize<'de>,
{
    let mut deserializer = Deserializer::from_bytes(input);
    let t = T::deserialize(&mut deserializer).map_err(|err| err.with_position(&deserializer))?;
    Ok(t)
}

/// # Errors
///
/// Returns `Err` if the deserialization fails.
pub fn escaped_from_str<'de, T>(input: &'de str) -> Result<T>
where
    T: Deserialize<'de>,
{
    let mut deserializer = Deserializer::escaped_from_str(input);
    let t = T::deserialize(&mut deserializer).map_err(|err| err.with_position(&deserializer))?;
    Ok(t)
}

/// # Errors
///
/// Returns `Err` if the deserialization fails.
pub fn escaped_from_bytes<'de, T>(input: &'de [u8]) -> Result<T>
where
    T: Deserialize<'de>,
{
    let mut deserializer = Deserializer::escaped_from_bytes(input);
    let t = T::deserialize(&mut deserializer).map_err(|err| err.with_position(&deserializer))?;
    Ok(t)
}

#[must_use]
pub struct Deserializer<'de> {
    original_input: &'de [u8],
    input: &'de [u8],
    remaining_depth: u8,
    last_key: Option<Cow<'de, [u8]>>,
    escaped: bool,
}

impl<'de> Deserializer<'de> {
    pub fn from_str(input: &'de str) -> Self {
        Self::new(input.as_bytes(), false)
    }

    pub fn from_bytes(input: &'de [u8]) -> Self {
        Self::new(input, false)
    }

    pub fn escaped_from_str(input: &'de str) -> Self {
        Self::new(input.as_bytes(), true)
    }

    pub fn escaped_from_bytes(input: &'de [u8]) -> Self {
        Self::new(input, true)
    }

    fn new(input: &'de [u8], escaped: bool) -> Self {
        Self {
            original_input: <&[u8]>::clone(&input),
            input,
            remaining_depth: 128,
            last_key: None,
            escaped,
        }
    }

    #[must_use]
    pub fn get_position(&self) -> Position {
        let offset = self.original_input.offset(self.input);
        let prefix = &self.original_input[..offset];
        let line = bytecount::count(prefix, b'\n') + 1;
        let column = prefix.iter().rev().position(|&b| b == b'\n').unwrap_or(0) + 1;
        Position { line, column }
    }

    fn parse<O, P>(
        &mut self,
        mut parser: P,
    ) -> result::Result<O, nom::Err<nom::error::Error<&'de [u8]>>>
    where
        P: FnMut(&'de [u8]) -> IResult<&'de [u8], O>,
    {
        let (rem, out) = parser(self.input)?;
        self.input = rem;
        Ok(out)
    }

    fn peek_char(&mut self) -> Result<char> {
        self.parse(parsers::peeked_char)
            .map_err(|_| Error::new(Reason::UnexpectedEof))
    }

    fn parse_value(&mut self) -> Result<&'de [u8]> {
        self.parse(parsers::any_value)
            .map_err(|_| Error::new(Reason::ExpectedValue))
    }

    fn parse_escaped_value(&mut self) -> Result<Cow<'de, [u8]>> {
        let value = self
            .parse(parsers::any_escaped_value)
            .map_err(|_| Error::new(Reason::ExpectedValue))?;
        Ok(maybe_unescape_str(value))
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
        str::from_utf8(value)?
            .parse()
            .map_err(|_| Error::new(Reason::InvalidInt))
    }

    fn parse_float<F>(&mut self) -> Result<F>
    where
        F: FromStr,
    {
        let value = self.parse_value()?;
        str::from_utf8(value)?
            .parse()
            .map_err(|_| Error::new(Reason::InvalidFloat))
    }

    fn parse_key(&mut self) -> Result<&'de [u8]> {
        self.parse(parsers::any_key)
            .map_err(|_| Error::new(Reason::ExpectedValue))
    }

    fn parse_escaped_key(&mut self) -> Result<Cow<'de, [u8]>> {
        let key = self
            .parse(parsers::any_escaped_key)
            .map_err(|_| Error::new(Reason::ExpectedValue))?;
        Ok(maybe_unescape_str(key))
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

    fn parsed_block_end_early(&mut self) -> bool {
        self.parse(parsers::block_end_early).is_ok()
    }

    fn peeked_block_end(&mut self) -> bool {
        self.parse(parsers::peeked_block_end).is_ok()
    }

    fn parsed_block_sep_and_token(&mut self, token: &[u8]) -> bool {
        self.parse(parsers::block_sep_and_token(token)).is_ok()
    }

    fn parsed_eof(&mut self) -> bool {
        self.parse(parsers::comment_eof).is_ok()
    }
}

impl<'de_ref, 'de> de::Deserializer<'de> for &'de_ref mut Deserializer<'de> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_map(visitor)
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match self.parse_value()? {
            b"0" => visitor.visit_bool(false),
            b"1" => visitor.visit_bool(true),
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
                Cow::Borrowed(str) => visitor.visit_borrowed_str(str::from_utf8(str)?),
                Cow::Owned(string) => visitor.visit_string(String::from_utf8(string)?),
            }
        } else {
            visitor.visit_borrowed_str(str::from_utf8(self.parse_value()?)?)
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
        visitor.visit_seq(RootAccess::new(self))
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
        visitor.visit_map(RootAccess::new(self))
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
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_enum(RootAccess::new(self))
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

struct RootAccess<'de_ref, 'de> {
    deserializer: &'de_ref mut Deserializer<'de>,
    first: bool,
}

impl<'de_ref, 'de> RootAccess<'de_ref, 'de> {
    fn new(value: &'de_ref mut Deserializer<'de>) -> Self {
        Self {
            deserializer: value,
            first: true,
        }
    }
}

impl<'de_ref, 'de> SeqAccess<'de> for RootAccess<'de_ref, 'de> {
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
        seed.deserialize(ValueDeserializer::new(&mut *self.deserializer))
            .map(Some)
    }
}

impl<'de_ref, 'de> MapAccess<'de> for RootAccess<'de_ref, 'de> {
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
            let value = seed.deserialize::<StrDeserializer<Error>>(
                str::from_utf8(key.as_ref())?.into_deserializer(),
            )?;
            (key, value)
        } else {
            let key = self.deserializer.parse_key()?;
            (
                Cow::Borrowed(key),
                seed.deserialize(BorrowedStrDeserializer::<Error>::new(str::from_utf8(key)?))?,
            )
        };
        self.deserializer.last_key = Some(key);
        Ok(Some(value))
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: de::DeserializeSeed<'de>,
    {
        seed.deserialize(ValueDeserializer::new(&mut *self.deserializer))
    }
}

impl<'de_ref, 'de> EnumAccess<'de> for RootAccess<'de_ref, 'de> {
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant)>
    where
        V: de::DeserializeSeed<'de>,
    {
        let (key, value) = if self.deserializer.escaped {
            let key = self.deserializer.parse_escaped_key()?;
            let value = seed.deserialize::<StrDeserializer<Error>>(
                str::from_utf8(key.as_ref())?.into_deserializer(),
            )?;
            (key, value)
        } else {
            let key = self.deserializer.parse_key()?;
            (
                Cow::Borrowed(key),
                seed.deserialize(BorrowedStrDeserializer::<Error>::new(str::from_utf8(key)?))?,
            )
        };
        self.deserializer.last_key = Some(key);
        Ok((value, self))
    }
}

impl<'de_ref, 'de> VariantAccess<'de> for RootAccess<'de_ref, 'de> {
    type Error = Error;

    fn unit_variant(self) -> Result<()> {
        self.deserializer.parse_empty_token()
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value>
    where
        T: de::DeserializeSeed<'de>,
    {
        seed.deserialize(ValueDeserializer::new(&mut *self.deserializer))
    }

    fn tuple_variant<V>(self, _len: usize, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        de::Deserializer::deserialize_seq(ValueDeserializer::new(&mut *self.deserializer), visitor)
    }

    fn struct_variant<V>(self, _fields: &'static [&'static str], visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        de::Deserializer::deserialize_map(ValueDeserializer::new(&mut *self.deserializer), visitor)
    }
}

struct ValueDeserializer<'de_ref, 'de> {
    deserializer: &'de_ref mut Deserializer<'de>,
}

impl<'de_ref, 'de> ValueDeserializer<'de_ref, 'de> {
    fn new(deserializer: &'de_ref mut Deserializer<'de>) -> Self {
        Self { deserializer }
    }
}

impl<'de_ref, 'de> de::Deserializer<'de> for ValueDeserializer<'de_ref, 'de> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match self.deserializer.peek_char()? {
            '{' => self.deserialize_map(visitor),
            _ => de::Deserializer::deserialize_str(self.deserializer, visitor),
        }
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserializer.deserialize_bool(visitor)
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserializer.deserialize_i8(visitor)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserializer.deserialize_i16(visitor)
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserializer.deserialize_i32(visitor)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserializer.deserialize_i64(visitor)
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserializer.deserialize_u8(visitor)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserializer.deserialize_u16(visitor)
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserializer.deserialize_u32(visitor)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserializer.deserialize_u64(visitor)
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserializer.deserialize_f32(visitor)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserializer.deserialize_f64(visitor)
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserializer.deserialize_char(visitor)
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserializer.deserialize_str(visitor)
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserializer.deserialize_string(visitor)
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserializer.deserialize_bytes(visitor)
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserializer.deserialize_byte_buf(visitor)
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if let Ok(..) = self.deserializer.parse_empty_token() {
            visitor.visit_none()
        } else {
            visitor.visit_some(self)
        }
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserializer.deserialize_unit(visitor)
    }

    fn deserialize_unit_struct<V>(self, name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserializer.deserialize_unit_struct(name, visitor)
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
        let element_key = self
            .deserializer
            .last_key
            .take()
            .ok_or_else(|| Error::new(Reason::SequenceUnknownKey))?;

        self.deserializer.remaining_depth -= 1;
        if self.deserializer.remaining_depth == 0 {
            return Err(Error::new(Reason::Recursion));
        }

        let res = visitor.visit_seq(SeqValueAccess::new(
            ValueDeserializer::new(&mut *self.deserializer),
            element_key,
        ));

        self.deserializer.remaining_depth += 1;

        res
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
        self.deserializer.parse_block_start()?;

        self.deserializer.remaining_depth -= 1;
        if self.deserializer.remaining_depth == 0 {
            return Err(Error::new(Reason::Recursion));
        }

        let res = visitor.visit_map(ValueAccess::new(ValueDeserializer::new(
            &mut *self.deserializer,
        )));

        self.deserializer.remaining_depth += 1;
        res
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
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match self.deserializer.peek_char()? {
            '{' => {
                self.deserializer.parse_block_start()?;

                self.deserializer.remaining_depth -= 1;
                if self.deserializer.remaining_depth == 0 {
                    return Err(Error::new(Reason::Recursion));
                }

                let value = visitor.visit_enum(ValueAccess::new(ValueDeserializer::new(
                    &mut *self.deserializer,
                )))?;

                self.deserializer.remaining_depth += 1;
                self.deserializer.parse_block_end()?;
                Ok(value)
            }
            _ => visitor.visit_enum(BorrowedStrDeserializer::new(str::from_utf8(
                self.deserializer.parse_value()?,
            )?)),
        }
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserializer.deserialize_identifier(visitor)
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_any(visitor)
    }
}

struct ValueAccess<'de_ref, 'de> {
    value: ValueDeserializer<'de_ref, 'de>,
    first: bool,
}

impl<'de_ref, 'de> ValueAccess<'de_ref, 'de> {
    fn new(value: ValueDeserializer<'de_ref, 'de>) -> Self {
        Self { value, first: true }
    }
}

impl<'de_ref, 'de> MapAccess<'de> for ValueAccess<'de_ref, 'de> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: de::DeserializeSeed<'de>,
    {
        if !self.first {
            if self.value.deserializer.parse_block_end().is_ok() {
                return Ok(None);
            }
            self.value.deserializer.parse_block_sep()?;
        } else if self.value.deserializer.parsed_block_end_early() {
            return Ok(None);
        }
        self.first = false;
        let (key, value) = if self.value.deserializer.escaped {
            let key = self.value.deserializer.parse_escaped_key()?;
            let value = seed.deserialize::<StrDeserializer<Error>>(
                str::from_utf8(key.as_ref())?.into_deserializer(),
            )?;
            (key, value)
        } else {
            let key = self.value.deserializer.parse_key()?;
            (
                Cow::Borrowed(key),
                seed.deserialize(BorrowedStrDeserializer::<Error>::new(str::from_utf8(key)?))?,
            )
        };
        self.value.deserializer.last_key = Some(key);
        Ok(Some(value))
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: de::DeserializeSeed<'de>,
    {
        seed.deserialize(ValueDeserializer::new(&mut *self.value.deserializer))
    }
}

impl<'de_ref, 'de> EnumAccess<'de> for ValueAccess<'de_ref, 'de> {
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant)>
    where
        V: de::DeserializeSeed<'de>,
    {
        let (key, value) = if self.value.deserializer.escaped {
            let key = self.value.deserializer.parse_escaped_key()?;
            let value = seed.deserialize::<StrDeserializer<Error>>(
                str::from_utf8(key.as_ref())?.into_deserializer(),
            )?;
            (key, value)
        } else {
            let key = self.value.deserializer.parse_key()?;
            (
                Cow::Borrowed(key),
                seed.deserialize(BorrowedStrDeserializer::<Error>::new(str::from_utf8(key)?))?,
            )
        };
        self.value.deserializer.last_key = Some(key);
        Ok((value, self))
    }
}

impl<'de_ref, 'de> VariantAccess<'de> for ValueAccess<'de_ref, 'de> {
    type Error = Error;

    fn unit_variant(self) -> Result<()> {
        self.value.deserializer.parse_empty_token()
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value>
    where
        T: de::DeserializeSeed<'de>,
    {
        seed.deserialize(self.value)
    }

    fn tuple_variant<V>(self, _len: usize, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        de::Deserializer::deserialize_seq(self.value, visitor)
    }

    fn struct_variant<V>(self, _fields: &'static [&'static str], visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        de::Deserializer::deserialize_map(self.value, visitor)
    }
}

struct SeqValueAccess<'de_ref, 'de> {
    value: ValueDeserializer<'de_ref, 'de>,
    first: bool,
    element_key: Cow<'de, [u8]>,
}

impl<'de_ref, 'de> SeqValueAccess<'de_ref, 'de> {
    fn new(value: ValueDeserializer<'de_ref, 'de>, element_key: Cow<'de, [u8]>) -> Self {
        Self {
            value,
            first: true,
            element_key,
        }
    }
}

impl<'de_ref, 'de> SeqAccess<'de> for SeqValueAccess<'de_ref, 'de> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: de::DeserializeSeed<'de>,
    {
        if self.value.deserializer.peeked_block_end() {
            return Ok(None);
        }
        if !self.first
            && !self
                .value
                .deserializer
                .parsed_block_sep_and_token(self.element_key.as_ref())
        {
            return Ok(None);
        }
        self.first = false;
        seed.deserialize(ValueDeserializer::new(&mut *self.value.deserializer))
            .map(Some)
    }
}
