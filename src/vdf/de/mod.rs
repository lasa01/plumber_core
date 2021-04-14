mod parsers;

use std::{result, str::FromStr};

use nom::{IResult, Offset};
use serde::{Deserialize, de::{self, EnumAccess, MapAccess, SeqAccess, VariantAccess, value::BorrowedStrDeserializer}};

use super::{Error, Result, error::{Position, Reason}};

pub fn from_str<'de, T>(mut input: &'de str) -> Result<T>
where
    T: Deserialize<'de>,
{
    let mut deserializer = Deserializer::<'_, 'de, Root>::from_str(&mut input);
    let t = T::deserialize(&mut deserializer).map_err(|err| err.with_position(&deserializer))?;
    Ok(t)
}

pub struct Deserializer<'lvl, 'de: 'lvl, T: DeserializerLevel<'lvl, 'de>> {
    original_input: &'de str,
    input: &'lvl mut &'de str,
    pub level: T,
    last_key: &'de str,
}

impl<'lvl, 'de, T> Deserializer<'lvl, 'de, T>
where
    T: DeserializerLevel<'lvl, 'de>,
{
    pub fn from_str(input: &'lvl mut &'de str) -> Deserializer<'lvl, 'de, Root> {
        Deserializer {
            original_input: <&str>::clone(input),
            input,
            level: Root,
            last_key: "",
        }
    }

    fn inner_level<'a, L>(&'a mut self, level: L) -> Deserializer<'a, 'de, L>
    where
        L: DeserializerLevel<'a, 'de>,
    {
        Deserializer {
            original_input: self.original_input,
            input: self.input,
            level,
            last_key: "",
        }
    }

    pub(crate) fn get_position(&self) -> Position {
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

    fn parsed_block_sep_and_token(&mut self, token: &'de str) -> bool {
        self.parse(parsers::block_sep_and_token(token)).is_ok()
    }

    fn parsed_eof(&mut self) -> bool {
        self.parse(parsers::eof).is_ok()
    }
}

pub trait DeserializerLevel<'lvl, 'de>: Sized {
    fn deserialize_any<V>(
        visitor: V,
        wrapper: &mut Deserializer<'lvl, 'de, Self>,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>;

    fn deserialize_seq<V>(
        visitor: V,
        wrapper: &mut Deserializer<'lvl, 'de, Self>,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>;

    fn deserialize_map<V>(
        visitor: V,
        wrapper: &mut Deserializer<'lvl, 'de, Self>,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>;

    fn deserialize_enum<V>(
        name: &'static str,
        variants: &'static [&'static str],
        visitor: V,
        wrapper: &mut Deserializer<'lvl, 'de, Self>,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>;
}

impl<'wr, 'lvl, 'de, T> de::Deserializer<'de> for &'wr mut Deserializer<'lvl, 'de, T>
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
        visitor.visit_borrowed_str(self.parse_value()?)
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
        wrapper: &mut Deserializer<'lvl, 'de, Self>,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        Self::deserialize_map(visitor, wrapper)
    }

    fn deserialize_seq<V>(
        visitor: V,
        wrapper: &mut Deserializer<'lvl, 'de, Self>,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_seq(RootAccess::new(&mut wrapper.inner_level(Value)))
    }

    fn deserialize_map<V>(
        visitor: V,
        wrapper: &mut Deserializer<'lvl, 'de, Self>,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_map(RootAccess::new(&mut wrapper.inner_level(Value)))
    }

    fn deserialize_enum<V>(
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
        wrapper: &mut Deserializer<'lvl, 'de, Self>,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_enum(RootAccess::new(&mut wrapper.inner_level(Value)))
    }
}

struct RootAccess<'wr, 'lvl, 'de> {
    wrapper: &'wr mut Deserializer<'lvl, 'de, Value>,
    first: bool,
}

impl<'wr, 'lvl, 'de> RootAccess<'wr, 'lvl, 'de> {
    fn new(wrapper: &'wr mut Deserializer<'lvl, 'de, Value>) -> Self {
        Self {
            wrapper,
            first: true,
        }
    }
}

impl<'wr, 'lvl, 'de> SeqAccess<'de> for RootAccess<'wr, 'lvl, 'de> {
    type Error = Error;

    fn next_element_seed<S>(&mut self, seed: S) -> Result<Option<S::Value>>
    where
        S: de::DeserializeSeed<'de>,
    {
        if self.wrapper.parsed_eof() {
            return Ok(None);
        }
        if !self.first {
            self.wrapper.parse_block_sep()?;
        }
        self.first = false;
        self.wrapper.parse_key()?;
        seed.deserialize(&mut *self.wrapper).map(Some)
    }
}

impl<'wr, 'lvl, 'de> MapAccess<'de> for RootAccess<'wr, 'lvl, 'de> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: de::DeserializeSeed<'de>,
    {
        if self.wrapper.parsed_eof() {
            return Ok(None);
        }
        if !self.first {
            self.wrapper.parse_block_sep()?;
        }
        self.first = false;
        let key = self.wrapper.parse_key()?;
        self.wrapper.last_key = key;
        seed.deserialize(BorrowedStrDeserializer::new(key)).map(Some)
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: de::DeserializeSeed<'de>,
    {
        seed.deserialize(&mut *self.wrapper)
    }
}

impl<'wr, 'lvl, 'de> EnumAccess<'de> for RootAccess<'wr, 'lvl, 'de> {
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant)>
    where
        V: de::DeserializeSeed<'de>,
    {
        let key = self.wrapper.parse_key()?;
        self.wrapper.last_key = key;
        Ok((seed.deserialize(BorrowedStrDeserializer::new(key))?, self))
    }
}

impl<'wr, 'lvl, 'de> VariantAccess<'de> for RootAccess<'wr, 'lvl, 'de> {
    type Error = Error;

    fn unit_variant(self) -> Result<()> {
        self.wrapper.parse_empty_token()
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value>
    where
        T: de::DeserializeSeed<'de>,
    {
        seed.deserialize(self.wrapper)
    }

    fn tuple_variant<V>(self, _len: usize, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        de::Deserializer::deserialize_seq(self.wrapper, visitor)
    }

    fn struct_variant<V>(self, _fields: &'static [&'static str], visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        de::Deserializer::deserialize_map(self.wrapper, visitor)
    }
}

struct Value;

impl<'lvl, 'de> DeserializerLevel<'lvl, 'de> for Value {
    fn deserialize_any<V>(
        visitor: V,
        wrapper: &mut Deserializer<'lvl, 'de, Self>,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match wrapper.peek_char()? {
            '{' => Self::deserialize_map(visitor, wrapper),
            _ => de::Deserializer::deserialize_str(wrapper, visitor),
        }
    }

    fn deserialize_seq<V>(
        visitor: V,
        wrapper: &mut Deserializer<'lvl, 'de, Self>,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_seq(SeqValueAccess::new(wrapper, wrapper.last_key))
    }

    fn deserialize_map<V>(
        visitor: V,
        wrapper: &mut Deserializer<'lvl, 'de, Self>,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        wrapper.parse_block_start()?;
        visitor.visit_map(ValueAccess::new(wrapper))
    }

    fn deserialize_enum<V>(
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
        wrapper: &mut Deserializer<'lvl, 'de, Self>,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match wrapper.peek_char()? {
            '{' => {
                wrapper.parse_block_start()?;
                let value = visitor.visit_enum(ValueAccess::new(wrapper))?;
                wrapper.parse_block_end()?;
                Ok(value)
            }
            _ => visitor.visit_enum(BorrowedStrDeserializer::new(wrapper.parse_value()?)),
        }
    }
}

struct ValueAccess<'wr, 'lvl, 'de> {
    wrapper: &'wr mut Deserializer<'lvl, 'de, Value>,
    first: bool,
}

impl<'wr, 'lvl, 'de> ValueAccess<'wr, 'lvl, 'de> {
    fn new(wrapper: &'wr mut Deserializer<'lvl, 'de, Value>) -> Self {
        Self {
            wrapper,
            first: true,
        }
    }
}

impl<'wr, 'lvl, 'de> MapAccess<'de> for ValueAccess<'wr, 'lvl, 'de> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: de::DeserializeSeed<'de>,
    {
        if let Ok(..) = self.wrapper.parse_block_end() {
            return Ok(None);
        }
        if !self.first {
            self.wrapper.parse_block_sep()?;
        }
        self.first = false;
        let key = self.wrapper.parse_key()?;
        self.wrapper.last_key = key;
        seed.deserialize(BorrowedStrDeserializer::new(key)).map(Some)
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: de::DeserializeSeed<'de>,
    {
        seed.deserialize(&mut *self.wrapper)
    }
}

impl<'wr, 'lvl, 'de> EnumAccess<'de> for ValueAccess<'wr, 'lvl, 'de> {
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant)>
    where
        V: de::DeserializeSeed<'de>,
    {
        let key = self.wrapper.parse_key()?;
        self.wrapper.last_key = key;
        Ok((seed.deserialize(BorrowedStrDeserializer::new(key))?, self))
    }
}

impl<'wr, 'lvl, 'de> VariantAccess<'de> for ValueAccess<'wr, 'lvl, 'de> {
    type Error = Error;

    fn unit_variant(self) -> Result<()> {
        self.wrapper.parse_empty_token()
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value>
    where
        T: de::DeserializeSeed<'de>,
    {
        seed.deserialize(self.wrapper)
    }

    fn tuple_variant<V>(self, _len: usize, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        de::Deserializer::deserialize_seq(self.wrapper, visitor)
    }

    fn struct_variant<V>(self, _fields: &'static [&'static str], visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        de::Deserializer::deserialize_map(self.wrapper, visitor)
    }
}

struct SeqValueAccess<'wr, 'lvl, 'de> {
    wrapper: &'wr mut Deserializer<'lvl, 'de, Value>,
    first: bool,
    element_key: &'de str,
}

impl<'wr, 'lvl, 'de> SeqValueAccess<'wr, 'lvl, 'de> {
    fn new(
        wrapper: &'wr mut Deserializer<'lvl, 'de, Value>,
        element_key: &'de str,
    ) -> Self {
        Self {
            wrapper,
            first: true,
            element_key,
        }
    }
}

impl<'wr, 'lvl, 'de> SeqAccess<'de> for SeqValueAccess<'wr, 'lvl, 'de> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: de::DeserializeSeed<'de>,
    {
        if self.wrapper.peeked_seq_end() {
            return Ok(None);
        }
        if !self.first && !self.wrapper.parsed_block_sep_and_token(self.element_key) {
            return Ok(None);
        }
        self.first = false;
        seed.deserialize(&mut *self.wrapper).map(Some)
    }
}
