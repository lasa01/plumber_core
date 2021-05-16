use std::fmt::{Display, Write};

use serde::{
    ser::{self, Impossible},
    Serialize,
};

use super::{
    error::{Error, Reason, Result},
    escape::write_escape_str,
};

/// # Errors
///
/// Returns `Err` if the serialization fails.
pub fn to_string<T>(value: &T) -> Result<String>
where
    T: Serialize,
{
    let mut serializer = Serializer {
        output: String::new(),
        last_key: None,
        indentation: 0,
        escaped: false,
    };
    value.serialize(&mut serializer)?;
    Ok(serializer.output)
}

/// # Errors
///
/// Returns `Err` if the serialization fails.
pub fn escaped_to_string<T>(value: &T) -> Result<String>
where
    T: Serialize,
{
    let mut serializer = Serializer {
        output: String::new(),
        last_key: None,
        indentation: 0,
        escaped: true,
    };
    value.serialize(&mut serializer)?;
    Ok(serializer.output)
}

pub struct Serializer {
    output: String,
    last_key: Option<String>,
    indentation: usize,
    escaped: bool,
}

impl Serializer {
    fn indent(&mut self) {
        self.output.reserve(self.indentation);
        for _ in 0..self.indentation {
            self.output += "\t";
        }
    }

    fn is_root(&self) -> bool {
        self.output.is_empty()
    }

    fn serialize_escaped_str(&mut self, str: &str) {
        write_escape_str(str, &mut self.output);
    }

    fn serialize_class(&mut self) -> SerializeClass {
        let is_root = self.is_root();
        if !is_root {
            self.output += "\n";
            self.indent();
            self.output += "{\n";
            self.indentation += 1;
            self.indent();
        }
        let key_before = self.last_key.take();
        SerializeClass {
            serializer: self,
            key_before,
            is_root,
            first: true,
        }
    }

    fn begin_serialize_enum(&mut self, variant: &'static str) -> Result<()> {
        if !self.is_root() {
            self.output += "\n";
            self.indent();
            self.output += "{\n";
            self.indentation += 1;
        }
        self.indent();
        ser::Serializer::serialize_str(self, variant)
    }
}

impl<'a> ser::Serializer for &'a mut Serializer {
    type Ok = ();

    type Error = Error;

    type SerializeSeq = SerializeSeq<'a>;
    type SerializeTuple = SerializeSeq<'a>;
    type SerializeTupleStruct = SerializeSeq<'a>;
    type SerializeTupleVariant = SerializeSeq<'a>;
    type SerializeMap = SerializeClass<'a>;
    type SerializeStruct = SerializeClass<'a>;
    type SerializeStructVariant = SerializeClass<'a>;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok> {
        self.serialize_str(if v { "1" } else { "0" })
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok> {
        self.collect_str(&v)
    }

    fn serialize_i16(self, v: i16) -> Result<Self::Ok> {
        self.collect_str(&v)
    }

    fn serialize_i32(self, v: i32) -> Result<Self::Ok> {
        self.collect_str(&v)
    }

    fn serialize_i64(self, v: i64) -> Result<Self::Ok> {
        self.collect_str(&v)
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok> {
        self.collect_str(&v)
    }

    fn serialize_u16(self, v: u16) -> Result<Self::Ok> {
        self.collect_str(&v)
    }

    fn serialize_u32(self, v: u32) -> Result<Self::Ok> {
        self.collect_str(&v)
    }

    fn serialize_u64(self, v: u64) -> Result<Self::Ok> {
        self.collect_str(&v)
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok> {
        self.collect_str(&v)
    }

    fn serialize_f64(self, v: f64) -> Result<Self::Ok> {
        self.collect_str(&v)
    }

    fn serialize_char(self, v: char) -> Result<Self::Ok> {
        self.collect_str(&v)
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok> {
        if self.escaped {
            self.serialize_escaped_str(v);
        } else {
            // write to string is infallible
            write!(self.output, "\"{}\"", v).unwrap();
        }
        Ok(())
    }

    fn serialize_bytes(self, _v: &[u8]) -> Result<Self::Ok> {
        unimplemented!("vdf cannot serialize bytes")
    }

    fn serialize_none(self) -> Result<Self::Ok> {
        self.serialize_unit()
    }

    fn serialize_some<T: ?Sized>(self, value: &T) -> Result<Self::Ok>
    where
        T: serde::Serialize,
    {
        value.serialize(self)
    }

    fn serialize_unit(self) -> Result<Self::Ok> {
        self.output += "\"\"";
        Ok(())
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<Self::Ok> {
        self.serialize_unit()
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok> {
        self.serialize_str(variant)
    }

    fn serialize_newtype_struct<T: ?Sized>(self, _name: &'static str, value: &T) -> Result<Self::Ok>
    where
        T: serde::Serialize,
    {
        value.serialize(self)
    }

    fn serialize_newtype_variant<T: ?Sized>(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok>
    where
        T: serde::Serialize,
    {
        self.begin_serialize_enum(variant)?;
        self.output += " ";
        value.serialize(&mut *self)?;
        self.output += "\n";
        self.indentation -= 1;
        self.indent();
        self.output += "}";
        Ok(())
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq> {
        self.last_key.take().map_or_else(
            || Err(Error::new(Reason::SequenceUnknownKey)),
            move |key| {
                Ok(SerializeSeq {
                    serializer: self,
                    key,
                    first: true,
                })
            },
        )
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant> {
        self.begin_serialize_enum(variant)?;
        self.output += " ";
        Ok(SerializeSeq {
            serializer: self,
            key: variant.into(),
            first: true,
        })
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap> {
        Ok(self.serialize_class())
    }

    fn serialize_struct(self, _name: &'static str, _len: usize) -> Result<Self::SerializeStruct> {
        Ok(self.serialize_class())
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant> {
        self.begin_serialize_enum(variant)?;
        self.output += "\n";
        self.indent();
        self.output += "{";
        self.indentation += 1;
        let is_root = self.is_root();
        if !is_root {
            self.output += "\n";
            self.indent();
        }
        let key_before = self.last_key.take();
        Ok(SerializeClass {
            serializer: self,
            key_before,
            is_root,
            first: true,
        })
    }

    fn collect_str<T: ?Sized>(self, value: &T) -> Result<Self::Ok>
    where
        T: Display,
    {
        if self.escaped {
            self.serialize_escaped_str(&value.to_string());
        } else {
            // write to string is infallible
            write!(self.output, "\"{}\"", value).unwrap();
        }
        Ok(())
    }
}

pub struct SerializeSeq<'a> {
    serializer: &'a mut Serializer,
    key: String,
    first: bool,
}

impl<'a> SerializeSeq<'a> {
    fn serialize_element<T: ?Sized>(&mut self, value: &T) -> Result<()>
    where
        T: Serialize,
    {
        if !self.first {
            self.serializer.output += "\n";
            self.serializer.indent();
            ser::Serializer::serialize_str(&mut *self.serializer, &self.key)?;
        }
        self.first = false;
        self.serializer.output += " ";
        value.serialize(&mut *self.serializer)
    }

    fn end(self) -> Result<()> {
        if self.first {
            return Err(Error::new(Reason::EmptySequence));
        }
        self.serializer.last_key = Some(self.key);
        Ok(())
    }
}

impl<'a> ser::SerializeSeq for SerializeSeq<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T: ?Sized>(&mut self, value: &T) -> Result<()>
    where
        T: Serialize,
    {
        self.serialize_element(value)
    }

    fn end(self) -> Result<Self::Ok> {
        self.end()
    }
}

impl<'a> ser::SerializeTuple for SerializeSeq<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T: ?Sized>(&mut self, value: &T) -> Result<()>
    where
        T: Serialize,
    {
        self.serialize_element(value)
    }

    fn end(self) -> Result<Self::Ok> {
        self.end()
    }
}

impl<'a> ser::SerializeTupleStruct for SerializeSeq<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized>(&mut self, value: &T) -> Result<()>
    where
        T: Serialize,
    {
        self.serialize_element(value)
    }

    fn end(self) -> Result<Self::Ok> {
        self.end()
    }
}

impl<'a> ser::SerializeTupleVariant for SerializeSeq<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized>(&mut self, value: &T) -> Result<()>
    where
        T: Serialize,
    {
        self.serialize_element(value)
    }

    fn end(self) -> Result<Self::Ok> {
        self.serializer.output += "\n";
        self.serializer.indentation -= 1;
        self.serializer.indent();
        self.serializer.output += "}";
        self.end()
    }
}

struct KeySerializer<'a>(&'a mut Serializer);

impl<'a> ser::Serializer for KeySerializer<'a> {
    type Ok = ();
    type Error = Error;

    type SerializeSeq = Impossible<(), Error>;
    type SerializeTuple = Impossible<(), Error>;
    type SerializeTupleStruct = Impossible<(), Error>;
    type SerializeTupleVariant = Impossible<(), Error>;
    type SerializeMap = Impossible<(), Error>;
    type SerializeStruct = Impossible<(), Error>;
    type SerializeStructVariant = Impossible<(), Error>;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok> {
        ser::Serializer::serialize_bool(self.0, v)
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok> {
        ser::Serializer::serialize_i8(self.0, v)
    }

    fn serialize_i16(self, v: i16) -> Result<Self::Ok> {
        ser::Serializer::serialize_i16(self.0, v)
    }

    fn serialize_i32(self, v: i32) -> Result<Self::Ok> {
        ser::Serializer::serialize_i32(self.0, v)
    }

    fn serialize_i64(self, v: i64) -> Result<Self::Ok> {
        ser::Serializer::serialize_i64(self.0, v)
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok> {
        ser::Serializer::serialize_u8(self.0, v)
    }

    fn serialize_u16(self, v: u16) -> Result<Self::Ok> {
        ser::Serializer::serialize_u16(self.0, v)
    }

    fn serialize_u32(self, v: u32) -> Result<Self::Ok> {
        ser::Serializer::serialize_u32(self.0, v)
    }

    fn serialize_u64(self, v: u64) -> Result<Self::Ok> {
        ser::Serializer::serialize_u64(self.0, v)
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok> {
        ser::Serializer::serialize_f32(self.0, v)
    }

    fn serialize_f64(self, v: f64) -> Result<Self::Ok> {
        ser::Serializer::serialize_f64(self.0, v)
    }

    fn serialize_char(self, v: char) -> Result<Self::Ok> {
        ser::Serializer::serialize_char(self.0, v)
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok> {
        ser::Serializer::serialize_str(&mut *self.0, v)?;
        if let Some(last_key) = &mut self.0.last_key {
            last_key.replace_range(.., v);
        } else {
            self.0.last_key = Some(v.into());
        }
        Ok(())
    }

    fn serialize_bytes(self, _v: &[u8]) -> Result<Self::Ok> {
        Err(Error::new(Reason::KeyMustBeString))
    }

    fn serialize_none(self) -> Result<Self::Ok> {
        Err(Error::new(Reason::KeyMustBeString))
    }

    fn serialize_some<T: ?Sized>(self, _value: &T) -> Result<Self::Ok>
    where
        T: Serialize,
    {
        Err(Error::new(Reason::KeyMustBeString))
    }

    fn serialize_unit(self) -> Result<Self::Ok> {
        Err(Error::new(Reason::KeyMustBeString))
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<Self::Ok> {
        Err(Error::new(Reason::KeyMustBeString))
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
    ) -> Result<Self::Ok> {
        Err(Error::new(Reason::KeyMustBeString))
    }

    fn serialize_newtype_struct<T: ?Sized>(
        self,
        _name: &'static str,
        _value: &T,
    ) -> Result<Self::Ok>
    where
        T: Serialize,
    {
        Err(Error::new(Reason::KeyMustBeString))
    }

    fn serialize_newtype_variant<T: ?Sized>(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _value: &T,
    ) -> Result<Self::Ok>
    where
        T: Serialize,
    {
        Err(Error::new(Reason::KeyMustBeString))
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq> {
        Err(Error::new(Reason::KeyMustBeString))
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple> {
        Err(Error::new(Reason::KeyMustBeString))
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleStruct> {
        Err(Error::new(Reason::KeyMustBeString))
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant> {
        Err(Error::new(Reason::KeyMustBeString))
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap> {
        Err(Error::new(Reason::KeyMustBeString))
    }

    fn serialize_struct(self, _name: &'static str, _len: usize) -> Result<Self::SerializeStruct> {
        Err(Error::new(Reason::KeyMustBeString))
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant> {
        Err(Error::new(Reason::KeyMustBeString))
    }
}

pub struct SerializeClass<'a> {
    serializer: &'a mut Serializer,
    key_before: Option<String>,
    is_root: bool,
    first: bool,
}

impl<'a> SerializeClass<'a> {
    fn serialize_field<T: ?Sized>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: Serialize,
    {
        if !self.first {
            self.serializer.output += "\n";
            self.serializer.indent();
        }
        self.first = false;
        ser::Serializer::serialize_str(&mut *self.serializer, key)?;
        if let Some(last_key) = &mut self.serializer.last_key {
            last_key.replace_range(.., key);
        } else {
            self.serializer.last_key = Some(key.into());
        }
        self.serializer.output += " ";
        value.serialize(&mut *self.serializer)
    }

    fn end(self) {
        self.serializer.output += "\n";
        if !self.is_root {
            self.serializer.indentation -= 1;
            self.serializer.indent();
            self.serializer.output += "}";
        }
        self.serializer.last_key = self.key_before;
    }
}

impl<'a> ser::SerializeStruct for SerializeClass<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: Serialize,
    {
        self.serialize_field(key, value)
    }

    fn end(self) -> Result<()> {
        self.end();
        Ok(())
    }
}

impl<'a> ser::SerializeStructVariant for SerializeClass<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: Serialize,
    {
        self.serialize_field(key, value)
    }

    fn end(self) -> Result<()> {
        self.serializer.output += "\n";
        self.serializer.indentation -= 1;
        self.serializer.indent();
        self.serializer.output += "}\n";
        if !self.is_root {
            self.serializer.indentation -= 1;
            self.serializer.indent();
            self.serializer.output += "}";
        }
        self.serializer.last_key = self.key_before;
        Ok(())
    }
}

impl<'a> ser::SerializeMap for SerializeClass<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_key<T: ?Sized>(&mut self, key: &T) -> Result<()>
    where
        T: Serialize,
    {
        if !self.first {
            self.serializer.output += "\n";
            self.serializer.indent();
        }
        self.first = false;
        key.serialize(KeySerializer(&mut *self.serializer))
    }

    fn serialize_value<T: ?Sized>(&mut self, value: &T) -> Result<()>
    where
        T: Serialize,
    {
        self.serializer.output += " ";
        value.serialize(&mut *self.serializer)
    }

    fn end(self) -> Result<()> {
        self.end();
        Ok(())
    }
}
