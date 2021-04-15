use serde::{Serialize, ser};

use super::error::{Error, Result};

/// # Errors
///
/// Will return `Err` if the serialization fails.
pub fn to_string<T>(value: &T) -> Result<String>
where
    T: Serialize,
{
    let mut serializer = Serializer {
        output: String::new(),
        last_key: None,
        indentation: 0,
    };
    value.serialize(&mut serializer)?;
    Ok(serializer.output)
}

pub struct Serializer {
    output: String,
    last_key: Option<&'static str>,
    indentation: usize,
}

impl Serializer {
    fn indent(&mut self) {
        self.output.reserve(self.indentation);
        for _ in 0..self.indentation {
            self.output += "\t";
        }
    }
}

impl<'a> ser::Serializer for &'a mut Serializer {
    type Ok = ();

    type Error = Error;

    type SerializeSeq = SerializeSeq<'a>;
    type SerializeTuple = SerializeSeq<'a>;
    type SerializeTupleStruct = SerializeSeq<'a>;
    type SerializeTupleVariant = SerializeSeq<'a>;
    type SerializeMap = SerializeMap<'a>;
    type SerializeStruct = SerializeStruct<'a>;
    type SerializeStructVariant = SerializeStruct<'a>;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok> {
        self.serialize_str(if v { "1" } else { "0" })
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_i16(self, v: i16) -> Result<Self::Ok> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_i32(self, v: i32) -> Result<Self::Ok> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_i64(self, v: i64) -> Result<Self::Ok> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_u16(self, v: u16) -> Result<Self::Ok> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_u32(self, v: u32) -> Result<Self::Ok> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_u64(self, v: u64) -> Result<Self::Ok> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_f64(self, v: f64) -> Result<Self::Ok> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_char(self, v: char) -> Result<Self::Ok> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok> {
        self.output += &format!("\"{}\"", v);
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
        T: serde::Serialize
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
    ) -> Result<Self::Ok>
    {
        self.serialize_str(variant)
    }

    fn serialize_newtype_struct<T: ?Sized>(
        self,
        _name: &'static str,
        value: &T,
    ) -> Result<Self::Ok>
    where
        T: serde::Serialize
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
        T: serde::Serialize
    {
        self.output += "\n";
        self.indent();
        self.output += "{\n";
        self.indentation += 1;
        self.indent();
        self.serialize_str(variant)?;
        self.output += " ";
        value.serialize(&mut *self)?;
        self.output += "\n";
        self.indentation -= 1;
        self.indent();
        self.output += "}";
        Ok(())
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq> {
        match self.last_key {
            Some(key) => Ok(SerializeSeq { serializer: self, key, first: true } ),
            None => panic!("vfd sequence can only be serialized inside a struct"),
        }
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
    ) -> Result<Self::SerializeTupleVariant>{
        self.output += "\n";
        self.indent();
        self.output += "{\n";
        self.indentation += 1;
        self.indent();
        self.serialize_str(variant)?;
        self.output += " ";
        Ok(SerializeSeq { serializer: self, key: variant, first: true })
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap> {
        let is_root = self.output.is_empty();
        if !is_root {
            self.output += "\n";
            self.indent();
            self.output += "{\n";
            self.indentation += 1;
            self.indent();
        }
        Ok(SerializeMap { serializer: self, is_root, first: true })
    }

    fn serialize_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStruct> {
        let is_root = self.output.is_empty();
        if !is_root {
            self.output += "\n";
            self.indent();
            self.output += "{\n";
            self.indentation += 1;
            self.indent();    
        }
        let key_before = self.last_key;
        Ok(SerializeStruct { serializer: self, key_before, is_root, first: true })
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant> {
        let is_root = self.output.is_empty();
        if !is_root {
            self.output += "\n";
            self.indent();
            self.output += "{\n";
            self.indentation += 1;
        }
        self.indent();
        self.serialize_str(variant)?;
        self.output += "\n";
        self.indent();
        self.output += "{";
        self.indentation += 1;
        if !is_root {
            self.output += "\n";
            self.indent();    
        }
        let key_before = self.last_key;
        Ok(SerializeStruct { serializer: self, key_before, is_root, first: true })
    }
}

pub struct SerializeSeq<'a> {
    serializer: &'a mut Serializer,
    key: &'static str,
    first: bool,
}

impl<'a> ser::SerializeSeq for SerializeSeq<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T: ?Sized>(&mut self, value: &T) -> Result<()>
    where
        T: Serialize
    {
        if !self.first {
            self.serializer.output += "\n";
            self.serializer.indent();
            ser::Serializer::serialize_str(&mut *self.serializer, self.key)?;
        }
        self.first = false;
        self.serializer.output += " ";
        value.serialize(&mut *self.serializer)
    }

    fn end(self) -> Result<Self::Ok> {
        Ok(())
    }
}

impl<'a> ser::SerializeTuple for SerializeSeq<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T: ?Sized>(&mut self, value: &T) -> Result<()>
    where
        T: Serialize
    {
        if !self.first {
            self.serializer.output += "\n";
            self.serializer.indent();
            ser::Serializer::serialize_str(&mut *self.serializer, self.key)?;
        }
        self.first = false;
        self.serializer.output += " ";
        value.serialize(&mut *self.serializer)
    }

    fn end(self) -> Result<Self::Ok> {
        Ok(())
    }
}

impl<'a> ser::SerializeTupleStruct for SerializeSeq<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized>(&mut self, value: &T) -> Result<()>
    where
        T: Serialize
    {
        if !self.first {
            self.serializer.output += "\n";
            self.serializer.indent();
            ser::Serializer::serialize_str(&mut *self.serializer, self.key)?;
        }
        self.first = false;
        self.serializer.output += " ";
        value.serialize(&mut *self.serializer)
    }

    fn end(self) -> Result<Self::Ok> {
        Ok(())
    }
}

impl<'a> ser::SerializeTupleVariant for SerializeSeq<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized>(&mut self, value: &T) -> Result<()>
    where
        T: Serialize
    {
        if !self.first {
            self.serializer.output += "\n";
            self.serializer.indent();
            ser::Serializer::serialize_str(&mut *self.serializer, self.key)?;
        }
        self.first = false;
        self.serializer.output += " ";
        value.serialize(&mut *self.serializer)
    }

    fn end(self) -> Result<Self::Ok> {
        self.serializer.output += "\n";
        self.serializer.indentation -= 1;
        self.serializer.indent();
        self.serializer.output += "}";
        Ok(())
    }
}

pub struct SerializeMap<'a> {
    serializer: &'a mut Serializer,
    is_root: bool,
    first: bool,
}

impl<'a> ser::SerializeMap for SerializeMap<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_key<T: ?Sized>(&mut self, key: &T) -> Result<()>
    where
        T: Serialize
    {
        if !self.first {
            self.serializer.output += "\n";
            self.serializer.indent();    
        }
        self.first = false;
        key.serialize(&mut *self.serializer)
    }

    fn serialize_value<T: ?Sized>(&mut self, value: &T) -> Result<()>
    where
        T: Serialize
    {
        self.serializer.output += " ";
        value.serialize(&mut *self.serializer)
    }

    fn end(self) -> Result<()> {
        self.serializer.output += "\n";
        if !self.is_root {
            self.serializer.indentation -= 1;
            self.serializer.indent();
            self.serializer.output += "}";
        }
        Ok(())
    }
}

pub struct SerializeStruct<'a> {
    serializer: &'a mut Serializer,
    key_before: Option<&'static str>,
    is_root: bool,
    first: bool,
}

impl<'a> ser::SerializeStruct for SerializeStruct<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<()>
    where
        T: Serialize
    {
        if !self.first {
            self.serializer.output += "\n";
            self.serializer.indent();    
        }
        self.first = false;
        ser::Serializer::serialize_str(&mut *self.serializer, key)?;
        self.serializer.last_key = Some(key);
        self.serializer.output += " ";
        value.serialize(&mut *self.serializer)
    }

    fn end(self) -> Result<()> {
        self.serializer.output += "\n";
        if !self.is_root {
            self.serializer.indentation -= 1;
            self.serializer.indent();
            self.serializer.output += "}";
        }
        self.serializer.last_key = self.key_before;
        Ok(())
    }
}

impl<'a> ser::SerializeStructVariant for SerializeStruct<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<()>
    where
        T: Serialize
    {
        if !self.first {
            self.serializer.output += "\n";
            self.serializer.indent();    
        }
        self.first = false;
        ser::Serializer::serialize_str(&mut *self.serializer, key)?;
        self.serializer.last_key = Some(key);
        self.serializer.output += " ";
        value.serialize(&mut *self.serializer)
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
