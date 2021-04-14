use std::{error, fmt::{self, Display}, result};

use serde::{de, ser};
use thiserror::Error;

use super::{Deserializer, de::DeserializerLevel};

#[derive(Error, Debug)]
pub enum Reason {
    #[error("unexpected eof")]
    UnexpectedEof,
    #[error("expected a value")]
    ExpectedValue,
    #[error("expected an empty value")]
    ExpectedEmptyValue,
    #[error("expected a newline")]
    ExpectedNewline,
    #[error("expected a `{{`")]
    ExpectedOpeningBracket,
    #[error("expected a `}}`")]
    ExpectedClosingBracket,
    #[error("invalid int")]
    InvalidInt,
    #[error("invalid float")]
    InvalidFloat,
    #[error("invalid bool")]
    InvalidBool,
    #[error("{0}")]
    Custom(String),
}

#[derive(Debug)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
       write!(f, "line {}, colum {}", self.line, self.column)
    }
}

#[derive(Debug)]
pub struct Error {
    reason: Reason,
    position: Option<Position>,
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.position {
            Some(position) => write!(f, "{} at {}", self.reason, position),
            None => self.reason.fmt(f),
        }
    }
}

impl error::Error for Error {}

impl Error {
    pub(crate) fn new(reason: Reason) -> Self {
        Self {
            reason, position: None,
        }
    }

    pub fn with_position<'lvl, 'de, T: DeserializerLevel<'lvl, 'de>>(mut self, deserializer: &Deserializer<'lvl, 'de, T>) -> Self {
        self.position = Some(deserializer.get_position());
        self
    }
}

pub type Result<T> = result::Result<T, Error>;

impl de::Error for Error {
    fn custom<T>(msg: T) -> Self
    where
        T: Display,
    {
        Self::new(Reason::Custom(msg.to_string()))
    }
}

impl ser::Error for Error {
    fn custom<T>(msg: T)->Self
    where
        T: Display,
    {
        Self::new(Reason::Custom(msg.to_string()))
    }
}
