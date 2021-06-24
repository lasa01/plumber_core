use std::{
    error,
    fmt::{self, Display},
    result,
    str::Utf8Error,
    string::FromUtf8Error,
};

use serde::{de, ser};
use thiserror::Error;

use super::Deserializer;

#[derive(Error, Debug, Clone)]
pub enum Reason {
    #[error("unexpected eof")]
    UnexpectedEof,
    #[error("expected a value")]
    ExpectedValue,
    #[error("expected an empty value")]
    ExpectedEmptyValue,
    #[error("expected a `{{`")]
    ExpectedOpeningBracket,
    #[error("expected a `}}`")]
    ExpectedClosingBracket,
    #[error("expected a newline")]
    ExpectedNewline,
    #[error("invalid int")]
    InvalidInt,
    #[error("invalid float")]
    InvalidFloat,
    #[error("invalid bool")]
    InvalidBool,
    #[error("key must be a string")]
    KeyMustBeString,
    #[error("sequence must have at least 1 element")]
    EmptySequence,
    #[error("sequence must be inside a class")]
    SequenceUnknownKey,
    #[error("contains invalid utf-8")]
    InvalidUtf8,
    #[error("recursion limit exceeded")]
    Recursion,
    #[error("{0}")]
    Custom(String),
}

#[derive(Debug, Clone, Copy)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "line {}, column {}", self.line, self.column)
    }
}

#[derive(Debug, Clone)]
pub struct Error {
    reason: Reason,
    position: Option<Position>,
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
            reason,
            position: None,
        }
    }

    #[must_use]
    pub fn with_position(mut self, deserializer: &Deserializer) -> Self {
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
    fn custom<T>(msg: T) -> Self
    where
        T: Display,
    {
        Self::new(Reason::Custom(msg.to_string()))
    }
}

impl From<Utf8Error> for Error {
    fn from(_: Utf8Error) -> Self {
        Self::new(Reason::InvalidUtf8)
    }
}

impl From<FromUtf8Error> for Error {
    fn from(_: FromUtf8Error) -> Self {
        Self::new(Reason::InvalidUtf8)
    }
}
