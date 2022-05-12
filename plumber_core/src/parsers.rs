use nom::{
    bytes::complete::is_not,
    character::complete::{char, multispace0},
    sequence::{delimited, preceded},
    IResult,
};

pub(crate) fn space_separated(input: &str) -> IResult<&str, &str> {
    preceded(multispace0, is_not(" \t\r\n[](){}"))(input)
}

pub(crate) fn bracketed<'a, O>(
    parser: impl FnMut(&'a str) -> IResult<&'a str, O>,
) -> impl FnMut(&'a str) -> IResult<&'a str, O> {
    preceded(multispace0, delimited(char('['), parser, char(']')))
}

pub(crate) fn parenthesed<'a, O>(
    parser: impl FnMut(&'a str) -> IResult<&'a str, O>,
) -> impl FnMut(&'a str) -> IResult<&'a str, O> {
    preceded(multispace0, delimited(char('('), parser, char(')')))
}

pub(crate) fn braced<'a, O>(
    parser: impl FnMut(&'a str) -> IResult<&'a str, O>,
) -> impl FnMut(&'a str) -> IResult<&'a str, O> {
    preceded(multispace0, delimited(char('{'), parser, char('}')))
}
