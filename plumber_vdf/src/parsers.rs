use nom::{
    branch::alt,
    bytes::complete::{escaped, is_a, is_not, tag, take_till, take_until},
    character::complete::{anychar, char, multispace1, none_of, one_of, space0, space1},
    combinator::{all_consuming, cut, eof, not, opt, peek, recognize, value},
    error::{ErrorKind, ParseError},
    sequence::{delimited, preceded, terminated},
    Err, IResult, Parser,
};

fn unit<I, O, E, F>(mut parser: F) -> impl FnMut(I) -> IResult<I, (), E>
where
    F: Parser<I, O, E>,
{
    move |input: I| parser.parse(input).map(|(i, _)| (i, ()))
}

fn ignore_many0<I, O, E, F>(mut f: F) -> impl FnMut(I) -> IResult<I, (), E>
where
    I: Clone + PartialEq,
    F: Parser<I, O, E>,
    E: ParseError<I>,
{
    move |mut input: I| {
        loop {
            match f.parse(input.clone()) {
                Ok((parsed_input, _)) => {
                    // loop trip must always consume (otherwise infinite loops)
                    if parsed_input == input {
                        return Err(Err::Error(E::from_error_kind(input, ErrorKind::Many0)));
                    }
                    input = parsed_input;
                }
                Err(Err::Error(_)) => return Ok((input, ())),
                Err(e) => {
                    return Err(e);
                }
            }
        }
    }
}

fn ignore_many1<I, O, E, F>(mut f: F) -> impl FnMut(I) -> IResult<I, (), E>
where
    I: Clone + PartialEq,
    F: Parser<I, O, E>,
    E: ParseError<I>,
{
    move |mut input: I| {
        match f.parse(input.clone()) {
            Err(Err::Error(_)) => Err(Err::Error(E::from_error_kind(input, ErrorKind::Many1))),
            Err(e) => Err(e),
            Ok((parsed_input, _)) => {
                input = parsed_input;
                loop {
                    match f.parse(input.clone()) {
                        Ok((parsed_input, _)) => {
                            // loop trip must always consume (otherwise infinite loops)
                            if parsed_input == input {
                                return Err(Err::Error(E::from_error_kind(
                                    input,
                                    ErrorKind::Many1,
                                )));
                            }
                            input = parsed_input;
                        }
                        Err(Err::Error(_)) => return Ok((input, ())),
                        Err(e) => {
                            return Err(e);
                        }
                    }
                }
            }
        }
    }
}

// this shouldn't probably be legal but someone seems to be using it
fn multiline_comment<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], &'a [u8], E> {
    delimited(tag(b"/*"), take_until(b"*/".as_ref()), tag(b"*/"))(i)
}

fn singleline_comment<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], &'a [u8], E> {
    preceded(tag(b"//"), take_till(|c| c == b'\r' || c == b'\n'))(i)
}

fn comment<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], &'a [u8], E> {
    alt((singleline_comment, multiline_comment))(i)
}

fn multispace_comment0<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], (), E> {
    ignore_many0(alt((multispace1, comment)))(i)
}

fn trash<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], Option<&'a [u8]>, E> {
    opt(is_not(b"\r\n{}".as_ref()))(i)
}

fn space_comment_trash0<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], (), E> {
    // ignore possible trash before line ending
    delimited(space0, unit(opt(comment)), trash)(i)
}

fn quoted_token<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], &'a [u8], E> {
    delimited(char('"'), take_till(|c| c == b'"'), char('"'))(i)
}

fn escaped_quoted_token<'a, E: ParseError<&'a [u8]>>(
    i: &'a [u8],
) -> IResult<&'a [u8], &'a [u8], E> {
    alt((
        delimited(
            char('"'),
            escaped(is_not(b"\"\\".as_ref()), '\\', one_of(b"nt\\\"".as_ref())),
            char('"'),
        ),
        value(b"".as_ref(), tag(b"\"\"")),
    ))(i)
}

fn unquoted_char_nonspace<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], char, E> {
    alt((
        none_of(b"{}\"\r\n/ \t".as_ref()),
        terminated(char('/'), not(char('/'))),
    ))(i)
}

fn unquoted_key<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], &'a [u8], E> {
    recognize(ignore_many1(unquoted_char_nonspace))(i)
}

fn unquoted_value<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], &'a [u8], E> {
    recognize(ignore_many1(alt((
        unit(unquoted_char_nonspace),
        unit(terminated(space1, unquoted_char_nonspace)),
    ))))(i)
}

fn specific_token<'a: 'b, 'b, E: ParseError<&'a [u8]> + 'a>(
    key: &'b [u8],
) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], &'a [u8], E> + 'b {
    preceded(
        multispace_comment0,
        alt((
            preceded(char('"'), cut(terminated(tag(key), char('"')))),
            tag(key),
        )),
    )
}

pub(crate) fn any_key<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], &'a [u8], E> {
    preceded(multispace_comment0, alt((quoted_token, unquoted_key)))(i)
}

pub(crate) fn any_escaped_key<'a, E: ParseError<&'a [u8]>>(
    i: &'a [u8],
) -> IResult<&'a [u8], &'a [u8], E> {
    preceded(
        multispace_comment0,
        alt((escaped_quoted_token, unquoted_key)),
    )(i)
}

pub(crate) fn empty_token<'a, E: ParseError<&'a [u8]>>(
    i: &'a [u8],
) -> IResult<&'a [u8], &'a [u8], E> {
    preceded(multispace_comment0, tag(b"\"\"".as_ref()))(i)
}

pub(crate) fn any_value<'a, E: ParseError<&'a [u8]>>(
    i: &'a [u8],
) -> IResult<&'a [u8], &'a [u8], E> {
    preceded(multispace_comment0, alt((quoted_token, unquoted_value)))(i)
}

pub(crate) fn any_escaped_value<'a, E: ParseError<&'a [u8]>>(
    i: &'a [u8],
) -> IResult<&'a [u8], &'a [u8], E> {
    preceded(
        multispace_comment0,
        alt((escaped_quoted_token, unquoted_value)),
    )(i)
}

pub(crate) fn block_start<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], (), E> {
    preceded(multispace_comment0, unit(char('{')))(i)
}

pub(crate) fn block_end<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], (), E> {
    preceded(
        preceded(space_comment_trash0, multispace_comment0),
        unit(alt((eof, tag(b"}")))),
    )(i)
}

pub(crate) fn block_end_early<'a, E: ParseError<&'a [u8]>>(
    i: &'a [u8],
) -> IResult<&'a [u8], (), E> {
    preceded(multispace_comment0, unit(alt((eof, tag(b"}")))))(i)
}

pub(crate) fn block_sep<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], (), E> {
    unit(preceded(space_comment_trash0, is_a(b"\r\n".as_ref())))(i)
}

pub(crate) fn peeked_char<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], char, E> {
    preceded(multispace_comment0, peek(anychar))(i)
}

pub(crate) fn comment_eof<'a, E: ParseError<&'a [u8]>>(i: &'a [u8]) -> IResult<&'a [u8], (), E> {
    all_consuming(multispace_comment0)(i)
}

pub(crate) fn peeked_block_end<'a, E: ParseError<&'a [u8]>>(
    i: &'a [u8],
) -> IResult<&'a [u8], (), E> {
    peek(block_end)(i)
}

pub(crate) fn block_sep_and_token<'a: 'b, 'b, E: ParseError<&'a [u8]> + 'a>(
    token: &'b [u8],
) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], &'a [u8], E> + 'b {
    preceded(block_sep, specific_token(token))
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::error::VerboseError;

    #[test]
    fn quoted_key() {
        assert_eq!(
            any_key::<VerboseError<&[u8]>>(b"\r\n\t    \"a quoted key\" value".as_ref()),
            IResult::Ok((b" value".as_ref(), b"a quoted key".as_ref()))
        );
    }

    #[test]
    fn unquoted_key() {
        assert_eq!(
            any_key::<VerboseError<&[u8]>>(b"\r\n\t    $unquotedKey remaining".as_ref()),
            IResult::Ok((b" remaining".as_ref(), b"$unquotedKey".as_ref()))
        );
    }

    #[test]
    fn quoted_value() {
        assert_eq!(
            any_value::<VerboseError<&[u8]>>(b" \"quoted value\"".as_ref()),
            IResult::Ok((b"".as_ref(), b"quoted value".as_ref()))
        );
    }

    #[test]
    fn unquoted_value() {
        assert_eq!(
            any_value::<VerboseError<&[u8]>>(b"\tcsgo\\models\\stuff.mdl".as_ref()),
            IResult::Ok((b"".as_ref(), b"csgo\\models\\stuff.mdl".as_ref()))
        );
    }

    #[test]
    fn unquoted_value_comment_terminated() {
        assert_eq!(
            any_value::<VerboseError<&[u8]>>(
                b" unquoted value with spaces/shit // and a comment too".as_ref()
            ),
            IResult::Ok((
                b" // and a comment too".as_ref(),
                b"unquoted value with spaces/shit".as_ref()
            ))
        );
    }

    #[test]
    fn comment_preceded_key() {
        assert_eq!(
            any_key::<VerboseError<&[u8]>>(
                b"\t//this is a comment\r\n\tNotComment A Value".as_ref()
            ),
            IResult::Ok((b" A Value".as_ref(), b"NotComment".as_ref()))
        );
    }

    #[test]
    fn empty_comment() {
        assert_eq!(
            multispace_comment0::<VerboseError<&[u8]>>(b"\r\n\t//\r\n".as_ref()),
            IResult::Ok((b"".as_ref(), ()))
        );
    }

    #[test]
    fn escaped() {
        assert_eq!(
            any_escaped_value::<VerboseError<&[u8]>>(b" \"escaped \\\" value\"".as_ref()),
            IResult::Ok((b"".as_ref(), b"escaped \\\" value".as_ref()))
        );

        assert_eq!(
            any_escaped_key::<VerboseError<&[u8]>>(b"\"\"".as_ref()),
            IResult::Ok((b"".as_ref(), b"".as_ref()))
        );
    }
}
