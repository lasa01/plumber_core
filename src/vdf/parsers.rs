use std::borrow::Cow;

use nom::{
    branch::alt,
    bytes::complete::{escaped, is_not, tag, take_till},
    character::complete::{
        anychar, char, line_ending, multispace1, none_of, one_of, space0, space1,
    },
    combinator::{all_consuming, cut, not, opt, peek, recognize, value},
    error::{ErrorKind, ParseError},
    sequence::{delimited, preceded, terminated},
    Err, IResult, Parser,
};

fn ignore<I, O, E, F>(mut parser: F) -> impl FnMut(I) -> IResult<I, (), E>
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

fn comment<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    preceded(tag("//"), take_till(|c| c == '\r' || c == '\n'))(i)
}

fn multispace_comment0<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, (), E> {
    ignore_many0(alt((multispace1, comment)))(i)
}

fn space_comment0<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, (), E> {
    preceded(space0, ignore(opt(comment)))(i)
}

fn quoted_token<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    delimited(char('"'), take_till(|c| c == '"'), char('"'))(i)
}

fn escaped_quoted_token<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    alt((
        delimited(
            char('"'),
            escaped(is_not("\"\\"), '\\', one_of("nt\\\"")),
            char('"'),
        ),
        value("", tag("\"\"")),
    ))(i)
}

fn unquoted_char_nonspace<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, char, E> {
    alt((
        none_of("{}\"\r\n/ \t"),
        terminated(char('/'), not(char('/'))),
    ))(i)
}

fn unquoted_key<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    recognize(ignore_many1(unquoted_char_nonspace))(i)
}

fn unquoted_value<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    recognize(ignore_many1(alt((
        ignore(unquoted_char_nonspace),
        ignore(terminated(space1, unquoted_char_nonspace)),
    ))))(i)
}

fn specific_token<'a: 'b, 'b, E: ParseError<&'a str> + 'a>(
    key: &'b str,
) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str, E> + 'b {
    preceded(
        multispace_comment0,
        alt((
            preceded(char('"'), cut(terminated(tag(key), char('"')))),
            tag(key),
        )),
    )
}

pub(crate) fn any_key<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    preceded(multispace_comment0, alt((quoted_token, unquoted_key)))(i)
}

pub(crate) fn any_escaped_key<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, &'a str, E> {
    preceded(
        multispace_comment0,
        alt((escaped_quoted_token, unquoted_key)),
    )(i)
}

pub(crate) fn empty_token<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    preceded(multispace_comment0, tag("\"\""))(i)
}

pub(crate) fn any_value<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    preceded(multispace_comment0, alt((quoted_token, unquoted_value)))(i)
}

pub(crate) fn any_escaped_value<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, &'a str, E> {
    preceded(
        multispace_comment0,
        alt((escaped_quoted_token, unquoted_value)),
    )(i)
}

pub(crate) fn block_start<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, (), E> {
    preceded(multispace_comment0, ignore(char('{')))(i)
}

pub(crate) fn block_end<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, (), E> {
    preceded(multispace_comment0, ignore(char('}')))(i)
}

pub(crate) fn block_sep<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, (), E> {
    preceded(space_comment0, ignore(line_ending))(i)
}

pub(crate) fn peeked_char<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, char, E> {
    preceded(multispace_comment0, peek(anychar))(i)
}

pub(crate) fn eof<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, (), E> {
    all_consuming(multispace_comment0)(i)
}

pub(crate) fn peeked_seq_end<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, (), E> {
    peek(preceded(
        multispace_comment0,
        alt((all_consuming(|i| Ok((i, ()))), ignore(char('}')))),
    ))(i)
}

pub(crate) fn block_sep_and_token<'a: 'b, 'b, E: ParseError<&'a str> + 'a>(
    token: &'b str,
) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str, E> + 'b {
    preceded(block_sep, specific_token(token))
}

fn escape(char: char) -> Option<char> {
    match char {
        't' => Some('\t'),
        'n' => Some('\n'),
        '\\' => Some('\\'),
        '"' => Some('"'),
        _ => None,
    }
}

pub(crate) fn maybe_escape_str(input: &str) -> Cow<str> {
    let mut char_iter = input.chars().enumerate();
    while let Some((i, ch)) = char_iter.next() {
        if ch == '\\' {
            if let Some((next_i, next_ch)) = char_iter.next() {
                if let Some(escaped) = escape(next_ch) {
                    let mut escaped_string = String::with_capacity(input.len() + 1);
                    escaped_string.push_str(&input[..i]);
                    escaped_string.push(escaped);
                    let mut char_iter = input[next_i + 1..].chars();
                    while let Some(ch) = char_iter.next() {
                        if ch == '\\' {
                            if let Some(next_ch) = char_iter.next() {
                                if let Some(escaped) = escape(next_ch) {
                                    escaped_string.push(escaped);
                                } else {
                                    escaped_string.push('\\');
                                    escaped_string.push(next_ch);
                                }
                            } else {
                                escaped_string.push('\\');
                            }
                        } else {
                            escaped_string.push(ch);
                        }
                    }
                    return Cow::Owned(escaped_string);
                }
            }
        }
    }
    Cow::Borrowed(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::error::VerboseError;

    #[test]
    fn quoted_key() {
        assert_eq!(
            any_key::<VerboseError<&str>>("\r\n\t    \"a quoted key\" value"),
            IResult::Ok((" value", "a quoted key"))
        )
    }

    #[test]
    fn unquoted_key() {
        assert_eq!(
            any_key::<VerboseError<&str>>("\r\n\t    $unquotedKey remaining"),
            IResult::Ok((" remaining", "$unquotedKey"))
        )
    }

    #[test]
    fn quoted_value() {
        assert_eq!(
            any_value::<VerboseError<&str>>(" \"quoted value\""),
            IResult::Ok(("", "quoted value"))
        )
    }

    #[test]
    fn unquoted_value() {
        assert_eq!(
            any_value::<VerboseError<&str>>("\tcsgo\\models\\stuff.mdl"),
            IResult::Ok(("", "csgo\\models\\stuff.mdl"))
        )
    }

    #[test]
    fn unquoted_value_comment_terminated() {
        assert_eq!(
            any_value::<VerboseError<&str>>(
                " unquoted value with spaces/shit // and a comment too"
            ),
            IResult::Ok((" // and a comment too", "unquoted value with spaces/shit"))
        )
    }

    #[test]
    fn comment_preceded_key() {
        assert_eq!(
            any_key::<VerboseError<&str>>("\t//this is a comment\r\n\tNotComment A Value"),
            IResult::Ok((" A Value", "NotComment"))
        )
    }

    #[test]
    fn empty_comment() {
        assert_eq!(
            multispace_comment0::<VerboseError<&str>>("\r\n\t//\r\n"),
            IResult::Ok(("", ()))
        )
    }

    #[test]
    fn escaped() {
        assert_eq!(
            any_escaped_value::<VerboseError<&str>>(" \"escaped \\\" value\""),
            IResult::Ok(("", "escaped \\\" value"))
        );

        assert_eq!(
            any_escaped_key::<VerboseError<&str>>("\"\""),
            IResult::Ok(("", ""))
        );
    }

    #[test]
    fn escaping() {
        assert_eq!(
            maybe_escape_str("not escaped"),
            Cow::Borrowed("not escaped"),
        );

        assert_eq!(maybe_escape_str("escaped \\\" value"), "escaped \" value",);

        assert_eq!(
            maybe_escape_str("escaped \\\" value more escapes \\\\ here"),
            "escaped \" value more escapes \\ here",
        );
    }
}
