use std::borrow::Cow;

fn unescape(char: u8) -> Option<u8> {
    match char {
        b't' => Some(b'\t'),
        b'n' => Some(b'\n'),
        b'\\' => Some(b'\\'),
        b'"' => Some(b'"'),
        _ => None,
    }
}

fn escape(char: char) -> Option<char> {
    match char {
        '\t' => Some('\t'),
        '\n' => Some('\n'),
        '\\' => Some('\\'),
        '"' => Some('"'),
        _ => None,
    }
}

pub(crate) fn maybe_unescape_str(input: &[u8]) -> Cow<[u8]> {
    let mut char_iter = input.iter().enumerate();
    while let Some((i, &ch)) = char_iter.next() {
        if ch == b'\\' {
            if let Some(escaped) = char_iter.next().and_then(|(_, &ch)| unescape(ch)) {
                let mut escaped_string = Vec::with_capacity(input.len() + 1);
                escaped_string.extend_from_slice(&input[..i]);
                escaped_string.push(escaped);
                while let Some((_, &ch)) = char_iter.next() {
                    if ch == b'\\' {
                        if let Some((_, &next_ch)) = char_iter.next() {
                            if let Some(escaped) = unescape(next_ch) {
                                escaped_string.push(escaped);
                            } else {
                                escaped_string.push(b'\\');
                                escaped_string.push(next_ch);
                            }
                        } else {
                            escaped_string.push(b'\\');
                        }
                    } else {
                        escaped_string.push(ch);
                    }
                }
                return Cow::Owned(escaped_string);
            }
        }
    }
    Cow::Borrowed(input)
}

pub(crate) fn write_escape_str(input: &str, target: &mut String) {
    target.reserve(input.len());
    for ch in input.chars() {
        if let Some(escaped) = escape(ch) {
            target.push('\\');
            target.push(escaped);
        } else {
            target.push(ch);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unescaping() {
        assert_eq!(
            maybe_unescape_str(b"not escaped"),
            Cow::Borrowed(b"not escaped"),
        );

        assert_eq!(
            maybe_unescape_str(b"escaped \\\" value").as_ref(),
            b"escaped \" value",
        );

        assert_eq!(
            maybe_unescape_str(b"escaped \\\" value more escapes \\\\ here").as_ref(),
            b"escaped \" value more escapes \\ here",
        );
    }

    #[test]
    fn escaping() {
        let mut buf = String::new();
        write_escape_str("no escaping", &mut buf);
        assert_eq!(&buf, "no escaping");

        let mut buf = String::new();
        write_escape_str("this \n definitely \" needs \\ escaping", &mut buf);
        assert_eq!(&buf, "this \\\n definitely \\\" needs \\\\ escaping");
    }
}
