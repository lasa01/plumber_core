use std::borrow::Cow;

fn unescape(char: char) -> Option<char> {
    match char {
        't' => Some('\t'),
        'n' => Some('\n'),
        '\\' => Some('\\'),
        '"' => Some('"'),
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

pub(crate) fn maybe_unescape_str(input: &str) -> Cow<str> {
    let mut char_iter = input.char_indices();
    while let Some((i, ch)) = char_iter.next() {
        if ch == '\\' {
            if let Some(escaped) = char_iter.next().and_then(|(_, ch)| unescape(ch)) {
                let mut escaped_string = String::with_capacity(input.len() + 1);
                escaped_string.push_str(&input[..i]);
                escaped_string.push(escaped);
                while let Some((_, ch)) = char_iter.next() {
                    if ch == '\\' {
                        if let Some((_, next_ch)) = char_iter.next() {
                            if let Some(escaped) = unescape(next_ch) {
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
    Cow::Borrowed(input)
}

pub(crate) fn write_escape_str(input: &str, target: &mut String) {
    target.reserve(input.len());
    for ch in input.chars() {
        if let Some(escaped) = escape(ch) {
            target.push('\\');
            target.push(escaped);
        } else {
            target.push(ch)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unescaping() {
        assert_eq!(
            maybe_unescape_str("not escaped"),
            Cow::Borrowed("not escaped"),
        );

        assert_eq!(maybe_unescape_str("escaped \\\" value"), "escaped \" value",);

        assert_eq!(
            maybe_unescape_str("escaped \\\" value more escapes \\\\ here"),
            "escaped \" value more escapes \\ here",
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
