use super::*;
use std::ops::{Bound, RangeBounds};

trait StringUtils {
    #[must_use]
    fn substring(&self, range: impl RangeBounds<usize>) -> &Self;
}
impl StringUtils for str {
    fn substring(&self, range: impl RangeBounds<usize>) -> &Self {
        let char_start_inclusive = match range.start_bound() {
            Bound::Included(start) => *start,
            Bound::Excluded(start) => start.saturating_sub(1),
            Bound::Unbounded => 0,
        };

        let char_end_exclusive = match range.end_bound() {
            Bound::Included(end) => Some(end.saturating_add(1)),
            Bound::Excluded(end) => Some(*end),
            Bound::Unbounded => None,
        };

        let char_len = match char_end_exclusive {
            Some(char_end_exclusive) => {
                Some(char_end_exclusive.saturating_sub(char_start_inclusive))
            }
            None => None,
        };

        let mut chars = self.char_indices();
        let byte_start_inclusive = chars.nth(char_start_inclusive).unwrap().0;
        let byte_end_exclusive: usize = match char_len {
            Some(char_len) => {
                if char_len == 0 {
                    byte_start_inclusive
                } else {
                    chars.nth(char_len - 1).unwrap().0
                }
            }
            None => self.len(),
        };

        &self[byte_start_inclusive..byte_end_exclusive]
    }
}

pub trait CharCollection {
    #[must_use]
    fn contains(&self, c: char) -> bool;
}
impl CharCollection for &[char] {
    fn contains(&self, c: char) -> bool {
        (*self).contains(&c)
    }
}
impl CharCollection for Vec<char> {
    fn contains(&self, c: char) -> bool {
        self.as_slice().contains(&c)
    }
}
impl CharCollection for &str {
    fn contains(&self, c: char) -> bool {
        self.chars().any(|other| c == other)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TextPosition {
    pub abs: usize,
    pub line: usize,
    pub column: usize,
}

#[derive(Clone, Copy)]
pub struct TextInput {
    text: &'static str,
    pos: TextPosition,
}
impl TextInput {
    #[must_use]
    pub const fn new(text: &'static str) -> Self {
        Self {
            text,
            pos: TextPosition {
                abs: 0,
                line: 0,
                column: 0,
            },
        }
    }

    #[inline]
    #[must_use]
    pub const fn as_str(&self) -> &str {
        self.text
    }

    #[inline]
    #[must_use]
    pub const fn len(&self) -> usize {
        self.text.len()
    }

    #[inline]
    #[must_use]
    pub const fn pos(&self) -> &TextPosition {
        &self.pos
    }

    #[inline]
    #[must_use]
    pub fn starts_with<'a, P: std::str::pattern::Pattern<'a>>(&'a self, pat: P) -> bool {
        self.text.starts_with(pat)
    }

    #[inline]
    #[must_use]
    pub fn chars(&self) -> std::str::Chars {
        self.text.chars()
    }

    #[must_use]
    pub fn advance(&self, count: usize) -> Self {
        if count == 0 {
            self.clone()
        } else {
            let advance_str = self.text.substring(..count);
            let mut line = self.pos.line;
            let mut column = self.pos.column;
            for c in advance_str.chars() {
                if c == '\n' {
                    line += 1;
                    column = 0;
                } else {
                    column += 1;
                }
            }

            Self {
                text: self.text.substring(count..),
                pos: TextPosition {
                    abs: self.pos.abs + count,
                    line,
                    column,
                },
            }
        }
    }
}

pub type TextParser<T> = Parser<T, TextInput>;

/// Creates a parser that matches any char.
pub fn any() -> TextParser<char> {
    Parser::new(|input: TextInput| match input.chars().nth(0) {
        Some(c) => ParseResult::Match(input.advance(1), c),
        None => ParseResult::NoMatch,
    })
}

/// Creates a parser that matches a specific char.
pub fn char(c: char) -> TextParser<char> {
    Parser::new(move |input: TextInput| {
        if input.starts_with(c) {
            ParseResult::Match(input.advance(1), c)
        } else {
            ParseResult::NoMatch
        }
    })
}

/// Creates a parser that matches a char based on a predicate function.
pub fn matches(predicate: impl Fn(&char) -> bool + 'static) -> TextParser<char> {
    Parser::new(move |input: TextInput| match input.chars().nth(0) {
        Some(c) => {
            if predicate(&c) {
                ParseResult::Match(input.advance(1), c)
            } else {
                ParseResult::NoMatch
            }
        }
        None => ParseResult::NoMatch,
    })
}

/// Creates a parser that matches one of the given chars. Chars are matched in order.
pub fn one_of<L: 'static + CharCollection>(list: L) -> TextParser<char> {
    Parser::new(move |input: TextInput| match input.chars().nth(0) {
        Some(c) => {
            if list.contains(c) {
                ParseResult::Match(input.advance(1), c)
            } else {
                ParseResult::NoMatch
            }
        }
        None => ParseResult::NoMatch,
    })
}

/// Creates a parser that matches a specific string.
pub fn string(s: &'static str) -> TextParser<String> {
    Parser::new(move |input: TextInput| {
        if input.starts_with(s) {
            ParseResult::Match(input.advance(s.len()), s.to_string())
        } else {
            ParseResult::NoMatch
        }
    })
}

/// Creates a parser that matches the end of the input.
pub fn eof() -> TextParser<()> {
    Parser::new(move |input: TextInput| {
        if input.len() == 0 {
            ParseResult::Match(input, ())
        } else {
            ParseResult::NoMatch
        }
    })
}

pub fn collect_string(parser: TextParser<Vec<char>>) -> TextParser<String> {
    parser.map(|chars| chars.iter().collect())
}

pub fn whitespace() -> TextParser<char> {
    matches(char::is_ascii_whitespace)
}

pub fn alphabetic() -> TextParser<char> {
    matches(char::is_ascii_alphabetic)
}

pub fn alphanumeric() -> TextParser<char> {
    matches(char::is_ascii_alphanumeric)
}

pub fn digit() -> TextParser<char> {
    matches(char::is_ascii_digit)
}

pub fn hexdigit() -> TextParser<char> {
    matches(char::is_ascii_hexdigit)
}

pub fn octdigit() -> TextParser<char> {
    one_of("01234567")
}

pub fn bindigit() -> TextParser<char> {
    one_of("01")
}

pub fn sign() -> TextParser<char> {
    one_of("+-")
}

pub fn newline() -> TextParser<String> {
    choice([string("\n"), string("\r\n")])
}

pub fn whitespace0() -> TextParser<String> {
    collect_string(whitespace().many())
}

pub fn whitespace1() -> TextParser<String> {
    collect_string(whitespace().many1())
}

pub fn alphabetic0() -> TextParser<String> {
    collect_string(alphabetic().many())
}

pub fn alphabetic1() -> TextParser<String> {
    collect_string(alphabetic().many1())
}

pub fn alphanumeric0() -> TextParser<String> {
    collect_string(alphanumeric().many())
}

pub fn alphanumeric1() -> TextParser<String> {
    collect_string(alphanumeric().many1())
}

pub fn digit0() -> TextParser<String> {
    collect_string(digit().many())
}

pub fn digit1() -> TextParser<String> {
    collect_string(digit().many1())
}

pub fn hexdigit0() -> TextParser<String> {
    collect_string(hexdigit().many())
}

pub fn hexdigit1() -> TextParser<String> {
    collect_string(hexdigit().many1())
}

pub fn octdigit0() -> TextParser<String> {
    collect_string(octdigit().many())
}

pub fn octdigit1() -> TextParser<String> {
    collect_string(octdigit().many1())
}

pub fn bindigit0() -> TextParser<String> {
    collect_string(bindigit().many())
}

pub fn bindigit1() -> TextParser<String> {
    collect_string(bindigit().many1())
}
