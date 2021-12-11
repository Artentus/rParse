#![feature(pattern)]

use std::ops::{BitAnd, BitOr, Shl, Shr};
use std::rc::Rc;
pub mod text;

#[derive(Debug)]
#[must_use]
pub enum ParseResult<T, I: Clone> {
    /// The input matched the parser pattern.
    Match(I, T),
    /// The input did not match the parser pattern.
    NoMatch,
    /// The input matched the parser pattern but was malformed or invalid.
    Err(I),
}

/// Parses some input into structured data.
#[must_use]
pub struct Parser<T, I: Clone> {
    parse_fn: Rc<dyn Fn(I) -> ParseResult<T, I>>,
}
impl<T, I: Clone> Parser<T, I> {
    /// Creates a new parser from a function.
    pub fn new<F: 'static + Fn(I) -> ParseResult<T, I>>(parse_fn: F) -> Self {
        Self {
            parse_fn: Rc::new(parse_fn),
        }
    }

    /// Runs the parser on the given input.
    pub fn run(&self, input: I) -> ParseResult<T, I> {
        (self.parse_fn)(input)
    }
}
impl<T, I: Clone> Clone for Parser<T, I> {
    fn clone(&self) -> Self {
        Self {
            parse_fn: Rc::clone(&self.parse_fn),
        }
    }
}

impl<T: 'static, I: 'static + Clone> Parser<T, I> {
    /// Errors if the parser did not match.
    pub fn require(self) -> Self {
        Parser::new(move |input: I| match self.run(input.clone()) {
            ParseResult::Match(remaining, result) => ParseResult::Match(remaining, result),
            ParseResult::NoMatch => ParseResult::Err(input),
            ParseResult::Err(location) => ParseResult::Err(location),
        })
    }

    /// Doesn't match if the given validation function returns false.
    pub fn verify(self, f: impl Fn(&T) -> bool + 'static) -> Self {
        Parser::new(move |input: I| match self.run(input.clone()) {
            ParseResult::Match(remaining, result) => {
                if f(&result) {
                    ParseResult::Match(remaining, result)
                } else {
                    ParseResult::NoMatch
                }
            }
            ParseResult::NoMatch => ParseResult::NoMatch,
            ParseResult::Err(location) => ParseResult::Err(location),
        })
    }

    /// Returns None if the parser did not match.
    pub fn opt(self) -> Parser<Option<T>, I> {
        Parser::new(move |input: I| match self.run(input.clone()) {
            ParseResult::Match(remaining, result) => ParseResult::Match(remaining, Some(result)),
            ParseResult::NoMatch => ParseResult::Match(input, None),
            ParseResult::Err(location) => ParseResult::Err(location),
        })
    }

    /// Returns a specific value if the parser matches.
    pub fn map_to<T2: 'static + Clone>(self, value: T2) -> Parser<T2, I> {
        Parser::new(move |input: I| match self.run(input) {
            ParseResult::Match(remaining, _) => ParseResult::Match(remaining, value.clone()),
            ParseResult::NoMatch => ParseResult::NoMatch,
            ParseResult::Err(location) => ParseResult::Err(location),
        })
    }

    /// Maps the parsed result using a mapping function if the parser matches.
    pub fn map<T2: 'static>(self, mapping: impl Fn(T) -> T2 + 'static) -> Parser<T2, I> {
        Parser::new(move |input: I| match self.run(input) {
            ParseResult::Match(remaining, result) => ParseResult::Match(remaining, mapping(result)),
            ParseResult::NoMatch => ParseResult::NoMatch,
            ParseResult::Err(location) => ParseResult::Err(location),
        })
    }

    /// Matches another parser in sequence.
    pub fn and_then<T2: 'static>(self, other: Parser<T2, I>) -> Parser<(T, T2), I> {
        Parser::new(move |input: I| match self.run(input) {
            ParseResult::Match(remaining, result1) => match other.run(remaining) {
                ParseResult::Match(remaining, result2) => {
                    ParseResult::Match(remaining, (result1, result2))
                }
                ParseResult::NoMatch => ParseResult::NoMatch,
                ParseResult::Err(location) => ParseResult::Err(location),
            },
            ParseResult::NoMatch => ParseResult::NoMatch,
            ParseResult::Err(location) => ParseResult::Err(location),
        })
    }

    /// Matches either this parser or another.
    pub fn or_else(self, other: Self) -> Self {
        Parser::new(move |input: I| match self.run(input.clone()) {
            ParseResult::Match(remaining, result) => ParseResult::Match(remaining, result),
            ParseResult::NoMatch => match other.run(input) {
                ParseResult::Match(remaining, result) => ParseResult::Match(remaining, result),
                ParseResult::NoMatch => ParseResult::NoMatch,
                ParseResult::Err(location) => ParseResult::Err(location),
            },
            ParseResult::Err(location) => ParseResult::Err(location),
        })
    }

    /// Matches the parser exactly 'count' times.
    pub fn repeat(self, count: usize) -> Parser<Vec<T>, I> {
        Parser::new(move |input: I| {
            let mut results = Vec::with_capacity(count);
            let mut remaining = input;

            for _ in 0..count {
                match self.run(remaining) {
                    ParseResult::Match(new_remaining, result) => {
                        remaining = new_remaining;
                        results.push(result);
                    }
                    ParseResult::NoMatch => return ParseResult::NoMatch,
                    ParseResult::Err(location) => return ParseResult::Err(location),
                }
            }

            ParseResult::Match(remaining, results)
        })
    }

    /// Matches the parser zero or more times.
    pub fn many(self) -> Parser<Vec<T>, I> {
        Parser::new(move |input: I| {
            let mut results = Vec::new();
            let mut remaining = input;

            loop {
                match self.run(remaining.clone()) {
                    ParseResult::Match(new_remaining, result) => {
                        remaining = new_remaining;
                        results.push(result);
                    }
                    ParseResult::NoMatch => break,
                    ParseResult::Err(location) => return ParseResult::Err(location),
                }
            }

            ParseResult::Match(remaining, results)
        })
    }

    /// Matches the parser one or more times.
    pub fn many1(self) -> Parser<Vec<T>, I> {
        Parser::new(move |input: I| match self.run(input) {
            ParseResult::Match(mut remaining, result) => {
                let mut results = Vec::new();
                results.push(result);

                loop {
                    match self.run(remaining.clone()) {
                        ParseResult::Match(new_remaining, result) => {
                            remaining = new_remaining;
                            results.push(result);
                        }
                        ParseResult::NoMatch => break,
                        ParseResult::Err(location) => return ParseResult::Err(location),
                    }
                }

                ParseResult::Match(remaining, results)
            }
            ParseResult::NoMatch => ParseResult::NoMatch,
            ParseResult::Err(location) => ParseResult::Err(location),
        })
    }

    /// Matches the parser one or more times, separated by the given separator.
    pub fn sep_by1<S: 'static>(
        self,
        separator: Parser<S, I>,
        parse_trailing: bool,
    ) -> Parser<Vec<T>, I> {
        let middle = prefixed(separator.clone(), self.clone()).many();

        let trailing = if parse_trailing {
            separator.opt().map_to(())
        } else {
            always(())
        };

        suffixed(self.and_then(middle), trailing).map(|(head, mut tail)| {
            tail.insert(0, head);
            tail
        })
    }

    /// Matches the parser zero or more times, separated by the given separator.
    pub fn sep_by<S: 'static>(
        self,
        separator: Parser<S, I>,
        parse_trailing: bool,
    ) -> Parser<Vec<T>, I> {
        self.sep_by1(separator, parse_trailing)
            .opt()
            .map(|list| list.unwrap_or(Vec::new()))
    }
}

impl<T: 'static + Clone, I: 'static + Clone> Parser<T, I> {
    /// Returns a default value if the parser did not match.
    pub fn or_default(self, default: T) -> Self {
        Parser::new(move |input: I| match self.run(input.clone()) {
            ParseResult::Match(remaining, result) => ParseResult::Match(remaining, result),
            ParseResult::NoMatch => ParseResult::Match(input, default.clone()),
            ParseResult::Err(location) => ParseResult::Err(location),
        })
    }
}

impl<T: 'static, Iter, I: 'static + Clone> Parser<Iter, I>
where
    Iter: 'static + IntoIterator<Item = &'static T>,
{
    /// Folds the parsed result into a single value using an aggregator function.
    pub fn fold<T2: 'static + Clone>(
        self,
        init: T2,
        f: impl Fn(T2, &T) -> T2 + 'static,
    ) -> Parser<T2, I> {
        Parser::new(move |input: I| match self.run(input) {
            ParseResult::Match(remaining, result) => ParseResult::Match(
                remaining,
                result
                    .into_iter()
                    .fold(init.clone(), |acc, item| f(acc, item)),
            ),
            ParseResult::NoMatch => ParseResult::NoMatch,
            ParseResult::Err(location) => ParseResult::Err(location),
        })
    }
}

/// Creates a parser that always matches and returns the specified value.
pub fn always<T: 'static + Clone, I: Clone>(value: T) -> Parser<T, I> {
    Parser::new(move |input: I| ParseResult::Match(input, value.clone()))
}

/// Creates a parser that never matches.
pub fn never<T, I: Clone>() -> Parser<T, I> {
    Parser::new(move |_input: I| ParseResult::NoMatch)
}

/// Creates a parser that matches any of the given parsers. Parsers are matched in order.
pub fn choice<T: 'static, I: 'static + Clone, const N: usize>(
    parsers: [Parser<T, I>; N],
) -> Parser<T, I> {
    Parser::new(move |input: I| {
        for parser in parsers.clone() {
            match parser.run(input.clone()) {
                ParseResult::Match(remaining, result) => {
                    return ParseResult::Match(remaining, result)
                }
                ParseResult::NoMatch => continue,
                ParseResult::Err(location) => return ParseResult::Err(location),
            }
        }

        ParseResult::NoMatch
    })
}

/// Creates a parser that matches all of the given parsers, in order.
pub fn sequence<T: 'static, I: 'static + Clone, const N: usize>(
    parsers: [Parser<T, I>; N],
) -> Parser<Vec<T>, I> {
    Parser::new(move |input: I| {
        let mut results = Vec::with_capacity(N);
        let mut remaining = input;

        for parser in parsers.clone() {
            match parser.run(remaining) {
                ParseResult::Match(new_remaining, result) => {
                    remaining = new_remaining;
                    results.push(result);
                }
                ParseResult::NoMatch => return ParseResult::NoMatch,
                ParseResult::Err(location) => return ParseResult::Err(location),
            }
        }

        ParseResult::Match(remaining, results)
    })
}

/// Creates a parser that matches two parsers in sequence, returning only the second result.
pub fn prefixed<T1: 'static, T2: 'static, I: 'static + Clone>(
    prefix: Parser<T1, I>,
    parser: Parser<T2, I>,
) -> Parser<T2, I> {
    Parser::new(move |input: I| match prefix.run(input) {
        ParseResult::Match(remaining, _) => match parser.run(remaining) {
            ParseResult::Match(remaining, result) => ParseResult::Match(remaining, result),
            ParseResult::NoMatch => ParseResult::NoMatch,
            ParseResult::Err(location) => ParseResult::Err(location),
        },
        ParseResult::NoMatch => ParseResult::NoMatch,
        ParseResult::Err(location) => ParseResult::Err(location),
    })
}

/// Creates a parser that matches two parsers in sequence, returning only the first result.
pub fn suffixed<T1: 'static, T2: 'static, I: 'static + Clone>(
    parser: Parser<T1, I>,
    suffix: Parser<T2, I>,
) -> Parser<T1, I> {
    Parser::new(move |input: I| match parser.run(input) {
        ParseResult::Match(remaining, result) => match suffix.run(remaining) {
            ParseResult::Match(remaining, _) => ParseResult::Match(remaining, result),
            ParseResult::NoMatch => ParseResult::NoMatch,
            ParseResult::Err(location) => ParseResult::Err(location),
        },
        ParseResult::NoMatch => ParseResult::NoMatch,
        ParseResult::Err(location) => ParseResult::Err(location),
    })
}

/// Creates a parser that matches three parsers in sequence, returning only the second result.
pub fn between<T1: 'static, T2: 'static, T3: 'static, I: 'static + Clone>(
    prefix: Parser<T1, I>,
    parser: Parser<T2, I>,
    suffix: Parser<T3, I>,
) -> Parser<T2, I> {
    Parser::new(move |input: I| match prefix.run(input) {
        ParseResult::Match(remaining, _) => match parser.run(remaining) {
            ParseResult::Match(remaining, result) => match suffix.run(remaining) {
                ParseResult::Match(remaining, _) => ParseResult::Match(remaining, result),
                ParseResult::NoMatch => ParseResult::NoMatch,
                ParseResult::Err(location) => ParseResult::Err(location),
            },
            ParseResult::NoMatch => ParseResult::NoMatch,
            ParseResult::Err(location) => ParseResult::Err(location),
        },
        ParseResult::NoMatch => ParseResult::NoMatch,
        ParseResult::Err(location) => ParseResult::Err(location),
    })
}

#[cfg(feature = "operators")]
impl<T1: 'static, T2: 'static, I: 'static + Clone> BitAnd<Parser<T2, I>> for Parser<T1, I> {
    type Output = Parser<(T1, T2), I>;

    fn bitand(self, rhs: Parser<T2, I>) -> Self::Output {
        self.and_then(rhs)
    }
}

#[cfg(feature = "operators")]
impl<T: 'static, I: 'static + Clone> BitOr for Parser<T, I> {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        self.or_else(rhs)
    }
}

#[cfg(feature = "operators")]
impl<T1: 'static, T2: 'static, I: 'static + Clone> Shl<Parser<T2, I>> for Parser<T1, I> {
    type Output = Parser<T1, I>;

    fn shl(self, rhs: Parser<T2, I>) -> Self::Output {
        suffixed(self, rhs)
    }
}

#[cfg(feature = "operators")]
impl<T1: 'static, T2: 'static, I: 'static + Clone> Shr<Parser<T2, I>> for Parser<T1, I> {
    type Output = Parser<T2, I>;

    fn shr(self, rhs: Parser<T2, I>) -> Self::Output {
        prefixed(self, rhs)
    }
}
