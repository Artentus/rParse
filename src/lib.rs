#![feature(pattern)]

pub mod text;

use std::rc::Rc;

#[derive(Debug)]
#[must_use]
pub enum ParseResult<T, E, I: Clone> {
    /// The input matched the parser pattern.
    Match(I, T),
    /// The input did not match the parser pattern.
    NoMatch,
    /// The input matched the parser pattern but was malformed or invalid.
    Err(I, E),
}
impl<T, E, I: Clone> ParseResult<T, E, I> {
    pub fn map<R>(self, f: impl FnOnce(T) -> R) -> ParseResult<R, E, I> {
        match self {
            Self::Match(remaining, t) => ParseResult::Match(remaining, f(t)),
            Self::NoMatch => ParseResult::NoMatch,
            Self::Err(remaining, err) => ParseResult::Err(remaining, err),
        }
    }
}

/// Parses some input into structured data.
#[must_use]
pub struct Parser<'a, T, E, I: Clone> {
    parse_fn: Rc<dyn Fn(I) -> ParseResult<T, E, I> + 'a>,
}
impl<'a, T, E, I: Clone> Parser<'a, T, E, I> {
    /// Creates a new parser from a function.
    pub fn new(parse_fn: impl Fn(I) -> ParseResult<T, E, I> + 'a) -> Self {
        Self {
            parse_fn: Rc::new(parse_fn),
        }
    }

    /// Runs the parser on the given input.
    pub fn run(&self, input: I) -> ParseResult<T, E, I> {
        (self.parse_fn)(input)
    }
}
impl<'a, T, E, I: Clone> Clone for Parser<'a, T, E, I> {
    fn clone(&self) -> Self {
        Self {
            parse_fn: Rc::clone(&self.parse_fn),
        }
    }
}

#[macro_export]
macro_rules! parser {
    ($input:ident: $input_type:ty => $body:block) => {
        Parser::new(move |$input: $input_type| $body)
    };
}

impl<'a, T: 'a, E: 'a, I: 'a + Clone> Parser<'a, T, E, I> {
    /// Errors if the parser did not match.
    pub fn require(self, gen_err: impl Fn(I) -> E + 'a) -> Self {
        parser!(input: I => {
            match self.run(input.clone()) {
                ParseResult::Match(remaining, result) => ParseResult::Match(remaining, result),
                ParseResult::NoMatch => ParseResult::Err(input.clone(), gen_err(input)),
                ParseResult::Err(location, err) => ParseResult::Err(location, err),
            }
        })
    }

    /// Doesn't match if the given validation function returns false.
    pub fn verify(self, f: impl Fn(&T) -> bool + 'a) -> Self {
        parser!(input: I => {
            match self.run(input.clone()) {
                ParseResult::Match(remaining, result) => {
                    if f(&result) {
                        ParseResult::Match(remaining, result)
                    } else {
                        ParseResult::NoMatch
                    }
                }
                ParseResult::NoMatch => ParseResult::NoMatch,
                ParseResult::Err(location, err) => ParseResult::Err(location, err),
            }
        })
    }

    /// Returns None if the parser did not match.
    pub fn opt(self) -> Parser<'a, Option<T>, E, I> {
        parser!(input: I => {
            match self.run(input.clone()) {
                ParseResult::Match(remaining, result) => ParseResult::Match(remaining, Some(result)),
                ParseResult::NoMatch => ParseResult::Match(input, None),
                ParseResult::Err(location, err) => ParseResult::Err(location, err),
            }
        })
    }

    /// Returns a specific value if the parser matches.
    pub fn map_to<T2: 'a + Clone>(self, value: T2) -> Parser<'a, T2, E, I> {
        parser!(input: I => {
            match self.run(input) {
                ParseResult::Match(remaining, _) => ParseResult::Match(remaining, value.clone()),
                ParseResult::NoMatch => ParseResult::NoMatch,
                ParseResult::Err(location, err) => ParseResult::Err(location, err),
            }
        })
    }

    /// Maps the parsed result using a mapping function if the parser matches.
    pub fn map<T2: 'a>(self, mapping: impl Fn(T) -> T2 + 'a) -> Parser<'a, T2, E, I> {
        parser!(input: I => {
            match self.run(input) {
                ParseResult::Match(remaining, result) => ParseResult::Match(remaining, mapping(result)),
                ParseResult::NoMatch => ParseResult::NoMatch,
                ParseResult::Err(location, err) => ParseResult::Err(location, err),
            }
        })
    }

    /// Matches another parser in sequence.
    pub fn and_then<T2: 'a>(self, other: Parser<'a, T2, E, I>) -> Parser<'a, (T, T2), E, I> {
        parser!(input: I => {
            match self.run(input) {
                ParseResult::Match(remaining, result1) => match other.run(remaining) {
                    ParseResult::Match(remaining, result2) => {
                        ParseResult::Match(remaining, (result1, result2))
                    }
                    ParseResult::NoMatch => ParseResult::NoMatch,
                    ParseResult::Err(location, err) => ParseResult::Err(location, err),
                },
                ParseResult::NoMatch => ParseResult::NoMatch,
                ParseResult::Err(location, err) => ParseResult::Err(location, err),
            }
        })
    }

    /// Matches either this parser or another.
    pub fn or_else(self, other: Self) -> Self {
        parser!(input: I => {
            match self.run(input.clone()) {
                ParseResult::Match(remaining, result) => ParseResult::Match(remaining, result),
                ParseResult::NoMatch => match other.run(input) {
                    ParseResult::Match(remaining, result) => ParseResult::Match(remaining, result),
                    ParseResult::NoMatch => ParseResult::NoMatch,
                    ParseResult::Err(location, err) => ParseResult::Err(location, err),
                },
                ParseResult::Err(location, err) => ParseResult::Err(location, err),
            }
        })
    }

    /// Matches the parser exactly 'count' times.
    pub fn repeat(self, count: usize) -> Parser<'a, Vec<T>, E, I> {
        parser!(input: I => {
            let mut results = Vec::with_capacity(count);
            let mut remaining = input;

            for _ in 0..count {
                match self.run(remaining) {
                    ParseResult::Match(new_remaining, result) => {
                        remaining = new_remaining;
                        results.push(result);
                    }
                    ParseResult::NoMatch => return ParseResult::NoMatch,
                    ParseResult::Err(location, err) => return ParseResult::Err(location, err),
                }
            }

            ParseResult::Match(remaining, results)
        })
    }

    /// Matches the parser zero or more times.
    pub fn many(self) -> Parser<'a, Vec<T>, E, I> {
        parser!(input: I => {
            let mut results = Vec::new();
            let mut remaining = input;

            loop {
                match self.run(remaining.clone()) {
                    ParseResult::Match(new_remaining, result) => {
                        remaining = new_remaining;
                        results.push(result);
                    }
                    ParseResult::NoMatch => break,
                    ParseResult::Err(location, err) => return ParseResult::Err(location, err),
                }
            }

            ParseResult::Match(remaining, results)
        })
    }

    /// Matches the parser one or more times.
    pub fn many1(self) -> Parser<'a, Vec<T>, E, I> {
        parser!(input: I => {
            match self.run(input) {
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
                            ParseResult::Err(location, err) => return ParseResult::Err(location, err),
                        }
                    }

                    ParseResult::Match(remaining, results)
                }
                ParseResult::NoMatch => ParseResult::NoMatch,
                ParseResult::Err(location, err) => ParseResult::Err(location, err),
            }
        })
    }

    /// Matches the parser one or more times, separated by the given separator.
    pub fn sep_by1<S: 'a>(
        self,
        separator: Parser<'a, S, E, I>,
        parse_trailing: bool,
    ) -> Parser<'a, Vec<T>, E, I> {
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
    pub fn sep_by<S: 'a>(
        self,
        separator: Parser<'a, S, E, I>,
        parse_trailing: bool,
    ) -> Parser<'a, Vec<T>, E, I> {
        self.sep_by1(separator, parse_trailing)
            .opt()
            .map(|list| list.unwrap_or(Vec::new()))
    }
}

impl<'a, T: 'a + Clone, E: 'a, I: 'a + Clone> Parser<'a, T, E, I> {
    /// Returns a default value if the parser did not match.
    pub fn or_default(self, default: T) -> Self {
        parser!(input: I => {
            match self.run(input.clone()) {
                ParseResult::Match(remaining, result) => ParseResult::Match(remaining, result),
                ParseResult::NoMatch => ParseResult::Match(input, default.clone()),
                ParseResult::Err(location, err) => ParseResult::Err(location, err),
            }
        })
    }
}

impl<'a, T: 'a, E: 'a, Iter, I: 'a + Clone> Parser<'a, Iter, E, I>
where
    Iter: 'a + IntoIterator<Item = &'a T>,
{
    /// Folds the parsed result into a single value using an aggregator function.
    pub fn fold<T2: 'a + Clone>(
        self,
        init: T2,
        f: impl Fn(T2, &T) -> T2 + 'a,
    ) -> Parser<'a, T2, E, I> {
        parser!(input: I => {
            match self.run(input) {
                ParseResult::Match(remaining, result) => ParseResult::Match(
                    remaining,
                    result
                        .into_iter()
                        .fold(init.clone(), |acc, item| f(acc, item)),
                ),
                ParseResult::NoMatch => ParseResult::NoMatch,
                ParseResult::Err(location, err) => ParseResult::Err(location, err),
            }
        })
    }
}

/// Creates a parser that always matches and returns the specified value.
pub fn always<'a, T: 'a + Clone, E: 'a, I: Clone>(value: T) -> Parser<'a, T, E, I> {
    parser!(input: I => { ParseResult::Match(input, value.clone()) })
}

/// Creates a parser that never matches.
pub fn never<'a, T, E: 'a, I: Clone>() -> Parser<'a, T, E, I> {
    parser!(_input: I => { ParseResult::NoMatch })
}

/// Creates a parser that matches any of the given parsers. Parsers are matched in order.
pub fn choice<'a, T: 'a, E: 'a, I: 'a + Clone, const N: usize>(
    parsers: [Parser<'a, T, E, I>; N],
) -> Parser<'a, T, E, I> {
    parser!(input: I => {
        for parser in parsers.clone() {
            match parser.run(input.clone()) {
                ParseResult::Match(remaining, result) => {
                    return ParseResult::Match(remaining, result)
                }
                ParseResult::NoMatch => continue,
                ParseResult::Err(location, err) => return ParseResult::Err(location, err),
            }
        }

        ParseResult::NoMatch
    })
}

/// Creates a parser that matches all of the given parsers, in order.
pub fn sequence<'a, T: 'a, E: 'a, I: 'a + Clone, const N: usize>(
    parsers: [Parser<'a, T, E, I>; N],
) -> Parser<'a, Vec<T>, E, I> {
    parser!(input: I => {
        let mut results = Vec::with_capacity(N);
        let mut remaining = input;

        for parser in parsers.clone() {
            match parser.run(remaining) {
                ParseResult::Match(new_remaining, result) => {
                    remaining = new_remaining;
                    results.push(result);
                }
                ParseResult::NoMatch => return ParseResult::NoMatch,
                ParseResult::Err(location, err) => return ParseResult::Err(location, err),
            }
        }

        ParseResult::Match(remaining, results)
    })
}

/// Creates a parser that matches two parsers in sequence, returning only the second result.
pub fn prefixed<'a, T1: 'a, T2: 'a, E: 'a, I: 'a + Clone>(
    prefix: Parser<'a, T1, E, I>,
    parser: Parser<'a, T2, E, I>,
) -> Parser<'a, T2, E, I> {
    parser!(input: I => {
        match prefix.run(input) {
            ParseResult::Match(remaining, _) => match parser.run(remaining) {
                ParseResult::Match(remaining, result) => ParseResult::Match(remaining, result),
                ParseResult::NoMatch => ParseResult::NoMatch,
                ParseResult::Err(location, err) => ParseResult::Err(location, err),
            },
            ParseResult::NoMatch => ParseResult::NoMatch,
            ParseResult::Err(location, err) => ParseResult::Err(location, err),
        }
    })
}

/// Creates a parser that matches two parsers in sequence, returning only the first result.
pub fn suffixed<'a, T1: 'a, T2: 'a, E: 'a, I: 'a + Clone>(
    parser: Parser<'a, T1, E, I>,
    suffix: Parser<'a, T2, E, I>,
) -> Parser<'a, T1, E, I> {
    parser!(input: I => {
        match parser.run(input) {
            ParseResult::Match(remaining, result) => match suffix.run(remaining) {
                ParseResult::Match(remaining, _) => ParseResult::Match(remaining, result),
                ParseResult::NoMatch => ParseResult::NoMatch,
                ParseResult::Err(location, err) => ParseResult::Err(location, err),
            },
            ParseResult::NoMatch => ParseResult::NoMatch,
            ParseResult::Err(location, err) => ParseResult::Err(location, err),
        }
    })
}

/// Creates a parser that matches three parsers in sequence, returning only the second result.
pub fn between<'a, T1: 'a, T2: 'a, T3: 'a, E: 'a, I: 'a + Clone>(
    prefix: Parser<'a, T1, E, I>,
    parser: Parser<'a, T2, E, I>,
    suffix: Parser<'a, T3, E, I>,
) -> Parser<'a, T2, E, I> {
    parser!(input: I => {
        match prefix.run(input) {
            ParseResult::Match(remaining, _) => match parser.run(remaining) {
                ParseResult::Match(remaining, result) => match suffix.run(remaining) {
                    ParseResult::Match(remaining, _) => ParseResult::Match(remaining, result),
                    ParseResult::NoMatch => ParseResult::NoMatch,
                    ParseResult::Err(location, err) => ParseResult::Err(location, err),
                },
                ParseResult::NoMatch => ParseResult::NoMatch,
                ParseResult::Err(location, err) => ParseResult::Err(location, err),
            },
            ParseResult::NoMatch => ParseResult::NoMatch,
            ParseResult::Err(location, err) => ParseResult::Err(location, err),
        }
    })
}

#[cfg(feature = "operators")]
use std::ops::{BitAnd, BitOr, Shl, Shr};

#[cfg(feature = "operators")]
impl<'a, T1: 'a, T2: 'a, E: 'a, I: 'a + Clone> BitAnd<Parser<'a, T2, E, I>>
    for Parser<'a, T1, E, I>
{
    type Output = Parser<'a, (T1, T2), E, I>;

    fn bitand(self, rhs: Parser<'a, T2, E, I>) -> Self::Output {
        self.and_then(rhs)
    }
}

#[cfg(feature = "operators")]
impl<'a, T: 'a, E: 'a, I: 'a + Clone> BitOr for Parser<'a, T, E, I> {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        self.or_else(rhs)
    }
}

#[cfg(feature = "operators")]
impl<'a, T1: 'a, T2: 'a, E: 'a, I: 'a + Clone> Shl<Parser<'a, T2, E, I>> for Parser<'a, T1, E, I> {
    type Output = Parser<'a, T1, E, I>;

    fn shl(self, rhs: Parser<'a, T2, E, I>) -> Self::Output {
        suffixed(self, rhs)
    }
}

#[cfg(feature = "operators")]
impl<'a, T1: 'a, T2: 'a, E: 'a, I: 'a + Clone> Shr<Parser<'a, T2, E, I>> for Parser<'a, T1, E, I> {
    type Output = Parser<'a, T2, E, I>;

    fn shr(self, rhs: Parser<'a, T2, E, I>) -> Self::Output {
        prefixed(self, rhs)
    }
}
