#![feature(pattern)]
#![feature(trait_alias)]

pub mod text;

#[derive(Debug)]
#[must_use]
pub enum ParseResult<T, I: Clone, E> {
    /// The input matched the parser pattern.
    Match { value: T, remaining: I },
    /// The input did not match the parser pattern.
    NoMatch,
    /// The input matched the parser pattern but was malformed or invalid.
    Err(E),
}
impl<T, I: Clone, E> ParseResult<T, I, E> {
    pub fn map<R>(self, f: impl FnOnce(T) -> R) -> ParseResult<R, I, E> {
        match self {
            Self::Match { value, remaining } => ParseResult::Match {
                value: f(value),
                remaining,
            },
            Self::NoMatch => ParseResult::NoMatch,
            Self::Err(err) => ParseResult::Err(err),
        }
    }
}

/// Parses some input into structured data.
pub trait Parser<T, I: Clone, E>: Fn(I) -> ParseResult<T, I, E> + Clone {}
impl<T, I: Clone, E, F> Parser<T, I, E> for F where F: Fn(I) -> ParseResult<T, I, E> + Clone {}

#[macro_export]
macro_rules! parser {
    ($input:ident: $input_type:ty => $body:block) => {
        move |$input: $input_type| $body
    };
}

/// Creates a parser that always matches and returns the specified value.
pub fn always<T: Clone, I: Clone, E>(value: T) -> impl Parser<T, I, E> {
    parser!(input: I => { ParseResult::Match { value: value.clone(), remaining: input } })
}

/// Creates a parser that never matches.
pub fn never<T, I: Clone, E>() -> impl Parser<T, I, E> {
    parser!(_input: I => { ParseResult::NoMatch })
}

/// Creates a parser that always errors.
pub fn fail<T, I: Clone, E>(gen_err: impl Fn(I) -> E + Clone) -> impl Parser<T, I, E> {
    parser!(input: I => { ParseResult::Err(gen_err(input)) })
}

/// Errors if the parser did not match.
pub fn require<T, I: Clone, E: Default>(p: impl Parser<T, I, E>) -> impl Parser<T, I, E> {
    parser!(input: I => {
        match p(input.clone()) {
            ParseResult::Match { value, remaining } => ParseResult::Match { value, remaining },
            ParseResult::NoMatch => ParseResult::Err(E::default()),
            ParseResult::Err(err) => ParseResult::Err(err),
        }
    })
}

/// Errors if the parser did not match.
pub fn require_map<T, I: Clone, E>(
    p: impl Parser<T, I, E>,
    gen_err: impl Fn(I) -> E + Clone,
) -> impl Parser<T, I, E> {
    parser!(input: I => {
        match p(input.clone()) {
            ParseResult::Match { value, remaining } => ParseResult::Match { value, remaining },
            ParseResult::NoMatch => ParseResult::Err(gen_err(input)),
            ParseResult::Err(err) => ParseResult::Err(err),
        }
    })
}

/// Doesn't match if the given validation function returns false.
pub fn verify<T, I: Clone, E>(
    p: impl Parser<T, I, E>,
    f: impl Fn(&T) -> bool + Clone,
) -> impl Parser<T, I, E> {
    parser!(input: I => {
        match p(input.clone()) {
            ParseResult::Match { value, remaining } => {
                if f(&value) {
                    ParseResult::Match { value, remaining }
                } else {
                    ParseResult::NoMatch
                }
            }
            ParseResult::NoMatch => ParseResult::NoMatch,
            ParseResult::Err(err) => ParseResult::Err(err),
        }
    })
}

/// Returns None if the parser did not match.
pub fn opt<T, I: Clone, E>(p: impl Parser<T, I, E>) -> impl Parser<Option<T>, I, E> {
    parser!(input: I => {
        match p(input.clone()) {
            ParseResult::Match { value, remaining } => ParseResult::Match { value: Some(value), remaining },
            ParseResult::NoMatch => ParseResult::Match { value: None, remaining: input },
            ParseResult::Err(err) => ParseResult::Err(err),
        }
    })
}

/// Returns a specific value if the parser matches.
pub fn map_to<T, I: Clone, E, R: Clone>(
    p: impl Parser<T, I, E>,
    return_value: R,
) -> impl Parser<R, I, E> {
    parser!(input: I => {
        match p(input) {
            ParseResult::Match { remaining, .. } => ParseResult::Match { value: return_value.clone(), remaining },
            ParseResult::NoMatch => ParseResult::NoMatch,
            ParseResult::Err(err) => ParseResult::Err(err),
        }
    })
}

/// Maps the parsed result using a mapping function if the parser matches.
pub fn map<T, I: Clone, E, R>(
    p: impl Parser<T, I, E>,
    mapping: impl Fn(T) -> R + Clone,
) -> impl Parser<R, I, E> {
    parser!(input: I => {
        match p(input) {
            ParseResult::Match { value, remaining } => ParseResult::Match { value: mapping(value), remaining },
            ParseResult::NoMatch => ParseResult::NoMatch,
            ParseResult::Err(err) => ParseResult::Err(err),
        }
    })
}

/// Matches another parser in sequence.
pub fn and_then<T1, T2, I: Clone, E>(
    first: impl Parser<T1, I, E>,
    second: impl Parser<T2, I, E>,
) -> impl Parser<(T1, T2), I, E> {
    parser!(input: I => {
        match first(input) {
            ParseResult::Match { value: value1, remaining } => match second(remaining) {
                ParseResult::Match { value: value2, remaining } => {
                    ParseResult::Match { value: (value1, value2), remaining }
                }
                ParseResult::NoMatch => ParseResult::NoMatch,
                ParseResult::Err(err) => ParseResult::Err(err),
            },
            ParseResult::NoMatch => ParseResult::NoMatch,
            ParseResult::Err(err) => ParseResult::Err(err),
        }
    })
}

/// Matches either this parser or another.
pub fn or_else<T, I: Clone, E>(
    first: impl Parser<T, I, E>,
    second: impl Parser<T, I, E>,
) -> impl Parser<T, I, E> {
    parser!(input: I => {
        match first(input.clone()) {
            ParseResult::Match { value, remaining } => ParseResult::Match { value, remaining },
            ParseResult::NoMatch => match second(input) {
                ParseResult::Match { value, remaining } => ParseResult::Match { value, remaining },
                ParseResult::NoMatch => ParseResult::NoMatch,
                ParseResult::Err(err) => ParseResult::Err(err),
            },
            ParseResult::Err(err) => ParseResult::Err(err),
        }
    })
}

/// Matches the parser exactly 'count' times.
pub fn repeat<T, I: Clone, E>(p: impl Parser<T, I, E>, count: usize) -> impl Parser<Vec<T>, I, E> {
    parser!(input: I => {
        let mut values = Vec::with_capacity(count);
        let mut remaining = input;

        for _ in 0..count {
            match p(remaining) {
                ParseResult::Match { value, remaining: new_remaining } => {
                    remaining = new_remaining;
                    values.push(value);
                }
                ParseResult::NoMatch => return ParseResult::NoMatch,
                ParseResult::Err(err) => return ParseResult::Err(err),
            }
        }

        ParseResult::Match { value: values, remaining }
    })
}

/// Matches the parser multiple times.
pub fn many<T, I: Clone, E>(
    p: impl Parser<T, I, E>,
    allow_empty: bool,
) -> impl Parser<Vec<T>, I, E> {
    parser!(input: I => {
        let mut values = Vec::new();
        let mut remaining = input;

        loop {
            match p(remaining.clone()) {
                ParseResult::Match { value, remaining: new_remaining } => {
                    remaining = new_remaining;
                    values.push(value);
                }
                ParseResult::NoMatch => break,
                ParseResult::Err(err) => return ParseResult::Err(err),
            }
        }

        if allow_empty || (values.len() > 0) {
            ParseResult::Match { value: values, remaining }
        } else {
            ParseResult::NoMatch
        }
    })
}

/// Matches the parser multiple times, separated by the given separator.
pub fn sep_by<T, I: Clone, E, S>(
    p: impl Parser<T, I, E>,
    s: impl Parser<S, I, E>,
    allow_empty: bool,
    allow_trailing_sep: bool,
) -> impl Parser<Vec<T>, I, E> {
    parser!(input: I => {
        match p(input.clone()) {
            ParseResult::Match { value, mut remaining } => {
                let mut values = Vec::new();
                values.push(value);

                loop {
                    match s(remaining.clone()) {
                        ParseResult::Match { remaining: sep_remaining, .. } => {
                            remaining = sep_remaining.clone();

                            match p(sep_remaining) {
                                ParseResult::Match { value, remaining: new_remaining } => {
                                    remaining = new_remaining;
                                    values.push(value);
                                }
                                ParseResult::NoMatch => {
                                    if allow_trailing_sep {
                                        break;
                                    } else {
                                        return ParseResult::NoMatch;
                                    }
                                },
                                ParseResult::Err(err) => return ParseResult::Err(err),
                            }
                        },
                        ParseResult::NoMatch => break,
                        ParseResult::Err(err) => return ParseResult::Err(err),
                    }
                }

                ParseResult::Match { value: values, remaining }
            },
            ParseResult::NoMatch => {
                if allow_empty {
                    ParseResult::Match { value: Vec::new(), remaining: input }
                } else {
                    ParseResult::NoMatch
                }
            },
            ParseResult::Err(err) => ParseResult::Err(err),
        }
    })
}

/// Returns a default value if the parser did not match.
pub fn or_default<T: Clone, I: Clone, E>(
    p: impl Parser<T, I, E>,
    default: T,
) -> impl Parser<T, I, E> {
    parser!(input: I => {
        match p(input.clone()) {
            ParseResult::Match { value, remaining } => ParseResult::Match { value, remaining },
            ParseResult::NoMatch => ParseResult::Match { value: default.clone(), remaining: input },
            ParseResult::Err(err) => ParseResult::Err(err),
        }
    })
}

/// Creates a parser that matches two parsers in sequence, returning only the second result.
pub fn prefixed<T1, T2, I: Clone, E>(
    prefix: impl Parser<T1, I, E>,
    parser: impl Parser<T2, I, E>,
) -> impl Parser<T2, I, E> {
    parser!(input: I => {
        match prefix(input) {
            ParseResult::Match { remaining, .. } => match parser(remaining) {
                ParseResult::Match { value, remaining } => ParseResult::Match { value, remaining },
                ParseResult::NoMatch => ParseResult::NoMatch,
                ParseResult::Err(err) => ParseResult::Err(err),
            },
            ParseResult::NoMatch => ParseResult::NoMatch,
            ParseResult::Err(err) => ParseResult::Err(err),
        }
    })
}

/// Creates a parser that matches two parsers in sequence, returning only the first result.
pub fn suffixed<T1, T2, I: Clone, E>(
    parser: impl Parser<T1, I, E>,
    suffix: impl Parser<T2, I, E>,
) -> impl Parser<T1, I, E> {
    parser!(input: I => {
        match parser(input) {
            ParseResult::Match { value, remaining } => match suffix(remaining) {
                ParseResult::Match { remaining, .. } => ParseResult::Match { value, remaining },
                ParseResult::NoMatch => ParseResult::NoMatch,
                ParseResult::Err(err) => ParseResult::Err(err),
            },
            ParseResult::NoMatch => ParseResult::NoMatch,
            ParseResult::Err(err) => ParseResult::Err(err),
        }
    })
}

/// Creates a parser that matches three parsers in sequence, returning only the second result.
pub fn between<T1, T2, T3, I: Clone, E>(
    prefix: impl Parser<T1, I, E>,
    parser: impl Parser<T2, I, E>,
    suffix: impl Parser<T3, I, E>,
) -> impl Parser<T2, I, E> {
    parser!(input: I => {
        match prefix(input) {
            ParseResult::Match { remaining, .. } => match parser(remaining) {
                ParseResult::Match { value, remaining } => match suffix(remaining) {
                    ParseResult::Match { remaining, .. } => ParseResult::Match { value, remaining },
                    ParseResult::NoMatch => ParseResult::NoMatch,
                    ParseResult::Err(err) => ParseResult::Err(err),
                },
                ParseResult::NoMatch => ParseResult::NoMatch,
                ParseResult::Err(err) => ParseResult::Err(err),
            },
            ParseResult::NoMatch => ParseResult::NoMatch,
            ParseResult::Err(err) => ParseResult::Err(err),
        }
    })
}

/// Creates a parser that matches any of the given parsers. Precedence is derived from order.
#[macro_export]
macro_rules! choice {
    ($p:expr) => {
        $p
    };
    ($head:expr, $($tail:expr),+) => {
        $crate::or_else($head, $crate::choice!($($tail),+))
    };
}

/// Creates a parser that matches all of the given parsers, in order.
#[macro_export]
macro_rules! sequence {
    ($p:expr) => {
        $p
    };
    ($head:expr, $($tail:expr),+) => {
        $crate::and_then($head, $crate::sequence!($($tail),+))
    };
}

#[macro_export]
macro_rules! build_parser {
    ($p:expr) => {
        $p
    };
    (?$p:expr) => {
        $crate::opt($p)
    };
    (%$p:expr) => {
        $crate::require($p)
    };

    ($lhs:expr, &, $rhs:expr, $($tail:tt)+) => {
        $crate::build_parser!($crate::and_then($lhs, $rhs), $($tail)+)
    };
    ($lhs:expr, &, $rhs:expr) => {
        $crate::and_then($lhs, $rhs)
    };
    (?$lhs:expr, &, $rhs:expr, $($tail:tt)+) => {
        $crate::build_parser!($crate::and_then($crate::opt($lhs), $rhs), $($tail)+)
    };
    (?$lhs:expr, &, $rhs:expr) => {
        $crate::and_then($crate::opt($lhs), $rhs)
    };
    ($lhs:expr, &, ?$rhs:expr, $($tail:tt)+) => {
        $crate::build_parser!($crate::and_then($lhs, $crate::opt($rhs)), $($tail)+)
    };
    ($lhs:expr, &, ?$rhs:expr) => {
        $crate::and_then($lhs, $crate::opt($rhs))
    };
    (?$lhs:expr, &, ?$rhs:expr, $($tail:tt)+) => {
        $crate::build_parser!($crate::and_then($crate::opt($lhs), $crate::opt($rhs)), $($tail)+)
    };
    (?$lhs:expr, &, ?$rhs:expr) => {
        $crate::and_then($crate::opt($lhs), $crate::opt($rhs))
    };
    (%$lhs:expr, &, $rhs:expr, $($tail:tt)+) => {
        $crate::build_parser!($crate::and_then($crate::require($lhs), $rhs), $($tail)+)
    };
    (%$lhs:expr, &, $rhs:expr) => {
        $crate::and_then($crate::require($lhs), $rhs)
    };
    ($lhs:expr, &, %$rhs:expr, $($tail:tt)+) => {
        $crate::build_parser!($crate::and_then($lhs, $crate::require($rhs)), $($tail)+)
    };
    ($lhs:expr, &, %$rhs:expr) => {
        $crate::and_then($lhs, $crate::require($rhs))
    };
    (%$lhs:expr, &, %$rhs:expr, $($tail:tt)+) => {
        $crate::build_parser!($crate::and_then($crate::require($lhs), $crate::require($rhs)), $($tail)+)
    };
    (%$lhs:expr, &, %$rhs:expr) => {
        $crate::and_then($crate::require($lhs), $crate::require($rhs))
    };

    ($lhs:expr, <&, $rhs:expr, $($tail:tt)+) => {
        $crate::build_parser!($crate::suffixed($lhs, $rhs), $($tail)+)
    };
    ($lhs:expr, <&, $rhs:expr) => {
        $crate::suffixed($lhs, $rhs)
    };
    (?$lhs:expr, <&, $rhs:expr, $($tail:tt)+) => {
        $crate::build_parser!($crate::suffixed($crate::opt($lhs), $rhs), $($tail)+)
    };
    (?$lhs:expr, <&, $rhs:expr) => {
        $crate::suffixed($crate::opt($lhs), $rhs)
    };
    ($lhs:expr, <&, ?$rhs:expr, $($tail:tt)+) => {
        $crate::build_parser!($crate::suffixed($lhs, $crate::opt($rhs)), $($tail)+)
    };
    ($lhs:expr, <&, ?$rhs:expr) => {
        $crate::suffixed($lhs, $crate::opt($rhs))
    };
    (?$lhs:expr, <&, ?$rhs:expr, $($tail:tt)+) => {
        $crate::build_parser!($crate::suffixed($crate::opt($lhs), $crate::opt($rhs)), $($tail)+)
    };
    (?$lhs:expr, <&, ?$rhs:expr) => {
        $crate::suffixed($crate::opt($lhs), $crate::opt($rhs))
    };
    (%$lhs:expr, <&, $rhs:expr, $($tail:tt)+) => {
        $crate::build_parser!($crate::suffixed($crate::require($lhs), $rhs), $($tail)+)
    };
    (%$lhs:expr, <&, $rhs:expr) => {
        $crate::suffixed($crate::require($lhs), $rhs)
    };
    ($lhs:expr, <&, %$rhs:expr, $($tail:tt)+) => {
        $crate::build_parser!($crate::suffixed($lhs, $crate::require($rhs)), $($tail)+)
    };
    ($lhs:expr, <&, %$rhs:expr) => {
        $crate::suffixed($lhs, $crate::require($rhs))
    };
    (%$lhs:expr, <&, %$rhs:expr, $($tail:tt)+) => {
        $crate::build_parser!($crate::suffixed($crate::require($lhs), $crate::require($rhs)), $($tail)+)
    };
    (%$lhs:expr, <&, %$rhs:expr) => {
        $crate::suffixed($crate::require($lhs), $crate::require($rhs))
    };

    ($lhs:expr, &>, $rhs:expr, $($tail:tt)+) => {
        $crate::build_parser!($crate::prefixed($lhs, $rhs), $($tail)+)
    };
    ($lhs:expr, &>, $rhs:expr) => {
        $crate::prefixed($lhs, $rhs)
    };
    (?$lhs:expr, &>, $rhs:expr, $($tail:tt)+) => {
        $crate::build_parser!($crate::prefixed($crate::opt($lhs), $rhs), $($tail)+)
    };
    (?$lhs:expr, &>, $rhs:expr) => {
        $crate::prefixed($crate::opt($lhs), $rhs)
    };
    ($lhs:expr, &>, ?$rhs:expr, $($tail:tt)+) => {
        $crate::build_parser!($crate::prefixed($lhs, $crate::opt($rhs)), $($tail)+)
    };
    ($lhs:expr, &>, ?$rhs:expr) => {
        $crate::prefixed($lhs, $crate::opt($rhs))
    };
    (?$lhs:expr, &>, ?$rhs:expr, $($tail:tt)+) => {
        $crate::build_parser!($crate::prefixed($crate::opt($lhs), $crate::opt($rhs)), $($tail)+)
    };
    (?$lhs:expr, &>, ?$rhs:expr) => {
        $crate::prefixed($crate::opt($lhs), $crate::opt($rhs))
    };
    (%$lhs:expr, &>, $rhs:expr, $($tail:tt)+) => {
        $crate::build_parser!($crate::prefixed($crate::require($lhs), $rhs), $($tail)+)
    };
    (%$lhs:expr, &>, $rhs:expr) => {
        $crate::prefixed($crate::require($lhs), $rhs)
    };
    ($lhs:expr, &>, %$rhs:expr, $($tail:tt)+) => {
        $crate::build_parser!($crate::prefixed($lhs, $crate::require($rhs)), $($tail)+)
    };
    ($lhs:expr, &>, %$rhs:expr) => {
        $crate::prefixed($lhs, $crate::require($rhs))
    };
    (%$lhs:expr, &>, %$rhs:expr, $($tail:tt)+) => {
        $crate::build_parser!($crate::prefixed($crate::require($lhs), $crate::require($rhs)), $($tail)+)
    };
    (%$lhs:expr, &>, %$rhs:expr) => {
        $crate::prefixed($crate::require($lhs), $crate::require($rhs))
    };

    ($lhs:expr, |, $($tail:tt)+) => {
        $crate::or_else($lhs, $crate::build_parser!($($tail)+))
    };
    ($lhs:expr, |, $rhs:expr) => {
        $crate::or_else($lhs, $rhs)
    };
    (?$lhs:expr, |, $($tail:tt)+) => {
        $crate::or_else($crate::opt($lhs), $crate::build_parser!($($tail)+))
    };
    (?$lhs:expr, |, $rhs:expr) => {
        $crate::or_else($crate::opt($lhs), $rhs)
    };
    ($lhs:expr, |, ?$rhs:expr) => {
        $crate::or_else($lhs, $crate::opt($rhs))
    };
    (?$lhs:expr, |, ?$rhs:expr) => {
        $crate::or_else($crate::opt($lhs), $crate::opt($rhs))
    };
    (%$lhs:expr, |, $($tail:tt)+) => {
        $crate::or_else($crate::require($lhs), $crate::build_parser!($($tail)+))
    };
    (%$lhs:expr, |, $rhs:expr) => {
        $crate::or_else($crate::require($lhs), $rhs)
    };
    ($lhs:expr, |, %$rhs:expr) => {
        $crate::or_else($lhs, $crate::require($rhs))
    };
    (%$lhs:expr, |, %$rhs:expr) => {
        $crate::or_else($crate::require($lhs), $crate::require($rhs))
    };
}
