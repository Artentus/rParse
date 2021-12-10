#![feature(pattern)]

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

/// Creates a parser that always matches and returns the specified value.
pub fn always<T: 'static + Clone, I: Clone>(value: T) -> Parser<T, I> {
    Parser::new(move |input: I| ParseResult::Match(input, value.clone()))
}

/// Creates a parser that never matches.
pub fn never<T, I: Clone>() -> Parser<T, I> {
    Parser::new(move |_input: I| ParseResult::NoMatch)
}

/// Creates a parser that errors if the inner parser did not match.
pub fn require<T: 'static, I: 'static + Clone>(parser: Parser<T, I>) -> Parser<T, I> {
    Parser::new(move |input: I| match parser.run(input.clone()) {
        ParseResult::Match(remaining, result) => ParseResult::Match(remaining, result),
        ParseResult::NoMatch => ParseResult::Err(input),
        ParseResult::Err(location) => ParseResult::Err(location),
    })
}

/// Creates a parser that doesn't match if the given validation function returns false.
pub fn verify<T: 'static, I: 'static + Clone>(
    parser: Parser<T, I>,
    f: impl Fn(&T) -> bool + 'static,
) -> Parser<T, I> {
    Parser::new(move |input: I| match parser.run(input.clone()) {
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

/// Creates a parser that returns None if the inner parser did not match.
pub fn opt<T: 'static, I: 'static + Clone>(parser: Parser<T, I>) -> Parser<Option<T>, I> {
    Parser::new(move |input: I| match parser.run(input.clone()) {
        ParseResult::Match(remaining, result) => ParseResult::Match(remaining, Some(result)),
        ParseResult::NoMatch => ParseResult::Match(input, None),
        ParseResult::Err(location) => ParseResult::Err(location),
    })
}

/// Creates a parser that runs the given inner parser and returns a specific value if it matches.
pub fn map_to<T1: 'static, T2: 'static + Clone, I: 'static + Clone>(
    parser: Parser<T1, I>,
    value: T2,
) -> Parser<T2, I> {
    Parser::new(move |input: I| match parser.run(input) {
        ParseResult::Match(remaining, _) => ParseResult::Match(remaining, value.clone()),
        ParseResult::NoMatch => ParseResult::NoMatch,
        ParseResult::Err(location) => ParseResult::Err(location),
    })
}

/// Creates a parser that runs the given inner parser and maps its result using a mapping function if it matches.
pub fn map<T1: 'static, T2, I: 'static + Clone>(
    parser: Parser<T1, I>,
    mapping: impl Fn(T1) -> T2 + 'static,
) -> Parser<T2, I> {
    Parser::new(move |input: I| match parser.run(input) {
        ParseResult::Match(remaining, result) => ParseResult::Match(remaining, mapping(result)),
        ParseResult::NoMatch => ParseResult::NoMatch,
        ParseResult::Err(location) => ParseResult::Err(location),
    })
}

/// Creates a parser from a given inner parser that matches a list and folds its result into a single value using an aggregator function.
pub fn fold<
    T1: 'static,
    Iter: 'static + IntoIterator<Item = &'static T1>,
    T2: 'static + Clone,
    I: 'static + Clone,
>(
    parser: Parser<Iter, I>,
    init: T2,
    f: impl Fn(T2, &T1) -> T2 + 'static,
) -> Parser<T2, I> {
    Parser::new(move |input: I| match parser.run(input) {
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

/// Creates a parser that matches two parsers in sequence.
pub fn and_then<T1: 'static, T2: 'static, I: 'static + Clone>(
    parser1: Parser<T1, I>,
    parser2: Parser<T2, I>,
) -> Parser<(T1, T2), I> {
    Parser::new(move |input: I| match parser1.run(input) {
        ParseResult::Match(remaining, result1) => match parser2.run(remaining) {
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

/// Creates a parser that matches the given inner parser N exactly times.
pub fn repeat<T: 'static, I: 'static + Clone>(
    parser: Parser<T, I>,
    count: usize,
) -> Parser<Vec<T>, I> {
    Parser::new(move |input: I| {
        let mut results = Vec::with_capacity(count);
        let mut remaining = input;

        for _ in 0..count {
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

/// Creates a parser that matches the given inner parser zero or more times.
pub fn many<T: 'static, I: 'static + Clone>(parser: Parser<T, I>) -> Parser<Vec<T>, I> {
    Parser::new(move |input: I| {
        let mut results = Vec::new();
        let mut remaining = input;

        loop {
            match parser.run(remaining.clone()) {
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

/// Creates a parser that matches the given inner parser one or more times.
pub fn many1<T: 'static, I: 'static + Clone>(parser: Parser<T, I>) -> Parser<Vec<T>, I> {
    Parser::new(move |input: I| match parser.run(input) {
        ParseResult::Match(mut remaining, result) => {
            let mut results = Vec::new();
            results.push(result);

            loop {
                match parser.run(remaining.clone()) {
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

/// Creates a parser that matches the given inner parser one or more times, separated by the given separator.
pub fn sep_by1<T: 'static, S: 'static, I: 'static + Clone>(
    parser: Parser<T, I>,
    separator: Parser<S, I>,
    parse_trailing: bool,
) -> Parser<Vec<T>, I> {
    let middle = many(prefixed(separator.clone(), parser.clone()));

    let trailing = if parse_trailing {
        map_to(opt(separator), ())
    } else {
        always(())
    };

    map(
        suffixed(and_then(parser, middle), trailing),
        |(head, mut tail)| {
            tail.insert(0, head);
            tail
        },
    )
}

/// Creates a parser that matches the given inner parser zero or more times, separated by the given separator.
pub fn sep_by<T: 'static, S: 'static, I: 'static + Clone>(
    parser: Parser<T, I>,
    separator: Parser<S, I>,
    parse_trailing: bool,
) -> Parser<Vec<T>, I> {
    map(opt(sep_by1(parser, separator, parse_trailing)), |list| {
        list.unwrap_or(Vec::new())
    })
}
