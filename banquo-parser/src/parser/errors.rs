use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

#[derive(Clone, Debug)]
pub struct MissingPredicateError {
    name: String,
}

impl From<String> for MissingPredicateError {
    fn from(name: String) -> Self {
        Self { name }
    }
}

impl Display for MissingPredicateError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Could not find predicate {}", &self.name)
    }
}

impl Error for MissingPredicateError {}

pub struct ParsedFormulaError {
    inner: Box<dyn Error>,
}

impl ParsedFormulaError {
    pub fn from_err<E: Error + 'static>(err: E) -> Self {
        Self { inner: Box::new(err) }
    }
}

impl Display for ParsedFormulaError {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.inner, f)
    }
}

impl Debug for ParsedFormulaError {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.inner, f)
    }
}

impl Error for ParsedFormulaError {}

#[derive(Debug)]
pub struct IncompleteParseError<'a>(&'a str);

impl<'a> From<&'a str> for IncompleteParseError<'a> {
    fn from(rest: &'a str) -> Self {
        Self(rest)
    }
}

impl<'a> Display for IncompleteParseError<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "could not parse remaining input \"{}\")", self.0)
    }
}

impl<'a> Error for IncompleteParseError<'a> {}
