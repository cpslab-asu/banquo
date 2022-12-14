use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

use nom::{IResult, Parser};
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, digit0, space0};
use nom::sequence::{delimited, pair};

use crate::formula::{Formula, HybridDistanceFormula};
use crate::trace::Trace;

pub struct ParsedFormulaError {
    inner: Box<dyn Error>,
}

impl ParsedFormulaError {
    fn from_err<E: Error + 'static>(err: E) -> Self {
        Self { inner: Box::new(err) }
    }
}

impl Display for ParsedFormulaError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.inner, f)
    }
}

impl Debug for ParsedFormulaError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.inner, f)
    }
}

impl Error for ParsedFormulaError {}

pub struct WrappedFormula<F>(F);

impl<F> WrappedFormula<F> {
    pub fn wrap(formula: F) -> Self {
        WrappedFormula(formula)
    }
}

impl<F> Formula<HashMap<String, f64>> for WrappedFormula<F>
where
    F: Formula<HashMap<String, f64>>,
    F::Error: 'static,
{
    type Error = ParsedFormulaError;

    fn robustness(&self, trace: &Trace<HashMap<String, f64>>) -> Result<Trace<f64>, Self::Error> {
        self.0.robustness(trace).map_err(ParsedFormulaError::from_err)
    }
}

impl<L, F> HybridDistanceFormula<HashMap<String, f64>, L> for WrappedFormula<F>
where
    F: HybridDistanceFormula<HashMap<String, f64>, L>,
    F::Error: 'static,
{
    type Error = ParsedFormulaError;

    fn hybrid_distance(&self, trace: &Trace<(HashMap<String, f64>, L)>) -> crate::Result<crate::HybridDistance, Self::Error> {
        self.0.hybrid_distance(trace).map_err(ParsedFormulaError::from_err)
    }
}

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

pub fn var_name(input: &str) -> IResult<&str, String> {
    let mut parser = pair(alpha1, digit0);
    let (rest, (s1, s2)) = parser(input)?;
    let name = s1.to_string() + s2;

    Ok((rest, name))
}

pub fn op0<'a>(op: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
    move |input: &'a str| -> IResult<&'a str, &'a str> {
        let mut parser = delimited(space0, tag(op), space0);
        parser(input)
    }
}

pub fn subformula<'a, F, T>(subparser: F) -> impl FnMut(&'a str) -> IResult<&'a str, T>
where
    F: Parser<&'a str, T, nom::error::Error<&'a str>>    
{
    let mut parser = delimited(tag("("), subparser, tag(")"));

    move |input: &str| {
        parser.parse(input)
    }
}
