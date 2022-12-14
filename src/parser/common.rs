
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

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