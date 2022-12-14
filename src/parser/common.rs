use std::collections::HashMap;

use nom::{IResult, Parser};
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, digit0, space0};
use nom::sequence::{delimited, pair};

use crate::formula::{Formula, HybridDistanceFormula};
use crate::trace::Trace;
use super::errors::ParsedFormulaError;

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
