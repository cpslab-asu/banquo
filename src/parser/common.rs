use std::collections::HashMap;
use std::str::FromStr;

use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, digit0, digit1, space0};
use nom::combinator::{map_res, opt, recognize};
use nom::sequence::{delimited, pair};
use nom::IResult;

use super::errors::ParsedFormulaError;
use crate::formula::{Formula, HybridDistance, HybridDistanceFormula, Result};
use crate::trace::Trace;

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

    fn robustness(&self, trace: &Trace<HashMap<String, f64>>) -> Result<f64, Self::Error> {
        self.0.robustness(trace).map_err(ParsedFormulaError::from_err)
    }
}

impl<L, F> HybridDistanceFormula<HashMap<String, f64>, L> for WrappedFormula<F>
where
    F: HybridDistanceFormula<HashMap<String, f64>, L>,
    F::Error: 'static,
{
    type Error = ParsedFormulaError;

    fn hybrid_distance(&self, trace: &Trace<(HashMap<String, f64>, L)>) -> Result<HybridDistance, Self::Error> {
        self.0.hybrid_distance(trace).map_err(ParsedFormulaError::from_err)
    }
}

pub fn var_name(input: &str) -> IResult<&str, String> {
    let mut parser = pair(alpha1, digit0);
    let (rest, (s1, s2)) = parser(input)?;
    let name = s1.to_string() + s2;

    Ok((rest, name))
}

pub fn pos_num(input: &str) -> IResult<&str, f64> {
    let make_number = |(front, back): (&str, Option<&str>)| {
        let num_str = front.to_string() + back.unwrap_or("");
        f64::from_str(&num_str)
    };

    let back_parser = pair(tag("."), digit1);
    let num_parser = pair(digit1, opt(recognize(back_parser)));
    let mut parser = map_res(num_parser, make_number);

    parser(input)
}

pub fn op0<'a>(op: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
    move |input: &'a str| -> IResult<&'a str, &'a str> {
        let mut parser = delimited(space0, tag(op), space0);
        parser(input)
    }
}
