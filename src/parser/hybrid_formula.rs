use std::collections::HashMap;
use std::error::Error;
use std::hash::Hash;

use nom::{IResult, Parser};
use nom::branch::alt;
use nom::combinator::{map, map_res};

use crate::expressions::HybridPredicate;
use crate::formula::HybridDistanceFormula;
use crate::trace::Trace;
use super::common::{WrappedFormula, subformula, var_name};
use super::errors::{IncompleteParseError, MissingPredicateError, ParsedFormulaError};
use super::operators;

type VariableMap = HashMap<String, f64>;
type PredicateMap<L> = HashMap<String, HybridPredicate<L>>;

pub struct ParsedHybridFormula<L> {
    inner: Box<dyn HybridDistanceFormula<HashMap<String, f64>, L, Error = ParsedFormulaError>>
}

impl<L> ParsedHybridFormula<L> {
    fn new<F>(formula: F) -> Self
    where
        F: HybridDistanceFormula<HashMap<String, f64>, L> + 'static,
        F::Error: 'static,
    {
        Self {
            inner: Box::new(WrappedFormula::wrap(formula))
        }
    }
}

impl<L> HybridDistanceFormula<VariableMap, L> for ParsedHybridFormula<L> {
    type Error = ParsedFormulaError;    

    #[inline]
    fn hybrid_distance(&self, trace: &Trace<(VariableMap, L)>) -> crate::formula::Result<crate::HybridDistance, Self::Error> {
        self.inner.hybrid_distance(trace)
    }
}

fn predicate<'a, 'b, L>(predicates: &'a PredicateMap<L>) -> impl FnMut(&'b str) -> IResult<&'b str, HybridPredicate<L>> + 'a
where
    L: Copy
{
    move |input: &'b str| -> IResult<&'b str, HybridPredicate<L>> {
        let get_predicate = |name: String| predicates.get(&name).ok_or(MissingPredicateError::from(name));
        let mut parser = map_res(var_name, get_predicate);
        let (rest, predicate) = parser(input)?;

        Ok((rest, predicate.clone()))
    }
}

fn operand<'a, L>(predicates: &'a PredicateMap<L>) -> impl FnMut(&'a str) -> IResult<&'a str, ParsedHybridFormula<L>>
where
    L: Copy + Ord + Hash + 'static
{
    move |input: &'a str| {
        let p1 = map(predicate(predicates), ParsedHybridFormula::<L>::new);
        let p2 = subformula(hybrid_formula(predicates));    
        let mut parser = alt((p1, p2));
        parser(input)
    }
}

fn not<'a, L>(predicates: &'a PredicateMap<L>) -> impl FnMut(&'a str) -> IResult<&'a str, ParsedHybridFormula<L>>
where
    L: Copy + Ord + Hash + 'static
{
    move |input: &'a str| {
        let mut parser = operators::not(operand(predicates));
        let (rest, formula) = parser(input)?;

        Ok((rest, ParsedHybridFormula::new(formula)))
    }
}

fn and<'a, L>(predicates: &'a PredicateMap<L>) -> impl FnMut(&'a str) -> IResult<&'a str, ParsedHybridFormula<L>>
where
    L: Copy + Ord + Hash + 'static
{
    move |input: &'a str| {
        let mut parser = operators::and(operand(predicates), operand(predicates));
        let (rest, formula) = parser(input)?;

        Ok((rest, ParsedHybridFormula::new(formula)))
    }
}

fn or<'a, L>(predicates: &'a PredicateMap<L>) -> impl FnMut(&'a str) -> IResult<&'a str, ParsedHybridFormula<L>>
where
    L: Copy + Ord + Hash + 'static
{
    move |input: &'a str| {
        let mut parser = operators::or(operand(predicates), operand(predicates));
        let (rest, formula) = parser(input)?;

        Ok((rest, ParsedHybridFormula::new(formula)))
    }
}

fn implies<'a, L>(predicates: &'a PredicateMap<L>) -> impl FnMut(&'a str) -> IResult<&'a str, ParsedHybridFormula<L>>
where
    L: Copy + Ord + Hash + 'static
{
    move |input: &'a str| {
        let mut parser = operators::implies(operand(predicates), operand(predicates));
        let (rest, formula) = parser(input)?;

        Ok((rest, ParsedHybridFormula::new(formula)))
    }
}

fn next<'a, L>(predicates: &'a PredicateMap<L>) -> impl FnMut(&'a str) -> IResult<&'a str, ParsedHybridFormula<L>>
where
    L: Copy + Ord + Hash + 'static
{
    move |input: &'a str| {
        let mut parser = operators::next(operand(predicates));
        let (rest, formula) = parser(input)?;

        Ok((rest, ParsedHybridFormula::new(formula)))
    }
}

fn always<'a, L>(predicates: &'a PredicateMap<L>) -> impl FnMut(&'a str) -> IResult<&'a str, ParsedHybridFormula<L>>
where
    L: Copy + Ord + Hash + 'static
{
    move |input: &'a str| {
        let mut parser = operators::always(operand(predicates));
        let (rest, formula) = parser(input)?;

        Ok((rest, ParsedHybridFormula::new(formula)))
    }
}

fn eventually<'a, L>(predicates: &'a PredicateMap<L>) -> impl FnMut(&'a str) -> IResult<&'a str, ParsedHybridFormula<L>>
where
    L: Copy + Ord + Hash + 'static
{
    move |input: &'a str| {
        let mut parser = operators::eventually(operand(predicates));
        let (rest, formula) = parser(input)?;

        Ok((rest, ParsedHybridFormula::new(formula)))
    }
}

fn hybrid_formula<'a, L>(predicates: &'a PredicateMap<L>) -> impl FnMut(&'a str) -> IResult<&'a str, ParsedHybridFormula<L>>
where
    L: Copy + Ord + Hash + 'static
{
    let mut parser = alt((
        not(predicates),
        next(predicates),
        always(predicates),
        eventually(predicates),
        and(predicates),
        or(predicates),
        implies(predicates),
        operand(predicates),
    ));

    move |input: &'a str| parser.parse(input)
}

pub fn parse_hybrid_formula<'a, L>(input: &'a str, predicates: &'a PredicateMap<L>) -> Result<ParsedHybridFormula<L>, Box<dyn Error + 'a>>
where
    L: Copy + Ord + Hash + 'static
{
    let mut parser = hybrid_formula(predicates);
    let (rest, formula) = parser(input)?;

    if rest.len() > 0 {
        Err(Box::new(IncompleteParseError::from(rest)))
    } else {
        Ok(formula)
    }
}
