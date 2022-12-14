use std::collections::HashMap;
use std::error::Error;

use nom::IResult;

use crate::expressions::HybridPredicate;
use crate::formula::HybridDistanceFormula;
use super::common::{ParsedFormulaError, WrappedFormula, IncompleteParseError};

pub struct ParsedHybridFormula<L> {
    inner: Box<dyn HybridDistanceFormula<HashMap<String, f64>, L, Error = ParsedFormulaError>>
}

impl<L> ParsedHybridFormula<L> {
    fn wrap<F>(formula: F) -> Self
    where
        F: HybridDistanceFormula<HashMap<String, f64>, L> + 'static,
        F::Error: 'static,
    {
        Self {
            inner: Box::new(WrappedFormula::wrap(formula))
        }
    }
}

fn hybrid_predicate<'a, L>(predicates: &HashMap<String, HybridPredicate<L>>) -> impl Fn(&'a str) -> IResult<&'a str, HybridPredicate<L>> {
    let parser = |input: &str| -> IResult<&str, HybridPredicate<L>> {
        todo!()
    };

    parser
}

fn hybrid_formula<L>(input: &str, predicates: HashMap<String, HybridPredicate<L>>) -> IResult<&str, ParsedHybridFormula<L>> {
    todo!()
}

pub fn parse_hybrid_formula<'a, L>(input: &'a str, predicates: HashMap<String, HybridPredicate<L>>) -> Result<ParsedHybridFormula<L>, Box<dyn Error + 'a>> {
    let (rest, formula) = hybrid_formula(input, predicates)?;

    if rest.len() > 0 {
        Err(Box::new(IncompleteParseError::from(rest)))
    } else {
        Ok(formula)
    }
}
