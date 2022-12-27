use std::collections::HashMap;
use std::error::Error;
use std::rc::Rc;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{space0, space1};
use nom::combinator::{map, map_res};
use nom::error::Error as NomError;
use nom::sequence::{delimited, preceded, terminated};
use nom::Parser;

use super::common::{var_name, WrappedFormula};
use super::errors::{IncompleteParseError, MissingPredicateError, ParsedFormulaError};
use super::operators;
use crate::formulas::{HybridDistance, HybridDistanceFormula};
use crate::trace::Trace;

type VariableMap = HashMap<String, f64>;

pub struct ParsedHybridFormula<'a, L> {
    inner: Box<dyn HybridDistanceFormula<HashMap<String, f64>, L, Error = ParsedFormulaError> + 'a>,
}

impl<'a, L> ParsedHybridFormula<'a, L> {
    fn new<F>(formula: F) -> Self
    where
        F: HybridDistanceFormula<HashMap<String, f64>, L> + 'a,
        F::Error: 'static,
    {
        Self {
            inner: Box::new(WrappedFormula::wrap(formula)),
        }
    }
}

impl<'a, L> HybridDistanceFormula<VariableMap, L> for ParsedHybridFormula<'a, L> {
    type Error = ParsedFormulaError;

    #[inline]
    fn hybrid_distance(&self, trace: &Trace<(VariableMap, L)>) -> Result<Trace<HybridDistance>, Self::Error> {
        self.inner.hybrid_distance(trace)
    }
}

struct PredicateParser<F> {
    predicates: Rc<HashMap<String, Rc<F>>>,
}

impl<F> PredicateParser<F> {
    fn new(predicates: &Rc<HashMap<String, Rc<F>>>) -> Self {
        Self {
            predicates: predicates.clone(),
        }
    }
}

impl<'a, F> Parser<&'a str, Rc<F>, NomError<&'a str>> for PredicateParser<F> {
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, Rc<F>> {
        let get_predicate = |name: String| {
            self.predicates
                .get(&name)
                .ok_or_else(move || MissingPredicateError::from(name))
        };
        let mut parser = map_res(var_name, get_predicate);
        let (rest, predicate) = parser.parse(input)?;

        Ok((rest, predicate.clone()))
    }
}

struct SubformulaParser<F> {
    predicates: Rc<HashMap<String, Rc<F>>>,
}

impl<F> SubformulaParser<F> {
    fn new(predicates: &Rc<HashMap<String, Rc<F>>>) -> Self {
        Self {
            predicates: predicates.clone(),
        }
    }
}

impl<'a, 'b, F, L> Parser<&'a str, ParsedHybridFormula<'b, L>, NomError<&'a str>> for SubformulaParser<F>
where
    'b: 'a,
    F: HybridDistanceFormula<VariableMap, L> + 'b,
    F::Error: 'static,
    L: 'b,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedHybridFormula<'b, L>> {
        let inner = delimited(space0, hybrid_formula(self.predicates.clone()), space0);
        let mut parser = delimited(tag("("), inner, tag(")"));

        parser.parse(input)
    }
}

fn loperand<'a, 'b, F, L>(
    predicates: &Rc<HashMap<String, Rc<F>>>,
) -> impl Parser<&'a str, ParsedHybridFormula<'b, L>, NomError<&'a str>>
where
    'b: 'a,
    F: HybridDistanceFormula<VariableMap, L> + 'b,
    F::Error: 'static,
    L: 'b,
{
    let mut p1 = terminated(PredicateParser::new(predicates), space1);
    let mut p2 = terminated(SubformulaParser::new(predicates), space0);

    move |input: &'a str| {
        if let Ok((rest, pred)) = p1.parse(input) {
            Ok((rest, ParsedHybridFormula::new(pred)))
        } else {
            p2.parse(input)
        }
    }
}

fn roperand<'a, 'b, F, L>(
    predicates: &Rc<HashMap<String, Rc<F>>>,
) -> impl Parser<&'a str, ParsedHybridFormula<'b, L>, NomError<&'a str>>
where
    'b: 'a,
    F: HybridDistanceFormula<VariableMap, L> + 'b,
    F::Error: 'static,
    L: 'b,
{
    let mut p1 = preceded(space1, PredicateParser::new(predicates));
    let mut p2 = preceded(space0, SubformulaParser::new(predicates));

    move |input: &'a str| {
        if let Ok((rest, pred)) = p1.parse(input) {
            Ok((rest, ParsedHybridFormula::new(pred)))
        } else {
            p2.parse(input)
        }
    }
}

struct NotParser<F> {
    predicates: Rc<HashMap<String, Rc<F>>>,
}

impl<F> NotParser<F> {
    fn new(predicates: &Rc<HashMap<String, Rc<F>>>) -> Self {
        Self {
            predicates: predicates.clone(),
        }
    }
}

impl<'a, 'b, F, L> Parser<&'a str, ParsedHybridFormula<'b, L>, NomError<&'a str>> for NotParser<F>
where
    'b: 'a,
    F: HybridDistanceFormula<VariableMap, L> + 'b,
    F::Error: 'static,
    L: 'b,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedHybridFormula<'b, L>> {
        let mut parser = operators::not(roperand(&self.predicates));
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedHybridFormula::new(formula)))
    }
}

struct AndParser<F> {
    predicates: Rc<HashMap<String, Rc<F>>>,
}

impl<F> AndParser<F> {
    fn new(predicates: &Rc<HashMap<String, Rc<F>>>) -> Self {
        Self {
            predicates: predicates.clone(),
        }
    }
}

impl<'a, 'b, F, L> Parser<&'a str, ParsedHybridFormula<'b, L>, NomError<&'a str>> for AndParser<F>
where
    'b: 'a,
    F: HybridDistanceFormula<VariableMap, L> + 'b,
    F::Error: 'static,
    L: 'b,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedHybridFormula<'b, L>> {
        let mut parser = operators::and(loperand(&self.predicates), roperand(&self.predicates));
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedHybridFormula::new(formula)))
    }
}

struct OrParser<F> {
    predicates: Rc<HashMap<String, Rc<F>>>,
}

impl<F> OrParser<F> {
    fn new(predicates: &Rc<HashMap<String, Rc<F>>>) -> Self {
        Self {
            predicates: predicates.clone(),
        }
    }
}

impl<'a, 'b, F, L> Parser<&'a str, ParsedHybridFormula<'b, L>, NomError<&'a str>> for OrParser<F>
where
    'b: 'a,
    F: HybridDistanceFormula<VariableMap, L> + 'b,
    F::Error: 'static,
    L: 'b,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedHybridFormula<'b, L>> {
        let mut parser = operators::or(loperand(&self.predicates), roperand(&self.predicates));
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedHybridFormula::new(formula)))
    }
}

struct ImpliesParser<F> {
    predicates: Rc<HashMap<String, Rc<F>>>,
}

impl<F> ImpliesParser<F> {
    fn new(predicates: &Rc<HashMap<String, Rc<F>>>) -> Self {
        Self {
            predicates: predicates.clone(),
        }
    }
}

impl<'a, 'b, F, L> Parser<&'a str, ParsedHybridFormula<'b, L>, NomError<&'a str>> for ImpliesParser<F>
where
    'b: 'a,
    F: HybridDistanceFormula<VariableMap, L> + 'b,
    F::Error: 'static,
    L: 'b,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedHybridFormula<'b, L>> {
        let mut parser = operators::implies(loperand(&self.predicates), roperand(&self.predicates));
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedHybridFormula::new(formula)))
    }
}

struct NextParser<F> {
    predicates: Rc<HashMap<String, Rc<F>>>,
}

impl<F> NextParser<F> {
    fn new(predicates: &Rc<HashMap<String, Rc<F>>>) -> Self {
        Self {
            predicates: predicates.clone(),
        }
    }
}

impl<'a, 'b, F, L> Parser<&'a str, ParsedHybridFormula<'b, L>, NomError<&'a str>> for NextParser<F>
where
    'b: 'a,
    F: HybridDistanceFormula<VariableMap, L> + 'b,
    F::Error: 'static,
    L: 'b,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedHybridFormula<'b, L>> {
        let mut parser = operators::next(roperand(&self.predicates));
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedHybridFormula::new(formula)))
    }
}

struct AlwaysParser<F> {
    predicates: Rc<HashMap<String, Rc<F>>>,
}

impl<F> AlwaysParser<F> {
    fn new(predicates: &Rc<HashMap<String, Rc<F>>>) -> Self {
        Self {
            predicates: predicates.clone(),
        }
    }
}

impl<'a, 'b, F, L> Parser<&'a str, ParsedHybridFormula<'b, L>, NomError<&'a str>> for AlwaysParser<F>
where
    'b: 'a,
    F: HybridDistanceFormula<VariableMap, L> + 'b,
    F::Error: 'static,
    L: 'b,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedHybridFormula<'b, L>> {
        let mut parser = operators::always(roperand(&self.predicates));
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedHybridFormula::new(formula)))
    }
}

struct EventuallyParser<F> {
    predicates: Rc<HashMap<String, Rc<F>>>,
}

impl<F> EventuallyParser<F> {
    fn new(predicates: &Rc<HashMap<String, Rc<F>>>) -> Self {
        Self {
            predicates: predicates.clone(),
        }
    }
}

impl<'a, 'b, F, L> Parser<&'a str, ParsedHybridFormula<'b, L>, NomError<&'a str>> for EventuallyParser<F>
where
    'b: 'a,
    F: HybridDistanceFormula<VariableMap, L> + 'b,
    F::Error: 'static,
    L: 'b,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedHybridFormula<'b, L>> {
        let mut parser = operators::eventually(roperand(&self.predicates));
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedHybridFormula::new(formula)))
    }
}

fn hybrid_formula<'a, 'b, F, L>(
    predicates: Rc<HashMap<String, Rc<F>>>,
) -> impl Parser<&'a str, ParsedHybridFormula<'b, L>, NomError<&'a str>>
where
    'b: 'a,
    F: HybridDistanceFormula<VariableMap, L> + 'b,
    F::Error: 'static,
    L: 'b,
{
    let mut parser = alt((
        NotParser::new(&predicates),
        NextParser::new(&predicates),
        AlwaysParser::new(&predicates),
        EventuallyParser::new(&predicates),
        AndParser::new(&predicates),
        OrParser::new(&predicates),
        ImpliesParser::new(&predicates),
        SubformulaParser::new(&predicates),
        map(PredicateParser::new(&predicates), ParsedHybridFormula::new),
    ));

    move |input: &'a str| parser.parse(input)
}

pub fn parse_hybrid_formula<'a, 'b, F, L>(
    input: &'a str,
    predicates: HashMap<String, F>,
) -> Result<ParsedHybridFormula<'b, L>, Box<dyn Error + 'a>>
where
    F: HybridDistanceFormula<VariableMap, L> + 'b,
    F::Error: 'static,
    L: 'b,
    'b: 'a,
{
    let predicates = predicates
        .into_iter()
        .map(|(name, predicate)| (name, Rc::new(predicate)))
        .collect();

    let mut parser = hybrid_formula(Rc::new(predicates));
    let (rest, formula) = parser.parse(input)?;

    if !rest.is_empty() {
        Err(Box::new(IncompleteParseError::from(rest)))
    } else {
        Ok(formula)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::error::Error;
    use std::rc::Rc;

    use nom::Parser;

    use super::{
        AlwaysParser, AndParser, EventuallyParser, ImpliesParser, NextParser, NotParser, OrParser, PredicateParser,
    };
    use crate::automaton::Automaton;
    use crate::expressions::HybridPredicate;

    fn get_predicates() -> Rc<HashMap<String, Rc<HybridPredicate<Automaton<i32>, i32>>>> {
        let elements = [("p1".to_string(), HybridPredicate::new(None, 1, Automaton::default()))];
        let predicates = elements
            .into_iter()
            .map(|(name, predicate)| (name, Rc::new(predicate)))
            .collect();

        Rc::new(predicates)
    }

    #[test]
    fn parse_predicate() -> Result<(), Box<dyn Error>> {
        let predicates = get_predicates();
        let mut parser = PredicateParser::new(&predicates);
        let (rest, _) = parser.parse("p1")?;

        assert!(rest.is_empty());

        Ok(())
    }

    #[test]
    fn parse_missing_predicate() {
        let predicates = get_predicates();
        let mut parser = PredicateParser::new(&predicates);
        let result = parser.parse("p2");

        assert!(result.is_err())
    }

    #[test]
    fn parse_not() -> Result<(), Box<dyn Error>> {
        let predicates = get_predicates();
        let mut parser = NotParser::new(&predicates);

        let (rest, _) = parser.parse("! p1")?;
        assert_eq!(rest, "");

        let (rest, _) = parser.parse("not p1")?;
        assert_eq!(rest, "");

        let (rest, _) = parser.parse("not (p1)")?;
        assert_eq!(rest, "");

        Ok(())
    }

    #[test]
    fn parse_and() -> Result<(), Box<dyn Error>> {
        let predicates = get_predicates();
        let mut parser = AndParser::new(&predicates);

        let (rest, _) = parser.parse(r"(p1)/\(p1)")?;
        assert_eq!(rest, "");

        let (rest, _) = parser.parse("p1 and p1")?;
        assert_eq!(rest, "");

        let (rest, _) = parser.parse(r"p1 and (p1 /\ p1)")?;
        assert_eq!(rest, "");

        Ok(())
    }

    #[test]
    fn parse_or() -> Result<(), Box<dyn Error>> {
        let predicates = get_predicates();
        let mut parser = OrParser::new(&predicates);

        let (rest, _) = parser.parse(r"(p1) \/ (p1)")?;
        assert_eq!(rest, "");

        let (rest, _) = parser.parse("p1 or p1")?;
        assert_eq!(rest, "");

        let (rest, _) = parser.parse(r"(p1 or p1) \/ p1")?;
        assert_eq!(rest, "");

        Ok(())
    }

    #[test]
    fn parse_implies() -> Result<(), Box<dyn Error>> {
        let predicates = get_predicates();
        let mut parser = ImpliesParser::new(&predicates);

        let (rest, _) = parser.parse("(p1) -> (p1)")?;
        assert_eq!(rest, "");

        let (rest, _) = parser.parse("p1 implies p1")?;
        assert_eq!(rest, "");

        Ok(())
    }

    #[test]
    fn parse_next() -> Result<(), Box<dyn Error>> {
        let predicates = get_predicates();
        let mut parser = NextParser::new(&predicates);

        let (rest, _) = parser.parse("X p1")?;
        assert_eq!(rest, "");

        let (rest, _) = parser.parse("() (p1)")?;
        assert_eq!(rest, "");

        let (rest, _) = parser.parse("next (p1)")?;
        assert_eq!(rest, "");

        Ok(())
    }

    #[test]
    fn parse_always() -> Result<(), Box<dyn Error>> {
        let predicates = get_predicates();
        let mut parser = AlwaysParser::new(&predicates);

        let (rest, _) = parser.parse("always p1")?;
        assert_eq!(rest, "");

        let (rest, _) = parser.parse("[]{0,10} p1")?;
        assert_eq!(rest, "");

        let (rest, _) = parser.parse("G{1,2} (p1)")?;
        assert_eq!(rest, "");

        Ok(())
    }

    #[test]
    fn parse_eventually() -> Result<(), Box<dyn Error>> {
        let predicates = get_predicates();
        let mut parser = EventuallyParser::new(&predicates);

        let (rest, _) = parser.parse("eventually p1")?;
        assert_eq!(rest, "");

        let (rest, _) = parser.parse("<>{0,10} p1")?;
        assert_eq!(rest, "");

        let (rest, _) = parser.parse("F{1,2} (p1)")?;
        assert_eq!(rest, "");

        Ok(())
    }
}
