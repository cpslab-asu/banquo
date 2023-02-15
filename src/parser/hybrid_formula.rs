use std::borrow::Cow;
use std::collections::HashMap;
use std::error::Error;
use std::hash::Hash;
use std::rc::Rc;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{space0, space1};
use nom::combinator::{map, map_res};
use nom::error::Error as NomError;
use nom::sequence::{delimited, preceded, terminated};
use nom::Parser;

use super::common::{var_name, FormulaWrapper};
use super::errors::{IncompleteParseError, MissingPredicateError, ParsedFormulaError};
use super::operators;
use crate::expressions::{HybridPredicate, HybridState};
use crate::formulas::Formula;
use crate::metric::HybridDistance;
use crate::trace::Trace;

pub struct ParsedFormula<'a, L> {
    formula: Box<dyn Formula<HybridState<L>, Metric = HybridDistance, Error = ParsedFormulaError> + 'a>,
}

impl<'a, L> ParsedFormula<'a, L> {
    fn new<F, E>(formula: F) -> Self
    where
        L: 'a,
        F: Formula<HybridState<L>, Metric = HybridDistance, Error = E> + 'a,
        E: Error + 'static,
    {
        Self {
            formula: Box::new(FormulaWrapper::wrap(formula)),
        }
    }
}

impl<'a, L> Formula<HybridState<L>> for ParsedFormula<'a, L> {
    type Metric = HybridDistance;
    type Error = ParsedFormulaError;

    #[inline]
    fn evaluate_trace(&self, trace: &Trace<HybridState<L>>) -> Result<Trace<HybridDistance>, Self::Error> {
        self.formula.evaluate_trace(trace)
    }
}

mod sealed {
    use std::borrow::Cow;
    use std::collections::HashMap;

    use super::HybridPredicate;

    pub trait IntoPredicateMapSealed {}

    impl<'a, T, L> IntoPredicateMapSealed for HashMap<T, HybridPredicate<'a, L>> where T: Into<Cow<'static, str>> {}
}

pub struct PredicateMap<'a, L>(HashMap<Cow<'static, str>, Rc<HybridPredicate<'a, L>>>);

pub trait IntoPredicateMap<'a, L>: sealed::IntoPredicateMapSealed {
    fn into_predicate_map(self) -> PredicateMap<'a, L>;
}

impl<'a, T, L> IntoPredicateMap<'a, L> for HashMap<T, HybridPredicate<'a, L>>
where
    T: Into<Cow<'static, str>>,
{
    fn into_predicate_map(self) -> PredicateMap<'a, L> {
        let predicates = self
            .into_iter()
            .map(|(name, predicate)| (name.into(), Rc::new(predicate)))
            .collect();

        PredicateMap(predicates)
    }
}

impl<'a, L> PredicateMap<'a, L> {
    fn get(&self, name: &str) -> Option<Rc<HybridPredicate<'a, L>>> {
        self.0.get(name).cloned()
    }
}

struct PredicateParser<'a, L>(Rc<PredicateMap<'a, L>>);

impl<'a, L> Parser<&'a str, Rc<HybridPredicate<'a, L>>, NomError<&'a str>> for PredicateParser<'a, L> {
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, Rc<HybridPredicate<'a, L>>> {
        let get_predicate = |name: String| {
            self.0
                .get(name.as_str())
                .ok_or_else(move || MissingPredicateError::from(name))
        };
        let mut parser = map_res(var_name, get_predicate);
        let (rest, predicate) = parser.parse(input)?;

        Ok((rest, predicate.clone()))
    }
}

struct SubformulaParser<'a, L>(Rc<PredicateMap<'a, L>>);

impl<'a, L> Parser<&'a str, ParsedFormula<'a, L>, NomError<&'a str>> for SubformulaParser<'a, L>
where
    L: Copy + Ord + Hash + 'a,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedFormula<'a, L>> {
        let inner = delimited(space0, HybridFormulaParser(self.0.clone()), space0);
        let mut parser = delimited(tag("("), inner, tag(")"));

        parser.parse(input)
    }
}

struct LoperandParser<'a, L>(Rc<PredicateMap<'a, L>>);

impl<'a, L> Parser<&'a str, ParsedFormula<'a, L>, NomError<&'a str>> for LoperandParser<'a, L>
where
    L: Copy + Ord + Hash + 'a,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedFormula<'a, L>> {
        let mut p1 = terminated(PredicateParser(self.0.clone()), space1);
        let mut p2 = terminated(SubformulaParser(self.0.clone()), space0);

        match p1.parse(input) {
            Ok((rest, predicate)) => Ok((rest, ParsedFormula::new(predicate))),
            Err(_) => p2.parse(input),
        }
    }
}

struct RoperandParser<'a, L>(Rc<PredicateMap<'a, L>>);

impl<'a, L> Parser<&'a str, ParsedFormula<'a, L>, NomError<&'a str>> for RoperandParser<'a, L>
where
    L: Copy + Ord + Hash + 'a,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedFormula<'a, L>> {
        let mut p1 = preceded(space1, PredicateParser(self.0.clone()));
        let mut p2 = preceded(space0, SubformulaParser(self.0.clone()));

        match p1.parse(input) {
            Ok((rest, predicate)) => Ok((rest, ParsedFormula::new(predicate))),
            Err(_) => p2.parse(input),
        }
    }
}

struct NotParser<'a, L>(Rc<PredicateMap<'a, L>>);

impl<'a, L> Parser<&'a str, ParsedFormula<'a, L>, NomError<&'a str>> for NotParser<'a, L>
where
    L: Copy + Ord + Hash + 'a,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedFormula<'a, L>> {
        let mut parser = operators::not(RoperandParser(self.0.clone()));
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedFormula::new(formula)))
    }
}

struct AndParser<'a, L>(Rc<PredicateMap<'a, L>>);

impl<'a, L> Parser<&'a str, ParsedFormula<'a, L>, NomError<&'a str>> for AndParser<'a, L>
where
    L: Copy + Ord + Hash + 'a,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedFormula<'a, L>> {
        let lop_parser = LoperandParser(self.0.clone());
        let rop_parser = RoperandParser(self.0.clone());

        let mut parser = operators::and(lop_parser, rop_parser);
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedFormula::new(formula)))
    }
}

struct OrParser<'a, L>(Rc<PredicateMap<'a, L>>);

impl<'a, L> Parser<&'a str, ParsedFormula<'a, L>, NomError<&'a str>> for OrParser<'a, L>
where
    L: Copy + Ord + Hash + 'a,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedFormula<'a, L>> {
        let lop_parser = LoperandParser(self.0.clone());
        let rop_parser = RoperandParser(self.0.clone());

        let mut parser = operators::or(lop_parser, rop_parser);
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedFormula::new(formula)))
    }
}

struct ImpliesParser<'a, L>(Rc<PredicateMap<'a, L>>);

impl<'a, L> Parser<&'a str, ParsedFormula<'a, L>, NomError<&'a str>> for ImpliesParser<'a, L>
where
    L: Copy + Ord + Hash + 'a,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedFormula<'a, L>> {
        let lop_parser = LoperandParser(self.0.clone());
        let rop_parser = RoperandParser(self.0.clone());

        let mut parser = operators::implies(lop_parser, rop_parser);
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedFormula::new(formula)))
    }
}

struct NextParser<'a, L>(Rc<PredicateMap<'a, L>>);

impl<'a, L> Parser<&'a str, ParsedFormula<'a, L>, NomError<&'a str>> for NextParser<'a, L>
where
    L: Copy + Ord + Hash + 'a,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedFormula<'a, L>> {
        let mut parser = operators::next(RoperandParser(self.0.clone()));
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedFormula::new(formula)))
    }
}

struct AlwaysParser<'a, L>(Rc<PredicateMap<'a, L>>);

impl<'a, L> Parser<&'a str, ParsedFormula<'a, L>, NomError<&'a str>> for AlwaysParser<'a, L>
where
    L: Copy + Ord + Hash + 'a,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedFormula<'a, L>> {
        let mut parser = operators::always(RoperandParser(self.0.clone()));
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedFormula::new(formula)))
    }
}

struct EventuallyParser<'a, L>(Rc<PredicateMap<'a, L>>);

impl<'a, L> Parser<&'a str, ParsedFormula<'a, L>, NomError<&'a str>> for EventuallyParser<'a, L>
where
    L: Copy + Ord + Hash + 'a,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedFormula<'a, L>> {
        let mut parser = operators::eventually(RoperandParser(self.0.clone()));
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedFormula::new(formula)))
    }
}

struct HybridFormulaParser<'a, L>(Rc<PredicateMap<'a, L>>);

impl<'a, L> Parser<&'a str, ParsedFormula<'a, L>, NomError<&'a str>> for HybridFormulaParser<'a, L>
where
    L: Copy + Ord + Hash + 'a,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedFormula<'a, L>> {
        let mut parser = alt((
            NotParser(self.0.clone()),
            NextParser(self.0.clone()),
            AlwaysParser(self.0.clone()),
            EventuallyParser(self.0.clone()),
            AndParser(self.0.clone()),
            OrParser(self.0.clone()),
            ImpliesParser(self.0.clone()),
            SubformulaParser(self.0.clone()),
            map(PredicateParser(self.0.clone()), ParsedFormula::new),
        ));

        parser.parse(input)
    }
}

type ParseResult<'a, L> = Result<ParsedFormula<'a, L>, Box<dyn Error + 'a>>;

pub fn parse_hybrid_formula<'a, P, L>(input: &'a str, predicates: P) -> ParseResult<'a, L>
where
    P: IntoPredicateMap<'a, L>,
    L: Copy + Ord + Hash + 'a,
{
    let map = predicates.into_predicate_map();
    let mut parser = HybridFormulaParser(Rc::new(map));
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
        AlwaysParser, AndParser, EventuallyParser, ImpliesParser, IntoPredicateMap, NextParser, NotParser, OrParser,
        ParsedFormula, PredicateMap, PredicateParser,
    };
    use crate::automaton::Automaton;
    use crate::expressions::HybridPredicate;

    type VarMap = HashMap<&'static str, f64>;

    fn get_predicates() -> Rc<PredicateMap<'static, i32>> {
        let automaton = Box::new(Automaton::default());
        let automaton_ref = Box::leak(automaton);
        let predicate = HybridPredicate::new(None, 1, automaton_ref);

        let mut predicates = HashMap::with_capacity(1);
        predicates.insert("p1", predicate);

        Rc::new(predicates.into_predicate_map())
    }

    #[test]
    fn parse_predicate() -> Result<(), Box<dyn Error>> {
        let predicates = get_predicates();
        let mut parser = PredicateParser(predicates);
        let (rest, _) = parser.parse("p1")?;

        assert!(rest.is_empty());

        Ok(())
    }

    #[test]
    fn parse_missing_predicate() {
        let predicates = get_predicates();
        let mut parser = PredicateParser(predicates);
        let result = parser.parse("p2");

        assert!(result.is_err())
    }

    fn parser_test_case<'a, 'b, P, const N: usize>(mut parser: P, formulas: [&'a str; N]) -> Result<(), Box<dyn Error>>
    where
        P: Parser<&'a str, ParsedFormula<'b, i32>, nom::error::Error<&'a str>>,
    {
        for formula in formulas {
            let (rest, _) = parser.parse(formula)?;
            assert!(rest.is_empty());
        }

        Ok(())
    }

    #[test]
    fn parse_not() -> Result<(), Box<dyn Error>> {
        let predicates = get_predicates();
        let parser = NotParser(predicates);
        let formulas = ["! p1", "not p1", "not (p1)"];

        parser_test_case(parser, formulas)
    }

    #[test]
    fn parse_and() -> Result<(), Box<dyn Error>> {
        let predicates = get_predicates();
        let parser = AndParser(predicates);
        let formulas = [r"(p1)/\(p1)", "p1 and p1", r"p1 and (p1 /\ p1)"];

        parser_test_case(parser, formulas)
    }

    #[test]
    fn parse_or() -> Result<(), Box<dyn Error>> {
        let predicates = get_predicates();
        let parser = OrParser(predicates);
        let formulas = [r"(p1) \/ (p1)", "p1 or p1", r"(p1 or p1) \/ p1"];

        parser_test_case(parser, formulas)
    }

    #[test]
    fn parse_implies() -> Result<(), Box<dyn Error>> {
        let predicates = get_predicates();
        let mut parser = ImpliesParser(predicates);
        let formulas = ["(p1) -> (p1)", "p1 implies p1"];

        parser_test_case(parser, formulas)
    }

    #[test]
    fn parse_next() -> Result<(), Box<dyn Error>> {
        let predicates = get_predicates();
        let parser = NextParser(predicates);
        let formulas = ["X p1", "() (p1)", "next (p1)"];

        parser_test_case(parser, formulas)
    }

    #[test]
    fn parse_always() -> Result<(), Box<dyn Error>> {
        let predicates = get_predicates();
        let parser = AlwaysParser(predicates);
        let formulas = ["always p1", "[]{0,10} p1", "G{1,2} (p1)"];

        parser_test_case(parser, formulas)
    }

    #[test]
    fn parse_eventually() -> Result<(), Box<dyn Error>> {
        let predicates = get_predicates();
        let parser = EventuallyParser(predicates);
        let formulas = ["eventually p1", "<>{0,10} p1", "F{1,2} (p1)"];

        parser_test_case(parser, formulas)
    }
}
