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
use crate::expressions::{HybridPredicate, HybridState, VarMap};
use crate::formulas::Formula;
use crate::metric::HybridDistance;
use crate::trace::Trace;

pub struct ParsedFormula<S, L> {
    formula: Box<dyn Formula<HybridState<S, L>, Metric = HybridDistance, Error = ParsedFormulaError>>,
}

impl<S, L> ParsedFormula<S, L> {
    fn new<F, E>(formula: F) -> Self
    where
        F: Formula<HybridState<S, L>, Metric = HybridDistance, Error = E>,
        E: Error + 'static,
    {
        Self {
            formula: Box::new(FormulaWrapper::wrap(formula)),
        }
    }
}

impl<S, L> Formula<HybridState<S, L>> for ParsedFormula<S, L> {
    type Metric = HybridDistance;
    type Error = ParsedFormulaError;

    #[inline]
    fn evaluate_trace(&self, trace: &Trace<HybridState<S, L>>) -> Result<Trace<HybridDistance>, Self::Error> {
        self.formula.evaluate_trace(trace)
    }
}

type HybridPredicateMap<'a, L> = HashMap<&'a str, Rc<HybridPredicate<'a, L>>>;

struct PredicateParser<'a, L>(Rc<HybridPredicateMap<'a, L>>);

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

struct SubformulaParser<'a, L>(Rc<HybridPredicateMap<'a, L>>);

impl<'a, S, L> Parser<&'a str, ParsedFormula<S, L>, NomError<&'a str>> for SubformulaParser<'a, L>
where
    S: VarMap,
    L: Copy + Ord + Hash,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedFormula<S, L>> {
        let inner = delimited(space0, HybridFormulaParser(self.0.clone()), space0);
        let mut parser = delimited(tag("("), inner, tag(")"));

        parser.parse(input)
    }
}

struct LoperandParser<'a, L>(Rc<HybridPredicateMap<'a, L>>);

impl<'a, S, L> Parser<&'a str, ParsedFormula<S, L>, NomError<&'a str>> for LoperandParser<'a, L>
where
    S: VarMap,
    L: Copy + Ord + Hash,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedFormula<S, L>> {
        let p1 = terminated(PredicateParser(self.0.clone()), space1);
        let p2 = terminated(SubformulaParser(self.0.clone()), space0);

        match p1.parse(input) {
            Ok((rest, predicate)) => Ok((rest, ParsedFormula::new(predicate))),
            Err(_) => p2.parse(input),
        }
    }
}

struct RoperandParser<'a, L>(Rc<HybridPredicateMap<'a, L>>);

impl<'a, S, L> Parser<&'a str, ParsedFormula<S, L>, NomError<&'a str>> for RoperandParser<'a, L>
where
    S: VarMap,
    L: Copy + Ord + Hash,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedFormula<S, L>, NomError<&'a str>> {
        let p1 = preceded(space1, PredicateParser(self.0.clone()));
        let p2 = preceded(space0, SubformulaParser(self.0.clone()));

        match p1.parse(input) {
            Ok((rest, predicate)) => Ok((rest, ParsedFormula::new(predicate))),
            Err(_) => p2.parse(input),
        }
    }
}

struct NotParser<'a, L>(Rc<HybridPredicateMap<'a, L>>);

impl<'a, S, L> Parser<&'a str, ParsedFormula<S, L>, NomError<&'a str>> for NotParser<'a, L>
where
    S: VarMap,
    L: Copy + Ord + Hash,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedFormula<S, L>> {
        let mut parser = operators::not(RoperandParser(self.0.clone()));
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedFormula::new(formula)))
    }
}

struct AndParser<'a, L>(Rc<HybridPredicateMap<'a, L>>);

impl<'a, S, L> Parser<&'a str, ParsedFormula<S, L>, NomError<&'a str>> for AndParser<'a, L>
where
    S: VarMap,
    L: Copy + Ord + Hash,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedFormula<S, L>> {
        let lop_parser = LoperandParser(self.0.clone());
        let rop_parser = RoperandParser(self.0.clone());

        let mut parser = operators::and(lop_parser, rop_parser);
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedFormula::new(formula)))
    }
}

struct OrParser<'a, L>(Rc<HybridPredicateMap<'a, L>>);

impl<'a, S, L> Parser<&'a str, ParsedFormula<S, L>, NomError<&'a str>> for OrParser<'a, L>
where
    S: VarMap,
    L: Copy + Ord + Hash,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedFormula<S, L>> {
        let lop_parser = LoperandParser(self.0.clone());
        let rop_parser = RoperandParser(self.0.clone());

        let mut parser = operators::or(lop_parser, rop_parser);
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedFormula::new(formula)))
    }
}

struct ImpliesParser<'a, L>(Rc<HybridPredicateMap<'a, L>>);

impl<'a, S, L> Parser<&'a str, ParsedFormula<S, L>, NomError<&'a str>> for ImpliesParser<'a, L>
where
    S: VarMap,
    L: Copy + Ord + Hash,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedFormula<S, L>> {
        let lop_parser = LoperandParser(self.0.clone());
        let rop_parser = RoperandParser(self.0.clone());

        let mut parser = operators::implies(lop_parser, rop_parser);
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedFormula::new(formula)))
    }
}

struct NextParser<'a, L>(Rc<HybridPredicateMap<'a, L>>);

impl<'a, S, L> Parser<&'a str, ParsedFormula<S, L>, NomError<&'a str>> for NextParser<'a, L>
where
    S: VarMap,
    L: Copy + Ord + Hash,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedFormula<S, L>> {
        let mut parser = operators::next(RoperandParser(self.0.clone()));
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedFormula::new(formula)))
    }
}

struct AlwaysParser<'a, L>(Rc<HybridPredicateMap<'a, L>>);

impl<'a, S, L> Parser<&'a str, ParsedFormula<S, L>, NomError<&'a str>> for AlwaysParser<'a, L>
where
    S: VarMap,
    L: Copy + Ord + Hash,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedFormula<S, L>> {
        let mut parser = operators::always(RoperandParser(self.0.clone()));
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedFormula::new(formula)))
    }
}

struct EventuallyParser<'a, L>(Rc<HybridPredicateMap<'a, L>>);

impl<'a, S, L> Parser<&'a str, ParsedFormula<S, L>, NomError<&'a str>> for EventuallyParser<'a, L>
where
    S: VarMap,
    L: Copy + Ord + Hash,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedFormula<S, L>> {
        let mut parser = operators::eventually(RoperandParser(self.0.clone()));
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedFormula::new(formula)))
    }
}

struct HybridFormulaParser<'a, L>(Rc<HybridPredicateMap<'a, L>>);

impl<'a, S, L> Parser<&'a str, ParsedFormula<S, L>, NomError<&'a str>> for HybridFormulaParser<'a, L>
where
    S: VarMap,
    L: Copy + Ord + Hash,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedFormula<S, L>, NomError<&'a str>> {
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

type ParserResult<'a, S, L> = Result<ParsedFormula<S, L>, Box<dyn Error + 'a>>;
type Predicates<'a, L> = HashMap<&'a str, HybridPredicate<'a, L>>;

pub fn parse_hybrid_formula<'a, S, L>(input: &'a str, predicates: Predicates<'a, L>) -> ParserResult<'a, S, L>
where
    S: VarMap,
    L: Copy + Ord + Hash,
{
    let predicates = predicates
        .into_iter()
        .map(|(name, predicate)| (name, Rc::new(predicate)))
        .collect();

    let mut parser = HybridFormulaParser(Rc::new(predicates));
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
        AlwaysParser, AndParser, EventuallyParser, ImpliesParser, NextParser, NotParser, OrParser, ParsedFormula,
        PredicateParser,
    };
    use crate::automaton::Automaton;
    use crate::expressions::HybridPredicate;

    type VarMap = HashMap<&'static str, f64>;

    fn get_predicates() -> Rc<HashMap<&'static str, Rc<HybridPredicate<'static, i32>>>> {
        let automaton = Box::new(Automaton::default());
        let automaton_ref = Box::leak(automaton);
        let predicate = HybridPredicate::new(None, 1, automaton_ref);

        let mut predicates = HashMap::with_capacity(1);
        predicates.insert("p1", Rc::new(predicate));

        Rc::new(predicates)
    }

    fn run_parser<'a, P>(parser: &mut P, formula: &'a str) -> nom::IResult<&'a str, ParsedFormula<VarMap, i32>>
    where
        P: Parser<&'a str, ParsedFormula<HashMap<&'a str, f64>, i32>, nom::error::Error<&'a str>>,
    {
        parser.parse(formula)
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

    #[test]
    fn parse_not() -> Result<(), Box<dyn Error>> {
        let predicates = get_predicates();
        let mut parser = NotParser(predicates);

        let (rest, _) = run_parser(&mut parser, "! p1")?;
        assert_eq!(rest, "");

        let (rest, _) = run_parser(&mut parser, "not p1")?;
        assert_eq!(rest, "");

        let (rest, _) = run_parser(&mut parser, "not (p1)")?;
        assert_eq!(rest, "");

        Ok(())
    }

    #[test]
    fn parse_and() -> Result<(), Box<dyn Error>> {
        let predicates = get_predicates();
        let mut parser = AndParser(predicates);

        let (rest, _) = run_parser(&mut parser, r"(p1)/\(p1)")?;
        assert_eq!(rest, "");

        let (rest, _) = run_parser(&mut parser, "p1 and p1")?;
        assert_eq!(rest, "");

        let (rest, _) = run_parser(&mut parser, r"p1 and (p1 /\ p1)")?;
        assert_eq!(rest, "");

        Ok(())
    }

    #[test]
    fn parse_or() -> Result<(), Box<dyn Error>> {
        let predicates = get_predicates();
        let mut parser = OrParser(predicates);

        let (rest, _) = run_parser(&mut parser, r"(p1) \/ (p1)")?;
        assert_eq!(rest, "");

        let (rest, _) = run_parser(&mut parser, "p1 or p1")?;
        assert_eq!(rest, "");

        let (rest, _) = run_parser(&mut parser, r"(p1 or p1) \/ p1")?;
        assert_eq!(rest, "");

        Ok(())
    }

    #[test]
    fn parse_implies() -> Result<(), Box<dyn Error>> {
        let predicates = get_predicates();
        let mut parser = ImpliesParser(predicates);

        let (rest, _) = run_parser(&mut parser, "(p1) -> (p1)")?;
        assert_eq!(rest, "");

        let (rest, _) = run_parser(&mut parser, "p1 implies p1")?;
        assert_eq!(rest, "");

        Ok(())
    }

    #[test]
    fn parse_next() -> Result<(), Box<dyn Error>> {
        let predicates = get_predicates();
        let mut parser = NextParser(predicates);

        let (rest, _) = run_parser(&mut parser, "X p1")?;
        assert_eq!(rest, "");

        let (rest, _) = run_parser(&mut parser, "() (p1)")?;
        assert_eq!(rest, "");

        let (rest, _) = run_parser(&mut parser, "next (p1)")?;
        assert_eq!(rest, "");

        Ok(())
    }

    #[test]
    fn parse_always() -> Result<(), Box<dyn Error>> {
        let predicates = get_predicates();
        let mut parser = AlwaysParser(predicates);

        let (rest, _) = run_parser(&mut parser, "always p1")?;
        assert_eq!(rest, "");

        let (rest, _) = run_parser(&mut parser, "[]{0,10} p1")?;
        assert_eq!(rest, "");

        let (rest, _) = run_parser(&mut parser, "G{1,2} (p1)")?;
        assert_eq!(rest, "");

        Ok(())
    }

    #[test]
    fn parse_eventually() -> Result<(), Box<dyn Error>> {
        let predicates = get_predicates();
        let mut parser = EventuallyParser(predicates);

        let (rest, _) = run_parser(&mut parser, "eventually p1")?;
        assert_eq!(rest, "");

        let (rest, _) = run_parser(&mut parser, "<>{0,10} p1")?;
        assert_eq!(rest, "");

        let (rest, _) = run_parser(&mut parser, "F{1,2} (p1)")?;
        assert_eq!(rest, "");

        Ok(())
    }
}
