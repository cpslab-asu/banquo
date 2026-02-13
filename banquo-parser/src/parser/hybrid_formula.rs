//! Parser for hybrid formulas: formulas over named hybrid predicates (mode + continuous state).

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
use crate::Formula;
use crate::Trace;
use banquo_hybrid_distance::{HybridDistance, HybridPredicate, HybridState};

/// Continuous state type for hybrid formula evaluation (variable names → values).
pub type HybridVars = HashMap<String, f64>;

pub struct ParsedFormula<'a, L> {
    formula: Box<dyn Formula<HybridState<HybridVars, L>, Metric = HybridDistance, Error = ParsedFormulaError> + 'a>,
}

impl<'a, L> ParsedFormula<'a, L> {
    fn new<F, E>(formula: F) -> Self
    where
        F: Formula<HybridState<HybridVars, L>, Metric = HybridDistance, Error = E> + 'a,
        E: Error + 'static,
    {
        Self {
            formula: Box::new(FormulaWrapper::wrap(formula)),
        }
    }
}

impl<'a, L> Formula<HybridState<HybridVars, L>> for ParsedFormula<'a, L> {
    type Metric = HybridDistance;
    type Error = ParsedFormulaError;

    #[inline]
    fn evaluate(&self, trace: &Trace<HybridState<HybridVars, L>>) -> Result<Trace<HybridDistance>, Self::Error> {
        self.formula.evaluate(trace)
    }
}

pub struct PredicateMap<'a, L>(HashMap<Cow<'static, str>, Rc<HybridPredicate<'a, L>>>)
where
    L: Copy + Ord + Hash;

impl<'a, L> PredicateMap<'a, L>
where
    L: Copy + Ord + Hash,
{
    fn get(&self, name: &str) -> Option<Rc<HybridPredicate<'a, L>>> {
        self.0.get(name).cloned()
    }
}

mod predicate_map {
    use super::{Cow, Hash, HashMap, HybridPredicate};

    pub trait IntoPredicateMapSealed {}

    impl<'a, T, L> IntoPredicateMapSealed for HashMap<T, HybridPredicate<'a, L>>
    where
        T: Into<Cow<'static, str>>,
        L: Copy + Ord + Hash,
    {
    }
}

pub trait IntoPredicateMap<'a, L>: predicate_map::IntoPredicateMapSealed
where
    L: Copy + Ord + Hash,
{
    fn into_predicate_map(self) -> PredicateMap<'a, L>;
}

impl<'a, T, L> IntoPredicateMap<'a, L> for HashMap<T, HybridPredicate<'a, L>>
where
    T: Into<Cow<'static, str>>,
    L: Copy + Ord + Hash,
{
    fn into_predicate_map(self) -> PredicateMap<'a, L> {
        let predicates = self
            .into_iter()
            .map(|(name, predicate)| (name.into(), Rc::new(predicate)))
            .collect();

        PredicateMap(predicates)
    }
}

struct PredicateParser<'a, L>(Rc<PredicateMap<'a, L>>)
where
    L: Copy + Ord + Hash;

impl<'i, 'a, L> Parser<&'i str, Rc<HybridPredicate<'a, L>>, NomError<&'i str>> for PredicateParser<'a, L>
where
    L: Copy + Ord + Hash,
{
    fn parse(&mut self, input: &'i str) -> nom::IResult<&'i str, Rc<HybridPredicate<'a, L>>> {
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

struct SubformulaParser<'a, L>(Rc<PredicateMap<'a, L>>)
where
    L: Copy + Ord + Hash;

impl<'i, 'a, L> Parser<&'i str, ParsedFormula<'a, L>, NomError<&'i str>> for SubformulaParser<'a, L>
where
    L: Copy + Ord + Hash + 'a,
    'a: 'i,
{
    fn parse(&mut self, input: &'i str) -> nom::IResult<&'i str, ParsedFormula<'a, L>> {
        let inner = delimited(space0, HybridFormulaParser(self.0.clone()), space0);
        let mut parser = delimited(tag("("), inner, tag(")"));

        parser.parse(input)
    }
}

struct LoperandParser<'a, L>(Rc<PredicateMap<'a, L>>)
where
    L: Copy + Ord + Hash;

impl<'i, 'a, L> Parser<&'i str, ParsedFormula<'a, L>, NomError<&'i str>> for LoperandParser<'a, L>
where
    L: Copy + Ord + Hash + 'a,
    'a: 'i,
{
    fn parse(&mut self, input: &'i str) -> nom::IResult<&'i str, ParsedFormula<'a, L>> {
        let mut p1 = terminated(PredicateParser(self.0.clone()), space1);
        let mut p2 = terminated(SubformulaParser(self.0.clone()), space0);

        match p1.parse(input) {
            Ok((rest, predicate)) => Ok((rest, ParsedFormula::new(predicate))),
            Err(_) => p2.parse(input),
        }
    }
}

struct RoperandParser<'a, L>(Rc<PredicateMap<'a, L>>)
where
    L: Copy + Ord + Hash;

impl<'i, 'a, L> Parser<&'i str, ParsedFormula<'a, L>, NomError<&'i str>> for RoperandParser<'a, L>
where
    L: Copy + Ord + Hash + 'a,
    'a: 'i,
{
    fn parse(&mut self, input: &'i str) -> nom::IResult<&'i str, ParsedFormula<'a, L>> {
        let mut p1 = preceded(space1, PredicateParser(self.0.clone()));
        let mut p2 = preceded(space0, SubformulaParser(self.0.clone()));

        match p1.parse(input) {
            Ok((rest, predicate)) => Ok((rest, ParsedFormula::new(predicate))),
            Err(_) => p2.parse(input),
        }
    }
}

struct NotParser<'a, L>(Rc<PredicateMap<'a, L>>)
where
    L: Copy + Ord + Hash;

impl<'i, 'a, L> Parser<&'i str, ParsedFormula<'a, L>, NomError<&'i str>> for NotParser<'a, L>
where
    L: Copy + Ord + Hash + 'a,
    'a: 'i,
{
    fn parse(&mut self, input: &'i str) -> nom::IResult<&'i str, ParsedFormula<'a, L>> {
        let mut parser = operators::not(RoperandParser(self.0.clone()));
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedFormula::new(formula)))
    }
}

struct AndParser<'a, L>(Rc<PredicateMap<'a, L>>)
where
    L: Copy + Ord + Hash;

impl<'i, 'a, L> Parser<&'i str, ParsedFormula<'a, L>, NomError<&'i str>> for AndParser<'a, L>
where
    L: Copy + Ord + Hash + 'a,
    'a: 'i,
{
    fn parse(&mut self, input: &'i str) -> nom::IResult<&'i str, ParsedFormula<'a, L>> {
        let lop_parser = LoperandParser(self.0.clone());
        let rop_parser = RoperandParser(self.0.clone());

        let mut parser = operators::and(lop_parser, rop_parser);
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedFormula::new(formula)))
    }
}

struct OrParser<'a, L>(Rc<PredicateMap<'a, L>>)
where
    L: Copy + Ord + Hash;

impl<'i, 'a, L> Parser<&'i str, ParsedFormula<'a, L>, NomError<&'i str>> for OrParser<'a, L>
where
    L: Copy + Ord + Hash + 'a,
    'a: 'i,
{
    fn parse(&mut self, input: &'i str) -> nom::IResult<&'i str, ParsedFormula<'a, L>> {
        let lop_parser = LoperandParser(self.0.clone());
        let rop_parser = RoperandParser(self.0.clone());

        let mut parser = operators::or(lop_parser, rop_parser);
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedFormula::new(formula)))
    }
}

struct ImpliesParser<'a, L>(Rc<PredicateMap<'a, L>>)
where
    L: Copy + Ord + Hash;

impl<'i, 'a, L> Parser<&'i str, ParsedFormula<'a, L>, NomError<&'i str>> for ImpliesParser<'a, L>
where
    L: Copy + Ord + Hash + 'a,
    'a: 'i,
{
    fn parse(&mut self, input: &'i str) -> nom::IResult<&'i str, ParsedFormula<'a, L>> {
        let lop_parser = LoperandParser(self.0.clone());
        let rop_parser = RoperandParser(self.0.clone());

        let mut parser = operators::implies(lop_parser, rop_parser);
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedFormula::new(formula)))
    }
}

struct NextParser<'a, L>(Rc<PredicateMap<'a, L>>)
where
    L: Copy + Ord + Hash;

impl<'i, 'a, L> Parser<&'i str, ParsedFormula<'a, L>, NomError<&'i str>> for NextParser<'a, L>
where
    L: Copy + Ord + Hash + 'a,
    'a: 'i,
{
    fn parse(&mut self, input: &'i str) -> nom::IResult<&'i str, ParsedFormula<'a, L>> {
        let mut parser = operators::next(RoperandParser(self.0.clone()));
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedFormula::new(formula)))
    }
}

struct AlwaysParser<'a, L>(Rc<PredicateMap<'a, L>>)
where
    L: Copy + Ord + Hash;

impl<'i, 'a, L> Parser<&'i str, ParsedFormula<'a, L>, NomError<&'i str>> for AlwaysParser<'a, L>
where
    L: Copy + Ord + Hash + 'a,
    'a: 'i,
{
    fn parse(&mut self, input: &'i str) -> nom::IResult<&'i str, ParsedFormula<'a, L>> {
        let mut parser = operators::always(RoperandParser(self.0.clone()));
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedFormula::new(formula)))
    }
}

struct EventuallyParser<'a, L>(Rc<PredicateMap<'a, L>>)
where
    L: Copy + Ord + Hash;

impl<'i, 'a, L> Parser<&'i str, ParsedFormula<'a, L>, NomError<&'i str>> for EventuallyParser<'a, L>
where
    L: Copy + Ord + Hash + 'a,
    'a: 'i,
{
    fn parse(&mut self, input: &'i str) -> nom::IResult<&'i str, ParsedFormula<'a, L>> {
        let mut parser = operators::eventually(RoperandParser(self.0.clone()));
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedFormula::new(formula)))
    }
}

struct HybridFormulaParser<'a, L>(Rc<PredicateMap<'a, L>>)
where
    L: Copy + Ord + Hash;

impl<'i, 'a, L> Parser<&'i str, ParsedFormula<'a, L>, NomError<&'i str>> for HybridFormulaParser<'a, L>
where
    L: Copy + Ord + Hash + 'a,
    'a: 'i,
{
    fn parse(&mut self, input: &'i str) -> nom::IResult<&'i str, ParsedFormula<'a, L>> {
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

pub fn parse_hybrid_formula<'a, P, L>(
    input: &'a str,
    predicates: P,
) -> Result<ParsedFormula<'a, L>, Box<dyn Error + 'a>>
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

    use banquo_core::predicate::{Predicate, Term};
    use nom::Parser;

    use super::{IntoPredicateMap, PredicateMap, PredicateParser};
    use banquo_hybrid_distance::automaton::{Automaton, Guard};
    use banquo_hybrid_distance::HybridPredicate;

    /// Build a minimal automaton and predicate map for mode label 1.
    fn make_predicate_map(automaton: &Automaton<i32>) -> PredicateMap<'_, i32> {
        let p1 = HybridPredicate::new(None, [1], automaton);
        let predicates = HashMap::from([("p1", p1)]);
        predicates.into_predicate_map()
    }

    fn assert_parses(formulas: &[&'static str], map: Rc<PredicateMap<'_, i32>>) {
        for formula in formulas {
            let mut parser = super::HybridFormulaParser(map.clone());
            let (rest, _) = parser.parse(formula).expect("parse");
            assert!(rest.is_empty(), "remaining input for {:?}: {:?}", formula, rest);
        }
    }

    #[test]
    fn parse_predicate() -> Result<(), Box<dyn Error>> {
        let guard = Guard::from(Predicate::from([Term::Constant(0.0)]));
        let automaton = Automaton::from([(1i32, 1i32, guard)]);
        let map = make_predicate_map(&automaton);
        let mut parser = PredicateParser(Rc::new(map));
        let (rest, _) = parser.parse("p1")?;
        assert!(rest.is_empty());
        Ok(())
    }

    #[test]
    fn parse_missing_predicate() {
        let guard = Guard::from(Predicate::from([Term::Constant(0.0)]));
        let automaton = Automaton::from([(1i32, 1i32, guard)]);
        let map = make_predicate_map(&automaton);
        let mut parser = PredicateParser(Rc::new(map));
        let result = parser.parse("p2");
        assert!(result.is_err());
    }

    #[test]
    fn parse_not() {
        let guard = Guard::from(Predicate::from([Term::Constant(0.0)]));
        let automaton = Automaton::from([(1i32, 1i32, guard)]);
        let map = Rc::new(make_predicate_map(&automaton));
        assert_parses(&["! p1", "not p1", "not (p1)"], map);
    }

    #[test]
    fn parse_and() {
        let guard = Guard::from(Predicate::from([Term::Constant(0.0)]));
        let automaton = Automaton::from([(1i32, 1i32, guard)]);
        let map = Rc::new(make_predicate_map(&automaton));
        assert_parses(&[r"(p1)/\(p1)", "p1 and p1", r"p1 and (p1 /\ p1)"], map);
    }

    #[test]
    fn parse_or() {
        let guard = Guard::from(Predicate::from([Term::Constant(0.0)]));
        let automaton = Automaton::from([(1i32, 1i32, guard)]);
        let map = Rc::new(make_predicate_map(&automaton));
        assert_parses(&[r"(p1) \/ (p1)", "p1 or p1", r"(p1 or p1) \/ p1"], map);
    }

    #[test]
    fn parse_implies() {
        let guard = Guard::from(Predicate::from([Term::Constant(0.0)]));
        let automaton = Automaton::from([(1i32, 1i32, guard)]);
        let map = Rc::new(make_predicate_map(&automaton));
        assert_parses(&["(p1) -> (p1)", "p1 implies p1"], map);
    }

    #[test]
    fn parse_next() {
        let guard = Guard::from(Predicate::from([Term::Constant(0.0)]));
        let automaton = Automaton::from([(1i32, 1i32, guard)]);
        let map = Rc::new(make_predicate_map(&automaton));
        assert_parses(&["X p1", "() (p1)", "next (p1)"], map);
    }

    #[test]
    fn parse_always() {
        let guard = Guard::from(Predicate::from([Term::Constant(0.0)]));
        let automaton = Automaton::from([(1i32, 1i32, guard)]);
        let map = Rc::new(make_predicate_map(&automaton));
        assert_parses(&["always p1", "[]{0,10} p1", "G{1,2} (p1)"], map);
    }

    #[test]
    fn parse_eventually() {
        let guard = Guard::from(Predicate::from([Term::Constant(0.0)]));
        let automaton = Automaton::from([(1i32, 1i32, guard)]);
        let map = Rc::new(make_predicate_map(&automaton));
        assert_parses(&["eventually p1", "<>{0,10} p1", "F{1,2} (p1)"], map);
    }
}
