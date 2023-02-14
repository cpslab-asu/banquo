use std::borrow::Borrow;
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
use crate::expressions::HybridPredicate;
use crate::formulas::{Formula, HybridDistance};
use crate::trace::Trace;

pub struct ParsedFormula<S> {
    formula: Box<dyn Formula<S, Metric = HybridDistance, Error = ParsedFormulaError>>,
}

impl<S> ParsedFormula<S> {
    fn new<F, E>(formula: F) -> Self
    where
        F: Formula<S, Metric = HybridDistance, Error = E>,
        E: Error + 'static,
    {
        Self {
            formula: Box::new(FormulaWrapper::wrap(formula)),
        }
    }
}

impl<S> Formula<S> for ParsedFormula<S> {
    type Metric = HybridDistance;
    type Error = ParsedFormulaError;

    #[inline]
    fn evaluate_trace(&self, trace: &Trace<S>) -> Result<Trace<HybridDistance>, Self::Error> {
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

type HybridState<K, L> = (HashMap<K, f64>, L);

impl<'a, K, L> Parser<&'a str, ParsedFormula<HybridState<K, L>>, NomError<&'a str>> for SubformulaParser<'a, L> {
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedFormula<HybridState<K, L>>> {
        let inner = delimited(space0, HybridFormulaParser(self.0.clone()), space0);
        let mut parser = delimited(tag("("), inner, tag(")"));

        parser.parse(input)
    }
}

struct LoperandParser<'a, L>(Rc<HybridPredicateMap<'a, L>>);

impl<'a, K, L> Parser<&'a str, ParsedFormula<HybridState<K, L>>, NomError<&'a str>> for LoperandParser<'a, L>
where
    K: Eq + Hash + Borrow<str>,
    L: Copy + Ord + Hash,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedFormula<HybridState<K, L>>> {
        let p1 = terminated(PredicateParser(self.0.clone()), space1);
        let p2 = terminated(SubformulaParser(self.0.clone()), space0);

        match p1.parse(input) {
            Ok((rest, predicate)) => Ok((rest, ParsedFormula::new(predicate))),
            Err(_) => p2.parse(input),
        }
    }
}

struct RoperandParser<'a, L>(Rc<HybridPredicateMap<'a, L>>);

impl<'a, K, L> Parser<&'a str, ParsedFormula<HybridState<K, L>>, NomError<&'a str>> for RoperandParser<'a, L>
where
    K: Eq + Hash + Borrow<str>,
    L: Copy + Ord + Hash,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedFormula<HybridState<K, L>>, NomError<&'a str>> {
        let p1 = preceded(space1, PredicateParser(self.0.clone()));
        let p2 = preceded(space0, SubformulaParser(self.0.clone()));

        match p1.parse(input) {
            Ok((rest, predicate)) => Ok((rest, ParsedFormula::new(predicate))),
            Err(_) => p2.parse(input),
        }
    }
}

struct NotParser<'a, L>(Rc<HybridPredicateMap<'a, L>>);

impl<'a, K, L> Parser<&'a str, ParsedFormula<HybridState<K, L>>, NomError<&'a str>> for NotParser<'a, L>
where
    K: Eq + Hash + Borrow<str>,
    L: Copy + Ord + Hash,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedFormula<HybridState<K, L>>> {
        let mut parser = operators::not(RoperandParser(self.0.clone()));
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedFormula::new(formula)))
    }
}

struct AndParser<'a, L>(Rc<HybridPredicateMap<'a, L>>);

impl<'a, 'b, K, L> Parser<&'a str, ParsedFormula<HybridState<K, L>>, NomError<&'a str>> for AndParser<'a, L>
where
    K: Eq + Hash + Borrow<str>,
    L: Copy + Ord + Hash,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedFormula<HybridState<K, L>>> {
        let lop_parser = LoperandParser(self.0.clone());
        let rop_parser = RoperandParser(self.0.clone());

        let mut parser = operators::and(lop_parser, rop_parser);
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedFormula::new(formula)))
    }
}

struct OrParser<'a, L>(Rc<HybridPredicateMap<'a, L>>);

impl<'a, K, L> Parser<&'a str, ParsedFormula<HybridState<K, L>>, NomError<&'a str>> for OrParser<'a, L>
where
    K: Eq + Hash + Borrow<str>,
    L: Copy + Ord + Hash,
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedFormula<HybridState<K, L>>> {
        let lop_parser = LoperandParser(self.0.clone());
        let rop_parser = RoperandParser(self.0.clone());

        let mut parser = operators::or(lop_parser, rop_parser);
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedFormula::new(formula)))
    }
}

struct ImpliesParser<'a, L>(Rc<HybridPredicateMap<'a, L>>);

impl<'a, K, L> Parser<&'a str, ParsedFormula<HybridState<K, L>>, NomError<&'a str>> for ImpliesParser<'a, L> {
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedFormula<HybridState<K, L>>> {
        let lop_parser = LoperandParser(self.0.clone());
        let rop_parser = RoperandParser(self.0.clone());

        let mut parser = operators::implies(lop_parser, rop_parser);
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedFormula::new(formula)))
    }
}

struct NextParser<'a, L> {
    predicates: Rc<HybridPredicateMap<'a, L>>,
}

impl<'a, K, L> Parser<&'a str, ParsedFormula<HybridState<K, L>>, NomError<&'a str>> for NextParser<'a, L> {
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedFormula<HybridState<K, L>>> {
        let mut parser = operators::next(RoperandParser(self.predicates.clone()));
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedFormula::new(formula)))
    }
}

struct AlwaysParser<'a, L>(Rc<HybridPredicateMap<'a, L>>);

impl<'a, K, L> Parser<&'a str, ParsedFormula<HybridState<K, L>>, NomError<&'a str>> for AlwaysParser<'a, L> {
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedFormula<HybridState<K, L>>> {
        let mut parser = operators::always(RoperandParser(self.0.clone()));
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedFormula::new(formula)))
    }
}

struct EventuallyParser<'a, L>(Rc<HybridPredicateMap<'a, L>>);

impl<'a, K, L> Parser<&'a str, ParsedFormula<HybridState<K, L>>, NomError<&'a str>> for EventuallyParser<'a, L> {
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedFormula<HybridState<K, L>>> {
        let mut parser = operators::eventually(RoperandParser(self.0.clone()));
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedFormula::new(formula)))
    }
}

struct HybridFormulaParser<'a, L>(Rc<HybridPredicateMap<'a, L>>);

impl<'a, K, L> Parser<&'a str, ParsedFormula<HybridState<K, L>>, NomError<&'a str>> for HybridFormulaParser<'a, L> {
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedFormula<HybridState<K, L>>, NomError<&'a str>> {
        let mut parser = alt((
            NotParser::new(self.0.clone()),
            NextParser::new(self.0.clone()),
            AlwaysParser::new(self.0.clone()),
            EventuallyParser::new(self.0.clone()),
            AndParser::new(self.0.clone()),
            OrParser::new(self.0.clone()),
            ImpliesParser::new(self.0.clone()),
            SubformulaParser::new(self.0.clone()),
            map(PredicateParser::new(self.0.clone()), ParsedFormula::new),
        ));

        parser.parse(input)
    }
}

pub fn parse_hybrid_formula<'a, K, L>(
    input: &'a str,
    predicates: HybridPredicateMap<'a, L>,
) -> Result<ParsedFormula<HybridState<K, L>>, Box<dyn Error + 'a>> {
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
