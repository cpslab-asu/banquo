use std::collections::HashMap;
use std::error::Error;
use std::hash::Hash;
use std::rc::Rc;

use nom::Parser;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::map_res;
use nom::sequence::delimited;

use crate::expressions::HybridPredicate;
use crate::formula::HybridDistanceFormula;
use crate::trace::Trace;
use super::common::{WrappedFormula, var_name};
use super::errors::{IncompleteParseError, MissingPredicateError, ParsedFormulaError};
use super::operators;

type VariableMap = HashMap<String, f64>;
type PredicateMap<L> = HashMap<String, Rc<HybridPredicate<L>>>;

pub struct ParsedHybridFormula<L> {
    inner: Box<dyn HybridDistanceFormula<HashMap<String, f64>, L, Error = ParsedFormulaError> + 'static>
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

struct PredicateParser<L> {
    predicates: Rc<PredicateMap<L>>,
}

impl<L> PredicateParser<L> {
    fn new(predicates: &Rc<PredicateMap<L>>) -> Self {
        Self {
            predicates: predicates.clone()
        }
    }
}

impl<'a, L> Parser<&'a str, Rc<HybridPredicate<L>>, nom::error::Error<&'a str>> for PredicateParser<L> {
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, Rc<HybridPredicate<L>>, nom::error::Error<&'a str>> {
        let get_predicate = |name: String| self.predicates.get(&name).ok_or(MissingPredicateError::from(name));
        let mut parser = map_res(var_name, get_predicate);
        let (rest, predicate) = parser.parse(input)?;

        Ok((rest, predicate.clone()))
    }
}

struct OperandParser<L> {
    predicates: Rc<PredicateMap<L>>,
}

impl<L> OperandParser<L> {
    fn new(predicates: &Rc<PredicateMap<L>>) -> Self {
        Self {
            predicates: predicates.clone()
        }
    }
}

impl<'a, L> Parser<&'a str, ParsedHybridFormula<L>, nom::error::Error<&'a str>> for OperandParser<L>
where
    L: Copy + Ord + Hash + 'static
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedHybridFormula<L>> {
        let mut p1 = PredicateParser::new(&self.predicates);
        let mut p2 = delimited(tag("("), hybrid_formula(self.predicates.clone()), tag(")"));

        if let Ok((rest, pred)) = p1.parse(input) {
            Ok((rest, ParsedHybridFormula::new(pred)))
        } else {
            p2.parse(input)
        }
    }
}

struct NotParser<L> {
    predicates: Rc<PredicateMap<L>>,
}

impl<L> NotParser<L> {
    fn new(predicates: &Rc<PredicateMap<L>>) -> Self {
        Self {
            predicates: predicates.clone()
        }
    }
}

impl <'a, L> Parser<&'a str, ParsedHybridFormula<L>, nom::error::Error<&'a str>> for NotParser<L>
where
    L: Copy + Ord + Hash + 'static
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedHybridFormula<L>> {
        let p1 = OperandParser::new(&self.predicates);
        let mut parser = operators::not(p1);
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedHybridFormula::new(formula)))
    }
}

struct AndParser<L> {
    predicates: Rc<PredicateMap<L>>,
}

impl<L> AndParser<L> {
    fn new(predicates: &Rc<PredicateMap<L>>) -> Self {
        Self {
            predicates: predicates.clone()
        }
    }
}

impl <'a, L> Parser<&'a str, ParsedHybridFormula<L>, nom::error::Error<&'a str>> for AndParser<L>
where
    L: Copy + Ord + Hash + 'static
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedHybridFormula<L>, nom::error::Error<&'a str>> {
        let left_parser = OperandParser::new(&self.predicates);
        let right_parser = OperandParser::new(&self.predicates);
        let mut parser = operators::and(left_parser, right_parser);
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedHybridFormula::new(formula)))
    }
}

struct OrParser<L> {
    predicates: Rc<PredicateMap<L>>,
}

impl<L> OrParser<L> {
    fn new(predicates: &Rc<PredicateMap<L>>) -> Self {
        Self {
            predicates: predicates.clone()
        }
    }
}

impl <'a, L> Parser<&'a str, ParsedHybridFormula<L>, nom::error::Error<&'a str>> for OrParser<L>
where
    L: Copy + Ord + Hash + 'static
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedHybridFormula<L>, nom::error::Error<&'a str>> {
        let left_parser = OperandParser::new(&self.predicates);
        let right_parser = OperandParser::new(&self.predicates);
        let mut parser = operators::or(left_parser, right_parser);
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedHybridFormula::new(formula)))
    }
}

struct ImpliesParser<L> {
    predicates: Rc<PredicateMap<L>>,
}

impl<L> ImpliesParser<L> {
    fn new(predicates: &Rc<PredicateMap<L>>) -> Self {
        Self {
            predicates: predicates.clone()
        }
    }
}

impl <'a, L> Parser<&'a str, ParsedHybridFormula<L>, nom::error::Error<&'a str>> for ImpliesParser<L>
where
    L: Copy + Ord + Hash + 'static
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedHybridFormula<L>, nom::error::Error<&'a str>> {
        let ante_parser = OperandParser::new(&self.predicates);
        let cons_parser = OperandParser::new(&self.predicates);
        let mut parser = operators::implies(ante_parser, cons_parser);
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedHybridFormula::new(formula)))
    }
}

struct NextParser<L> {
    predicates: Rc<PredicateMap<L>>,
}

impl<L> NextParser<L> {
    fn new(predicates: &Rc<PredicateMap<L>>) -> Self {
        Self {
            predicates: predicates.clone()
        }
    }
}

impl <'a, L> Parser<&'a str, ParsedHybridFormula<L>, nom::error::Error<&'a str>> for NextParser<L>
where
    L: Copy + Ord + Hash + 'static
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedHybridFormula<L>, nom::error::Error<&'a str>> {
        let p1 = OperandParser::new(&self.predicates);
        let mut parser = operators::next(p1);
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedHybridFormula::new(formula)))
    }
}

struct AlwaysParser<L> {
    predicates: Rc<PredicateMap<L>>,
}

impl<L> AlwaysParser<L> {
    fn new(predicates: &Rc<PredicateMap<L>>) -> Self {
        Self {
            predicates: predicates.clone()
        }
    }
}

impl <'a, L> Parser<&'a str, ParsedHybridFormula<L>, nom::error::Error<&'a str>> for AlwaysParser<L>
where
    L: Copy + Ord + Hash + 'static
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedHybridFormula<L>, nom::error::Error<&'a str>> {
        let p1 = OperandParser::new(&self.predicates);
        let mut parser = operators::always(p1);
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedHybridFormula::new(formula)))
    }
}

struct EventuallyParser<L> {
    predicates: Rc<PredicateMap<L>>,
}

impl<L> EventuallyParser<L> {
    fn new(predicates: &Rc<PredicateMap<L>>) -> Self {
        Self {
            predicates: predicates.clone()
        }
    }
}

impl <'a, L> Parser<&'a str, ParsedHybridFormula<L>, nom::error::Error<&'a str>> for EventuallyParser<L>
where
    L: Copy + Ord + Hash + 'static
{
    fn parse(&mut self, input: &'a str) -> nom::IResult<&'a str, ParsedHybridFormula<L>, nom::error::Error<&'a str>> {
        let p1 = OperandParser::new(&self.predicates);
        let mut parser = operators::eventually(p1);
        let (rest, formula) = parser.parse(input)?;

        Ok((rest, ParsedHybridFormula::new(formula)))
    }
}

fn hybrid_formula<'a, L>(predicates: Rc<PredicateMap<L>>) -> impl Parser<&'a str, ParsedHybridFormula<L>, nom::error::Error<&'a str>>
where
    L: Copy + Ord + Hash + 'static,
{
    let mut parser = alt((
        NotParser::new(&predicates),
        NextParser::new(&predicates),
        AlwaysParser::new(&predicates),
        EventuallyParser::new(&predicates),
        AndParser::new(&predicates),
        OrParser::new(&predicates),
        ImpliesParser::new(&predicates),
        OperandParser::new(&predicates),
    ));

    move |input: &'a str| {
        parser.parse(input)
    }
}

pub fn parse_hybrid_formula<'a, L>(input: &'a str, predicates: HashMap<String, HybridPredicate<L>>) -> Result<ParsedHybridFormula<L>, Box<dyn Error + 'a>>
where
    L: Copy + Ord + Hash + 'static
{
    let predicates = predicates
        .into_iter()
        .map(|(name, predicate)| (name, Rc::new(predicate)))
        .collect();

    let mut parser = hybrid_formula(Rc::new(predicates));
    let (rest, formula) = parser.parse(input)?;

    if rest.len() > 0 {
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

    use crate::automaton::Automaton;
    use crate::expressions::HybridPredicate;
    use super::{AlwaysParser, AndParser, EventuallyParser, ImpliesParser, NextParser, NotParser, OrParser, PredicateParser};

    fn get_predicates() -> Rc<HashMap<String, Rc<HybridPredicate<i32>>>> {
        let elements =  [
            ("p1".to_string(), HybridPredicate::new(None, 1, Automaton::default())),
        ];

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

        let (rest, _) = parser.parse(r"(p1) /\ (p1)")?;
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
