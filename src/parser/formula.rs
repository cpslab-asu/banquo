use std::collections::HashMap;
use std::error::Error;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{space0, space1};
use nom::combinator::{map, opt};
use nom::multi::many0;
use nom::sequence::{delimited, pair, preceded, separated_pair, terminated, tuple};
use nom::IResult;

use super::common::{op0, pos_num, var_name, FormulaWrapper};
use super::errors::{IncompleteParseError, ParsedFormulaError};
use super::operators;
use crate::expressions::{Polynomial, Predicate, Term};
use crate::trace::Trace;
use crate::Formula;

pub struct ParsedFormula<'a, Cost> {
    inner: Box<dyn Formula<Cost, State = HashMap<String, f64>, Error = ParsedFormulaError> + 'a>,
}

impl<'a, Cost> ParsedFormula<'a, Cost> {
    pub fn new<F>(formula: F) -> Self
    where
        F: Formula<Cost, State = HashMap<String, f64>> + 'a,
        F::Error: 'static,
    {
        Self {
            inner: Box::new(FormulaWrapper::wrap(formula)),
        }
    }
}

impl<'a, Cost> Formula<Cost> for ParsedFormula<'a, Cost> {
    type State = HashMap<String, f64>;
    type Error = ParsedFormulaError;

    #[inline]
    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<Cost>, Self::Error> {
        self.inner.evaluate_states(trace)
    }
}

pub fn pos_neg_num(input: &str) -> IResult<&str, f64> {
    let mut parser = pair(opt(tag("-")), pos_num);
    let (rest, (sign, num)) = parser(input)?;
    let signed_num = match sign {
        Some(_) => -num,
        None => num,
    };

    Ok((rest, signed_num))
}

pub fn coeff(input: &str) -> IResult<&str, Term> {
    let mut p1 = pair(var_name, opt(preceded(op0("*"), pos_neg_num)));

    if let Ok((rest, (name, coefficient))) = p1(input) {
        return Ok((rest, Term::variable(name, coefficient.unwrap_or(1.0))));
    }

    let mut p2 = separated_pair(pos_neg_num, op0("*"), var_name);
    let (rest, (coefficient, name)) = p2(input)?;

    Ok((rest, Term::variable(name, coefficient)))
}

pub fn term(input: &str) -> IResult<&str, Term> {
    let constant = map(pos_neg_num, |value: f64| Term::constant(value));
    let mut parser = alt((coeff, constant));
    let (rest, result) = parser(input)?;

    Ok((rest, result))
}

pub fn polynomial(input: &str) -> IResult<&str, Polynomial> {
    let terms = many0(preceded(op0("+"), term));
    let mut parser = pair(term, terms);
    let (rest, (first, others)) = parser(input)?;

    let mut polynomial: Polynomial = first.into();
    polynomial.extend(others);

    Ok((rest, polynomial))
}

fn predicate(input: &str) -> IResult<&str, Predicate> {
    let mut parser = tuple((polynomial, op0("<="), polynomial));
    let (rest, (left, _, right)) = parser(input)?;
    let predicate = Predicate::new(left, right);

    Ok((rest, predicate))
}

fn subformula(input: &str) -> IResult<&str, ParsedFormula<f64>> {
    let inner = delimited(space0, formula, space0);
    let mut parser = delimited(tag("("), inner, tag(")"));

    parser(input)
}

fn left_operand(input: &str) -> IResult<&str, ParsedFormula<f64>> {
    let p1 = terminated(map(predicate, ParsedFormula::new), space1);
    let p2 = terminated(subformula, space0);
    let mut parser = alt((p1, p2));

    parser(input)
}

fn right_operand(input: &str) -> IResult<&str, ParsedFormula<f64>> {
    let p1 = preceded(space1, map(predicate, ParsedFormula::new));
    let p2 = preceded(space0, subformula);
    let mut parser = alt((p1, p2));

    parser(input)
}

fn not(input: &str) -> IResult<&str, ParsedFormula<f64>> {
    let mut parser = operators::not(right_operand);
    let (rest, formula) = parser(input)?;

    Ok((rest, ParsedFormula::new(formula)))
}

fn and(input: &str) -> IResult<&str, ParsedFormula<f64>> {
    let mut parser = operators::and(left_operand, right_operand);
    let (rest, formula) = parser(input)?;

    Ok((rest, ParsedFormula::new(formula)))
}

fn or(input: &str) -> IResult<&str, ParsedFormula<f64>> {
    let mut parser = operators::or(left_operand, right_operand);
    let (rest, formula) = parser(input)?;

    Ok((rest, ParsedFormula::new(formula)))
}

fn implies(input: &str) -> IResult<&str, ParsedFormula<f64>> {
    let mut parser = operators::implies(left_operand, right_operand);
    let (rest, formula) = parser(input)?;

    Ok((rest, ParsedFormula::new(formula)))
}

fn next(input: &str) -> IResult<&str, ParsedFormula<f64>> {
    let mut parser = operators::next(right_operand);
    let (rest, formula) = parser(input)?;

    Ok((rest, ParsedFormula::new(formula)))
}

fn always(input: &str) -> IResult<&str, ParsedFormula<f64>> {
    let mut parser = operators::always(right_operand);
    let (rest, formula) = parser(input)?;

    Ok((rest, ParsedFormula::new(formula)))
}

fn eventually(input: &str) -> IResult<&str, ParsedFormula<f64>> {
    let mut parser = operators::eventually(right_operand);
    let (rest, formula) = parser(input)?;

    Ok((rest, ParsedFormula::new(formula)))
}

fn until(input: &str) -> IResult<&str, ParsedFormula<f64>> {
    let mut parser = operators::until(left_operand, right_operand);
    let (rest, formula) = parser(input)?;

    Ok((rest, ParsedFormula::new(formula)))
}

fn formula(input: &str) -> IResult<&str, ParsedFormula<f64>> {
    let mut parser = alt((
        next,
        always,
        eventually,
        next,
        not,
        and,
        or,
        implies,
        until,
        subformula,
        map(predicate, ParsedFormula::new),
    ));
    parser(input)
}

pub fn parse_formula<'a>(input: &'a str) -> Result<ParsedFormula<f64>, Box<dyn Error + 'a>> {
    let (rest, parsed) = formula(input)?;

    if !rest.is_empty() {
        Err(Box::new(IncompleteParseError::from(rest)))
    } else {
        Ok(parsed)
    }
}

pub fn parse_predicate<'a>(input: &'a str) -> Result<Predicate, Box<dyn Error + 'a>> {
    let (rest, predicate) = predicate(input)?;

    if !rest.is_empty() {
        Err(Box::new(IncompleteParseError::from(rest)))
    } else {
        Ok(predicate)
    }
}

#[cfg(test)]
mod tests {
    use std::error::Error;

    use super::{
        always, and, coeff, eventually, formula, implies, next, not, or, polynomial, pos_neg_num, predicate, until,
    };
    use crate::expressions::{Polynomial, Predicate, Term};

    #[test]
    fn parse_pos_neg_number() -> Result<(), Box<dyn Error>> {
        let (rest, value) = pos_neg_num("24.77")?;

        assert_eq!(rest, "");
        assert_eq!(value, 24.77);

        let (rest, value) = pos_neg_num("-24.77")?;

        assert_eq!(rest, "");
        assert_eq!(value, -24.77);

        Ok(())
    }

    #[test]
    fn parse_coefficient() -> Result<(), Box<dyn Error>> {
        let (rest, value) = coeff("12.0 * abc")?;

        assert_eq!(rest, "");
        assert_eq!(value, Term::variable("abc", 12.0));

        let (rest, value) = coeff("1.1*X")?;

        assert_eq!(rest, "");
        assert_eq!(value, Term::variable("X", 1.1));

        let (rest, value) = coeff("y*3")?;

        assert_eq!(rest, "");
        assert_eq!(value, Term::variable("y", 3.0));

        let (rest, value) = coeff("z * -0.3")?;

        assert_eq!(rest, "");
        assert_eq!(value, Term::variable("z", -0.3));

        let (rest, value) = coeff("L")?;

        assert_eq!(rest, "");
        assert_eq!(value, Term::variable("L", 1.0));

        Ok(())
    }

    #[test]
    fn parse_polynomial() -> Result<(), Box<dyn Error>> {
        let (rest, value) = polynomial("12.0 + 3.1*x + 22.4*y")?;
        let expected = Polynomial::from([
            Term::variable("x", 3.1),
            Term::variable("y", 22.4),
            Term::constant(12.0),
        ]);

        assert_eq!(rest, "");
        assert_eq!(value, expected);

        let (rest, value) = polynomial("3.1*x + 22.4*y")?;
        let expected = Polynomial::from([Term::variable("x", 3.1), Term::variable("y", 22.4)]);

        assert_eq!(rest, "");
        assert_eq!(value, expected);

        let (rest, value) = polynomial("12.0")?;
        let expected = Term::constant(12.0).into();

        assert_eq!(rest, "");
        assert_eq!(value, expected);

        Ok(())
    }

    #[test]
    fn parse_predicate() -> Result<(), Box<dyn Error>> {
        let left = Polynomial::from([
            Term::variable("x", 3.1),
            Term::variable("y", 22.4f64),
            Term::constant(12.0),
        ]);
        let right = Term::variable("z", 4.8f64);
        let expected = Predicate::new(left, right);
        let (rest, actual) = predicate("12.0 + 3.1*x + 22.4*y <= 4.8*z")?;

        assert_eq!(rest, "");
        assert_eq!(expected, actual);

        Ok(())
    }

    #[test]
    fn parse_not() -> Result<(), Box<dyn Error>> {
        let (rest, _) = not("!(3.1*x <= 0.5*y)")?;
        assert_eq!(rest, "");

        let (rest, _) = not("not 3.1*x <= 0.5*y")?;
        assert_eq!(rest, "");

        let (rest, _) = not("not (3.1*x <= 0.5*y)")?;
        assert_eq!(rest, "");

        Ok(())
    }

    #[test]
    fn parse_and() -> Result<(), Box<dyn Error>> {
        let (rest, _) = and(r"(3.1*x <= 0.5*y)/\(2.0*x <= 4.0)")?;
        assert_eq!(rest, "");

        let (rest, _) = and("3.1*x <= 0.5*y and 2.0*x <= 4.0")?;
        assert_eq!(rest, "");

        let (rest, _) = and(r"3.1*x <= 0.5*y and (2.0*x <= 4.0 /\ (2.0 <= 1.5*x))")?;
        assert_eq!(rest, "");

        Ok(())
    }

    #[test]
    fn parse_or() -> Result<(), Box<dyn Error>> {
        let (rest, _) = or(r"(3.1*x <= 0.5*y) \/ (2.0*x <= 4.0)")?;
        assert_eq!(rest, "");

        let (rest, _) = or("3.1*x <= 0.5*y or 2.0*x <= 4.0")?;
        assert_eq!(rest, "");

        let (rest, _) = or(r"(3.1*x <= 0.5*y or 2.0*x <= 4.0)\/(2.0 <= 1.5*x)")?;
        assert_eq!(rest, "");

        Ok(())
    }

    #[test]
    fn parse_implies() -> Result<(), Box<dyn Error>> {
        let (rest, _) = implies("2.0*x <= 4.0 -> (1.0*y <= 12.0)")?;
        assert_eq!(rest, "");

        let (rest, _) = implies("2.0*x <= 4.0 implies (1.0*y <= 12.0)")?;
        assert_eq!(rest, "");

        Ok(())
    }

    #[test]
    fn parse_next() -> Result<(), Box<dyn Error>> {
        let (rest, _) = next("X 3.1*x <= 0.5*y")?;
        assert_eq!(rest, "");

        let (rest, _) = next("()(3.1*x <= 0.5*y)")?;
        assert_eq!(rest, "");

        let (rest, _) = next("next (3.1*x <= 0.5*y)")?;
        assert_eq!(rest, "");

        Ok(())
    }

    #[test]
    fn parse_always() -> Result<(), Box<dyn Error>> {
        let (rest, _) = always("always 3.1*x <= 0.5*y")?;
        assert_eq!(rest, "");

        let (rest, _) = always("[]{0,10} 3.1*x <= 0.5*y")?;
        assert_eq!(rest, "");

        let (rest, _) = always("G{1,2}(3.1*x <= 0.5*y)")?;
        assert_eq!(rest, "");

        Ok(())
    }

    #[test]
    fn parse_eventually() -> Result<(), Box<dyn Error>> {
        let (rest, _) = eventually("eventually 3.1*x <= 0.5*y")?;
        assert_eq!(rest, "");

        let (rest, _) = eventually("<>{0,10} 3.1*x <= 0.5*y")?;
        assert_eq!(rest, "");

        let (rest, _) = eventually("F{1,2}(3.1*x <= 0.5*y)")?;
        assert_eq!(rest, "");

        Ok(())
    }

    #[test]
    fn parse_until() -> Result<(), Box<dyn Error>> {
        let (rest, _) = until("x <= 0.0 U 3.1*x <= 0.5*y")?;
        assert_eq!(rest, "");

        let (rest, _) = until("(3.1*x <= 0.5*y) U (1.2*x <= 5.1*z)")?;
        assert_eq!(rest, "");

        Ok(())
    }

    #[test]
    fn parse_formula() -> Result<(), Box<dyn Error>> {
        let (rest, _) = formula(r"[]{0,10} (3.1*x <= 0.5*y /\ (not 1.0*x <= 2.0))")?;
        assert_eq!(rest, "");

        Ok(())
    }
}
