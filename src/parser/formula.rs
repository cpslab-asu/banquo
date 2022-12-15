use std::collections::HashMap;
use std::error::Error;
use std::str::FromStr;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{digit1, space0, space1};
use nom::combinator::{map, opt, recognize};
use nom::multi::many0;
use nom::sequence::{pair, preceded, tuple, delimited, terminated};
use nom::IResult;

use crate::expressions::{Polynomial, Predicate};
use crate::formula::Formula;
use crate::trace::Trace;
use super::common::{WrappedFormula, op0, var_name};
use super::errors::{IncompleteParseError, ParsedFormulaError};
use super::operators;

pub struct ParsedFormula {
    inner: Box<dyn Formula<HashMap<String, f64>, Error = ParsedFormulaError>>,
}

impl ParsedFormula {
    fn new<F>(formula: F) -> Self
    where
        F: Formula<HashMap<String, f64>> + 'static,
    {
        Self {
            inner: Box::new(WrappedFormula::wrap(formula)),
        }
    }
}

impl Formula<HashMap<String, f64>> for ParsedFormula {
    type Error = ParsedFormulaError;

    #[inline]
    fn robustness(&self, trace: &Trace<HashMap<String, f64>>) -> Result<Trace<f64>, Self::Error> {
        self.inner.robustness(trace)
    }
}

pub fn decimal(input: &str) -> IResult<&str, f64> {
    let mut parser = recognize(tuple((opt(tag("-")), digit1, tag("."), digit1)));
    let (rest, value) = parser(input)?;

    Ok((rest, f64::from_str(value).unwrap()))
}

pub fn coeff(input: &str) -> IResult<&str, (f64, String)> {
    let mut parser = tuple((decimal, op0("*"), var_name));
    let (rest, (coefficient, _, variable_name)) = parser(input)?;

    Ok((rest, (coefficient, variable_name)))
}

pub fn term(input: &str) -> IResult<&str, (f64, Option<String>)> {
    let p1 = map(coeff, |(value, name): (f64, String)| (value, Some(name)));
    let p2 = map(decimal, |value: f64| (value, None));
    let mut parser = alt((p1, p2));
    let (rest, result) = parser(input)?;

    Ok((rest, result))
}

pub fn polynomial(input: &str) -> IResult<&str, Polynomial> {
    let terms = many0(preceded(op0("+"), term));
    let mut parser = pair(term, terms);
    let (rest, (first, others)) = parser(input)?;
    let mut constant = 0f64;
    let mut coefficients = HashMap::new();

    match first {
        (value, Some(name)) => {
            coefficients.insert(name, value);
        }
        (value, None) => {
            constant = value;
        }
    }

    for (value, name) in others {
        match name {
            Some(n) => {
                coefficients.insert(n, value);
            }
            None => constant += value,
        }
    }

    Ok((rest, Polynomial::with_constant(coefficients, constant)))
}

fn predicate(input: &str) -> IResult<&str, Predicate> {
    let mut parser = tuple((polynomial, op0("<="), polynomial));
    let (rest, (left, _, right)) = parser(input)?;
    let predicate = Predicate::new(left, right);

    Ok((rest, predicate))
}

fn subformula(input: &str) -> IResult<&str, ParsedFormula> {
    let inner = delimited(space0, formula, space0);
    let mut parser = delimited(tag("("), inner, tag(")"));

    parser(input)
}

fn left_operand(input: &str) -> IResult<&str, ParsedFormula> {
    let p1 = terminated(map(predicate, ParsedFormula::new), space1);
    let p2 = terminated(subformula, space0);
    let mut parser = alt((p1, p2));

    parser(input)
}

fn right_operand(input: &str) -> IResult<&str, ParsedFormula> {
    let p1 = preceded(space1, map(predicate, ParsedFormula::new));
    let p2 = preceded(space0, subformula);
    let mut parser = alt((p1, p2));

    parser(input)
}

fn not(input: &str) -> IResult<&str, ParsedFormula> {
    let mut parser = operators::not(right_operand);
    let (rest, formula) = parser(input)?;

    Ok((rest, ParsedFormula::new(formula)))
}

fn and(input: &str) -> IResult<&str, ParsedFormula> {
    let mut parser = operators::and(left_operand, right_operand);
    let (rest, formula) = parser(input)?;
    
    Ok((rest, ParsedFormula::new(formula)))
}

fn or(input: &str) -> IResult<&str, ParsedFormula> {
    let mut parser = operators::or(left_operand, right_operand);
    let (rest, formula) = parser(input)?;
    
    Ok((rest, ParsedFormula::new(formula)))
}

fn implies(input: &str) -> IResult<&str, ParsedFormula> {
    let mut parser = operators::implies(left_operand, right_operand);
    let (rest, formula) = parser(input)?;
    
    Ok((rest, ParsedFormula::new(formula)))
}

fn next(input: &str) -> IResult<&str, ParsedFormula> {
    let mut parser = operators::next(right_operand);
    let (rest, formula) = parser(input)?;
    
    Ok((rest, ParsedFormula::new(formula)))
}

fn always(input: &str) -> IResult<&str, ParsedFormula> {
    let mut parser = operators::always(right_operand);
    let (rest, formula) = parser(input)?;
    
    Ok((rest, ParsedFormula::new(formula)))
}

fn eventually(input: &str) -> IResult<&str, ParsedFormula> {
    let mut parser = operators::eventually(right_operand);
    let (rest, formula) = parser(input)?;
    
    Ok((rest, ParsedFormula::new(formula)))
}

fn formula(input: &str) -> IResult<&str, ParsedFormula> {
    let mut parser = alt((next, always, eventually, next, not, and, or, implies, subformula, map(predicate, ParsedFormula::new)));
    parser(input)
}

pub fn parse_formula<'a>(input: &'a str) -> Result<ParsedFormula, Box<dyn Error + 'a>> {
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
    use std::collections::HashMap;
    use std::error::Error;

    use super::{always, and, coeff, decimal, eventually, formula, implies, next, not, or, polynomial, predicate};
    use crate::expressions::{Polynomial, Predicate};

    #[test]
    fn parse_decimal() -> Result<(), Box<dyn Error>> {
        let (rest, value) = decimal("123.345")?;

        assert_eq!(rest, "");
        assert_eq!(value, 123.345);

        let (rest, value) = decimal("-24.77")?;

        assert_eq!(rest, "");
        assert_eq!(value, -24.77);

        Ok(())
    }

    #[test]
    fn parse_coefficient() -> Result<(), Box<dyn Error>> {
        let (rest, value) = coeff("12.0 * abc")?;

        assert_eq!(rest, "");
        assert_eq!(value, (12.0f64, "abc".to_string()));

        let (rest, value) = coeff("1.1*X")?;

        assert_eq!(rest, "");
        assert_eq!(value, (1.1f64, "X".to_string()));

        Ok(())
    }

    #[test]
    fn parse_polynomial() -> Result<(), Box<dyn Error>> {
        let (rest, value) = polynomial("12.0 + 3.1*x + 22.4*y")?;
        let coefficients = HashMap::from_iter([("x".to_string(), 3.1f64), ("y".to_string(), 22.4f64)]);
        let expected = Polynomial::with_constant(coefficients, 12.0);

        assert_eq!(rest, "");
        assert_eq!(value, expected);

        let (rest, value) = polynomial("3.1*x + 22.4*y")?;
        let coefficients = HashMap::from_iter([("x".to_string(), 3.1f64), ("y".to_string(), 22.4f64)]);
        let expected = Polynomial::new(coefficients);

        assert_eq!(rest, "");
        assert_eq!(value, expected);

        let (rest, value) = polynomial("12.0")?;
        let expected = Polynomial::with_constant(HashMap::new(), 12.0);

        assert_eq!(rest, "");
        assert_eq!(value, expected);

        Ok(())
    }

    #[test]
    fn parse_predicate() -> Result<(), Box<dyn Error>> {
        let coefficients = HashMap::from_iter([("x".to_string(), 3.1f64), ("y".to_string(), 22.4f64)]);
        let left = Polynomial::with_constant(coefficients, 12.0);
        let right = Polynomial::new(HashMap::from_iter([("z".to_string(), 4.8f64)]));
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
    fn parse_formula() -> Result<(), Box<dyn Error>> {
        let (rest, _) = formula(r"[]{0,10} (3.1*x <= 0.5*y /\ (not 1.0*x <= 2.0))")?;
        assert_eq!(rest, "");

        Ok(())
    }
}
