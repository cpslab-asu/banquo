use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::str::FromStr;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, digit1, space0, space1};
use nom::combinator::{map, map_res, opt, recognize};
use nom::multi::many0;
use nom::sequence::{delimited, pair, preceded, separated_pair, terminated, tuple};
use nom::IResult;

use crate::expressions::{Polynomial, Predicate};
use crate::formula::Formula;
use crate::operators::{Always, And, Eventually, Implies, Next, Not, Or};
use crate::trace::Trace;

pub struct ParsedFormulaError {
    inner: Box<dyn Error>,
}

impl ParsedFormulaError {
    fn from_err<E: Error + 'static>(err: E) -> Self {
        Self { inner: Box::new(err) }
    }
}

impl Display for ParsedFormulaError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.inner, f)
    }
}

impl Debug for ParsedFormulaError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.inner, f)
    }
}

impl Error for ParsedFormulaError {}

struct WrappedFormula<F>(F);

impl<F> Formula<HashMap<String, f64>> for WrappedFormula<F>
where
    F: Formula<HashMap<String, f64>>,
    F::Error: 'static,
{
    type Error = ParsedFormulaError;

    fn robustness(&self, trace: &Trace<HashMap<String, f64>>) -> Result<Trace<f64>, Self::Error> {
        self.0.robustness(trace).map_err(ParsedFormulaError::from_err)
    }
}

type InnerFormula = Box<dyn Formula<HashMap<String, f64>, Error = ParsedFormulaError>>;

pub struct ParsedFormula {
    inner: InnerFormula,
}

impl ParsedFormula {
    fn new<F>(formula: F) -> Self
    where
        F: Formula<HashMap<String, f64>, Error = ParsedFormulaError> + 'static,
    {
        Self {
            inner: Box::new(formula),
        }
    }
}

impl Formula<HashMap<String, f64>> for ParsedFormula {
    type Error = ParsedFormulaError;

    fn robustness(&self, trace: &Trace<HashMap<String, f64>>) -> Result<Trace<f64>, Self::Error> {
        self.inner.robustness(trace)
    }
}

impl From<Predicate> for ParsedFormula {
    fn from(p: Predicate) -> Self {
        Self::new(WrappedFormula(p))
    }
}

fn decimal(input: &str) -> IResult<&str, f64> {
    let mut parser = recognize(tuple((opt(tag("-")), digit1, tag("."), digit1)));
    let (rest, value) = parser(input)?;

    Ok((rest, f64::from_str(value).unwrap()))
}

fn op0<'a>(value: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
    move |input: &'a str| -> IResult<&'a str, &'a str> {
        let mut parser = delimited(space0, tag(value), space0);
        parser(input)
    }
}

fn coeff(input: &str) -> IResult<&str, (f64, String)> {
    let mut parser = tuple((decimal, op0("*"), alpha1));
    let (rest, (coefficient, _, variable_name)) = parser(input)?;

    Ok((rest, (coefficient, variable_name.to_string())))
}

fn term(input: &str) -> IResult<&str, (f64, Option<String>)> {
    let p1 = map(coeff, |(value, name): (f64, String)| (value, Some(name)));
    let p2 = map(decimal, |value: f64| (value, None));
    let mut parser = alt((p1, p2));
    let (rest, result) = parser(input)?;

    Ok((rest, result))
}

fn polynomial(input: &str) -> IResult<&str, Polynomial> {
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

fn parsed_predicate(input: &str) -> IResult<&str, ParsedFormula> {
    let mut parser = map(predicate, |p| p.into());
    parser(input)
}

fn subformula(input: &str) -> IResult<&str, ParsedFormula> {
    let mut parser = delimited(tag("("), formula, tag(")"));
    parser(input)
}

fn not(input: &str) -> IResult<&str, Not<InnerFormula>> {
    let form1 = preceded(tag("!"), subformula);
    let form2 = preceded(pair(tag("not"), space1), alt((subformula, parsed_predicate)));

    let mut parser = alt((form1, form2));
    let (rest, parsed_formula) = parser(input)?;
    let formula = Not::new(parsed_formula.inner);

    Ok((rest, formula))
}

fn and(input: &str) -> IResult<&str, WrappedFormula<And<InnerFormula, InnerFormula>>> {
    let p1 = alt((subformula, parsed_predicate));
    let p2 = alt((map(and, ParsedFormula::new), subformula, parsed_predicate));
    let mut parser = separated_pair(p1, alt((op0("/\\"), op0("and"))), p2);

    let (rest, (left, right)) = parser(input)?;
    let formula = And::new(left.inner, right.inner);
    let wrapped = WrappedFormula(formula);

    Ok((rest, wrapped))
}

fn or(input: &str) -> IResult<&str, WrappedFormula<Or<InnerFormula, InnerFormula>>> {
    let p1 = alt((subformula, parsed_predicate));
    let p2 = alt((map(or, ParsedFormula::new), subformula, parsed_predicate));
    let mut parser = separated_pair(p1, alt((op0("\\/"), op0("or"))), p2);

    let (rest, (left, right)) = parser(input)?;
    let formula = Or::new(left.inner, right.inner);
    let wrapped = WrappedFormula(formula);

    Ok((rest, wrapped))
}

fn implies(input: &str) -> IResult<&str, WrappedFormula<Implies<InnerFormula, InnerFormula>>> {
    let ante = terminated(alt((parsed_predicate, subformula)), space1);
    let op = alt((tag("->"), tag("implies")));
    let cons = preceded(space1, alt((parsed_predicate, subformula)));

    let mut parser = separated_pair(ante, op, cons);
    let (rest, (ante_formula, cons_formula)) = parser(input)?;
    let formula = Implies::new(ante_formula.inner, cons_formula.inner);
    let wrapped = WrappedFormula(formula);

    Ok((rest, wrapped))
}

fn next(input: &str) -> IResult<&str, Next<InnerFormula>> {
    let ops = alt((tag("X"), tag("()"), tag("next")));
    let p1 = preceded(space1, parsed_predicate);
    let p2 = preceded(space0, subformula);

    let mut parser = preceded(ops, alt((p1, p2)));
    let (rest, parsed_formula) = parser(input)?;
    let formula = Next::new(parsed_formula.inner);

    Ok((rest, formula))
}

fn integer(input: &str) -> IResult<&str, usize> {
    let mut parser = map_res(digit1, usize::from_str);
    parser(input)
}

fn time_bounds(input: &str) -> IResult<&str, (usize, usize)> {
    let mut parser = tuple((tag("{"), integer, tag(","), integer, tag("}")));
    let (rest, (_, t_start, _, t_end, _)) = parser(input)?;

    Ok((rest, (t_start, t_end)))
}

fn always(input: &str) -> IResult<&str, Always<InnerFormula>> {
    let op = alt((tag("always"), tag("[]"), tag("G")));
    let p1 = preceded(space1, parsed_predicate);
    let p2 = preceded(space0, subformula);

    let mut parser = preceded(op, pair(opt(time_bounds), alt((p1, p2))));
    let (rest, (t_bounds, parsed_formula)) = parser(input)?;
    let formula = Always::new(parsed_formula.inner, t_bounds);

    Ok((rest, formula))
}

fn eventually(input: &str) -> IResult<&str, Eventually<InnerFormula>> {
    let op = alt((tag("eventually"), tag("<>"), tag("F")));
    let p1 = preceded(space1, parsed_predicate);
    let p2 = preceded(space0, subformula);

    let mut parser = preceded(op, pair(opt(time_bounds), alt((p1, p2))));
    let (rest, (t_bounds, parsed_formula)) = parser(input)?;
    let formula = Eventually::new(parsed_formula.inner, t_bounds);

    Ok((rest, formula))
}

fn formula(input: &str) -> IResult<&str, ParsedFormula> {
    let mut parser = alt((
        map(next, ParsedFormula::new),
        map(always, ParsedFormula::new),
        map(eventually, ParsedFormula::new),
        map(not, ParsedFormula::new),
        map(and, ParsedFormula::new),
        map(or, ParsedFormula::new),
        map(implies, ParsedFormula::new),
        parsed_predicate,
    ));

    parser(input)
}

#[derive(Debug)]
struct IncompleteParseError<'a>(&'a str);

impl<'a> Display for IncompleteParseError<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "could not parse remaining input \"{}\")", self.0)
    }
}

impl<'a> Error for IncompleteParseError<'a> {}

pub fn parse_formula<'a>(input: &'a str) -> Result<ParsedFormula, Box<dyn Error + 'a>> {
    let (rest, parsed) = formula(input)?;

    if rest.len() > 0 {
        Err(Box::new(IncompleteParseError(rest)))
    } else {
        Ok(parsed)
    }
}

pub fn parse_predicate<'a>(input: &'a str) -> Result<Predicate, Box<dyn Error + 'a>> {
    let (rest, predicate) = predicate(input)?;

    if rest.len() > 0 {
        Err(Box::new(IncompleteParseError(rest)))
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
        let (rest, _) = and("(3.1*x <= 0.5*y) /\\ (2.0*x <= 4.0)")?;
        assert_eq!(rest, "");

        let (rest, _) = and("3.1*x <= 0.5*y and 2.0*x <= 4.0")?;
        assert_eq!(rest, "");

        let (rest, _) = and("3.1*x <= 0.5*y and 2.0*x <= 4.0 /\\ (2.0 <= 1.5*x)")?;
        assert_eq!(rest, "");

        Ok(())
    }

    #[test]
    fn parse_or() -> Result<(), Box<dyn Error>> {
        let (rest, _) = or("(3.1*x <= 0.5*y) \\/ (2.0*x <= 4.0)")?;
        assert_eq!(rest, "");

        let (rest, _) = or("3.1*x <= 0.5*y or 2.0*x <= 4.0")?;
        assert_eq!(rest, "");

        let (rest, _) = or("3.1*x <= 0.5*y or 2.0*x <= 4.0 \\/ (2.0 <= 1.5*x)")?;
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
        let (rest, _) = formula("[]{0,10} (3.1*x <= 0.5*y /\\ (not 1.0*x <= 2.0))")?;
        assert_eq!(rest, "");

        Ok(())
    }
}
