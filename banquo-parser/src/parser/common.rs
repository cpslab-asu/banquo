use std::error::Error;
use std::str::FromStr;

use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::{digit0, digit1, space0};
use nom::combinator::{map_res, opt, recognize};
use nom::sequence::{delimited, pair};
use nom::IResult;

use super::errors::ParsedFormulaError;
use crate::Formula;
use crate::Trace;

pub struct FormulaWrapper<F> {
    inner: F,
}

impl<F> FormulaWrapper<F> {
    pub fn wrap<State>(formula: F) -> Self
    where
        F: Formula<State>,
    {
        Self { inner: formula }
    }
}

impl<State, F, E> Formula<State> for FormulaWrapper<F>
where
    F: Formula<State, Error = E>,
    E: Error + 'static,
{
    type Metric = F::Metric;
    type Error = ParsedFormulaError;

    #[inline]
    fn evaluate(&self, trace: &Trace<State>) -> Result<Trace<Self::Metric>, Self::Error> {
        self.inner.evaluate(trace).map_err(ParsedFormulaError::from_err)
    }
}

pub fn var_name(input: &str) -> IResult<&str, String> {
    // Identifiers: start with ASCII letter or '_', followed by any combination
    // of ASCII letters, digits, or '_' (e.g., `x`, `y1`, `roll_rate`).
    fn is_ident_start(c: char) -> bool {
        c.is_ascii_alphabetic() || c == '_'
    }
    fn is_ident_char(c: char) -> bool {
        c.is_ascii_alphanumeric() || c == '_'
    }

    let mut ident = recognize(pair(take_while1(is_ident_start), opt(take_while1(is_ident_char))));
    let (rest, name) = ident(input)?;

    Ok((rest, name.to_string()))
}

pub fn pos_num(input: &str) -> IResult<&str, f64> {
    let make_number = |(front, back): (&str, Option<&str>)| {
        let num_str = front.to_string() + back.unwrap_or("");
        f64::from_str(&num_str)
    };

    let back_parser = pair(tag("."), digit1);
    let num_parser = pair(digit1, opt(recognize(back_parser)));
    let mut parser = map_res(num_parser, make_number);

    parser(input)
}

pub fn op0<'a>(op: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
    move |input: &'a str| -> IResult<&'a str, &'a str> {
        let mut parser = delimited(space0, tag(op), space0);
        parser(input)
    }
}

#[cfg(test)]
mod tests {
    use std::error::Error;

    use super::{pos_num, var_name};

    #[test]
    fn parse_var_name() -> Result<(), Box<dyn Error>> {
        let (rest, value) = var_name("myvar")?;

        assert_eq!(rest, "");
        assert_eq!(value, "myvar");

        let (rest, value) = var_name("x1")?;

        assert_eq!(rest, "");
        assert_eq!(value, "x1");

        let (rest, value) = var_name("roll_rate2")?;

        assert_eq!(rest, "");
        assert_eq!(value, "roll_rate2");

        Ok(())
    }

    #[test]
    fn parse_pos_num() -> Result<(), Box<dyn Error>> {
        let (rest, value) = pos_num("123.345")?;

        assert_eq!(rest, "");
        assert_eq!(value, 123.345);

        let (rest, value) = pos_num("123")?;

        assert_eq!(rest, "");
        assert_eq!(value, 123.0);

        Ok(())
    }
}
