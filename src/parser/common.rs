use std::error::Error;
use std::str::FromStr;

use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, digit0, digit1, space0};
use nom::combinator::{map_res, opt, recognize};
use nom::sequence::{delimited, pair};
use nom::IResult;

use super::errors::ParsedFormulaError;
use crate::formulas::Formula;
use crate::trace::Trace;

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
    fn evaluate_trace(&self, trace: &Trace<State>) -> Result<Trace<Self::Metric>, Self::Error> {
        self.inner.evaluate_trace(trace).map_err(ParsedFormulaError::from_err)
    }
}

pub fn var_name(input: &str) -> IResult<&str, String> {
    let mut parser = pair(alpha1, digit0);
    let (rest, (s1, s2)) = parser(input)?;
    let name = s1.to_string() + s2;

    Ok((rest, name))
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
