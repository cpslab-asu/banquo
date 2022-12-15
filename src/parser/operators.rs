use std::str::FromStr;

use nom::{IResult, Parser};
use nom::branch::{Alt, alt};
use nom::bytes::complete::tag;
use nom::character::complete::digit1;
use nom::combinator::{map_res, opt};
use nom::sequence::{preceded, tuple};

use crate::operators::{Always, And, Eventually, Next, Not, Or, Implies};

fn unaryop<'a, O, S, T, F, U>(ops: O, mut subparser: S, func: F) -> impl FnMut(&'a str) -> IResult<&'a str, U>
where
    O: Alt<&'a str, &'a str, nom::error::Error<&'a str>>,
    S: Parser<&'a str, T, nom::error::Error<&'a str>>,
    F: Fn(T) -> U
{
    let mut op = alt(ops);
    
    move |input: &'a str| {
        let (next, _) = op.parse(input)?;
        let (rest, subformula) = subparser.parse(next)?;

        Ok((rest, func(subformula)))
    }
}

pub fn not<'a, F, T>(subparser: F) -> impl FnMut(&'a str) -> IResult<&'a str, Not<T>>
where
    F: Parser<&'a str, T, nom::error::Error<&'a str>>
{
    unaryop((tag("!"), tag("not")), subparser, Not::new)
}

fn binop<'a, O, P1, L, P2, R, F, T>(ops: O, mut left_parser: P1, right_parser: P2, func: F) -> impl FnMut(&'a str) -> IResult<&'a str, T>
where
    O: Alt<&'a str, &'a str, nom::error::Error<&'a str>>,
    P1: Parser<&'a str, L, nom::error::Error<&'a str>>,
    P2: Parser<&'a str, R, nom::error::Error<&'a str>>,
    F: Fn(L, R) -> T
{
    let mut right_parser = preceded(alt(ops), right_parser);

    move |input: &'a str| {
        let (next, left) = left_parser.parse(input)?;
        let (rest, right) = right_parser.parse(next)?;

        Ok((rest, func(left, right)))
    }
}

pub fn and<'a, P1, L, P2, R>(left_parser: P1, right_parser: P2) -> impl FnMut(&'a str) -> IResult<&'a str, And<L, R>>
where
    P1: Parser<&'a str, L, nom::error::Error<&'a str>>,
    P2: Parser<&'a str, R, nom::error::Error<&'a str>>,
{
    binop((tag(r"/\"), tag("and")), left_parser, right_parser, And::new)
}

pub fn or<'a, P1, L, P2, R>(left_parser: P1, right_parser: P2) -> impl FnMut(&'a str) -> IResult<&'a str, Or<L, R>>
where
    P1: Parser<&'a str, L, nom::error::Error<&'a str>>,
    P2: Parser<&'a str, R, nom::error::Error<&'a str>>,
{
    binop((tag(r"\/"), tag("or")), left_parser, right_parser, Or::new)
}

pub fn implies<'a, P1, A, P2, C>(left_parser: P1, right_parser: P2) -> impl FnMut(&'a str) -> IResult<&'a str, Implies<A, C>>
where
    P1: Parser<&'a str, A, nom::error::Error<&'a str>>,
    P2: Parser<&'a str, C, nom::error::Error<&'a str>>,
{
    binop((tag("->"), tag("implies")), left_parser, right_parser, Implies::new)
}

pub fn next<'a, 's, S, T>(subparser: S) -> impl FnMut(&'a str) -> IResult<&'a str, Next<T>>
where
    's: 'a,
    T: 's,
    S: Parser<&'a str, T, nom::error::Error<&'a str>> + 's,
{
    unaryop((tag("X"), tag("()"), tag("next")), subparser, Next::new)
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
 
fn boundedop<'a, O, S, T, F, U>(ops: O, mut subparser: S, func: F) -> impl FnMut(&'a str) -> IResult<&'a str, U>
where
    O: Alt<&'a str, &'a str, nom::error::Error<&'a str>>,
    S: Parser<&'a str, T, nom::error::Error<&'a str>>,
    F: Fn(T, Option<(usize, usize)>) -> U
{
    let mut bounds = preceded(alt(ops), opt(time_bounds));

    move |input: &'a str| {
        let (next, t_bounds) = bounds.parse(input)?;
        let (rest, formula) = subparser.parse(next)?;

        Ok((rest, func(formula, t_bounds)))
    }
}

pub fn always<'a, S, T>(subparser: S) -> impl FnMut(&'a str) -> IResult<&'a str, Always<T>>
where
    S: Parser<&'a str, T, nom::error::Error<&'a str>>,
{
    boundedop((tag("always"), tag("[]"), tag("G")), subparser, Always::new)
}

pub fn eventually<'a, S, T>(subparser: S) -> impl FnMut(&'a str) -> IResult<&'a str, Eventually<T>>
where
    S: Parser<&'a str, T, nom::error::Error<&'a str>>,
{
    boundedop((tag("eventually"), tag("<>"), tag("F")), subparser, Eventually::new)
}
