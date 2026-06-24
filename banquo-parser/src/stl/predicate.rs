use std::ops::Add;

use banquo_core::predicate::{Predicate, Term};
use chumsky::prelude::*;

use crate::num;

struct Terms(std::vec::IntoIter<Term>);

impl Iterator for Terms {
    type Item = Term;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl Terms {
    fn negate_variables(self) -> impl Iterator<Item = Term> {
        self.map(|term| match term {
            Term::Variable(name, value) => Term::Variable(name, -value),
            t => t,
        })
    }

    fn negate_constants(self) -> impl Iterator<Item = Term> {
        self.map(|term| match term {
            Term::Constant(c) => Term::Constant(-c),
            t => t,
        })
    }
}

#[derive(Debug, PartialEq)]
struct Sum(Vec<Term>);

impl Sum {
    fn into_terms(self) -> Terms {
        Terms(self.0.into_iter())
    }
}

impl From<Term> for Sum {
    fn from(term: Term) -> Self {
        Self(vec![term])
    }
}

impl Add<Term> for Sum {
    type Output = Self;

    fn add(self, rhs: Term) -> Self::Output {
        let mut terms = self.0;
        terms.push(rhs);

        Self(terms)
    }
}

fn variable<'src>() -> impl Parser<'src, &'src str, (String, f64)> + Clone {
    let mul = just("*").padded();
    let name = text::ascii::ident().map(|name: &'src str| name.to_string());

    choice((
        num().then_ignore(mul).then(name).map(|(val, var)| (var, val)),
        name.then_ignore(mul).then(num()),
        just("-")
            .to(-1.0)
            .or_not()
            .then(name)
            .map(|(sign, var)| (var, sign.unwrap_or(1.0))),
    ))
}

fn term<'src>() -> impl Parser<'src, &'src str, Term> + Clone {
    choice((
        variable().map(|(name, val)| Term::Variable(name, val)),
        num().map(Term::Constant),
    ))
}

#[derive(Clone, Debug)]
enum SumOp {
    ADD,
    SUB,
}

fn sum<'src>() -> impl Parser<'src, &'src str, Sum> + Clone {
    let sum_ops = choice((just("+").padded().to(SumOp::ADD), just("-").padded().to(SumOp::SUB)));
    let op_term = sum_ops.then(term());
    let combine = |sum: Sum, (op, term): (SumOp, Term)| -> Sum {
        match op {
            SumOp::ADD => sum + term,
            SumOp::SUB => sum + -term,
        }
    };

    term().map(Sum::from).foldl(op_term.repeated(), combine)
}

#[derive(Clone, Debug)]
enum CmpOp {
    LTE,
    GTE,
}

pub fn predicate<'src>() -> impl Parser<'src, &'src str, Predicate> + Clone {
    let cmp_ops = choice((just("<=").to(CmpOp::LTE), just(">=").to(CmpOp::GTE))).padded();

    sum().then(cmp_ops).then(sum()).map(|((lhs, op), rhs)| -> Predicate {
        match op {
            CmpOp::LTE => lhs
                .into_terms()
                .negate_constants()
                .chain(rhs.into_terms().negate_variables())
                .collect(),
            CmpOp::GTE => lhs
                .into_terms()
                .negate_variables()
                .chain(rhs.into_terms().negate_constants())
                .collect(),
        }
    })
}

#[cfg(test)]
mod tests {
    use banquo_core::predicate::Term;
    use chumsky::Parser;

    struct Case<T, E> {
        actual: chumsky::ParseResult<T, E>,
        expected: T,
    }

    impl<T, E> Case<T, E>
    where
        T: std::fmt::Debug + PartialEq,
        E: std::fmt::Debug + PartialEq,
    {
        fn run(self) {
            assert_eq!(self.actual.into_result(), Ok(self.expected))
        }
    }

    #[test]
    fn test_variable() {
        let parser = super::variable();
        let cases = [
            Case {
                actual: parser.parse("4.0 * foo"),
                expected: ("foo".to_string(), 4.0),
            },
            Case {
                actual: parser.parse("bar * 2.9"),
                expected: ("bar".to_string(), 2.9),
            },
            Case {
                actual: parser.parse("baz"),
                expected: ("baz".to_string(), 1.0),
            },
            Case {
                actual: parser.parse("-spam"),
                expected: ("spam".to_string(), -1.0),
            },
            Case {
                actual: parser.parse("-3.4 * eggs"),
                expected: ("eggs".to_string(), -3.4),
            },
        ];

        for case in cases {
            case.run()
        }
    }

    #[test]
    fn test_sum() {
        let parser = super::sum();
        let cases = [
            Case {
                actual: parser.parse("1.0"),
                expected: super::Sum(vec![Term::Constant(1.0)]),
            },
            Case {
                actual: parser.parse("foo"),
                expected: super::Sum(vec![Term::Variable("foo".to_string(), 1.0)]),
            },
            Case {
                actual: parser.parse("4.0 * bar"),
                expected: super::Sum(vec![Term::Variable("bar".to_string(), 4.0)]),
            },
            Case {
                actual: parser.parse("baz * 2.9"),
                expected: super::Sum(vec![Term::Variable("baz".to_string(), 2.9)]),
            },
            Case {
                actual: parser.parse("x + 1.9 * y - z * 3.8 + 5.5"),
                expected: super::Sum(vec![
                    Term::Variable("x".to_string(), 1.0),
                    Term::Variable("y".to_string(), 1.9),
                    Term::Variable("z".to_string(), -3.8),
                    Term::Constant(5.5),
                ]),
            },
        ];

        for case in cases {
            case.run()
        }
    }
}
