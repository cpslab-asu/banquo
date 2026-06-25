mod predicate;

use std::collections::VecDeque;
use std::ops::RangeInclusive;

use banquo_core::predicate::{Predicate, VariableSet};
use banquo_core::trace::Trace;
use banquo_core::Formula;
use chumsky::prelude::*;
use thiserror::Error;

use crate::ltl;
use crate::mtl;
use predicate::predicate;

type Not = banquo_core::operators::Not<()>;
type Always = banquo_core::operators::Always<()>;
type Eventually = banquo_core::operators::Eventually<()>;
type And = banquo_core::operators::And<(), ()>;
type Or = banquo_core::operators::Or<(), ()>;
type Implies = banquo_core::operators::Implies<(), ()>;

#[derive(Clone, Debug, PartialEq)]
enum Op {
    Predicate(Predicate),
    Not,
    Always(Option<RangeInclusive<f64>>),
    Eventually(Option<RangeInclusive<f64>>),
    And,
    Or,
    Implies,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ParsedFormula {
    stack: VecDeque<Op>,
}

#[derive(Debug, Error)]
enum EvaluationErrorKind {
    #[error("Error evaluating formula: {0}")]
    OperatorError(Box<dyn std::error::Error>),

    #[error("Evaluation stack was unexpectedly empty")]
    EmptyStack,
}

#[derive(Debug, Error)]
#[error(transparent)]
pub struct EvaluationError {
    #[from]
    kind: EvaluationErrorKind,
}

fn unary_op<F, E>(stack: &mut VecDeque<Trace<f64>>, op: F) -> Result<Trace<f64>, EvaluationErrorKind>
where
    F: Fn(Trace<f64>) -> Result<Trace<f64>, E>,
    E: std::error::Error + 'static,
{
    stack
        .pop_front()
        .ok_or(EvaluationErrorKind::EmptyStack)
        .and_then(|value| op(value).map_err(|e| EvaluationErrorKind::OperatorError(Box::new(e))))
}

fn binary_op<F, E>(stack: &mut VecDeque<Trace<f64>>, op: F) -> Result<Trace<f64>, EvaluationErrorKind>
where
    F: Fn(Trace<f64>, Trace<f64>) -> Result<Trace<f64>, E>,
    E: std::error::Error + 'static,
{
    let lhs = stack.pop_front().ok_or(EvaluationErrorKind::EmptyStack)?;
    let rhs = stack.pop_front().ok_or(EvaluationErrorKind::EmptyStack)?;

    op(lhs, rhs).map_err(|e| EvaluationErrorKind::OperatorError(Box::new(e)))
}

impl<T> Formula<T> for ParsedFormula
where
    T: VariableSet,
{
    type Error = EvaluationError;
    type Metric = f64;

    fn evaluate(&self, trace: &Trace<T>) -> Result<Trace<Self::Metric>, Self::Error> {
        let mut stack = VecDeque::new();
        let ops = self.stack.iter().rev();

        for op in ops {
            let new_value: Result<Trace<f64>, EvaluationErrorKind> = match op {
                Op::Predicate(p) => p
                    .evaluate(trace)
                    .map_err(|e| EvaluationErrorKind::OperatorError(Box::new(e))),
                Op::Not => unary_op(&mut stack, |value| -> Result<Trace<f64>, EvaluationErrorKind> {
                    Ok(Not::apply(value))
                }),
                Op::Always(bounds) => unary_op(&mut stack, |value| Always::apply(bounds.as_ref(), value)),
                Op::Eventually(bounds) => unary_op(&mut stack, |value| Eventually::apply(bounds.as_ref(), value)),
                Op::And => binary_op(&mut stack, And::apply),
                Op::Or => binary_op(&mut stack, Or::apply),
                Op::Implies => binary_op(&mut stack, Implies::apply),
            };

            stack.push_front(new_value?);
        }

        stack
            .pop_front()
            .ok_or(EvaluationError::from(EvaluationErrorKind::EmptyStack))
    }
}

impl From<Op> for ParsedFormula {
    fn from(op: Op) -> Self {
        let mut stack = VecDeque::new();
        stack.push_back(op);

        Self { stack }
    }
}

impl<const N: usize> From<[Op; N]> for ParsedFormula {
    fn from(ops: [Op; N]) -> Self {
        Self { stack: VecDeque::from(ops) }
    }
}

impl ParsedFormula {
    fn prepend(self, op: Op) -> Self {
        let mut stack = self.stack;
        stack.push_front(op);

        Self { stack }
    }

    fn concat(self, other: ParsedFormula) -> Self {
        let mut stack = self.stack;
        stack.extend(other.stack);

        Self { stack }
    }
}

fn formula<'src>() -> impl Parser<'src, &'src str, ParsedFormula> {
    recursive(|expr| {
        let predicate = predicate().padded().map(|p| ParsedFormula::from(Op::Predicate(p)));
        let subformula = expr.delimited_by(just("(").padded(), just(")").padded());
        let atom = predicate.or(subformula);

        let unary_op = choice((
            ltl::neg().to(Op::Not),
            mtl::always().map(|bounds| Op::Always(bounds)),
            mtl::eventually().map(|bounds| Op::Eventually(bounds)),
        ));
        let unary = unary_op.repeated().foldr(atom, |op, formula| formula.prepend(op));

        let binary_op = choice((
            ltl::and().to(Op::And),
            ltl::or().to(Op::Or),
            ltl::implies().to(Op::Implies),
        ));
        let binary = unary.clone().foldl(binary_op.then(unary).repeated(), |lhs, (op, rhs)| {
            lhs.concat(rhs).prepend(op)
        });

        binary
    })
}

#[derive(Debug, PartialEq)]
enum ParseErrorKind {
    Unknown,
}

impl Default for ParseErrorKind {
    fn default() -> Self {
        Self::Unknown
    }
}

#[derive(Debug, Default, PartialEq)]
pub struct ParseError {
    kind: ParseErrorKind,
}

pub fn parse(phi: &str) -> Result<ParsedFormula, ParseError> {
    formula().parse(phi).into_result().map_err(|_| ParseError::default())
}

#[cfg(test)]
mod tests {
    use super::{Op, ParsedFormula};
    use banquo_core::predicate;

    #[test]
    fn test_predicate() {
        let p = predicate! { 3.0 * x <= 11.0 };
        let expected = ParsedFormula::from([Op::Predicate(p)]);

        assert_eq!(super::parse("3.0 * x <= 11.0"), Ok(expected));
    }

    #[test]
    fn test_negation() {
        let p = predicate! { 3.0 * x <= 11.0 };
        let expected = Ok(ParsedFormula::from([Op::Not, Op::Predicate(p)]));

        assert_eq!(super::parse("not 3.0 * x <= 11.0"), expected);
        assert_eq!(super::parse("!(3.0 * x <= 11.0)"), expected);
    }

    #[test]
    fn test_conjunction() {
        let p1 = predicate! { 3.0 * x <= 11.0 };
        let p2 = predicate! { -2.0 * y + z <= 4.0 };
        let expected = Ok(ParsedFormula::from([
            Op::And,
            Op::Predicate(p1.clone()),
            Op::Predicate(p2.clone()),
        ]));

        assert_eq!(super::parse("3.0 * x <= 11.0 && -2.0 * y + z <= 4.0"), expected);
        assert_eq!(super::parse("(3.0 * x <= 11.0) and -2.0 * y + z <= 4.0"), expected);
        assert_eq!(super::parse(r"(3.0 * x <= 11.0) /\ -2.0 * y + z <= 4.0"), expected);

        let p3 = predicate! { a <= 5.0 };
        let expected = ParsedFormula::from([
            Op::And,
            Op::And,
            Op::Predicate(p1),
            Op::Predicate(p2),
            Op::Predicate(p3),
        ]);

        assert_eq!(
            super::parse("(3.0 * x <= 11.0 && -2.0 * y + z <= 4.0) and a <= 5.0"),
            Ok(expected)
        );
    }
}
