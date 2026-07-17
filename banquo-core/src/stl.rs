use std::collections::VecDeque;

use thiserror::Error;

use crate::operators;
use crate::predicate::Predicate;
use crate::trace::Trace;

type Not = operators::Not<()>;
type Next = operators::Next<()>;
type Always = operators::Always<()>;
type Eventually = operators::Eventually<()>;
type And = operators::And<(), ()>;
type Or = operators::Or<(), ()>;
type Implies = operators::Implies<(), ()>;
type Until = operators::Until<(), ()>;

#[derive(Clone, Debug, PartialEq)]
pub enum Symbol {
    Predicate(Predicate),
    Not,
    Next,
    Always(Option<operators::Interval>),
    Eventually(Option<operators::Interval>),
    And,
    Or,
    Implies,
    Until,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Formula {
    stack: VecDeque<Symbol>,
}

impl Formula {
    fn prepend(self, op: Symbol) -> Self {
        let mut stack = self.stack;
        stack.push_front(op);

        Self { stack }
    }

    fn concat(self, other: Formula) -> Self {
        let mut stack = self.stack;
        stack.extend(other.stack);

        Self { stack }
    }
}

pub struct IntoSymbols(std::collections::vec_deque::IntoIter<Symbol>);

impl Iterator for IntoSymbols {
    type Item = Symbol;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl IntoIterator for Formula {
    type Item = Symbol;
    type IntoIter = IntoSymbols;

    fn into_iter(self) -> Self::IntoIter {
        IntoSymbols(self.stack.into_iter())
    }
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

impl<T> crate::Formula<T> for Formula
where
    T: crate::predicate::VariableSet,
{
    type Error = EvaluationError;
    type Metric = f64;

    fn evaluate(&self, trace: &Trace<T>) -> Result<Trace<Self::Metric>, Self::Error> {
        let mut stack = VecDeque::new();
        let ops = self.stack.iter().rev();

        for op in ops {
            let new_value: Result<Trace<f64>, EvaluationErrorKind> = match op {
                Symbol::Predicate(p) => p
                    .evaluate(trace)
                    .map_err(|e| EvaluationErrorKind::OperatorError(Box::new(e))),
                Symbol::Not => unary_op(&mut stack, |trace| -> Result<Trace<f64>, EvaluationErrorKind> {
                    Ok(Not::apply(trace))
                }),
                Symbol::Next => unary_op(&mut stack, |trace| -> Result<Trace<f64>, EvaluationErrorKind> {
                    Ok(Next::apply(trace))
                }),
                Symbol::Always(bounds) => unary_op(&mut stack, |value| Always::apply(bounds.clone(), value)),
                Symbol::Eventually(bounds) => unary_op(&mut stack, |value| Eventually::apply(bounds.clone(), value)),
                Symbol::And => binary_op(&mut stack, And::apply),
                Symbol::Or => binary_op(&mut stack, Or::apply),
                Symbol::Implies => binary_op(&mut stack, Implies::apply),
                Symbol::Until => binary_op(&mut stack, |lhs, rhs| -> Result<Trace<f64>, EvaluationErrorKind> {
                    Ok(Until::apply(lhs, rhs))
                }),
            };

            stack.push_front(new_value?);
        }

        stack
            .pop_front()
            .ok_or(EvaluationError::from(EvaluationErrorKind::EmptyStack))
    }
}

impl From<Symbol> for Formula {
    fn from(op: Symbol) -> Self {
        let mut stack = VecDeque::new();
        stack.push_back(op);

        Self { stack }
    }
}

impl<const N: usize> From<[Symbol; N]> for Formula {
    fn from(ops: [Symbol; N]) -> Self {
        Self { stack: VecDeque::from(ops) }
    }
}

impl FromIterator<Symbol> for Formula {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = Symbol>,
    {
        Self {
            stack: VecDeque::from_iter(iter),
        }
    }
}

pub trait ToSTL {
    fn to_stl(&self) -> Formula;
}

impl ToSTL for Predicate {
    fn to_stl(&self) -> Formula {
        Formula {
            stack: VecDeque::from([Symbol::Predicate(self.clone())]),
        }
    }
}

impl<F> ToSTL for operators::Not<F>
where
    F: ToSTL,
{
    fn to_stl(&self) -> Formula {
        self.inner().to_stl().prepend(Symbol::Not)
    }
}

impl<F> ToSTL for operators::Always<F>
where
    F: ToSTL,
{
    fn to_stl(&self) -> Formula {
        self.inner().to_stl().prepend(Symbol::Always(self.bounds().cloned()))
    }
}

impl<F> ToSTL for operators::Eventually<F>
where
    F: ToSTL,
{
    fn to_stl(&self) -> Formula {
        self.inner()
            .to_stl()
            .prepend(Symbol::Eventually(self.bounds().cloned()))
    }
}

impl<L, R> ToSTL for operators::And<L, R>
where
    L: ToSTL,
    R: ToSTL,
{
    fn to_stl(&self) -> Formula {
        self.left().to_stl().concat(self.right().to_stl()).prepend(Symbol::And)
    }
}

impl<L, R> ToSTL for operators::Or<L, R>
where
    L: ToSTL,
    R: ToSTL,
{
    fn to_stl(&self) -> Formula {
        self.left().to_stl().concat(self.right().to_stl()).prepend(Symbol::Or)
    }
}

impl<L, R> ToSTL for operators::Implies<L, R>
where
    L: ToSTL,
    R: ToSTL,
{
    fn to_stl(&self) -> Formula {
        self.ante()
            .to_stl()
            .concat(self.cons().to_stl())
            .prepend(Symbol::Implies)
    }
}

impl<T> ToSTL for &T
where
    T: ToSTL,
{
    fn to_stl(&self) -> Formula {
        (*self).to_stl()
    }
}

impl<T> From<T> for Formula
where
    T: ToSTL,
{
    fn from(value: T) -> Self {
        value.to_stl()
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::Symbol;
    use crate::operators;
    use crate::predicate;
    use crate::Formula;
    use crate::Trace;

    fn make_trace() -> Trace<HashMap<&'static str, f64>> {
        let values = [(0, (0.0, 1.0)), (1, (1.0, 0.0)), (2, (2.0, 4.0)), (3, (3.0, 6.0))];

        values
            .into_iter()
            .map(|(time, (x, y))| (time, HashMap::from([("x", x), ("y", y)])))
            .collect()
    }

    #[test]
    fn not_equivalence() {
        let p1 = predicate! { x <= 5.0 };
        let formula = operators::Not::new(p1.clone());
        let expected = super::Formula::from([Symbol::Not, Symbol::Predicate(p1)]);
        let converted = super::Formula::from(&formula);

        assert_eq!(expected, converted);

        let trace = make_trace();

        assert_eq!(formula.evaluate(&trace).unwrap(), expected.evaluate(&trace).unwrap())
    }

    #[test]
    fn always_equivalence() {
        let p1 = predicate! { x <= 5.0 };
        let formula = operators::Always::unbounded(p1.clone());
        let expected = super::Formula::from([Symbol::Always(None), Symbol::Predicate(p1)]);
        let converted = super::Formula::from(&formula);

        assert_eq!(expected, converted);

        let trace = make_trace();

        assert_eq!(formula.evaluate(&trace).unwrap(), expected.evaluate(&trace).unwrap())
    }

    #[test]
    fn eventually_equivalence() {
        let p1 = predicate! { x <= 5.0 };
        let formula = operators::Eventually::unbounded(p1.clone());
        let expected = super::Formula::from([Symbol::Eventually(None), Symbol::Predicate(p1)]);
        let converted = super::Formula::from(&formula);

        assert_eq!(expected, converted);

        let trace = make_trace();

        assert_eq!(formula.evaluate(&trace).unwrap(), expected.evaluate(&trace).unwrap())
    }

    #[test]
    fn and_equivalence() {
        let p1 = predicate! { x <= 5.0 };
        let p2 = predicate! { y <= 10.0 };
        let formula = operators::And::new(p1.clone(), p2.clone());
        let expected = super::Formula::from([Symbol::And, Symbol::Predicate(p1), Symbol::Predicate(p2)]);
        let converted = super::Formula::from(&formula);

        assert_eq!(expected, converted);

        let trace = make_trace();

        assert_eq!(formula.evaluate(&trace).unwrap(), expected.evaluate(&trace).unwrap())
    }

    #[test]
    fn or_equivalence() {
        let p1 = predicate! { x <= 5.0 };
        let p2 = predicate! { y <= 10.0 };
        let formula = operators::Or::new(p1.clone(), p2.clone());
        let expected = super::Formula::from([Symbol::Or, Symbol::Predicate(p1), Symbol::Predicate(p2)]);
        let converted = super::Formula::from(&formula);

        assert_eq!(expected, converted);

        let trace = make_trace();

        assert_eq!(formula.evaluate(&trace).unwrap(), expected.evaluate(&trace).unwrap())
    }

    #[test]
    fn implies_equivalence() {
        let p1 = predicate! { x <= 5.0 };
        let p2 = predicate! { y <= 10.0 };
        let formula = operators::Implies::new(p1.clone(), p2.clone());
        let expected = super::Formula::from([Symbol::Implies, Symbol::Predicate(p1), Symbol::Predicate(p2)]);
        let converted = super::Formula::from(&formula);

        assert_eq!(expected, converted);

        let trace = make_trace();

        assert_eq!(formula.evaluate(&trace).unwrap(), expected.evaluate(&trace).unwrap())
    }
}
