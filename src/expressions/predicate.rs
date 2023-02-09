use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

use super::polynomial::{Polynomial, SumError, Term, VarMap};
use crate::formulas::Formula;
use crate::trace::Trace;

#[derive(Debug, Clone, Copy)]
enum ErrorSide {
    Left,
    Right,
}

#[derive(Debug, Clone)]
pub struct PredicateError {
    inner: SumError,
    side: ErrorSide,
}

impl PredicateError {
    fn left(error: SumError) -> Self {
        Self {
            inner: error,
            side: ErrorSide::Left,
        }
    }

    fn right(error: SumError) -> Self {
        Self {
            inner: error,
            side: ErrorSide::Right,
        }
    }
}

impl Display for PredicateError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let side_str = match self.side {
            ErrorSide::Left => "left",
            ErrorSide::Right => "right",
        };

        write!(f, "{} side error: {}", side_str, &self.inner)
    }
}

impl Error for PredicateError {}

#[derive(Debug, Clone)]
pub struct TimedPredicateError {
    inner: PredicateError,
    time: f64,
}

impl Display for TimedPredicateError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "at time {} - {}", self.time, self.inner)
    }
}

impl Error for TimedPredicateError {}

/// Representation of a predicate composed of two polynomials with left <= right.
#[derive(Clone, Debug, PartialEq)]
pub struct Predicate {
    left: Polynomial,
    right: Polynomial,
}

impl Predicate {
    pub fn new<L, R>(left: L, right: R) -> Self
    where
        L: Into<Polynomial>,
        R: Into<Polynomial>,
    {
        Self {
            left: left.into(),
            right: right.into(),
        }
    }

    pub fn simple(name: &str, coefficient: f64, bound: f64) -> Self {
        Self::new(Term::variable(name, coefficient), Term::constant(bound))
    }
}

impl Display for Predicate {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} <= {}", &self.left, &self.right)
    }
}

impl Predicate {
    pub fn evaluate_state<V>(&self, state: &V) -> Result<f64, PredicateError>
    where
        V: VarMap,
    {
        let right = self.right.sum(state).map_err(PredicateError::left)?;
        let left = self.left.sum(state).map_err(PredicateError::right)?;

        Ok(right - left)
    }
}

impl<V> Formula<V> for Predicate
where
    V: VarMap,
{
    type Metric = f64;
    type Error = TimedPredicateError;

    fn evaluate_trace(&self, trace: &Trace<V>) -> Result<Trace<Self::Metric>, Self::Error> {
        let eval_timed_state = |(time, state)| -> Result<(f64, f64), TimedPredicateError> {
            self.evaluate_state(state)
                .map(|robustness| (time, robustness))
                .map_err(|inner| TimedPredicateError { inner, time })
        };

        trace.into_iter().map(eval_timed_state).collect::<Result<Trace<_>, _>>()
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::error::Error;

    use super::Expression;
    use super::{Polynomial, Predicate, Term};

    #[test]
    fn predicate_robustness() -> Result<(), Box<dyn Error>> {
        let left = Polynomial::from([Term::variable("a", 1.0), Term::variable("b", 1.0)]); // sum is exprected to be 7
        let right = Polynomial::from([Term::variable("x", 1.0), Term::variable("y", -1.0), Term::constant(2.0)]); // sum is expected to be 10
        let predicate = Predicate::new(left, right);

        let variable_map = HashMap::from([
            ("a".to_string(), 3.0),
            ("b".to_string(), 4.0),
            ("x".to_string(), 10.0),
            ("y".to_string(), 2.0),
        ]);
        let robustness = predicate.evaluate_state(&variable_map)?;

        assert_eq!(robustness, 3.0);
        Ok(())
    }

    #[test]
    fn predicate_to_string() {
        let left = Polynomial::from([Term::variable("a", 1.0), Term::variable("b", 1.0)]); // sum is exprected to be 7
        let right = Polynomial::from([Term::variable("x", 1.0), Term::variable("y", -1.0), Term::constant(2.0)]); // sum is expected to be 10
        let predicate = Predicate::new(left, right);
        let expected = "1 * a + 1 * b + 0 <= 1 * x + -1 * y + 2";

        assert_eq!(predicate.to_string(), expected);
    }
}
