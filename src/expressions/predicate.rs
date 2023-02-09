use std::borrow::Borrow;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;

use super::polynomial::{Polynomial, SumError, Term};
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
    pub fn evaluate_state<K>(&self, state: &HashMap<K, f64>) -> Result<f64, PredicateError>
    where
        K: Eq + Hash + Borrow<str>,
    {
        let right = self.right.sum(state).map_err(PredicateError::left)?;
        let left = self.left.sum(state).map_err(PredicateError::right)?;

        Ok(right - left)
    }
}

impl<K> Formula<HashMap<K, f64>> for Predicate
where
    K: Eq + Hash + Borrow<str>,
{
    type Metric = f64;
    type Error = TimedPredicateError;

    fn evaluate_trace(&self, trace: &Trace<HashMap<K, f64>>) -> Result<Trace<Self::Metric>, Self::Error> {
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

    use super::{Predicate, PredicateError, TimedPredicateError};
    use crate::expressions::{Polynomial, Term};
    use crate::formulas::Formula;
    use crate::trace::Trace;

    #[test]
    fn evaluate_state() -> Result<(), PredicateError> {
        let left = Polynomial::from([Term::variable("a", 1.0), Term::variable("b", 1.0)]); // sum is exprected to be 7
        let right = Polynomial::from([Term::variable("x", 1.0), Term::variable("y", -1.0), Term::constant(2.0)]); // sum is expected to be 10
        let predicate = Predicate::new(left, right);

        let variable_map = HashMap::from([("a", 3.0), ("b", 4.0), ("x", 10.0), ("y", 2.0)]);
        let robustness = predicate.evaluate_state(&variable_map)?;

        assert_eq!(robustness, 3.0);
        Ok(())
    }

    #[test]
    fn evaluate_trace() -> Result<(), TimedPredicateError> {
        let left = Polynomial::from([Term::variable("a", 1.0), Term::variable("b", 1.0)]);
        let right = Polynomial::from([Term::variable("x", 1.0), Term::variable("y", -1.0), Term::constant(2.0)]);
        let predicate = Predicate::new(left, right);

        let trace: Trace<HashMap<&str, f64>> = Trace::from_iter([
            (0.0, HashMap::from([("a", 3.0), ("b", 4.0), ("x", 10.0), ("y", 2.0)])),
            (1.0, HashMap::from([("a", 2.0), ("b", 6.0), ("x", 1.0), ("y", -2.0)])),
            (2.0, HashMap::from([("a", 7.0), ("b", 7.0), ("x", 7.0), ("y", 7.0)])),
            (3.0, HashMap::from([("a", -1.0), ("b", 4.2), ("x", 1.1), ("y", 2.7)])),
        ]);

        let result = predicate.evaluate_trace(&trace)?;
        let expected = Trace::from_iter([(0.0, 3.0), (1.0, -3.0), (2.0, -12.0), (3.0, -1.9)]);

        assert_eq!(result, expected);
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
