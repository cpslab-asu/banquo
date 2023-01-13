use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

use super::{Expression, VariableMap};
use super::polynomial::{Polynomial, PolynomialError};
use crate::formulas::RobustnessFormula;
use crate::trace::Trace;

#[derive(Debug)]
pub struct PredicateError {
    inner: PolynomialError,
    time: f64,
}

impl Display for PredicateError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "error {} at time {}", &self.inner, self.time)
    }
}

impl Error for PredicateError {}

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
        Predicate {
            left: left.into(),
            right: right.into(),
        }
    }

    pub fn simple(name: &str, coefficient: f64, bound: f64) -> Self {
        Predicate {
            left: Polynomial::from([(name, coefficient)]),
            right: Polynomial::from(bound),
        }
    }
}

impl Display for Predicate {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} <= {}", &self.left, &self.right)
    }
}

impl Expression for Predicate {
    type Error = PolynomialError;

    fn evaluate_state(&self, variable_map: &VariableMap) -> Result<f64, Self::Error> {
        let right = self.right.evaluate_state(variable_map)?;
        let left = self.left.evaluate_state(variable_map)?;

        Ok(right - left)
    }
}

impl RobustnessFormula<VariableMap> for Predicate {
    type Error = PredicateError;

    fn robustness(&self, trace: &Trace<VariableMap>) -> Result<Trace<f64>, Self::Error> {
        let eval_timed_state = |(time, state)| -> Result<(f64, f64), PredicateError> {
            self.evaluate_state(state)
                .map(|robustness| (time, robustness))
                .map_err(|inner| PredicateError { inner, time })
        };

        let robustness = trace
            .into_iter()
            .map(eval_timed_state)
            .collect::<Result<Trace<_>, _>>()?;

        Ok(robustness)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::error::Error;

    use super::Expression;
    use super::{Polynomial, Predicate};

    #[test]
    fn polynomial_sum() -> Result<(), Box<dyn Error>> {
        let polynomial = Polynomial::new([("x", 1.0), ("y", 2.0)], 2.0);
        let variable_map = HashMap::from([("x".to_string(), 3.0), ("y".to_string(), 5.0)]);
        let sum = polynomial.evaluate_state(&variable_map)?;

        assert_eq!(sum, 15.0);
        Ok(())
    }

    #[test]
    fn polynomial_to_string() {
        let polynomial = Polynomial::new([("x", 1.0), ("y", 2.0)], 2.0);
        let expected = "1 * x + 2 * y + 2";

        assert_eq!(polynomial.to_string(), expected);
    }

    #[test]
    fn predicate_robustness() -> Result<(), Box<dyn Error>> {
        let left = Polynomial::new([("a", 1.0), ("b", 1.0)], None); // sum is exprected to be 7
        let right = Polynomial::new([("x", 1.0), ("y", -1.0)], 2.0); // sum is expected to be 10
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
        let left = Polynomial::new([("a", 1.0), ("b", 1.0)], None); // sum is exprected to be 7
        let right = Polynomial::new([("x", 1.0), ("y", -1.0)], 2.0); // sum is expected to be 10
        let predicate = Predicate::new(left, right);
        let expected = "1 * a + 1 * b + 0 <= 1 * x + -1 * y + 2";

        assert_eq!(predicate.to_string(), expected);
    }
}
