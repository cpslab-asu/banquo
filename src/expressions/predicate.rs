use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

use super::{Expression, VariableMap};
use crate::formula::Formula;
use crate::trace::Trace;

pub struct PolynomialError {
    variable_name: String,
}

impl Debug for PolynomialError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "PolynomialError(variable_name={})", self.variable_name)
    }
}

impl Display for PolynomialError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Missing variable {}", self.variable_name)
    }
}

impl Error for PolynomialError {}

#[derive(Clone, Debug, PartialEq)]
pub struct Polynomial {
    coefficients: VariableMap,
    constant: f64,
}

impl Polynomial {
    pub fn new(coefficients: VariableMap) -> Self {
        Self {
            coefficients,
            constant: 0f64,
        }
    }

    pub fn with_constant(coefficients: VariableMap, constant: f64) -> Self {
        Self { coefficients, constant }
    }
}

impl Display for Polynomial {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut sorted_entries = self.coefficients.iter().collect::<Vec<(&String, &f64)>>();
        sorted_entries.sort_by(|(a, _), (b, _)| a.cmp(b)); // Sort the coefficients that the output is deterministic

        for (variable, coefficient) in sorted_entries {
            write!(f, "{} * {} + ", f64::to_string(coefficient), variable)?;
        }

        write!(f, "{}", self.constant)
    }
}

impl Expression for Polynomial {
    type Error = PolynomialError;

    fn evaluate_state(&self, variable_map: &VariableMap) -> Result<f64, Self::Error> {
        let variable_values = self
            .coefficients
            .iter()
            .map(|(variable_name, coefficient)| match variable_map.get(variable_name) {
                Some(value) => Ok(coefficient * value),
                None => Err(PolynomialError {
                    variable_name: variable_name.clone(),
                }),
            })
            .collect::<Result<Vec<_>, _>>();

        let variable_sum: f64 = variable_values?.into_iter().sum();

        Ok(variable_sum + self.constant)
    }
}

/// Representation of a predicate composed of two polynomials with left <= right.
#[derive(Clone, Debug, PartialEq)]
pub struct Predicate {
    left: Polynomial,
    right: Polynomial,
}

impl Predicate {
    pub fn new(left: Polynomial, right: Polynomial) -> Self {
        Predicate { left, right }
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

impl Formula<VariableMap> for Predicate {
    type Error = PolynomialError;

    fn robustness(&self, trace: &Trace<VariableMap>) -> crate::formula::Result<f64, Self::Error> {
        let robustness = trace
            .into_iter()
            .map(|(time, state)| self.evaluate_state(state).map(|robustness| (time, robustness)))
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
        let coefficients = HashMap::from([("x".to_string(), 1.0), ("y".to_string(), 2.0)]);
        let polynomial = Polynomial::with_constant(coefficients, 2.0);

        let variable_map = HashMap::from([("x".to_string(), 3.0), ("y".to_string(), 5.0)]);
        let sum = polynomial.evaluate_state(&variable_map)?;

        assert_eq!(sum, 15.0);
        Ok(())
    }

    #[test]
    fn polynomial_to_string() {
        let coefficients = HashMap::from([("x".to_string(), 1.0), ("y".to_string(), 2.0)]);
        let polynomial = Polynomial::with_constant(coefficients, 2.0);
        let expected = "1 * x + 2 * y + 2";

        assert_eq!(polynomial.to_string(), expected);
    }

    #[test]
    fn predicate_robustness() -> Result<(), Box<dyn Error>> {
        let left_coefficients = HashMap::from([("a".to_string(), 1.0), ("b".to_string(), 1.0)]);
        let left = Polynomial::new(left_coefficients); // sum is exprected to be 7

        let right_coefficients = HashMap::from([("x".to_string(), 1.0), ("y".to_string(), -1.0)]);
        let right = Polynomial::with_constant(right_coefficients, 2.0); // sum is expected to be 10
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
        let left_coefficients = HashMap::from([("a".to_string(), 1.0), ("b".to_string(), 1.0)]);
        let left = Polynomial::new(left_coefficients); // sum is exprected to be 7

        let right_coefficients = HashMap::from([("x".to_string(), 1.0), ("y".to_string(), -1.0)]);
        let right = Polynomial::with_constant(right_coefficients, 2.0); // sum is expected to be 10
        let predicate = Predicate::new(left, right);
        let expected = "1 * a + 1 * b + 0 <= 1 * x + -1 * y + 2";

        assert_eq!(predicate.to_string(), expected);
    }
}
