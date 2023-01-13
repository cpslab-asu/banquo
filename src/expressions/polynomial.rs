use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::ops::Add;

use super::{Expression, VariableMap};

#[derive(Debug)]
pub struct PolynomialError {
    variable_name: String,
    equation: String,
}

impl Display for PolynomialError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Missing variable {} in polynomial {}", self.variable_name, self.equation)
    }
}

impl Error for PolynomialError {}

#[derive(Clone, Debug, PartialEq)]
pub struct Polynomial {
    terms: HashMap<String, f64>,
    constant: f64,
}

impl Polynomial {
    pub fn new<T, I>(terms: I, constant: f64) -> Self
    where
        I: IntoIterator<Item = (T, f64)>,
        T: AsRef<str>,
    {
        let terms_iter = terms
            .into_iter()
            .map(|(name, coefficient)| (name.as_ref().to_string(), coefficient));

        Self {
            terms: HashMap::from_iter(terms_iter),
            constant: 0.0,
        }
    }

    pub fn add_term<T>(&mut self, name: T, coefficient: f64) -> Option<f64>
    where
        T: Into<String>
    {
        self.terms.insert(name.into(), coefficient)
    }

    pub fn set_constant(&mut self, value: f64) -> f64 {
        let prev_const = self.constant;
        self.constant = value;
        
        prev_const
    }
}

impl Display for Polynomial {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut sorted_entries = self.terms.iter().collect::<Vec<(&String, &f64)>>();
        sorted_entries.sort_by(|(a, _), (b, _)| a.cmp(b)); // Sort the coefficients that the output is deterministic

        for (variable, coefficient) in sorted_entries {
            write!(f, "{} * {} + ", f64::to_string(coefficient), variable)?;
        }

        write!(f, "{}", self.constant)
    }
}

impl From<f64> for Polynomial {
    fn from(value: f64) -> Self {
        Polynomial::new::<&str, _>([], value)
    }
}

impl<T, const N: usize> From<[(T, f64); N]> for Polynomial
where
    T: AsRef<str>,
{
    fn from(terms: [(T, f64); N]) -> Self {
        Polynomial::from_iter(terms)
    }
}

impl<T> FromIterator<(T, f64)> for Polynomial
where
    T: AsRef<str>
{
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = (T, f64)>,
    {
        Polynomial::new(iter, 0.0)
    }
}

impl Add for Polynomial {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let mut combined_terms = self.terms;

        for (term_name, rhs_coeff) in rhs.terms {
            let lhs_coeff = combined_terms
                .get(&term_name)
                .cloned()
                .unwrap_or(0.0);

            combined_terms.insert(term_name, lhs_coeff + rhs_coeff);
        }

        Polynomial {
            terms: combined_terms,
            constant: self.constant + rhs.constant,
        }
    }
}

impl Expression for Polynomial {
    type Error = PolynomialError;

    fn evaluate_state(&self, variable_map: &VariableMap) -> Result<f64, Self::Error> {
        let variable_values = self
            .terms
            .iter()
            .map(|(variable_name, coefficient)| match variable_map.get(variable_name) {
                Some(value) => Ok(coefficient * value),
                None => Err(PolynomialError {
                    variable_name: variable_name.clone(),
                    equation: self.to_string(),
                }),
            })
            .collect::<Result<Vec<_>, _>>();

        let variable_sum: f64 = variable_values?.into_iter().sum();

        Ok(variable_sum + self.constant)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::error::Error;

    use super::Polynomial;
    use crate::expressions::Expression;

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
}
