use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::ops::{Add, AddAssign};

use super::{Expression, VariableMap};

#[derive(Clone, Debug, PartialEq)]
enum TermType {
    Variable {
        name: String,
        coefficient: f64,
    },
    Constant {
        value: f64
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Term(TermType);

impl Term {
    pub fn variable<Name, Coeff>(name: Name, coefficient: Coeff) -> Self
    where
        Name: Into<String>,
        Coeff: Into<f64>,
    {
        let inner = TermType::Variable {
            name: name.into(),
            coefficient: coefficient.into(),
        };

        Self(inner)
    }

    pub fn constant<C>(value: C) -> Self
    where
        C: Into<f64>,
    {
        let inner = TermType::Constant {
            value: value.into(),
        };

        Self(inner)
    }
}

impl Add for Term {
    type Output = Polynomial;

    fn add(self, rhs: Self) -> Self::Output {
        let mut terms = HashMap::new();
        let mut constant = 0.0;

        match self.0 {
            TermType::Variable { name, coefficient } => {
                terms.insert(name, coefficient);
            }
            TermType::Constant { value } => {
                constant += value;
            }
        };

        match rhs.0 {
            TermType::Variable { name, coefficient } => {
                terms.insert(name, coefficient);
            }
            TermType::Constant { value } => {
                constant += value;
            }
        };

        Polynomial { terms, constant }
    }
}

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

impl Default for Polynomial {
    fn default() -> Self {
        Self {
            terms: HashMap::default(),
            constant: f64::default(),
        }
    }
}

impl From<Term> for Polynomial {
    fn from(value: Term) -> Polynomial {
        let mut terms = HashMap::new();
        let mut constant = 0.0;

        match value.0 {
            TermType::Variable { name, coefficient } => {
                terms.insert(name, coefficient);
            },
            TermType::Constant { value } => {
                constant += value;
            }
        };

        Polynomial { terms, constant }
    }
}

impl<const N: usize> From<[Term; N]> for Polynomial {
    fn from(value: [Term; N]) -> Self {
        Self::from_iter(value.into_iter())
    }
}

impl FromIterator<Term> for Polynomial {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = Term>,
    {
        iter.into_iter().fold(Self::default(), |p, t| p + t)
    }
}

impl Add<Term> for Polynomial {
    type Output = Self;

    fn add(self, rhs: Term) -> Self::Output {
        self + Into::<Polynomial>::into(rhs)
    }
}

impl AddAssign for Polynomial {
    fn add_assign(&mut self, rhs: Self) {
        for (term_name, rhs_coeff) in rhs.terms {
            let lhs_coeff = self.terms
                .get(&term_name)
                .cloned()
                .unwrap_or(0.0);

            self.terms.insert(term_name, lhs_coeff + rhs_coeff);
        }

        self.constant += rhs.constant;
    }
}

impl Add for Polynomial {
    type Output = Self;

    fn add(mut self, rhs: Self) -> Self::Output {
        self += rhs;
        self
    }
}

impl Extend<Term> for Polynomial {
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = Term>,    
    {
        *self += Polynomial::from_iter(iter);
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

    use super::{Polynomial, Term};
    use crate::expressions::Expression;

    #[test]
    fn polynomial_sum() -> Result<(), Box<dyn Error>> {
        let polynomial = Polynomial::from([
            Term::variable("x", 1.0),
            Term::variable("y", 2.0),
            Term::constant(2.0)
        ]);
        let variable_map = HashMap::from([("x".to_string(), 3.0), ("y".to_string(), 5.0)]);
        let sum = polynomial.evaluate_state(&variable_map)?;

        assert_eq!(sum, 15.0);
        Ok(())
    }

    #[test]
    fn polynomial_to_string() {
        let polynomial = Polynomial::from([
            Term::variable("x", 1.0),
            Term::variable("y", 2.0),
            Term::constant(2.0),
        ]);
        let expected = "1 * x + 2 * y + 2";

        assert_eq!(polynomial.to_string(), expected);
    }
}
