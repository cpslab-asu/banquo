//! Compatibility layer: parser types that map to banquo-core.

use std::collections::HashMap;

use banquo_core::predicate::{Predicate as CorePredicate, Term as CoreTerm, VariableSet};
use banquo_core::{Formula, Trace};

/// State type for traces: map from variable names to values.
pub type Variables = HashMap<String, f64>;

/// Term in a polynomial (variable or constant). Mirrors core's Term for parser use.
#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Variable(String, f64),
    Constant(f64),
}

impl Term {
    pub fn variable(name: impl Into<String>, coeff: f64) -> Self {
        Term::Variable(name.into(), coeff)
    }

    pub fn constant(value: f64) -> Self {
        Term::Constant(value)
    }
}

impl From<Term> for CoreTerm {
    fn from(t: Term) -> Self {
        match t {
            Term::Variable(name, value) => CoreTerm::Variable(name, value),
            Term::Constant(value) => CoreTerm::Constant(value),
        }
    }
}

/// Sum of terms (LHS or RHS of an inequality).
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Polynomial(Vec<Term>);

impl From<Term> for Polynomial {
    fn from(t: Term) -> Self {
        Polynomial(vec![t])
    }
}

impl<const N: usize> From<[Term; N]> for Polynomial {
    fn from(terms: [Term; N]) -> Self {
        Polynomial(terms.into_iter().collect())
    }
}

impl Polynomial {
    pub fn extend<I>(&mut self, other: I)
    where
        I: IntoIterator<Item = Term>,
    {
        self.0.extend(other);
    }
}

/// Predicate of the form left <= right, built from two polynomials.
#[derive(Clone, Debug, PartialEq)]
pub struct Predicate(CorePredicate);

impl Predicate {
    pub fn new(left: Polynomial, right: Polynomial) -> Self {
        let mut p = CorePredicate::new();
        for t in left.0 {
            p += CoreTerm::from(t);
        }
        for t in right.0 {
            p -= CoreTerm::from(t);
        }
        Self(p)
    }
}

impl<State> Formula<State> for Predicate
where
    State: VariableSet,
{
    type Metric = f64;
    type Error = banquo_core::predicate::FormulaError;

    fn evaluate(&self, trace: &Trace<State>) -> Result<Trace<Self::Metric>, Self::Error> {
        self.0.evaluate(trace)
    }
}
