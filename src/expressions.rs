use std::collections::HashMap;
use std::error::Error;

type VariableMap = HashMap<String, f64>;

pub trait Expression {
    type Error: Error;
    fn evaluate_state(&self, variable_map: &VariableMap) -> Result<f64, Self::Error>;
}

mod hybrid_predicate;
mod predicate;

pub use hybrid_predicate::HybridPredicate;
pub use predicate::{Polynomial, PolynomialError, Predicate};
