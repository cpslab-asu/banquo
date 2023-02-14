use std::collections::HashMap;

type VariableMap = HashMap<String, f64>;

mod hybrid_predicate;
mod polynomial;
mod predicate;

pub use hybrid_predicate::HybridPredicate;
pub use polynomial::{Polynomial, PolynomialError, Term};
pub use predicate::{Predicate, PredicateError};
