#![deny(clippy::all)]

pub mod automaton;
pub mod predicate;
pub use predicate::{HybridDistance, HybridPredicate, HybridState};
