#![deny(clippy::all)]

pub mod automaton;
pub mod expressions;
pub mod operators;
pub mod parser;
pub mod trace;
pub mod formulas;

pub use crate::formula::{Formula, HybridDistance, HybridDistanceFormula, Result};
pub use crate::trace::{States, Times, Trace};
