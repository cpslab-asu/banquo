#![deny(clippy::all)]

pub mod automaton;
pub mod expressions;
pub mod operators;
pub mod parser;
pub mod trace;
pub mod formulas;

pub use crate::trace::Trace;
