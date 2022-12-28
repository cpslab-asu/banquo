#![deny(clippy::all)]

pub mod automaton;
pub mod expressions;
pub mod formulas;
pub mod operators;
pub mod parser;
pub mod trace;

pub use crate::trace::Trace;
