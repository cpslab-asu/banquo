#![deny(clippy::all)]

pub mod automaton;
pub mod expressions;
pub mod formulas;
pub mod operators;
pub mod parser;
pub mod trace;

pub use crate::trace::Trace;

/// Evaluate a trace of states into their associated output values
pub trait Formula<Output> {
    /// The type of state the formula can evaluate
    type State;

    /// The type of error that can be generated during evaluation
    type Error;

    /// Evaluate each state in a trace into an output value
    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<Output>, Self::Error>;
}
