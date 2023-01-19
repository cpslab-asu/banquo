#![deny(clippy::all)]

use std::rc::Rc;
use std::sync::Arc;

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
    type Error: std::error::Error;

    /// Evaluate each state in a trace into an output value
    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<Output>, Self::Error>;
}

impl<Output, T> Formula<Output> for &T
where
    T: Formula<Output> + ?Sized,
{
    type State = T::State;
    type Error = T::Error;

    #[inline]
    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<Output>, Self::Error> {
        (**self).evaluate_states(trace)
    }
}

impl<Output, T> Formula<Output> for Box<T>
where
    T: Formula<Output> + ?Sized,
{
    type State = T::State;
    type Error = T::Error;

    #[inline]
    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<Output>, Self::Error> {
        (**self).evaluate_states(trace)
    }
}

impl<Output, T> Formula<Output> for Rc<T>
where
    T: Formula<Output> + ?Sized,
{
    type State = T::State;
    type Error = T::Error;

    #[inline]
    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<Output>, Self::Error> {
        (**self).evaluate_states(trace)
    }
}

impl<Output, T> Formula<Output> for Arc<T>
where
    T: Formula<Output> + ?Sized,
{
    type State = T::State;
    type Error = T::Error;

    #[inline]
    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<Output>, Self::Error> {
        (**self).evaluate_states(trace)
    }
}
