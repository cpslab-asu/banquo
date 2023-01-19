#![deny(clippy::all)]

use std::error::Error;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use std::sync::Arc;

pub mod automaton;
pub mod expressions;
pub mod formulas;
pub mod operators;
pub mod parser;
pub mod trace;

use formulas::HybridDistance;

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

#[derive(Debug)]
pub enum EvaluationError<'a> {
    EmptyTrace,
    FormulaError(Box<dyn Error + 'a>),
}

impl<'a> Display for EvaluationError<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EmptyTrace => write!(f, "Cannot evaluate an empty trace"),
            Self::FormulaError(err) => Display::fmt(err, f),
        }
    }
}

impl<'a> Error for EvaluationError<'a> {}

pub fn eval_robustness<'a, F>(formula: F, trace: impl AsRef<Trace<F::State>>) -> Result<f64, EvaluationError<'a>>
where
    F: Formula<f64>,
    F::Error: 'a,
{
    let robustness_trace = formula.evaluate_states(trace.as_ref()).map_err(|err| EvaluationError::FormulaError(Box::new(err)))?;
    let (_, rho) = robustness_trace
        .into_iter()
        .next()
        .ok_or_else(|| EvaluationError::EmptyTrace)?;

    Ok(rho)
}

pub fn eval_hybrid_distance<'a, F>(formula: F, trace: impl AsRef<Trace<F::State>>) -> Result<HybridDistance, EvaluationError<'a>>
where
    F: Formula<HybridDistance>,
    F::Error: 'a,
{
    let distance_trace = formula.evaluate_states(trace.as_ref()).map_err(|err| EvaluationError::FormulaError(Box::new(err)))?;
    let (_, distance) = distance_trace
        .into_iter()
        .next()
        .ok_or_else(|| EvaluationError::EmptyTrace)?;

    Ok(distance)
}
