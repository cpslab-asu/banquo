use std::borrow::Borrow;
use std::sync::Arc;
use std::rc::Rc;

use thiserror::Error;

pub mod metrics;
pub mod operators;
pub mod trace;

pub use crate::trace::Trace;
pub use crate::metrics::{Top, Bottom, Meet, Join};

pub trait Formula<State> {
    type Metric;
    type Error;
    fn evaluate(&self, trace: &Trace<State>) -> Result<Trace<Self::Metric>, Self::Error>;
}

impl<State, T> Formula<State> for &T
where
    T: Formula<State> + ?Sized,
{
    type Metric = T::Metric;
    type Error = T::Error;

    fn evaluate(&self, trace: &Trace<State>) -> Result<Trace<Self::Metric>, Self::Error> {
        (**self).evaluate(trace)
    }
}

impl<State, T> Formula<State> for Box<T>
where
    T: Formula<State> + ?Sized,
{
    type Metric = T::Metric;
    type Error = T::Error;

    fn evaluate(&self, trace: &Trace<State>) -> Result<Trace<Self::Metric>, Self::Error> {
        (**self).evaluate(trace)
    }
}

impl<State, T> Formula<State> for Arc<T>
where
    T: Formula<State> + ?Sized,
{
    type Metric = T::Metric;
    type Error = T::Error;

    fn evaluate(&self, trace: &Trace<State>) -> Result<Trace<Self::Metric>, Self::Error> {
        (**self).evaluate(trace)
    }
}

impl<State, T> Formula<State> for Rc<T>
where
    T: Formula<State> + ?Sized,
{
    type Metric = T::Metric;
    type Error = T::Error;

    fn evaluate(&self, trace: &Trace<State>) -> Result<Trace<Self::Metric>, Self::Error> {
        (**self).evaluate(trace)
    }
}

#[derive(Debug, Error)]
enum ErrorKind<Err> {
    #[error("Error evaluating formula: {0}")]
    FormulaError(Err),

    #[error("Empty trace")]
    EmptyTraceError,
}

#[derive(Debug, Error)]
#[error(transparent)]
pub struct EvaluationError<Err>(ErrorKind<Err>);

impl<Err> EvaluationError<Err> {
    fn wrap(err: Err) -> Self {
        Self(ErrorKind::FormulaError(err))
    }

    fn empty() -> Self {
        Self(ErrorKind::EmptyTraceError)
    }
}

pub fn evaluate<T, F, State>(trace: T, formula: F) -> Result<F::Metric, EvaluationError<F::Error>>
where
    T: Borrow<Trace<State>>,
    F: Formula<State>,
{
    formula
        .evaluate(trace.borrow())
        .map_err(EvaluationError::wrap)
        .and_then(|trace| {
            trace
                .into_iter()
                .next()
                .map(|(_, metric)| metric)
                .ok_or_else(EvaluationError::empty)
        })
}
