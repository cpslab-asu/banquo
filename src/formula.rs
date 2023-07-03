use std::error::Error;

use thiserror::Error;

use crate::trace::Trace;

pub trait Formula<State> {
    type Metric;
    type Error: Error;
    fn evaluate(&self, trace: &Trace<State>) -> Result<Trace<Self::Metric>, Self::Error>;
}

#[derive(Debug, Error)]
pub enum EvaluationError<E>
where
    E: Error,
{
    #[error("Error evaluation formula: {0}")]
    FormulaError(E),

    #[error("Cannot evaluate empty trace")]
    EmptyTrace,
}

pub fn evaluate<F, State>(formula: F, trace: impl AsRef<Trace<State>>) -> Result<F::Metric, EvaluationError<F::Error>>
where
    F: Formula<State>,
{
    formula
        .evaluate(trace.as_ref())
        .map_err(EvaluationError::FormulaError)
        .and_then(|evaluated| evaluated.into_iter().next().ok_or(EvaluationError::EmptyTrace))
        .map(|first| first.1)
}
