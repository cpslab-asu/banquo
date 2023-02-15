#![deny(clippy::all)]

pub mod automaton;
pub mod expressions;
pub mod formulas;
pub mod metric;
pub mod operators;
pub mod trace;

#[cfg(feature = "parser")]
pub mod parser;

use std::error::Error;
use std::fmt::{Display, Formatter};

use formulas::Formula;
use metric::HybridDistance;
use trace::Trace;

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

type EvalResult<'a, M> = Result<M, EvaluationError<'a>>;

pub fn eval_robustness<'a, F, S>(formula: F, trace: impl AsRef<Trace<S>>) -> EvalResult<'a, f64>
where
    F: Formula<S, Metric = f64>,
    F::Error: 'a,
{
    let robustness_trace = formula
        .evaluate_states(trace.as_ref())
        .map_err(|err| EvaluationError::FormulaError(Box::new(err)))?;
    let (_, rho) = robustness_trace
        .into_iter()
        .next()
        .ok_or_else(|| EvaluationError::EmptyTrace)?;

    Ok(rho)
}

pub fn eval_hybrid_distance<'a, F, S>(formula: F, trace: impl AsRef<Trace<S>>) -> EvalResult<'a, HybridDistance>
where
    F: Formula<S, Metric = HybridDistance>,
    F::Error: 'a,
{
    let distance_trace = formula
        .evaluate_states(trace.as_ref())
        .map_err(|err| EvaluationError::FormulaError(Box::new(err)))?;
    let (_, distance) = distance_trace
        .into_iter()
        .next()
        .ok_or_else(|| EvaluationError::EmptyTrace)?;

    Ok(distance)
}
