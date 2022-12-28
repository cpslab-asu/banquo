use std::error::Error;
use std::fmt::{Display, Formatter};

use crate::trace::Trace;

mod debug_robustness;
mod hybrid_distance;
mod robustness;

pub use debug_robustness::{DebugRobustness, DebugRobustnessFormula};
pub use hybrid_distance::{HybridDistance, HybridDistanceFormula, PathGuardDistance};
pub use robustness::RobustnessFormula;

#[derive(Debug)]
pub enum EvaluationError<E> {
    EmptyTrace,
    FormulaError(E),
}

impl<E> Display for EvaluationError<E>
where
    E: Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EmptyTrace => write!(f, "Empty trace provided"),
            Self::FormulaError(e) => e.fmt(f),
        }
    }
}

impl<E> Error for EvaluationError<E> where E: Error {}

type EvalResult<T, E> = Result<T, EvaluationError<E>>;

pub fn eval_robustness<F, S>(formula: F, trace: &Trace<S>) -> EvalResult<f64, F::Error>
where
    F: RobustnessFormula<S>,
{
    let robustness_trace = formula.robustness(trace).map_err(EvaluationError::FormulaError)?;
    let (_, robustness) = robustness_trace.into_iter().next().ok_or(EvaluationError::EmptyTrace)?;

    Ok(robustness)
}

pub fn eval_dbg_robustness<F, S>(formula: F, trace: &Trace<S>) -> EvalResult<DebugRobustness<F::Prev>, F::Error>
where
    F: DebugRobustnessFormula<S>,
{
    let debug_trace = formula.debug_robustness(trace).map_err(EvaluationError::FormulaError)?;
    let (_, debug_robustness) = debug_trace.into_iter().next().ok_or(EvaluationError::EmptyTrace)?;

    Ok(debug_robustness)
}

pub fn eval_hybrid_dist<F, S, L>(formula: F, trace: &Trace<(S, L)>) -> EvalResult<HybridDistance, F::Error>
where
    F: HybridDistanceFormula<S, L>,
{
    let distance_trace = formula.hybrid_distance(trace).map_err(EvaluationError::FormulaError)?;
    let (_, hybrid_distance) = distance_trace.into_iter().next().ok_or(EvaluationError::EmptyTrace)?;

    Ok(hybrid_distance)
}
