mod debug_robustness;
mod hybrid_distance;
mod robustness;

use std::error::Error;
use std::rc::Rc;
use std::sync::Arc;

use crate::trace::Trace;

pub use debug_robustness::DebugRobustness;
pub use hybrid_distance::{HybridDistance, PathGuardDistance};

/// Evaluate a trace of states into their associated metric values
pub trait Formula<State> {
    /// The type of metric produced by evaluating the trace
    type Metric;

    /// The type of error that can be generated during evaluation
    type Error: Error;

    /// Evaluate each state in a trace into an output value
    fn evaluate_states(&self, trace: &Trace<State>) -> Result<Trace<Self::Metric>, Self::Error>;
}

impl<T, F> Formula<T> for &F
where
    F: Formula<T> + ?Sized,
{
    type Metric = F::Metric;
    type Error = F::Error;

    #[inline]
    fn evaluate_states(&self, trace: &Trace<T>) -> Result<Trace<Self::Metric>, Self::Error> {
        (**self).evaluate_states(trace)
    }
}

impl<T, F> Formula<T> for Box<F>
where
    F: Formula<T> + ?Sized,
{
    type Metric = F::Metric;
    type Error = F::Error;

    #[inline]
    fn evaluate_states(&self, trace: &Trace<T>) -> Result<Trace<Self::Metric>, Self::Error> {
        (**self).evaluate_states(trace)
    }
}

impl<T, F> Formula<T> for Rc<F>
where
    F: Formula<T> + ?Sized,
{
    type Metric = F::Metric;
    type Error = F::Error;

    #[inline]
    fn evaluate_states(&self, trace: &Trace<T>) -> Result<Trace<Self::Metric>, Self::Error> {
        (**self).evaluate_states(trace)
    }
}

impl<T, F> Formula<T> for Arc<F>
where
    F: Formula<T> + ?Sized,
{
    type Metric = F::Metric;
    type Error = F::Error;

    #[inline]
    fn evaluate_states(&self, trace: &Trace<T>) -> Result<Trace<Self::Metric>, Self::Error> {
        (**self).evaluate_states(trace)
    }
}
