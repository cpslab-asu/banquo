#![deny(clippy::all)]

use std::borrow::Borrow;
use std::sync::Arc;
use std::rc::Rc;

use thiserror::Error;

pub mod metrics;
pub mod operators;
pub mod predicate;
pub mod trace;

pub use crate::trace::Trace;
pub use crate::metrics::{Top, Bottom, Meet, Join};

/// Trait representing a temporal logic formula that can evaluate a timed set of system states
/// called a [`Trace`].
///
/// In general, temporal logic formulas are used to represent requirements over the behaviors of a
/// system, and are evaluated to quantify the degree to which the system satisfies or violates the
/// requirement. There are many types of metrics used for this analysis, but this library defaults
/// to [robustness] which is simply a measure of distance representing how "close" the system
/// came to violation.
///
/// Implementations of this trait are expected to maintain the length of the trace, and should be
/// able to handle input traces with no elements.
///
/// # Examples
///
/// For additional examples, please refer to the [`Predicate`](crate::predicate::Predicate) type and the
/// [`operators`](crate::operators) module.
///
/// [robustness]: https://link.springer.com/chapter/10.1007/11940197_12
///
/// ```rust
/// # use banquo::{Formula, Trace};
/// struct UnitFormula;
///
/// impl<T> Formula<T> for UnitFormula {
///     type Metric = ();
///     type Error = ();
///
///     fn evaluate(&self, trace: &Trace<T>) -> Result<Trace<Self::Metric>, Self::Error> {
///         let result = trace
///             .iter()
///             .map_states(|_| ())
///             .collect();
///
///         Ok(result)
///     }
/// }
/// ```
pub trait Formula<State> {
    /// The output metric from this formula
    type Metric;

    /// The type of error that may be produced during evaluation
    type Error;

    /// Evaluate a given trace of system states into either a trace of metric values or an error.
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
enum ErrorKind {
    #[error("Error evaluating formula: {source}")]
    FormulaError {
        source: Box<dyn std::error::Error>,
    },

    #[error("Empty trace")]
    EmptyTraceError,
}

/// Error produced while evaluating a [`Trace`] using the [`evaluate`] function.
///
/// Two conditions are represented using this error type:
///   1. An error is encountered during evaluation of the formula
///   2. The trace does not contain any elements.
#[derive(Debug, Error)]
#[error(transparent)]
pub struct EvaluationError(ErrorKind);

impl EvaluationError {
    /// Create a new evaluation error for an arbitrary inner error.
    ///
    /// The error provided this function is expected to be generated from a [`Formula`] evaluation.
    /// This function allocates on the heap.
    pub fn wrap<E>(source: E) -> Self
    where
        E: std::error::Error + 'static,
    {
        Self(ErrorKind::FormulaError { source: source.into() })
    }

    /// Create a new evaluation error when a trace does not contain any elements.
    pub fn empty() -> Self {
        Self(ErrorKind::EmptyTraceError)
    }
}

/// Evaluate a [`Trace`] using a given [`Formula`] and return the metric value for the earliest
/// time.
///
/// This function accepts either a borrowed or owned trace value, and any [`Formula`] type. The
/// trace is evaluated using the formula, which may produce an error, and then the first
/// metric is extracted from the trace , which will also produce an error if the trace is empty.
///
/// # Examples
///
/// ```rust
/// use std::collections::HashMap;
///
/// use banquo::{EvaluationError, Trace, evaluate, predicate};
/// use banquo::predicate::PredicateError;
///
/// let phi = predicate!{ x <= 10.0 };
///
/// let t1 = Trace::from([
///     (0.0, HashMap::from([("x", 8.0)])),
///     (1.0, HashMap::from([("x", 4.0)])),
/// ]);
///
/// let t2 = Trace::from([
///     (0.0, HashMap::from([("y", 8.0)])),
/// ]);
///
/// let t3: Trace<HashMap<&'static str, f64>> = Trace::new();
///
/// evaluate(&t1, &phi); // Ok -> 2.0
/// evaluate(&t2, &phi); // Error -> predicate evaluation error
/// evaluate(t3, &phi);  // Error -> empty trace
/// ```
pub fn evaluate<T, F, State, M, E>(trace: T, formula: F) -> Result<M, EvaluationError>
where
    T: Borrow<Trace<State>>,
    F: Formula<State, Metric = M, Error = E>,
    E: std::error::Error + 'static,
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
