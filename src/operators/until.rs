use crate::Trace;
use crate::formula::{Formula, HybridDistance, HybridDistanceFormula, Result};
use super::binary::BinaryOperatorError;

pub struct Until<L, R> {
    left: L,
    right: R,
}

impl<L, R> Until<L, R> {
    pub fn new(left: L, right: R) -> Self {
        Self { left, right }
    }
} 

fn until<T, F, G>(
    left_trace: Trace<T>,
    right_trace: Trace<T>,
    trace_initial: T,
    subtrace_initial: T,
    f: F,
    g: G,
) -> Trace<T>
where
    T: Copy,
    F: Fn(T, T) -> T,
    G: Fn(T, T) -> T,
{
    let mut elements = Vec::new();
    let mut last_rob = trace_initial;

    for (time, right_rob) in right_trace.into_iter().rev() {
        let left_rob = left_trace
            .range(..=time)
            .into_iter()
            .fold(subtrace_initial.clone(), |m, (_, &r)| f(m, r));

        let time_rob = f(left_rob, right_rob);

        last_rob = g(time_rob, last_rob);
        elements.push((time, last_rob));
    }

    Trace::from_iter(elements)
}

impl<L, R, S> Formula<S> for Until<L, R>
where
    L: Formula<S>,
    R: Formula<S>,
{
    type Error = BinaryOperatorError<L::Error, R::Error>;

    fn robustness(&self, trace: &Trace<S>) -> Result<f64, Self::Error> {
        let left_trace = self.left.robustness(trace).map_err(BinaryOperatorError::LeftError)?;
        let right_trace = self.right.robustness(trace).map_err(BinaryOperatorError::RightError)?;
        let trace = until(
            left_trace,
            right_trace,
            f64::NEG_INFINITY,
            f64::INFINITY,
            f64::min,
            f64::max,
        );

        Ok(trace)
    }
}

impl<L, R, S, T> HybridDistanceFormula<S, T> for Until<L, R>
where
    L: HybridDistanceFormula<S, T>,
    R: HybridDistanceFormula<S, T>,
{
    type Error = BinaryOperatorError<L::Error, R::Error>;

    fn hybrid_distance(&self, trace: &Trace<(S, T)>) -> Result<HybridDistance, Self::Error> {
        let left_trace = self.left.hybrid_distance(trace).map_err(BinaryOperatorError::LeftError)?;
        let right_trace = self.right.hybrid_distance(trace).map_err(BinaryOperatorError::RightError)?;
        let trace = until(
            left_trace,
            right_trace,
            HybridDistance::Robustness(f64::INFINITY),
            HybridDistance::Infinite,
            HybridDistance::min,
            HybridDistance::max,
        );

        Ok(trace)
    }
}

