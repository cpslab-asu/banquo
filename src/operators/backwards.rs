use super::binary::BinaryOperatorError;
use crate::formulas::HybridDistance;
use crate::trace::Trace;
use crate::Formula;

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

impl<Left, Right, State> Formula<f64> for Until<Left, Right>
where
    Left: Formula<f64, State = State>,
    Right: Formula<f64, State = State>,
{
    type State = State;
    type Error = BinaryOperatorError<Left::Error, Right::Error>;

    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<f64>, Self::Error> {
        let left_trace = self
            .left
            .evaluate_states(trace)
            .map_err(BinaryOperatorError::LeftError)?;
        let right_trace = self
            .right
            .evaluate_states(trace)
            .map_err(BinaryOperatorError::RightError)?;
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

impl<Left, Right, State> Formula<HybridDistance> for Until<Left, Right>
where
    Left: Formula<HybridDistance, State = State>,
    Right: Formula<HybridDistance, State = State>,
{
    type State = State;
    type Error = BinaryOperatorError<Left::Error, Right::Error>;

    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<HybridDistance>, Self::Error> {
        let left_trace = self
            .left
            .evaluate_states(trace)
            .map_err(BinaryOperatorError::LeftError)?;
        let right_trace = self
            .right
            .evaluate_states(trace)
            .map_err(BinaryOperatorError::RightError)?;
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

#[cfg(test)]
mod tests {
    use super::{BinaryOperatorError, Until};
    use crate::operators::testing::{Const, ConstError};
    use crate::trace::Trace;
    use crate::Formula;

    #[test]
    fn test_robustness() -> Result<(), BinaryOperatorError<ConstError, ConstError>> {
        let left_trace = Trace::from_iter([(0.0, 3.0), (1.0, 1.5), (2.0, 1.4), (3.0, 1.1)]);
        let right_trace = Trace::from_iter([(0.0, -2.1), (1.0, 3.7), (2.0, 1.2), (3.0, 2.2)]);

        let formula = Until::new(Const::from(left_trace), Const::from(right_trace));
        let input = Trace::default();
        let robustness = formula.evaluate_states(&input)?;

        assert_eq!(robustness[3.0], 1.1);
        assert_eq!(robustness[2.0], 1.2);
        assert_eq!(robustness[1.0], 1.5);
        assert_eq!(robustness[0.0], 1.5);

        Ok(())
    }
}
