use super::binary::BinaryOperatorError;
use crate::formulas::Formula;
use crate::metric::{Bottom, Join, Meet, Top};
use crate::trace::Trace;

pub struct Until<Left, Right> {
    left: Left,
    right: Right,
}

impl<Left, Right> Until<Left, Right> {
    pub fn new(left: Left, right: Right) -> Self {
        Self { left, right }
    }
}

impl<Left, Right, State, Metric> Formula<State> for Until<Left, Right>
where
    Left: Formula<State, Metric = Metric>,
    Right: Formula<State, Metric = Metric>,
    Metric: Top + Bottom + for<'a> Meet<&'a Metric> + Meet + for<'a> Join<&'a Metric>,
{
    type Metric = Metric;
    type Error = BinaryOperatorError<Left::Error, Right::Error>;

    fn evaluate_trace(&self, trace: &Trace<State>) -> Result<Trace<Self::Metric>, Self::Error> {
        let left_trace = self
            .left
            .evaluate_trace(trace)
            .map_err(BinaryOperatorError::LeftError)?;

        let right_trace = self
            .right
            .evaluate_trace(trace)
            .map_err(BinaryOperatorError::RightError)?;

        let mut evaluated_trace = Trace::default();
        let mut last_metric = Metric::bottom();

        for (time, right_metric) in right_trace.into_iter().rev() {
            let left_metric = left_trace
                .range(..=time)
                .into_iter()
                .fold(Metric::top(), |l, (_, r)| l.meet(r));

            let combined_metric = left_metric.meet(right_metric);
            let next_metric = combined_metric.join(&last_metric);

            evaluated_trace.insert_state(time, last_metric);
            last_metric = next_metric;
        }

        Ok(evaluated_trace)
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
