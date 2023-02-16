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

fn until_eval_time<M>(left: &Trace<M>, time: f64, right: M, prev: &M) -> M
where
    M: Top + Meet + for<'a> Meet<&'a M> + for<'a> Join<&'a M>,
{
    let left_metric = left.range(..=time).into_iter().fold(M::top(), |l, (_, r)| l.meet(r));
    let combined_metric = left_metric.meet(right);

    combined_metric.join(prev)
}

fn until_op<M, I>(left: Trace<M>, right: I, mut prev_time: f64, mut prev_metric: M) -> Trace<M>
where
    I: Iterator<Item = (f64, M)>,
    M: Top + Bottom + Meet + for<'a> Meet<&'a M> + for<'a> Join<&'a M>,
{
    let mut trace = Trace::default();
    let bottom = M::bottom();

    prev_metric = until_eval_time(&left, prev_time, prev_metric, &bottom);

    for (time, right_metric) in right {
        let next_metric = until_eval_time(&left, time, right_metric, &prev_metric);

        trace.insert_state(prev_time, prev_metric);
        prev_time = time;
        prev_metric = next_metric;
    }

    trace.insert_state(prev_time, prev_metric);
    trace
}

impl<Left, Right, State, Metric> Formula<State> for Until<Left, Right>
where
    Left: Formula<State, Metric = Metric>,
    Right: Formula<State, Metric = Metric>,
    Metric: Clone + Top + Bottom + Meet + for<'a> Meet<&'a Metric> + for<'a> Join<&'a Metric>,
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

        let mut iter = right_trace.into_iter().rev();

        let evaluated_trace = if let Some((prev_time, prev_metric)) = iter.next() {
            until_op(left_trace, iter, prev_time, prev_metric)
        } else {
            Trace::default()
        };

        Ok(evaluated_trace)
    }
}

#[cfg(test)]
mod tests {
    use super::{BinaryOperatorError, Until};
    use crate::formulas::Formula;
    use crate::operators::testing::{Const, ConstError};
    use crate::trace::Trace;

    #[test]
    fn test_robustness() -> Result<(), BinaryOperatorError<ConstError, ConstError>> {
        let left_trace = Trace::from_iter([(0.0, 3.0), (1.0, 1.5), (2.0, 1.4), (3.0, 1.1)]);
        let right_trace = Trace::from_iter([(0.0, -2.1), (1.0, 3.7), (2.0, 1.2), (3.0, 2.2)]);

        let formula = Until::new(Const::from(left_trace), Const::from(right_trace));
        let input: Trace<()> = Trace::default();
        let robustness = formula.evaluate_trace(&input)?;

        assert_eq!(robustness[3.0], 1.1);
        assert_eq!(robustness[2.0], 1.2);
        assert_eq!(robustness[1.0], 1.5);
        assert_eq!(robustness[0.0], 1.5);

        Ok(())
    }
}
