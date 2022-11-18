use std::rc::Rc;

use super::temporal::TemporalOperator;
use crate::formula::{DebugFormula, DebugRobustness, Formula, HybridDistance, HybridDistanceFormula};
use crate::trace::Trace;

pub struct Eventually<F>(TemporalOperator<F>);

impl<F> Eventually<F> {
    pub fn new<B, S>(subformula: F, t_bounds: B) -> Self
    where
        B: Into<Option<(usize, usize)>>,
        F: Formula<S>,
    {
        Self(TemporalOperator {
            subformula,
            t_bounds: t_bounds.into(),
        })
    }
}

impl<S, F> Formula<S> for Eventually<F>
where
    F: Formula<S>,
{
    type Error = F::Error;

    fn robustness(&self, trace: &Trace<S>) -> Result<Trace<f64>, Self::Error> {
        self.0.apply(trace, F::robustness, f64::NEG_INFINITY, |r_max, &r_curr| {
            f64::max(r_max, r_curr)
        })
    }
}

pub struct MaxOfTimes<T>(Trace<Rc<DebugRobustness<T>>>);

fn make_debug<T>(trace: Trace<&Rc<DebugRobustness<T>>>) -> DebugRobustness<MaxOfTimes<T>> {
    let mut min_robustness = f64::NEG_INFINITY;
    let mut debug_trace: Trace<Rc<DebugRobustness<T>>> = Trace::default();

    for (time, debug) in trace {
        min_robustness = f64::max(min_robustness, debug.robustness);
        debug_trace.insert_state(time, debug.clone());
    }

    DebugRobustness {
        robustness: min_robustness,
        previous: MaxOfTimes(debug_trace),
    }
}

impl<S, F> DebugFormula<S> for Eventually<F>
where
    F: DebugFormula<S>,
{
    type Error = F::Error;
    type Prev = MaxOfTimes<F::Prev>;

    fn debug_robustness(&self, trace: &Trace<S>) -> crate::formula::Result<DebugRobustness<Self::Prev>, Self::Error> {
        let debug_eval = |subformula: &F, trace: &Trace<S>| subformula.debug_robustness(trace).map(Trace::into_shared);

        self.0.apply_subtrace(trace, debug_eval, make_debug)
    }
}

impl<S, L, F> HybridDistanceFormula<S, L> for Eventually<F>
where
    F: HybridDistanceFormula<S, L>,
{
    type Error = F::Error;

    fn hybrid_distance(&self, trace: &Trace<(S, L)>) -> Result<Trace<HybridDistance>, Self::Error> {
        self.0
            .apply(trace, F::hybrid_distance, HybridDistance::Infinite, |d_max, &d_curr| {
                HybridDistance::max(d_max, d_curr)
            })
    }
}

#[cfg(test)]
mod tests {
    use super::Eventually;
    use crate::formula::Formula;
    use crate::operators::{Const, ConstError};
    use crate::trace::Trace;

    #[test]
    fn unbounded_robustness() -> Result<(), ConstError> {
        let inner = Trace::from_iter([(0, 4.0), (1, 2.0), (2, 3.0), (3, 1.0), (4, 3.0)]);
        let formula = Eventually::new(Const(inner), None);

        let input = Trace::default();
        let robustness = formula.robustness(&input)?;
        let expected = Trace::from_iter([(0, 4.0), (1, 3.0), (2, 3.0), (3, 3.0), (4, 3.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn bounded_robustness() -> Result<(), ConstError> {
        let inner = Trace::from_iter([(0, 4.0), (1, 2.0), (2, 1.0), (3, 5.0), (4, 3.0)]);
        let formula = Eventually::new(Const(inner), (0, 2));

        let input = Trace::default();
        let robustness = formula.robustness(&input)?;
        let expected = Trace::from_iter([(0, 4.0), (1, 5.0), (2, 5.0), (3, 5.0), (4, 3.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }
}
