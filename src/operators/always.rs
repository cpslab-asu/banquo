use std::rc::Rc;

use super::temporal::TemporalOperator;
use crate::formula::{DebugFormula, DebugRobustness, Formula, HybridDistance, HybridDistanceFormula};
use crate::trace::Trace;

#[derive(Clone, Debug)]
pub struct Always<F>(TemporalOperator<F>);

impl<F> Always<F> {
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

impl<S, F> Formula<S> for Always<F>
where
    F: Formula<S>,
{
    type Error = F::Error;

    fn robustness(&self, trace: &Trace<S>) -> Result<Trace<f64>, Self::Error> {
        self.0.apply(trace, F::robustness, f64::INFINITY, |r_min, r_curr| {
            f64::min(r_min, *r_curr)
        })
    }
}

pub struct MinOfTimes<T>(Trace<Rc<DebugRobustness<T>>>);

fn make_debug<T>(trace: Trace<&Rc<DebugRobustness<T>>>) -> DebugRobustness<MinOfTimes<T>> {
    let mut min_robustness = f64::INFINITY;
    let mut debug_trace: Trace<Rc<DebugRobustness<T>>> = Trace::default();

    for (time, debug) in trace {
        min_robustness = f64::min(min_robustness, debug.robustness);
        debug_trace.insert_state(time, debug.clone());
    }

    DebugRobustness {
        robustness: min_robustness,
        previous: MinOfTimes(debug_trace),
    }
}

impl<S, F> DebugFormula<S> for Always<F>
where
    F: DebugFormula<S>,
{
    type Error = F::Error;
    type Prev = MinOfTimes<F::Prev>;

    fn debug_robustness(&self, trace: &Trace<S>) -> crate::formula::Result<DebugRobustness<Self::Prev>, Self::Error> {
        let debug_eval = |subformula: &F, trace: &Trace<S>| subformula.debug_robustness(trace).map(Trace::into_shared);

        self.0.apply_subtrace(trace, debug_eval, make_debug)
    }
}

impl<S, L, F> HybridDistanceFormula<S, L> for Always<F>
where
    F: HybridDistanceFormula<S, L>,
{
    type Error = F::Error;

    fn hybrid_distance(&self, trace: &Trace<(S, L)>) -> Result<Trace<HybridDistance>, Self::Error> {
        self.0
            .apply(trace, F::hybrid_distance, HybridDistance::Infinite, |d_min, d_curr| {
                HybridDistance::min(d_min, *d_curr)
            })
    }
}

#[cfg(test)]
mod tests {
    use super::Always;
    use crate::formula::Formula;
    use crate::operators::{Const, ConstError};
    use crate::trace::Trace;

    #[test]
    fn unbounded_robustness() -> Result<(), ConstError> {
        let inner = Trace::from_iter([(0, 4.0), (1, 2.0), (2, 3.0), (3, 1.0), (4, 3.0)]);
        let formula = Always::new(Const(inner), None);

        let input = Trace::default();
        let robustness = formula.robustness(&input)?;
        let expected = Trace::from_iter([(0, 1.0), (1, 1.0), (2, 1.0), (3, 1.0), (4, 3.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn bounded_robustness() -> Result<(), ConstError> {
        let inner = Trace::from_iter([(0, 4.0), (1, 2.0), (2, 3.0), (3, 1.0), (4, 3.0)]);
        let formula = Always::new(Const(inner), (0, 2));

        let input = Trace::default();
        let robustness = formula.robustness(&input)?;
        let expected = Trace::from_iter([(0, 2.0), (1, 1.0), (2, 1.0), (3, 1.0), (4, 3.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }
}
