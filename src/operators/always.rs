use std::rc::Rc;

use super::temporal::TemporalOperator;
use crate::formulas::{
    DebugRobustness, DebugRobustnessFormula, HybridDistance, HybridDistanceFormula, RobustnessFormula,
};
use crate::trace::Trace;

#[derive(Clone, Debug)]
pub struct Always<F>(TemporalOperator<F>);

impl<F> Always<F> {
    pub fn new_unbounded(subformula: F) -> Self {
        let operator = TemporalOperator {
            subformula,
            t_bounds: None,
        };

        Self(operator)
    }

    pub fn new_bounded<B>(subformula: F, (lower, upper): (B, B)) -> Self
    where
        B: Into<f64>,
    {
        let t_bounds = (lower.into(), upper.into());
        let operator = TemporalOperator {
            subformula,
            t_bounds: Some(t_bounds),
        };

        Self(operator)
    }

    pub fn subformula(&self) -> &F {
        &self.0.subformula
    }

    pub fn t_bounds(&self) -> &Option<(f64, f64)> {
        &self.0.t_bounds
    }
}

impl<S, F> RobustnessFormula<S> for Always<F>
where
    F: RobustnessFormula<S>,
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

impl<S, F> DebugRobustnessFormula<S> for Always<F>
where
    F: DebugRobustnessFormula<S>,
{
    type Error = F::Error;
    type Prev = MinOfTimes<F::Prev>;

    fn debug_robustness(&self, trace: &Trace<S>) -> Result<Trace<DebugRobustness<Self::Prev>>, Self::Error> {
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
        let initial = HybridDistance::Robustness(f64::INFINITY);
        let combine = |d_min: HybridDistance, d: &HybridDistance| HybridDistance::min(d_min, *d);

        self.0.apply(trace, F::hybrid_distance, initial, combine)
    }
}

#[cfg(test)]
mod tests {
    use super::Always;
    use crate::formulas::RobustnessFormula;
    use crate::operators::{Const, ConstError};
    use crate::trace::Trace;

    #[test]
    fn unbounded_robustness() -> Result<(), ConstError> {
        let inner = Trace::from_iter([(0, 4.0), (1, 2.0), (2, 3.0), (3, 1.0), (4, 3.0)]);
        let formula = Always::new_unbounded(Const(inner));

        let input = Trace::default();
        let robustness = formula.robustness(&input)?;
        let expected = Trace::from_iter([(0, 1.0), (1, 1.0), (2, 1.0), (3, 1.0), (4, 3.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn bounded_robustness() -> Result<(), ConstError> {
        let inner = Trace::from_iter([(0, 4.0), (1, 2.0), (2, 3.0), (3, 1.0), (4, 3.0)]);
        let formula = Always::new_bounded(Const(inner), (0, 2));

        let input = Trace::default();
        let robustness = formula.robustness(&input)?;
        let expected = Trace::from_iter([(0, 2.0), (1, 1.0), (2, 1.0), (3, 1.0), (4, 3.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }
}
