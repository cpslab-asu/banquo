use crate::formulas::{
    DebugRobustness, DebugRobustnessFormula, HybridDistance, HybridDistanceFormula, RobustnessFormula,
};
use crate::trace::Trace;

#[derive(Clone, Debug)]
pub struct Next<F>(F);

impl<F> Next<F> {
    pub fn new(subformula: F) -> Self {
        Self(subformula)
    }

    pub fn subformula(&self) -> &F {
        &self.0
    }
}

impl<S, F> DebugRobustnessFormula<S> for Next<F>
where
    F: DebugRobustnessFormula<S>,
    F::Prev: Clone,
{
    type Error = F::Error;
    type Prev = DebugRobustness<F::Prev>;

    fn debug_robustness(&self, trace: &Trace<S>) -> Result<Trace<DebugRobustness<Self::Prev>>, Self::Error> {
        let inner = self.0.debug_robustness(trace)?;
        let mut iter = inner.into_iter().peekable();
        let mut trace = Trace::default();

        while let Some((time, debug)) = iter.next() {
            let robustness = if let Some((_, next_debug)) = iter.peek() {
                next_debug.robustness
            } else {
                f64::NEG_INFINITY
            };

            trace.insert_state(
                time,
                DebugRobustness {
                    robustness,
                    previous: debug,
                },
            );
        }

        Ok(trace)
    }
}

impl<S, F> RobustnessFormula<S> for Next<F>
where
    F: RobustnessFormula<S>,
{
    type Error = F::Error;

    fn robustness(&self, trace: &Trace<S>) -> Result<Trace<f64>, Self::Error> {
        let inner = self.0.robustness(trace)?;
        let mut iter = inner.into_iter().peekable();
        let mut trace = Trace::default();

        while let Some((time, _)) = iter.next() {
            let robustness = if let Some((_, next)) = iter.peek() {
                *next
            } else {
                f64::NEG_INFINITY
            };

            trace.insert_state(time, robustness);
        }

        Ok(trace)
    }
}

impl<S, L, F> HybridDistanceFormula<S, L> for Next<F>
where
    F: HybridDistanceFormula<S, L>,
{
    type Error = F::Error;

    fn hybrid_distance(&self, trace: &Trace<(S, L)>) -> Result<Trace<HybridDistance>, Self::Error> {
        let inner = self.0.hybrid_distance(trace)?;
        let mut iter = inner.into_iter().peekable();
        let mut trace = Trace::default();

        while let Some((time, _)) = iter.next() {
            let robustness = if let Some((_, next)) = iter.peek() {
                *next
            } else {
                HybridDistance::Robustness(f64::NEG_INFINITY)
            };

            trace.insert_state(time, robustness);
        }

        Ok(trace)
    }
}

#[cfg(test)]
mod tests {
    use super::Next;
    use crate::formulas::RobustnessFormula;
    use crate::operators::{Const, ConstError};
    use crate::trace::Trace;

    #[test]
    fn robustness() -> Result<(), ConstError> {
        let inner = Trace::from_iter([(0, 1.0), (1, 2.0), (2, 3.0), (3, 4.0)]);
        let formula = Next::new(Const(inner));

        let input = Trace::default();
        let robustness = formula.robustness(&input)?;
        let expected = Trace::from_iter([(0, 2.0), (1, 3.0), (2, 4.0), (3, f64::NEG_INFINITY)]);

        assert_eq!(robustness, expected);
        Ok(())
    }
}
