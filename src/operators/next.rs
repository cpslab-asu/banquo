use crate::formula::{DebugFormula, DebugRobustness, Formula, HybridDistance, HybridDistanceFormula};
use crate::trace::Trace;

#[derive(Clone, Debug)]
pub struct Next<F> {
    subformula: F,
}

impl<F> Next<F> {
    pub fn new<S>(subformula: F) -> Self
    where
        F: Formula<S>,
    {
        Self { subformula }
    }
}

impl<S, F> DebugFormula<S> for Next<F>
where
    F: DebugFormula<S>,
    F::Prev: Clone,
{
    type Error = F::Error;
    type Prev = DebugRobustness<F::Prev>;

    fn debug_robustness(&self, trace: &Trace<S>) -> crate::formula::Result<DebugRobustness<Self::Prev>, Self::Error> {
        let inner = self.subformula.debug_robustness(trace)?;
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

impl<S, F> Formula<S> for Next<F>
where
    F: Formula<S>,
{
    type Error = F::Error;

    fn robustness(&self, trace: &Trace<S>) -> Result<Trace<f64>, Self::Error> {
        let inner = self.subformula.robustness(trace)?;
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
        let inner = self.subformula.hybrid_distance(trace)?;
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
    use crate::formula::Formula;
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
