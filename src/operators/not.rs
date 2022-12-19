use std::ops::Neg;

use crate::formula::{DebugFormula, Formula, HybridDistanceFormula, Result};
use crate::formula::{DebugRobustness, HybridDistance};
use crate::trace::Trace;

#[derive(Clone, Debug)]
pub struct Not<F>(F);

impl<F> Not<F> {
    pub fn new(subformula: F) -> Self {
        Self(subformula)
    }

    pub fn subformula(&self) -> &F {
        &self.0
    }
}

fn not<S>(trace: Trace<S>) -> Trace<S::Output>
where
    S: Neg,
{
    trace.into_iter().map(|(time, state)| (time, -state)).collect()
}

impl<S, F> Formula<S> for Not<F>
where
    F: Formula<S>,
{
    type Error = F::Error;

    fn robustness(&self, trace: &Trace<S>) -> Result<f64, Self::Error> {
        self.0.robustness(trace).map(not)
    }
}

pub struct NegOf<T>(pub DebugRobustness<T>);

fn make_debug<T>((time, previous): (f64, DebugRobustness<T>)) -> (f64, DebugRobustness<NegOf<T>>) {
    let neg_robustness = DebugRobustness {
        robustness: -previous.robustness,
        previous: NegOf(previous),
    };

    (time, neg_robustness)
}

impl<S, F> DebugFormula<S> for Not<F>
where
    F: DebugFormula<S>,
{
    type Error = F::Error;
    type Prev = NegOf<F::Prev>;

    fn debug_robustness(&self, trace: &Trace<S>) -> Result<DebugRobustness<Self::Prev>, Self::Error> {
        let previous = self.0.debug_robustness(trace)?;
        let debug_trace = previous.into_iter().map(make_debug).collect();

        Ok(debug_trace)
    }
}

impl<S, L, F> HybridDistanceFormula<S, L> for Not<F>
where
    F: HybridDistanceFormula<S, L>,
{
    type Error = F::Error;

    fn hybrid_distance(&self, trace: &Trace<(S, L)>) -> Result<HybridDistance, Self::Error> {
        self.0.hybrid_distance(trace).map(not)
    }
}

#[cfg(test)]
mod tests {
    use super::Not;
    use crate::formula::Formula;
    use crate::operators::{Const, ConstError};
    use crate::trace::Trace;

    #[test]
    fn robustness() -> Result<(), ConstError> {
        let trace = Trace::from_iter([(0, 0.0), (1, 1.0), (2, 2.0), (3, 3.0)]);
        let formula = Not::new(Const(trace));

        let input = Trace::default();
        let robustness = formula.robustness(&input)?;
        let expected = Trace::from_iter([(0, 0.0), (1, -1.0), (2, -2.0), (3, -3.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }
}
