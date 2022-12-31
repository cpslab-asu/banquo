use std::ops::{Neg, Deref};

use crate::formulas::{
    DebugRobustness, DebugRobustnessFormula, HybridDistance, HybridDistanceFormula, RobustnessFormula,
};
use crate::trace::Trace;

#[derive(Clone, Debug)]
pub struct UnaryOperator<F> {
    pub subformula: F
}

#[derive(Clone, Debug)]
pub struct Not<F>(UnaryOperator<F>);

impl<F> Not<F> {
    pub fn new(subformula: F) -> Self {
        Self(UnaryOperator { subformula })
    }
}

impl<F> Deref for Not<F> {
    type Target = UnaryOperator<F>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

fn not<S>(trace: Trace<S>) -> Trace<S::Output>
where
    S: Neg,
{
    trace.into_iter().map_states(|value| -value).collect()
}

impl<S, F> RobustnessFormula<S> for Not<F>
where
    F: RobustnessFormula<S>,
{
    type Error = F::Error;

    fn robustness(&self, trace: &Trace<S>) -> Result<Trace<f64>, Self::Error> {
        self.subformula.robustness(trace).map(not)
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

impl<S, F> DebugRobustnessFormula<S> for Not<F>
where
    F: DebugRobustnessFormula<S>,
{
    type Error = F::Error;
    type Prev = NegOf<F::Prev>;

    fn debug_robustness(&self, trace: &Trace<S>) -> Result<Trace<DebugRobustness<Self::Prev>>, Self::Error> {
        let previous = self.subformula.debug_robustness(trace)?;
        let debug_trace = previous.into_iter().map(make_debug).collect();

        Ok(debug_trace)
    }
}

impl<S, L, F> HybridDistanceFormula<S, L> for Not<F>
where
    F: HybridDistanceFormula<S, L>,
{
    type Error = F::Error;

    fn hybrid_distance(&self, trace: &Trace<(S, L)>) -> Result<Trace<HybridDistance>, Self::Error> {
        self.subformula.hybrid_distance(trace).map(not)
    }
}

#[cfg(test)]
mod tests {
    use super::Not;
    use crate::formulas::RobustnessFormula;
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
