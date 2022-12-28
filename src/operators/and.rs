use super::binary::{BinaryOperator, BinaryOperatorError};
use crate::formulas::{
    DebugRobustness, DebugRobustnessFormula, HybridDistance, HybridDistanceFormula, RobustnessFormula,
};
use crate::trace::Trace;

#[derive(Clone, Debug)]
pub struct And<L, R>(BinaryOperator<L, R>);

impl<L, R> And<L, R> {
    pub fn new(left: L, right: R) -> Self {
        Self(BinaryOperator { left, right })
    }

    pub fn left(&self) -> &L {
        &self.0.left
    }

    pub fn right(&self) -> &R {
        &self.0.right
    }
}

impl<S, L, R> RobustnessFormula<S> for And<L, R>
where
    L: RobustnessFormula<S>,
    R: RobustnessFormula<S>,
{
    type Error = BinaryOperatorError<L::Error, R::Error>;

    fn robustness(&self, trace: &Trace<S>) -> Result<Trace<f64>, Self::Error> {
        self.0.apply(trace, L::robustness, R::robustness, f64::min)
    }
}

pub struct MinOf<L, R>(DebugRobustness<L>, DebugRobustness<R>);

fn make_debug<L, R>(left: DebugRobustness<L>, right: DebugRobustness<R>) -> DebugRobustness<MinOf<L, R>> {
    DebugRobustness {
        robustness: f64::min(left.robustness, right.robustness),
        previous: MinOf(left, right),
    }
}

impl<S, L, R> DebugRobustnessFormula<S> for And<L, R>
where
    L: DebugRobustnessFormula<S>,
    R: DebugRobustnessFormula<S>,
{
    type Error = BinaryOperatorError<L::Error, R::Error>;
    type Prev = MinOf<L::Prev, R::Prev>;

    fn debug_robustness(&self, trace: &Trace<S>) -> Result<Trace<DebugRobustness<Self::Prev>>, Self::Error> {
        self.0
            .apply(trace, L::debug_robustness, R::debug_robustness, make_debug)
    }
}

impl<S, L, F, G> HybridDistanceFormula<S, L> for And<F, G>
where
    F: HybridDistanceFormula<S, L>,
    G: HybridDistanceFormula<S, L>,
{
    type Error = BinaryOperatorError<F::Error, G::Error>;

    fn hybrid_distance(&self, trace: &Trace<(S, L)>) -> Result<Trace<HybridDistance>, Self::Error> {
        self.0
            .apply(trace, F::hybrid_distance, G::hybrid_distance, HybridDistance::min)
    }
}

#[cfg(test)]
mod tests {
    use super::{And, BinaryOperatorError};
    use crate::formulas::RobustnessFormula;
    use crate::operators::{Const, ConstError};
    use crate::trace::Trace;

    #[test]
    fn robustness() -> Result<(), BinaryOperatorError<ConstError, ConstError>> {
        let left = Trace::from_iter([(0, 0.0), (1, 1.0), (2, 2.0), (3, 3.0)]);
        let right = Trace::from_iter([(0, 1.0), (1, 0.0), (2, 4.0), (3, 6.0)]);
        let formula = And::new(Const(left), Const(right));

        let input = Trace::default();
        let robustness = formula.robustness(&input)?;
        let expected = Trace::from_iter([(0, 0.0), (1, 0.0), (2, 2.0), (3, 3.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }
}
