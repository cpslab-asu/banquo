use super::binary::{BinaryOperator, BinaryOperatorError};
use crate::formulas::{
    DebugRobustness, DebugRobustnessFormula, HybridDistance, HybridDistanceFormula, RobustnessFormula,
};
use crate::trace::Trace;

#[derive(Clone, Debug)]
pub struct Or<L, R>(BinaryOperator<L, R>);

impl<L, R> Or<L, R> {
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

impl<S, L, R> RobustnessFormula<S> for Or<L, R>
where
    L: RobustnessFormula<S>,
    R: RobustnessFormula<S>,
{
    type Error = BinaryOperatorError<L::Error, R::Error>;

    fn robustness(&self, trace: &Trace<S>) -> Result<Trace<f64>, Self::Error> {
        self.0.apply(trace, L::robustness, R::robustness, f64::max)
    }
}

pub struct MaxOf<L, R>(pub DebugRobustness<L>, pub DebugRobustness<R>);

impl<S, L, R> DebugRobustnessFormula<S> for Or<L, R>
where
    L: DebugRobustnessFormula<S>,
    R: DebugRobustnessFormula<S>,
{
    type Error = BinaryOperatorError<L::Error, R::Error>;
    type Prev = MaxOf<L::Prev, R::Prev>;

    fn debug_robustness(&self, trace: &Trace<S>) -> Result<Trace<DebugRobustness<Self::Prev>>, Self::Error> {
        let make_debug = |left: DebugRobustness<L::Prev>, right: DebugRobustness<R::Prev>| DebugRobustness {
            robustness: f64::max(left.robustness, right.robustness),
            previous: MaxOf(left, right),
        };

        self.0
            .apply(trace, L::debug_robustness, R::debug_robustness, make_debug)
    }
}

impl<S, L, F, G> HybridDistanceFormula<S, L> for Or<F, G>
where
    F: HybridDistanceFormula<S, L>,
    G: HybridDistanceFormula<S, L>,
{
    type Error = BinaryOperatorError<F::Error, G::Error>;

    fn hybrid_distance(&self, trace: &Trace<(S, L)>) -> Result<Trace<HybridDistance>, Self::Error> {
        self.0
            .apply(trace, F::hybrid_distance, G::hybrid_distance, HybridDistance::max)
    }
}

#[cfg(test)]
mod tests {
    use super::{BinaryOperatorError, Or};
    use crate::formulas::RobustnessFormula;
    use crate::operators::{Const, ConstError};
    use crate::trace::Trace;

    #[test]
    fn robustness() -> Result<(), BinaryOperatorError<ConstError, ConstError>> {
        let left = Trace::from_iter([(0, 0.0), (1, 1.0), (2, 2.0), (3, 3.0)]);
        let right = Trace::from_iter([(0, 1.0), (1, 0.0), (2, 4.0), (3, 6.0)]);
        let formula = Or::new(Const(left), Const(right));

        let input = Trace::default();
        let robustness = formula.robustness(&input)?;
        let expected = Trace::from_iter([(0, 1.0), (1, 1.0), (2, 4.0), (3, 6.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }
}
