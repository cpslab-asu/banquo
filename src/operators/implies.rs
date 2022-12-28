use super::binary::{BinaryOperator, BinaryOperatorError};
use super::not::NegOf;
use super::or::MaxOf;
use crate::formulas::{
    DebugRobustness, DebugRobustnessFormula, HybridDistance, HybridDistanceFormula, RobustnessFormula,
};
use crate::trace::Trace;

#[derive(Clone, Debug)]
pub struct Implies<A, C>(BinaryOperator<A, C>);

impl<A, C> Implies<A, C> {
    pub fn new(antecedent: A, consequent: C) -> Self {
        let operator = BinaryOperator {
            left: antecedent,
            right: consequent,
        };

        Self(operator)
    }

    pub fn antecedent(&self) -> &A {
        &self.0.left
    }

    pub fn consequent(&self) -> &C {
        &self.0.right
    }
}

impl<S, A, C> RobustnessFormula<S> for Implies<A, C>
where
    A: RobustnessFormula<S>,
    C: RobustnessFormula<S>,
{
    type Error = BinaryOperatorError<A::Error, C::Error>;

    fn robustness(&self, trace: &Trace<S>) -> Result<Trace<f64>, Self::Error> {
        self.0
            .apply(trace, A::robustness, C::robustness, |ra, rc| f64::max(-ra, rc))
    }
}

fn make_debug<L, R>(left: DebugRobustness<L>, right: DebugRobustness<R>) -> DebugRobustness<MaxOf<NegOf<L>, R>> {
    let neg_left = DebugRobustness {
        robustness: -left.robustness,
        previous: NegOf(left),
    };

    DebugRobustness {
        robustness: f64::max(neg_left.robustness, right.robustness),
        previous: MaxOf(neg_left, right),
    }
}

impl<S, A, C> DebugRobustnessFormula<S> for Implies<A, C>
where
    A: DebugRobustnessFormula<S>,
    C: DebugRobustnessFormula<S>,
{
    type Error = BinaryOperatorError<A::Error, C::Error>;
    type Prev = MaxOf<NegOf<A::Prev>, C::Prev>;

    fn debug_robustness(&self, trace: &Trace<S>) -> Result<Trace<DebugRobustness<Self::Prev>>, Self::Error> {
        self.0
            .apply(trace, A::debug_robustness, C::debug_robustness, make_debug)
    }
}

impl<S, L, A, C> HybridDistanceFormula<S, L> for Implies<A, C>
where
    A: HybridDistanceFormula<S, L>,
    C: HybridDistanceFormula<S, L>,
{
    type Error = BinaryOperatorError<A::Error, C::Error>;

    fn hybrid_distance(&self, trace: &Trace<(S, L)>) -> Result<Trace<HybridDistance>, Self::Error> {
        self.0.apply(trace, A::hybrid_distance, C::hybrid_distance, |da, dc| {
            HybridDistance::max(-da, dc)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::{BinaryOperatorError, Implies};
    use crate::formulas::RobustnessFormula;
    use crate::operators::{Const, ConstError};
    use crate::trace::Trace;

    #[test]
    fn robustness() -> Result<(), BinaryOperatorError<ConstError, ConstError>> {
        let antecedent = Trace::from_iter([(0, 0.0), (1, 1.0), (2, -4.0), (3, 3.0)]);
        let consequent = Trace::from_iter([(0, 1.0), (1, 0.0), (2, 2.0), (3, 6.0)]);
        let formula = Implies::new(Const(antecedent), Const(consequent));

        let input = Trace::default();
        let robustness = formula.robustness(&input)?;
        let expected = Trace::from_iter([(0, 1.0), (1, 0.0), (2, 4.0), (3, 6.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }
}
