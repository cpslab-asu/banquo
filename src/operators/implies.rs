use super::binary::{BinaryOperator, BinaryOperatorError};
use super::not::NegOf;
use super::or::MaxOf;
use crate::formula::{DebugFormula, DebugRobustness, Formula, HybridDistance, HybridDistanceFormula, Result};
use crate::trace::Trace;

#[derive(Clone, Debug)]
pub struct Implies<A, C>(BinaryOperator<A, C>);

impl<A, C> Implies<A, C> {
    pub fn new<S>(antecedent: A, consequent: C) -> Self
    where
        A: Formula<S>,
        C: Formula<S>,
    {
        Self(BinaryOperator {
            left: antecedent,
            right: consequent,
        })
    }
}

impl<S, A, C> Formula<S> for Implies<A, C>
where
    A: Formula<S>,
    C: Formula<S>,
{
    type Error = BinaryOperatorError<A::Error, C::Error>;

    fn robustness(&self, trace: &Trace<S>) -> Result<f64, Self::Error> {
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

impl<S, A, C> DebugFormula<S> for Implies<A, C>
where
    A: DebugFormula<S>,
    C: DebugFormula<S>,
{
    type Error = BinaryOperatorError<A::Error, C::Error>;
    type Prev = MaxOf<NegOf<A::Prev>, C::Prev>;

    fn debug_robustness(&self, trace: &Trace<S>) -> Result<DebugRobustness<Self::Prev>, Self::Error> {
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

    fn hybrid_distance(&self, trace: &Trace<(S, L)>) -> Result<HybridDistance, Self::Error> {
        self.0.apply(trace, A::hybrid_distance, C::hybrid_distance, |da, dc| {
            HybridDistance::max(-da, dc)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::{BinaryOperatorError, Implies};
    use crate::formula::Formula;
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
