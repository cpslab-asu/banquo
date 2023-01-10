use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

use super::unary::NegOf;
use crate::formulas::{
    DebugRobustness, DebugRobustnessFormula, HybridDistance, HybridDistanceFormula, RobustnessFormula,
};
use crate::trace::Trace;

#[derive(Debug)]
pub enum BinaryOperatorError<L, R> {
    LeftError(L),
    RightError(R),
}

impl<L, R> Display for BinaryOperatorError<L, R>
where
    L: Display,
    R: Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LeftError(err) => write!(f, "left subformula error: {}", err),
            Self::RightError(err) => write!(f, "right subformula error: {}", err),
        }
    }
}

impl<L, R> Error for BinaryOperatorError<L, R>
where
    L: Error,
    R: Error,
{
}

type BinOpResult<T, E1, E2> = Result<Trace<T>, BinaryOperatorError<E1, E2>>;

fn binop<A, E1, B, E2, F, C>(left: Result<Trace<A>, E1>, right: Result<Trace<B>, E2>, f: F) -> BinOpResult<C, E1, E2>
where
    F: Fn(A, B) -> C,
{
    let l_trace = left.map_err(BinaryOperatorError::LeftError)?;
    let r_trace = right.map_err(BinaryOperatorError::RightError)?;
    let combined_trace = l_trace
        .zip(r_trace)
        .into_iter()
        .map_states(|(lvalue, rvalue)| f(lvalue, rvalue))
        .collect();

    Ok(combined_trace)
}

#[derive(Clone)]
pub struct BinaryOperator<L, R> {
    pub left: L,
    pub right: R,
}

impl<L, R> BinaryOperator<L, R> {
    fn robustness<S, F>(&self, trace: &Trace<S>, f: F) -> BinOpResult<f64, L::Error, R::Error>
    where
        L: RobustnessFormula<S>,
        R: RobustnessFormula<S>,
        F: Fn(f64, f64) -> f64,
    {
        binop(self.left.robustness(trace), self.right.robustness(trace), f)
    }

    fn debug_robustness<S, F, T>(&self, trace: &Trace<S>, f: F) -> BinOpResult<DebugRobustness<T>, L::Error, R::Error>
    where
        L: DebugRobustnessFormula<S>,
        R: DebugRobustnessFormula<S>,
        F: Fn(DebugRobustness<L::Prev>, DebugRobustness<R::Prev>) -> DebugRobustness<T>,
    {
        binop(self.left.debug_robustness(trace), self.right.debug_robustness(trace), f)
    }

    fn hybrid_distance<S, T, F>(&self, trace: &Trace<(S, T)>, f: F) -> BinOpResult<HybridDistance, L::Error, R::Error>
    where
        L: HybridDistanceFormula<S, T>,
        R: HybridDistanceFormula<S, T>,
        F: Fn(HybridDistance, HybridDistance) -> HybridDistance,
    {
        binop(self.left.hybrid_distance(trace), self.right.hybrid_distance(trace), f)
    }
}

pub struct Or<L, R>(BinaryOperator<L, R>);

impl<L, R> Or<L, R> {
    pub fn new(left: L, right: R) -> Self {
        Or(BinaryOperator { left, right })
    }
}

impl<S, L, R> RobustnessFormula<S> for Or<L, R>
where
    L: RobustnessFormula<S>,
    R: RobustnessFormula<S>,
{
    type Error = BinaryOperatorError<L::Error, R::Error>;

    fn robustness(&self, trace: &Trace<S>) -> Result<Trace<f64>, Self::Error> {
        self.0.robustness(trace, f64::max)
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

        self.0.debug_robustness(trace, make_debug)
    }
}

impl<S, L, F, G> HybridDistanceFormula<S, L> for Or<F, G>
where
    F: HybridDistanceFormula<S, L>,
    G: HybridDistanceFormula<S, L>,
{
    type Error = BinaryOperatorError<F::Error, G::Error>;

    fn hybrid_distance(&self, trace: &Trace<(S, L)>) -> Result<Trace<HybridDistance>, Self::Error> {
        self.0.hybrid_distance(trace, HybridDistance::max)
    }
}

#[derive(Clone)]
pub struct And<L, R>(BinaryOperator<L, R>);

impl<L, R> And<L, R> {
    pub fn new(left: L, right: R) -> Self {
        Self(BinaryOperator { left, right })
    }
}

impl<S, L, R> RobustnessFormula<S> for And<L, R>
where
    L: RobustnessFormula<S>,
    R: RobustnessFormula<S>,
{
    type Error = BinaryOperatorError<L::Error, R::Error>;

    fn robustness(&self, trace: &Trace<S>) -> Result<Trace<f64>, Self::Error> {
        self.0.robustness(trace, f64::min)
    }
}

pub struct MinOf<L, R>(DebugRobustness<L>, DebugRobustness<R>);

impl<S, L, R> DebugRobustnessFormula<S> for And<L, R>
where
    L: DebugRobustnessFormula<S>,
    R: DebugRobustnessFormula<S>,
{
    type Error = BinaryOperatorError<L::Error, R::Error>;
    type Prev = MinOf<L::Prev, R::Prev>;

    fn debug_robustness(&self, trace: &Trace<S>) -> Result<Trace<DebugRobustness<Self::Prev>>, Self::Error> {
        let make_debug = |left: DebugRobustness<L::Prev>, right: DebugRobustness<R::Prev>| DebugRobustness {
            robustness: f64::min(left.robustness, right.robustness),
            previous: MinOf(left, right),
        };

        self.0.debug_robustness(trace, make_debug)
    }
}

impl<S, L, F, G> HybridDistanceFormula<S, L> for And<F, G>
where
    F: HybridDistanceFormula<S, L>,
    G: HybridDistanceFormula<S, L>,
{
    type Error = BinaryOperatorError<F::Error, G::Error>;

    fn hybrid_distance(&self, trace: &Trace<(S, L)>) -> Result<Trace<HybridDistance>, Self::Error> {
        self.0.hybrid_distance(trace, HybridDistance::min)
    }
}

#[derive(Clone)]
pub struct Implies<A, C>(BinaryOperator<A, C>);

impl<A, C> Implies<A, C> {
    pub fn new(antecedent: A, consequent: C) -> Self {
        Self(BinaryOperator {
            left: antecedent,
            right: consequent,
        })
    }
}

impl<S, A, C> RobustnessFormula<S> for Implies<A, C>
where
    A: RobustnessFormula<S>,
    C: RobustnessFormula<S>,
{
    type Error = BinaryOperatorError<A::Error, C::Error>;

    fn robustness(&self, trace: &Trace<S>) -> Result<Trace<f64>, Self::Error> {
        self.0.robustness(trace, |ra, rc| f64::max(-ra, rc))
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
        let make_debug = |left: DebugRobustness<A::Prev>, right: DebugRobustness<C::Prev>| {
            let neg_left = DebugRobustness {
                robustness: -left.robustness,
                previous: NegOf(left),
            };

            DebugRobustness {
                robustness: f64::max(neg_left.robustness, right.robustness),
                previous: MaxOf(neg_left, right),
            }
        };

        self.0.debug_robustness(trace, make_debug)
    }
}

impl<S, L, A, C> HybridDistanceFormula<S, L> for Implies<A, C>
where
    A: HybridDistanceFormula<S, L>,
    C: HybridDistanceFormula<S, L>,
{
    type Error = BinaryOperatorError<A::Error, C::Error>;

    fn hybrid_distance(&self, trace: &Trace<(S, L)>) -> Result<Trace<HybridDistance>, Self::Error> {
        self.0.hybrid_distance(trace, |da, dc| HybridDistance::max(-da, dc))
    }
}

#[cfg(test)]
mod tests {
    use super::{And, BinaryOperatorError, Implies, Or};
    use crate::formulas::RobustnessFormula;
    use crate::operators::{Const, ConstError};
    use crate::Trace;

    #[test]
    fn or_robustness() -> Result<(), BinaryOperatorError<ConstError, ConstError>> {
        let left = Trace::from_iter([(0, 0.0), (1, 1.0), (2, 2.0), (3, 3.0)]);
        let right = Trace::from_iter([(0, 1.0), (1, 0.0), (2, 4.0), (3, 6.0)]);
        let formula = Or::new(Const(left), Const(right));

        let input = Trace::default();
        let robustness = formula.robustness(&input)?;
        let expected = Trace::from_iter([(0, 1.0), (1, 1.0), (2, 4.0), (3, 6.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn and_robustness() -> Result<(), BinaryOperatorError<ConstError, ConstError>> {
        let left = Trace::from_iter([(0, 0.0), (1, 1.0), (2, 2.0), (3, 3.0)]);
        let right = Trace::from_iter([(0, 1.0), (1, 0.0), (2, 4.0), (3, 6.0)]);
        let formula = And::new(Const(left), Const(right));

        let input = Trace::default();
        let robustness = formula.robustness(&input)?;
        let expected = Trace::from_iter([(0, 0.0), (1, 0.0), (2, 2.0), (3, 3.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn implies_robustness() -> Result<(), BinaryOperatorError<ConstError, ConstError>> {
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
