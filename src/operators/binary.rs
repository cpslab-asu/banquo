/// Binary operator definitions
///
/// Binary operators combine the outputs of two subformulas for each time-step. Instead of
/// computing each formula at each time step, each subformula is evaluated completely and then
/// combined together.

use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

use super::unary::NegOf;
use crate::formulas::{
    DebugRobustness, DebugRobustnessFormula, HybridDistance, HybridDistanceFormula, RobustnessFormula,
};
use crate::trace::Trace;
use crate::Formula;

/// Representation of an error in either the left or right subformula of a binary operator
///
/// This type only represents an error in one of the formulas, meaning that it cannot represent the
/// case where an error is occured in both formulas. This is not normally an issue because the
/// binary operators short-circuit when the first error is encountered, but it is important if you
/// use this error type for your own implementations.
#[derive(Debug)]
pub enum BinaryOperatorError<L, R> {
    /// An error produced by the subformula on the left of the operator
    LeftError(L),

    /// An error produced by the subformula on the right of the operator
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

/// First-order operator that requires either of its subformulas to hold
///
/// This operator evaluates a trace using both subformulas and takes the maximum of the
/// two states for each time. The intuition behind this operator is that given two states where
/// negative values represent failure the operator should only return a negative value if both of
/// its operands are negative, mirroring the first-order logic behavior.
///
/// Here is an example evaluation of the disjunction operator:
///
/// | time | left | right | or   |
/// | ---- | ---- | ----- | ---- |
/// |  0.0 |  1.0 |   2.0 |  2.0 |
/// |  1.0 | -1.0 |   5.0 |  5.0 |
/// |  2.0 | -3.0 |  -1.0 | -1.0 |
///
/// The following is an example of creating a formula using the Or operator:
///
/// ```rust
/// use banquo::expressions::Predicate;
/// use banquo::operators::Or;
///
/// let left = Predicate::new(("x", 1.0), 2.0)
/// let right = Predicate::new(("x", -1.0), -2.0)
/// let formula = Or::new(left, right);
/// ```
pub struct Or<L, R>(BinaryOperator<L, R>);

impl<L, R> Or<L, R> {
    pub fn new(left: L, right: R) -> Self {
        Or(BinaryOperator { left, right })
    }
}

impl<Left, Right, State> Formula<f64> for Or<Left, Right>
where
    Left: Formula<f64, State = State>,
    Right: Formula<f64, State = State>,
{
    type State = State;
    type Error = BinaryOperatorError<Left::Error, Right::Error>;

    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<f64>, Self::Error> {
        binop(self.0.left.evaluate_states(trace), self.0.right.evaluate_states(trace), f64::max)
    }
}

type OrDebug<LPrev, RPrev> = DebugRobustness<MaxOf<LPrev, RPrev>>;

impl<LPrev, RPrev, Left, Right, State> Formula<OrDebug<LPrev, RPrev>> for Or<Left, Right>
where
    Left: Formula<DebugRobustness<LPrev>, State = State>,
    Right: Formula<DebugRobustness<RPrev>, State = State>,
{
    type State = State;
    type Error = BinaryOperatorError<Left::Error, Right::Error>;

    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<OrDebug<LPrev, RPrev>>, Self::Error> {
        let debug_max = |ldebug: DebugRobustness<LPrev>, rdebug: DebugRobustness<RPrev>| {
            DebugRobustness {
                robustness: f64::max(ldebug.robustness, rdebug.robustness),
                previous: MaxOf(ldebug, rdebug)
            }
        };

        binop(self.0.left.evaluate_states(trace), self.0.right.evaluate_states(trace), debug_max)
    }
}

impl<Left, Right, State> Formula<HybridDistance> for Or<Left, Right>
where
    Left: Formula<HybridDistance, State = State>,
    Right: Formula<HybridDistance, State = State>,
{
    type State = State;
    type Error = BinaryOperatorError<Left::Error, Right::Error>;

    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<HybridDistance>, Self::Error> {
        binop(self.0.left.evaluate_states(trace), self.0.right.evaluate_states(trace), HybridDistance::max)
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

/// Semantic representation of the Or operator merging operation
///
/// This struct is nothing more than a wrapper around two [DebugRobustness] values that represents
/// taking the maximum of the two cost values. This type is intended to be used in debug formula
/// implementations to indicate to the user what operation has been performed when visually
/// debugging.
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

/// First-order operator that requires both of its subformulas to hold
///
/// This operator evaluates a trace with both subformulas and takes the minimum of the
/// two states for each time. The intuition behind this operator is that given two states where
/// negative values represent failure the operator should only return a positive value if both of
/// its operands are positive, mirroring the first-order logic behavior.
///
/// Here is an example evaluation of the conjunction operator:
///
/// | time | left | right | or   |
/// | ---- | ---- | ----- | ---- |
/// |  0.0 |  1.0 |   2.0 |  2.0 |
/// |  1.0 | -1.0 |   5.0 |  5.0 |
/// |  2.0 | -3.0 |  -1.0 | -1.0 |
///
/// The following is an example of creating a formula using the And operator:
///
/// ```rust
/// use banquo::expressions::Predicate;
/// use banquo::operators::And;
///
/// let left = Predicate::new(("x", 1.0), 2.0)
/// let right = Predicate::new(("x", -1.0), -2.0)
/// let formula = And::new(left, right);
/// ```
#[derive(Clone)]
pub struct And<L, R>(BinaryOperator<L, R>);

impl<L, R> And<L, R> {
    pub fn new(left: L, right: R) -> Self {
        Self(BinaryOperator { left, right })
    }
}

impl<Left, Right, State> Formula<f64> for And<Left, Right>
where
    Left: Formula<f64, State = State>,
    Right: Formula<f64, State = State>,
{
    type State = State;
    type Error = BinaryOperatorError<Left::Error, Right::Error>;

    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<f64>, Self::Error> {
        binop(self.0.left.evaluate_states(trace), self.0.right.evaluate_states(trace), f64::min)
    }
}

type AndDebug<LPrev, RPrev> = DebugRobustness<MinOf<LPrev, RPrev>>;

impl<LPrev, RPrev, Left, Right, State> Formula<AndDebug<LPrev, RPrev>> for And<Left, Right>
where
    Left: Formula<DebugRobustness<LPrev>, State = State>,
    Right: Formula<DebugRobustness<RPrev>, State = State>,
{
    type State = State;
    type Error = BinaryOperatorError<Left::Error, Right::Error>;

    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<AndDebug<LPrev, RPrev>>, Self::Error> {
        let debug_min = |ldebug: DebugRobustness<LPrev>, rdebug: DebugRobustness<RPrev>| {
            DebugRobustness {
                robustness: f64::min(ldebug.robustness, rdebug.robustness),
                previous: MinOf(ldebug, rdebug),
            }
        };

        binop(self.0.left.evaluate_states(trace), self.0.right.evaluate_states(trace), debug_min)
    }
}

impl<Left, Right, State> Formula<HybridDistance> for And<Left, Right>
where
    Left: Formula<HybridDistance, State = State>,
    Right: Formula<HybridDistance, State = State>,
{
    type State = State;
    type Error = BinaryOperatorError<Left::Error, Right::Error>;

    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<HybridDistance>, Self::Error> {
        binop(self.0.left.evaluate_states(trace), self.0.right.evaluate_states(trace), HybridDistance::min)
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

/// Semantic representation of the And operator merging operation
///
/// This struct is nothing more than a wrapper around two [DebugRobustness] values that represents
/// taking the minimum of the two cost values. This type is intended to be used in debug formula
/// implementations to indicate to the user what operation has been performed when visually
/// debugging.
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

/// First-order operator that requires the right subformula to hold if the left subformula holds.
///
/// The implication operator is a binary operator, which means that it operates over two
/// subformulas. This operator evaluates a trace with both subformulas and takes the maximum of the
/// negation of the left state and the right state for each time. The implication operator can be
/// represented as Or(Not(L), R), resulting in the behavior described above. 
///
/// Here is an example evaluation of the impliation operator:
///
/// | time | left | right | or   |
/// | ---- | ---- | ----- | ---- |
/// |  0.0 |  1.0 |   2.0 |  2.0 |
/// |  1.0 | -1.0 |   5.0 |  5.0 |
/// |  2.0 | -3.0 |  -1.0 |  3.0 |
///
/// The following is an example of creating a formula using the Implies operator:
///
/// ```rust
/// use banquo::expressions::Predicate;
/// use banquo::operators::Implies;
///
/// let left = Predicate::new(("x", 1.0), 2.0)
/// let right = Predicate::new(("x", -1.0), -2.0)
/// let formula = Implies::new(left, right);
/// ```
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

impl<Ante, Cons, State> Formula<f64> for Implies<Ante, Cons>
where
    Ante: Formula<f64, State = State>,
    Cons: Formula<f64, State = State>,
{
    type State = State;
    type Error = BinaryOperatorError<Ante::Error, Cons::Error>;

    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<f64>, Self::Error> {
        let f = |arho: f64, crho: f64| f64::max(-arho, crho);
        binop(self.0.left.evaluate_states(trace), self.0.right.evaluate_states(trace), f)
    }
}

type ImpliesDebug<LPrev, RPrev> = DebugRobustness<MaxOf<NegOf<LPrev>, RPrev>>;

impl<APrev, CPrev, Ante, Cons, State> Formula<ImpliesDebug<APrev, CPrev>> for Implies<Ante, Cons>
where
    Ante: Formula<DebugRobustness<APrev>, State = State>,
    Cons: Formula<DebugRobustness<CPrev>, State = State>,
{
    type State = State;
    type Error = BinaryOperatorError<Ante::Error, Cons::Error>;

    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<ImpliesDebug<APrev, CPrev>>, Self::Error> {
        let debug_implies = |adebug: DebugRobustness<APrev>, cdebug: DebugRobustness<CPrev>| {
            let adebug_neg = DebugRobustness {
                robustness: -adebug.robustness,
                previous: NegOf(adebug),
            };

            DebugRobustness {
                robustness: f64::max(adebug_neg.robustness, cdebug.robustness),
                previous: MaxOf(adebug_neg, cdebug)
            }
        };

        binop(self.0.left.evaluate_states(trace), self.0.right.evaluate_states(trace), debug_implies)
    }
}

impl<Ante, Cons, State> Formula<HybridDistance> for Implies<Ante, Cons>
where
    Ante: Formula<HybridDistance, State = State>,
    Cons: Formula<HybridDistance, State = State>,
{
    type State = State;
    type Error = BinaryOperatorError<Ante::Error, Cons::Error>;

    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<HybridDistance>, Self::Error> {
        let f = |adist: HybridDistance, cdist: HybridDistance| HybridDistance::max(-adist, cdist);
        binop(self.0.left.evaluate_states(trace), self.0.right.evaluate_states(trace), f)
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
