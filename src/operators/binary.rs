/// Binary operator definitions
///
/// Binary operators combine the outputs of two subformulas for each time-step. Instead of
/// computing each formula at each time step, each subformula is evaluated completely and then
/// combined together.
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

use crate::formulas::Formula;
use crate::trace::Trace;

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

/// Trait representing the binary operator that computes the least upper bound of two values.
///
/// In general, this equates to the maximum of the two values, but this behavior is not guaranteed.
/// When provided along with a partial ordering, the type forms a Join-Semilattice.
pub trait Join {
    /// Compute the least upper bound of self and other
    fn join(self, other: Self) -> Self;
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
/// use banquo::expressions::{Predicate, Term};
/// use banquo::operators::Or;
///
/// let left = Predicate::new(Term::variable("x", 1.0), Term::constant(2.0));
/// let right = Predicate::new(Term::variable("x", -1.0), Term::constant(-2.0));
/// let formula = Or::new(left, right);
/// ```
pub struct Or<Left, Right> {
    left: Left,
    right: Right,
}

impl<Left, Right> Or<Left, Right> {
    pub fn new(left: Left, right: Right) -> Self {
        Or { left, right }
    }
}

impl<Left, Right, State, Metric> Formula<State> for Or<Left, Right>
where
    Left: Formula<State, Metric = Metric>,
    Right: Formula<State, Metric = Metric>,
    Metric: Join,
{
    type Metric = Metric;
    type Error = BinaryOperatorError<Left::Error, Right::Error>;

    fn evaluate_trace(&self, trace: &Trace<State>) -> Result<Trace<Self::Metric>, Self::Error> {
        let left = self.left.evaluate_trace(trace);
        let right = self.right.evaluate_trace(trace);

        binop(left, right, Metric::join)
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
/// use banquo::expressions::{Predicate, Term};
/// use banquo::operators::And;
///
/// let left = Predicate::new(Term::variable("x", 1.0), Term::constant(2.0));
/// let right = Predicate::new(Term::variable("x", -1.0), Term::constant(-2.0));
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
        binop(
            self.0.left.evaluate_states(trace),
            self.0.right.evaluate_states(trace),
            f64::min,
        )
    }
}

/// Semantic representation of the And operator merging operation
///
/// This struct is nothing more than a wrapper around two [DebugRobustness] values that represents
/// taking the minimum of the two cost values. This type is intended to be used in debug formula
/// implementations to indicate to the user what operation has been performed when visually
/// debugging.
pub struct MinOf<L, R>(DebugRobustness<L>, DebugRobustness<R>);

type AndDebug<LPrev, RPrev> = DebugRobustness<MinOf<LPrev, RPrev>>;

impl<LPrev, RPrev, Left, Right, State> Formula<AndDebug<LPrev, RPrev>> for And<Left, Right>
where
    Left: Formula<DebugRobustness<LPrev>, State = State>,
    Right: Formula<DebugRobustness<RPrev>, State = State>,
{
    type State = State;
    type Error = BinaryOperatorError<Left::Error, Right::Error>;

    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<AndDebug<LPrev, RPrev>>, Self::Error> {
        let debug_min = |ldebug: DebugRobustness<LPrev>, rdebug: DebugRobustness<RPrev>| DebugRobustness {
            robustness: f64::min(ldebug.robustness, rdebug.robustness),
            previous: MinOf(ldebug, rdebug),
        };

        binop(
            self.0.left.evaluate_states(trace),
            self.0.right.evaluate_states(trace),
            debug_min,
        )
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
        binop(
            self.0.left.evaluate_states(trace),
            self.0.right.evaluate_states(trace),
            HybridDistance::min,
        )
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
/// use banquo::expressions::{Predicate, Term};
/// use banquo::operators::Implies;
///
/// let left = Predicate::new(Term::variable("x", 1.0), Term::constant(2.0));
/// let right = Predicate::new(Term::variable("x", -1.0), Term::constant(-2.0));
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
        binop(
            self.0.left.evaluate_states(trace),
            self.0.right.evaluate_states(trace),
            f,
        )
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
                previous: MaxOf(adebug_neg, cdebug),
            }
        };

        binop(
            self.0.left.evaluate_states(trace),
            self.0.right.evaluate_states(trace),
            debug_implies,
        )
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
        binop(
            self.0.left.evaluate_states(trace),
            self.0.right.evaluate_states(trace),
            f,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::{And, BinaryOperatorError, Implies, Or};
    use crate::operators::testing::{Const, ConstError};
    use crate::trace::Trace;
    use crate::Formula;

    #[test]
    fn or_robustness() -> Result<(), BinaryOperatorError<ConstError, ConstError>> {
        let left = Trace::from_iter([(0, 0.0), (1, 1.0), (2, 2.0), (3, 3.0)]);
        let right = Trace::from_iter([(0, 1.0), (1, 0.0), (2, 4.0), (3, 6.0)]);
        let formula = Or::new(Const::from(left), Const::from(right));

        let input = Trace::default();
        let robustness = formula.evaluate_states(&input)?;
        let expected = Trace::from_iter([(0, 1.0), (1, 1.0), (2, 4.0), (3, 6.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn and_robustness() -> Result<(), BinaryOperatorError<ConstError, ConstError>> {
        let left = Trace::from_iter([(0, 0.0), (1, 1.0), (2, 2.0), (3, 3.0)]);
        let right = Trace::from_iter([(0, 1.0), (1, 0.0), (2, 4.0), (3, 6.0)]);
        let formula = And::new(Const::from(left), Const::from(right));

        let input = Trace::default();
        let robustness = formula.evaluate_states(&input)?;
        let expected = Trace::from_iter([(0, 0.0), (1, 0.0), (2, 2.0), (3, 3.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn implies_robustness() -> Result<(), BinaryOperatorError<ConstError, ConstError>> {
        let antecedent = Trace::from_iter([(0, 0.0), (1, 1.0), (2, -4.0), (3, 3.0)]);
        let consequent = Trace::from_iter([(0, 1.0), (1, 0.0), (2, 2.0), (3, 6.0)]);
        let formula = Implies::new(Const::from(antecedent), Const::from(consequent));

        let input = Trace::default();
        let robustness = formula.evaluate_states(&input)?;
        let expected = Trace::from_iter([(0, 1.0), (1, 0.0), (2, 4.0), (3, 6.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }
}
