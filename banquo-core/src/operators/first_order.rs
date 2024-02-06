use std::ops::Neg;

use thiserror::Error;

use crate::Formula;
use crate::metrics::{Meet, Join};
use crate::trace::Trace;

/// First-order operator that inverts its subformula, written `!`, or `not`.
///
/// The `Not` operator is a unary operator, which means that it operates on a single subformula.
/// This operator evaluates a given trace using its subformula, then for each time in the resulting
/// trace negates the state. For floating point numbers, this would look as follows:
///
/// | time | subformula | not  |
/// | ---- | ---------- | ---- |
/// | 0.0  |        1.0 | -1.0 |
/// | 1.0  |        3.0 | -3.0 |
/// | 1.0  |       -2.0 |  2.0 |
///
/// The following is an example of creating a formula using the not operator:
///
/// ```rust
/// use banquo::predicate;
/// use banquo::operators::Not;
///
/// let subformula = predicate!{ x * 1.0 <= 1.0 };
/// let formula = Not::new(subformula);
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct Not<F> {
    subformula: F,
}

impl<F> Not<F> {
    pub fn new(subformula: F) -> Self {
        Self { subformula }
    }
}

impl<T, F, M> Formula<T> for Not<F>
where
    F: Formula<T, Metric = M>,
    M: Neg<Output = M>,
{
    type Metric = M;
    type Error = F::Error;

    fn evaluate(&self, trace: &Trace<T>) -> Result<Trace<Self::Metric>, Self::Error> {
        self.subformula
            .evaluate(trace)
            .map(|result| {
                result.into_iter().map_states(|state| state.neg()).collect()
            })
    }
}

/// Error produced during application of a binary operation.
///
/// This error can be produced in the following circumstances:
///
///   1. The two input traces do not have the same lengths
///   2. When iterating over both traces, the corresponding times do not match.
///
#[derive(Debug, Clone, Error)]
pub enum BinaryEvaluationError {
    #[error("Metric traces have mismatched lengths: [{0}] [{1}]")]
    MismatchedLengths(usize, usize),

    #[error("Mismatched times between traces: [{0}] [{1}]")]
    MismatchedTimes(f64, f64),
}

/// Error produced during the evaluation of a binary operator.
///
/// An error can occur when evaluating a binary operator in the following circumstances:
///
///   1. An error occurs during the evaluation of the left subformula
///   2. An error occurs during the evaluation of the right subformula
///   3. An error occurs during the application of the binary operation on the outputs of the
///      sub-formulas, resulting in a [`BinaryEvaluationError`].
#[derive(Debug, Clone, Error)]
pub enum BinaryOperatorError<L, R> {
    /// An error produced by the subformula on the left of the operator
    #[error("Left suformula error: {0}")]
    LeftError(L),

    /// An error produced by the subformula on the right of the operator
    #[error("Right subformula error: {0}")]
    RightError(R),

    #[error("Error evaluation binary operator: {0}")]
    EvaluationError(#[from] BinaryEvaluationError)
}


type BinOpResult<T, E1, E2> = Result<Trace<T>, BinaryOperatorError<E1, E2>>;

#[derive(Debug, Clone)]
struct Binop<Left, Right> {
    left: Left,
    right: Right,
}

impl<Left, Right> Binop<Left, Right> {
    fn evaluate_left<State, Metric>(&self, trace: &Trace<State>) -> BinOpResult<Metric, Left::Error, Right::Error>
    where
        Left: Formula<State, Metric = Metric>,
        Right: Formula<State>,
    {
        self.left.evaluate(trace).map_err(BinaryOperatorError::LeftError)
    }

    fn evaluate_right<State, Metric>(&self, trace: &Trace<State>) -> BinOpResult<Metric, Left::Error, Right::Error>
    where
        Left: Formula<State>,
        Right: Formula<State, Metric = Metric>,
    {
        self.right.evaluate(trace).map_err(BinaryOperatorError::RightError)
    }
}

fn binop<I1, I2, F, M>(left: I1, right: I2, f: F) -> Result<Trace<M>, BinaryEvaluationError>
where
    I1: ExactSizeIterator<Item = (f64, M)>,
    I2: ExactSizeIterator<Item = (f64, M)>,
    F: Fn(&M, &M) -> M,
{
    if left.len() != right.len() {
        return Err(BinaryEvaluationError::MismatchedLengths(left.len(), right.len()));
    }

    left.zip(right)
        .map(|((lt, ls), (rt, rs))| {
            if lt == rt {
                Ok((lt, f(&ls, &rs)))
            } else {
                Err(BinaryEvaluationError::MismatchedTimes(lt, rt))
            }
        })
        .collect()
}

/// First-order operator that requires either of its subformulas to hold written `\/`, `||`, or
/// `or`.
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
/// use banquo::predicate;
/// use banquo::operators::Or;
///
/// let left = predicate!(x * 1.0 <= 2.0);
/// let right = predicate!(x * -1.0 <= -2.0);
/// let formula = Or::new(left, right);
/// ```
#[derive(Debug, Clone)]
pub struct Or<Left, Right>(Binop<Left, Right>);

impl<Left, Right> Or<Left, Right> {
    pub fn new(left: Left, right: Right) -> Self {
        Self(Binop { left, right }) 
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

    fn evaluate(&self, trace: &Trace<State>) -> Result<Trace<Self::Metric>, Self::Error> {
        let left = self.0.evaluate_left(trace)?;
        let right = self.0.evaluate_right(trace)?;
        let result = binop(left.into_iter(), right.into_iter(), Metric::max)?;

        Ok(result)
    }
}

/// First-order operator that requires both of its subformulas to hold, written `/\`, `&&`, or
/// `and`.
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
/// use banquo::predicate;
/// use banquo::operators::And;
///
/// let left = predicate!(x * 1.0 <= 2.0);
/// let right = predicate!(x * -1.0 <=-2.0);
/// let formula = And::new(left, right);
/// ```
#[derive(Debug, Clone)]
pub struct And<Left, Right>(Binop<Left, Right>);

impl<Left, Right> And<Left, Right> {
    pub fn new(left: Left, right: Right) -> Self {
        Self(Binop { left, right })
    }
}

impl<Left, Right, State, Metric> Formula<State> for And<Left, Right>
where
    Left: Formula<State, Metric = Metric>,
    Right: Formula<State, Metric = Metric>,
    Metric: Meet,
{
    type Metric = Metric;
    type Error = BinaryOperatorError<Left::Error, Right::Error>;

    fn evaluate(&self, trace: &Trace<State>) -> Result<Trace<Self::Metric>, Self::Error> {
        let left = self.0.evaluate_left(trace)?;
        let right = self.0.evaluate_right(trace)?;
        let result = binop(left.into_iter(), right.into_iter(), Metric::min)?;

        Ok(result)
    }
}

/// First-order operator that requires the right subformula to hold if the left subformula holds,
/// written `->` or `implies`.
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
/// use banquo::predicate;
/// use banquo::operators::Implies;
///
/// let left = predicate!(x * 1.0 <= 2.0);
/// let right = predicate!(x * -1.0 <= -2.0);
/// let formula = Implies::new(left, right);
/// ```
#[derive(Clone)]
pub struct Implies<Ante, Cons>(Binop<Ante, Cons>);

impl<Ante, Cons> Implies<Ante, Cons> {
    pub fn new(ante: Ante, cons: Cons) -> Self {
        Self(Binop { left: ante, right: cons })
    }
}

impl<Ante, Cons, State, Metric> Formula<State> for Implies<Ante, Cons>
where
    Ante: Formula<State, Metric = Metric>,
    Cons: Formula<State, Metric = Metric>,
    Metric: Neg<Output = Metric> + Join,
{
    type Metric = Metric;
    type Error = BinaryOperatorError<Ante::Error, Cons::Error>;

    fn evaluate(&self, trace: &Trace<State>) -> Result<Trace<Self::Metric>, Self::Error> {
        let ante = self.0
            .evaluate_left(trace)?
            .into_iter()
            .map_states(|state| -state);

        let cons = self.0.evaluate_right(trace)?;
        let result = binop(ante, cons.into_iter(), |neg_a, c| Metric::max(neg_a, c))?;

        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use crate::Formula;
    use crate::operators::test::*;
    use crate::operators::BinaryOperatorError;
    use crate::trace::Trace;
    use super::{And, Implies, Not, Or};

    #[test]
    fn not() -> Result<(), ConstError> {
        let input = Trace::from_iter([
            (0, 0.0),
            (1, 1.0),
            (2, 2.0),
            (3, 3.0),
        ]);

        let formula = Not::new(Const);
        let robustness = formula.evaluate(&input)?;
        let expected = Trace::from_iter([
            (0, 0.0),
            (1, -1.0),
            (2, -2.0),
            (3, -3.0),
        ]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn or() -> Result<(), BinaryOperatorError<ConstError, ConstError>> {
        let input = Trace::from_iter([
            (0, (0.0, 1.0)),
            (1, (1.0, 0.0)),
            (2, (2.0, 4.0)),
            (3, (3.0, 6.0)),
        ]);

        let formula = Or::new(ConstLeft, ConstRight);
        let robustness = formula.evaluate(&input)?;
        let expected = Trace::from_iter([
            (0, 1.0),
            (1, 1.0),
            (2, 4.0),
            (3, 6.0),
        ]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn and() -> Result<(), BinaryOperatorError<ConstError, ConstError>> {
        let input = Trace::from_iter([
            (0, (0.0, 1.0)),
            (1, (1.0, 0.0)),
            (2, (2.0, 4.0)),
            (3, (3.0, 6.0)),
        ]);

        let formula = And::new(ConstLeft, ConstRight);
        let robustness = formula.evaluate(&input)?;
        let expected = Trace::from_iter([(0, 0.0), (1, 0.0), (2, 2.0), (3, 3.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn implies() -> Result<(), BinaryOperatorError<ConstError, ConstError>> {
        let input = Trace::from_iter([
            (0, (0.0, 1.0)),
            (1, (1.0, 0.0)),
            (2, (-4.0, 2.0)),
            (3, (3.0, 6.0)),
        ]);

        let formula = Implies::new(ConstLeft, ConstRight);
        let robustness = formula.evaluate(&input)?;
        let expected = Trace::from_iter([
            (0, 1.0),
            (1, 0.0),
            (2, 4.0),
            (3, 6.0),
        ]);

        assert_eq!(robustness, expected);
        Ok(())
    }
}
