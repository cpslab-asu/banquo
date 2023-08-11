use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::ops::{Bound, Neg, RangeBounds};

use thiserror::Error;

use crate::formulas::Formula;
use crate::metric::{Bottom, Join, Meet, Top};
use crate::trace::{Range, Trace};

/// First-order operator that inverts its subformula
///
/// The not operator is a unary operator, which means that it operates on a single subformula.
/// The not operator evaluates a given trace using its subformula, then for each time in the
/// resulting trace negates the state. For floating point numbers, this would look as follows:
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
/// use banquo::expressions::{Predicate, Term};
/// use banquo::operators::Not;
///
/// let subformula = Predicate::new(Term::variable("x", 1.0), Term::constant(1.0));
/// let formula = Not::new(subformula);
/// ```
#[derive(Clone, Debug)]
pub struct Not<F> {
    subformula: F,
}

impl<F> Not<F> {
    pub fn new(subformula: F) -> Self {
        Self { subformula }
    }
}

fn not<S>(trace: Trace<S>) -> Trace<S::Output>
where
    S: Neg,
{
    trace.into_iter().map_states(|value| -value).collect()
}

impl<T, F, M> Formula<T> for Not<F>
where
    F: Formula<T, Metric = M>,
    M: Neg,
{
    type Metric = M::Output;
    type Error = F::Error;

    fn evaluate_trace(&self, trace: &Trace<T>) -> Result<Trace<Self::Metric>, Self::Error> {
        self.subformula.evaluate_trace(trace).map(not)
    }
}

/// Binary operator definitions
///
/// Binary operators combine the outputs of two subformulas for each time-step. Instead of
/// computing each formula at each time step, each subformula is evaluated completely and then
/// combined together.

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
            Self::LeftError(err) => write!(f, "left subformula error: {err}"),
            Self::RightError(err) => write!(f, "right subformula error: {err}"),
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

        binop(left, right, |l, r| Metric::join(&l, &r))
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
pub struct And<Left, Right> {
    left: Left,
    right: Right,
}

impl<Left, Right> And<Left, Right> {
    pub fn new(left: Left, right: Right) -> Self {
        Self { left, right }
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

    fn evaluate_trace(&self, trace: &Trace<State>) -> Result<Trace<Self::Metric>, Self::Error> {
        let left = self.left.evaluate_trace(trace);
        let right = self.right.evaluate_trace(trace);

        binop(left, right, |l, r| Metric::meet(&l, &r))
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
pub struct Implies<Ante, Cons> {
    ante: Ante,
    cons: Cons,
}

impl<Ante, Cons> Implies<Ante, Cons> {
    pub fn new(ante: Ante, cons: Cons) -> Self {
        Self { ante, cons }
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

    fn evaluate_trace(&self, trace: &Trace<State>) -> Result<Trace<Self::Metric>, Self::Error> {
        let ante = self.ante.evaluate_trace(trace);
        let cons = self.cons.evaluate_trace(trace);
        let f = |a_val: Metric, c_val: Metric| {
            let neg_a = -a_val;
            Metric::join(&neg_a, &c_val)
        };

        binop(ante, cons, f)
    }
}

/// Definitions of forward temporal operators
///
/// Most forward operators are optionally bounded. If an operator is unbounded, then the times
/// included in the sub-trace at a given time value are from the given time value to the end of the
/// trace. If a bound is provided then the times included in the sub-trace begin at the current time
/// offset by the lower bound value to the current time offset by the upper bound value.
///
/// Below is a visual example of sub-trace for an unbounded operator starting at time T2:
///
/// T1  T2  T3  T4 ... Tn
/// S1  S2  S3  S4 ... S4
///     |               |
///     -----------------
///
/// Compare this to the subtrace of a bounded operator starting at time T2 with a bound (1, 3):
///
/// T1  T2  T3  T4  T5  T6 ... Tn
/// S1  S2  S3  S4  S5  S6 ... S4
///         |            |
///         --------------
///
/// All forward operators fold the sub-trace for each time into a single value, which becomes the
/// new state for that time.

pub type TimeBounds = (f64, f64);

fn fw_op_unbounded<T, U, F>(trace: Trace<T>, initial: U, combine: F) -> Trace<U>
where
    F: Fn(T, &U) -> U,
{
    let mut iter = trace.into_iter().rev();
    let first_element = iter.next();
    let mut u_trace = Trace::default();

    if first_element.is_none() {
        return u_trace;
    }

    let (t1, m1) = first_element.unwrap();
    let mut prev_time = t1;
    let mut prev_u = combine(m1, &initial);

    for (time, metric) in iter {
        let next_u = combine(metric, &prev_u);
        u_trace.insert(prev_time, prev_u);
        prev_u = next_u;
        prev_time = time;
    }

    u_trace.insert(prev_time, prev_u);
    u_trace
}

fn fw_op_bounded<T, F1, F2, U>(trace: Trace<T>, bounds: TimeBounds, initialize: F1, combine: F2) -> Trace<U>
where
    F1: Fn() -> U,
    F2: Fn(U, &T) -> U,
{
    let eval_subtrace = |time: f64| -> (f64, U) {
        let time_range = (time + bounds.0)..=(time + bounds.1);
        let subtrace = trace.range(time_range);
        let initial = initialize();
        let result = subtrace
            .into_iter()
            .rev()
            .fold(initial, |acc, (_, metric)| combine(acc, metric));

        (time, result)
    };

    trace.times().map(eval_subtrace).collect()
}

struct UnboundedIter<'a, T, F> {
    rest: Range<'a, T>,
    state: Option<(f64, T)>,
    combine: F,
}

impl<'a, T, F> UnboundedIter<'a, T, F>
where
    F: Fn(&T, &T) -> T,
{
    fn new(mut range: Range<'a, T>, init: T, combine: F) -> Self {
        Self {
            state: range.next_back().map(|(time, value)| (time, combine(&init, value))),
            rest: range,
            combine,
        }
    }
}

impl<'a, T, F> Iterator for UnboundedIter<'a, T, F>
where
    F: Fn(&T, &T) -> T,
{
    type Item = (f64, T);

    fn next(&mut self) -> Option<Self::Item> {
        let state = self.state.take()?;
        self.state = self
            .rest
            .next_back()
            .map(|(time, value)| (time, (self.combine)(&state.1, value)));

        Some(state)
    }
}

#[derive(Debug, Error)]
pub enum BoundedError<F> {
    #[error("Bounded formula error: {inner}")]
    FormulaError { inner: F },

    #[error("Subtrace evaluation for interval ({start:?}, {end:?}) is empty")]
    EmptySubtraceEvaluation { start: Bound<f64>, end: Bound<f64> },

    #[error("Empty interval")]
    EmptyInterval,
}

#[derive(Clone)]
struct UnboundedUnary<F> {
    subformula: F,
}

impl<F> UnboundedUnary<F> {
    fn new(formula: F) -> Self {
        Self { subformula: formula }
    }

    fn evaluate_range<'a, T, I, C>(&self, range: Range<'a, T>, init: I, combine: C) -> UnboundedIter<'a, T, C>
    where
        I: Fn() -> T,
        C: Fn(&T, &T) -> T,
    {
        UnboundedIter::new(range, init(), combine)
    }

    fn evaluate<S, M, I, C>(&self, trace: &Trace<S>, init: I, combine: C) -> Result<Trace<M>, BoundedError<F::Error>>
    where
        F: Formula<S, Metric = M>,
        I: Fn() -> M,
        C: Fn(&M, &M) -> M,
    {
        let subformula_evaluation = self
            .subformula
            .evaluate_trace(trace)
            .map_err(|inner| BoundedError::FormulaError { inner })?;

        let range = subformula_evaluation.range(..);
        let result = self.evaluate_range(range, init, combine).collect();

        Ok(result)
    }
}

#[derive(Clone)]
struct BoundedUnary<F> {
    inner: UnboundedUnary<F>,
    start: Bound<f64>,
    end: Bound<f64>,
}

impl<F> BoundedUnary<F> {
    fn new<R>(range: R, formula: F) -> Self
    where
        R: RangeBounds<f64>,
    {
        Self {
            inner: UnboundedUnary { subformula: formula },
            start: range.start_bound().cloned(),
            end: range.end_bound().cloned(),
        }
    }

    fn compute_start(&self, time: f64) -> Bound<f64> {
        match self.start {
            Bound::Unbounded => Bound::Included(time),
            Bound::Excluded(b) => Bound::Excluded(b + time),
            Bound::Included(b) => Bound::Included(b + time),
        }
    }

    fn compute_end(&self, time: f64) -> Bound<f64> {
        match self.end {
            Bound::Unbounded => Bound::Unbounded,
            Bound::Excluded(b) => Bound::Excluded(b + time),
            Bound::Included(b) => Bound::Included(b + time),
        }
    }

    fn evaluate<S, M, I, C>(&self, trace: &Trace<S>, init: I, combine: C) -> Result<Trace<M>, BoundedError<F::Error>>
    where
        F: Formula<S, Metric = M>,
        I: Fn() -> M,
        C: Fn(&M, &M) -> M,
    {
        let subformula_evaluation = self
            .inner
            .subformula
            .evaluate_trace(trace)
            .map_err(|inner| BoundedError::FormulaError { inner })?;

        let evaluate_time = |time: f64| -> Result<(f64, M), BoundedError<F::Error>> {
            let start = self.compute_start(time);
            let end = self.compute_end(time);
            let range = subformula_evaluation.range((start, end));
            let iter = self.inner.evaluate_range(range, &init, &combine);

            iter.last()
                .ok_or(BoundedError::EmptySubtraceEvaluation { start, end })
                .map(|(_, value)| (time, value))
        };

        subformula_evaluation.times().map(evaluate_time).collect()
    }
}

#[derive(Clone)]
enum BoundedUnaryOperator<F> {
    Bounded(BoundedUnary<F>),
    Unbounded(UnboundedUnary<F>),
}

impl<F> BoundedUnaryOperator<F> {
    fn unbounded(formula: F) -> Self {
        Self::Unbounded(UnboundedUnary::new(formula))
    }

    fn bounded<R>(range: R, formula: F) -> Self
    where
        R: RangeBounds<f64>,
    {
        Self::Bounded(BoundedUnary::new(range, formula))
    }

    fn evaluate<S, M, I, C>(&self, trace: &Trace<S>, init: I, combine: C) -> Result<Trace<M>, BoundedError<F::Error>>
    where
        F: Formula<S, Metric = M>,
        I: Fn() -> M,
        C: Fn(&M, &M) -> M,
    {
        match self {
            Self::Bounded(b) => b.evaluate(trace, init, combine),
            Self::Unbounded(u) => u.evaluate(trace, init, combine),
        }
    }
}

impl<F> From<BoundedUnary<F>> for BoundedUnaryOperator<F> {
    fn from(value: BoundedUnary<F>) -> Self {
        Self::Bounded(value)
    }
}

impl<F> From<UnboundedUnary<F>> for BoundedUnaryOperator<F> {
    fn from(value: UnboundedUnary<F>) -> Self {
        Self::Unbounded(value)
    }
}

/// Temporal operator that requires its subformula to always hold
///
/// The always operator works by scanning forward at each time and taking the minimum of all
/// included values. In cases where negative values represent failure, this behavior ensures that
/// the value for each time is positive only if all forward values are also positive. For floating
/// point values, the an example evaluation would look like the following:
///
/// | time | subformula | Always |
/// | ---- | ---------- | ------ |
/// |  0.0 |        1.0 |   -5.0 |
/// |  1.0 |        2.0 |   -5.0 |
/// |  2.0 |       -5.0 |   -5.0 |
/// |  3.0 |        4.0 |    4.0 |
/// |  4.0 |        6.0 |    6.0 |
///
/// Always formulas can be created either with or without bounds. An example of each can be found
/// below:
///
/// ```rust
/// use banquo::expressions::{Predicate, Term};
/// use banquo::operators::Always;
///
/// let subformula = Predicate::new(Term::variable("x", 1.0), Term::constant(2.0));
/// let unbounded_formula = Always::unbounded(subformula);
///
/// let subformula = Predicate::new(Term::variable("x", -1.0), Term::constant(-2.0));
/// let bounded_formula = Always::bounded(0.0, 4.0, subformula);
/// ```
#[derive(Clone)]
pub struct Always<F>(BoundedUnaryOperator<F>);

impl<F> Always<F> {
    /// Create an unbounded always formula
    ///
    /// An unbounded formula analyzes to the end of the trace for each time step in the trace
    /// produced by the subformula.
    pub fn unbounded(formula: F) -> Self {
        Self(BoundedUnaryOperator::unbounded(formula))
    }

    pub fn bounded<R>(range: R, formula: F) -> Self
    where
        R: RangeBounds<f64>,
    {
        Self(BoundedUnaryOperator::bounded(range, formula))
    }
}

impl<State, F, M> Formula<State> for Always<F>
where
    F: Formula<State, Metric = M>,
    M: Top + Meet,
{
    type Metric = M;
    type Error = BoundedError<F::Error>;

    fn evaluate_trace(&self, trace: &Trace<State>) -> Result<Trace<Self::Metric>, Self::Error> {
        self.0.evaluate(trace, M::top, M::meet)
    }
}

/// Temporal operator that requires its subformula to eventually hold
///
/// The eventually operator works by scanning forward at each time and taking the maximum of all
/// included values. In cases where negative values represent failure, this behavior ensures that
/// the value for each time is positive if any forward values are also positive. For floating point
/// values, the an example evaluation would look like the following:
///
/// | time | subformula | Always |
/// | ---- | ---------- | ------ |
/// |  0.0 |        1.0 |   -5.0 |
/// |  1.0 |        2.0 |   -5.0 |
/// |  2.0 |       -5.0 |   -5.0 |
/// |  3.0 |        4.0 |    4.0 |
/// |  4.0 |        6.0 |    6.0 |
///
/// Eventually formulas can be created either with or without bounds. An example of each can be
/// found below:
///
/// ```rust
/// use banquo::expressions::{Predicate, Term};
/// use banquo::operators::Always;
///
/// let subformula = Predicate::new(Term::variable("x", 1.0), Term::constant(2.0));
/// let unbounded_formula = Always::unbounded(subformula);
///
/// let subformula = Predicate::new(Term::variable("x", -1.0), Term::constant(-2.0));
/// let bounded_formula = Always::bounded(0.0, 4.0, subformula);
/// ```
#[derive(Clone)]
pub struct Eventually<F>(BoundedUnaryOperator<F>);

impl<F> Eventually<F> {
    /// Create an unbounded eventually formula
    ///
    /// An unbounded formula analyzes to the end of the trace for each time step in the trace
    /// produced by the subformula.
    pub fn unbounded(formula: F) -> Self {
        Self(BoundedUnaryOperator::unbounded(formula))
    }

    pub fn bounded<R>(range: R, formula: F) -> Self
    where
        R: RangeBounds<f64>,
    {
        Self(BoundedUnaryOperator::bounded(range, formula))
    }
}

impl<State, F, M> Formula<State> for Eventually<F>
where
    F: Formula<State, Metric = M>,
    M: Bottom + Join,
{
    type Metric = M;
    type Error = BoundedError<F::Error>;

    fn evaluate_trace(&self, trace: &Trace<State>) -> Result<Trace<Self::Metric>, Self::Error> {
        self.0.evaluate(trace, M::bottom, M::join)
    }
}

/// Temporal operator that requires its subformula to hold at next time
///
/// The next operator is a special case of temporal operator that only operates on the next value in
/// the output of the subformula rather than a sub-trace of arbitrary length like [Always] and
/// [Eventually]. This operator can be seen to some extent as a left-shift operation, where each
/// state is moved to the time value directly before it. In the case of the last element in the
/// trace, the state is replaced with a default value. For floating point values, an example
/// evalution would look like the following:
///
/// | time | subformula | next |
/// | ---- | ---------- | ---- |
/// |  0.0 |        1.0 |  2.1 |
/// |  1.0 |        2.1 |  3.5 |
/// |  2.0 |        3.5 | -1.7 |
/// |  3.0 |       -1.7 |  2.3 |
/// |  4.0 |        2.3 | -inf |
///
/// Creating a formula using the Next operator can be accomplished like so:
///
/// ```rust
/// use banquo::expressions::{Predicate, Term};
/// use banquo::operators::Next;
///
/// let subformula = Predicate::new(Term::variable("x", 1.0), Term::constant(3.0));
/// let formula = Next::new(subformula);
/// ```
///
#[derive(Clone, Debug)]
pub struct Next<F> {
    subformula: F,
}

impl<F> Next<F> {
    pub fn new(subformula: F) -> Self {
        Self { subformula }
    }
}

fn next_op<T, F, U>(trace: Trace<T>, f: F) -> Trace<U>
where
    F: Fn(&T, T) -> U,
    U: Bottom,
{
    let mut iter = trace.into_iter().rev();
    let mut trace = Trace::default();

    if let Some((time, mut metric)) = iter.next() {
        trace.insert(time, U::bottom());

        for (prev_time, prev_metric) in iter {
            trace.insert(prev_time, f(&prev_metric, metric));
            metric = prev_metric;
        }
    }

    trace
}

impl<State, F, Metric> Formula<State> for Next<F>
where
    F: Formula<State, Metric = Metric>,
    Metric: Bottom,
{
    type Metric = F::Metric;
    type Error = F::Error;

    fn evaluate_trace(&self, trace: &Trace<State>) -> Result<Trace<Self::Metric>, Self::Error> {
        self.subformula
            .evaluate_trace(trace)
            .map(|inner_trace| next_op(inner_trace, |_, metric| metric))
    }
}

pub struct Until<Left, Right> {
    left: Left,
    right: Right,
}

impl<Left, Right> Until<Left, Right> {
    pub fn new(left: Left, right: Right) -> Self {
        Self { left, right }
    }
}

fn until_eval_time<M>(left: &Trace<M>, time: f64, right: M, prev: &M) -> M
where
    M: Top + Meet + Join,
{
    let left_metric = left.range(..=time).into_iter().fold(M::top(), |l, (_, r)| l.meet(r)); // Minimum of left trace until time
    let combined_metric = left_metric.meet(&right); // minimum of ^ and right metric
    combined_metric.join(prev) // Maximum of ^  and previous metric
}

fn until_op<M, I>(left: Trace<M>, right: I, mut prev_time: f64, mut prev_metric: M) -> Trace<M>
where
    I: Iterator<Item = (f64, M)>,
    M: Top + Bottom + Meet + Join,
{
    let mut trace = Trace::default();
    let bottom = M::bottom();

    prev_metric = until_eval_time(&left, prev_time, prev_metric, &bottom);

    for (time, right_metric) in right {
        let next_metric = until_eval_time(&left, time, right_metric, &prev_metric);

        trace.insert(prev_time, prev_metric);
        prev_time = time;
        prev_metric = next_metric;
    }

    trace.insert(prev_time, prev_metric);
    trace
}

impl<Left, Right, State, Metric> Formula<State> for Until<Left, Right>
where
    Left: Formula<State, Metric = Metric>,
    Right: Formula<State, Metric = Metric>,
    Metric: Clone + Top + Bottom + Meet + Join,
{
    type Metric = Metric;
    type Error = BinaryOperatorError<Left::Error, Right::Error>;

    fn evaluate_trace(&self, trace: &Trace<State>) -> Result<Trace<Self::Metric>, Self::Error> {
        let left_trace = self
            .left
            .evaluate_trace(trace)
            .map_err(BinaryOperatorError::LeftError)?;

        let right_trace = self
            .right
            .evaluate_trace(trace)
            .map_err(BinaryOperatorError::RightError)?;

        let mut iter = right_trace.into_iter().rev();

        let evaluated_trace = if let Some((prev_time, prev_metric)) = iter.next() {
            until_op(left_trace, iter, prev_time, prev_metric)
        } else {
            Trace::default()
        };

        Ok(evaluated_trace)
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;

    use thiserror::Error;

    use super::{Always, And, BinaryOperatorError, BoundedError, Eventually, Implies, Next, Not, Or, Until};
    use crate::formulas::Formula;
    use crate::trace::Trace;

    pub struct Const;

    #[derive(Debug, Error)]
    pub enum ConstError {}

    impl<S> Formula<S> for Const
    where
        S: Clone,
    {
        type Metric = S;
        type Error = ConstError;

        fn evaluate_trace(&self, trace: &Trace<S>) -> Result<Trace<Self::Metric>, Self::Error> {
            Ok(trace.clone())
        }
    }

    pub struct ConstLeft;

    impl<L, R> Formula<(L, R)> for ConstLeft
    where
        L: Clone,
    {
        type Metric = L;
        type Error = ConstError;

        fn evaluate_trace(&self, trace: &Trace<(L, R)>) -> Result<Trace<Self::Metric>, Self::Error> {
            let left_trace = trace.iter().map(|(time, (left, _))| (time, left.clone())).collect();

            Ok(left_trace)
        }
    }

    pub struct ConstRight;

    impl<L, R> Formula<(L, R)> for ConstRight
    where
        R: Clone,
    {
        type Metric = R;
        type Error = ConstError;

        fn evaluate_trace(&self, trace: &Trace<(L, R)>) -> Result<Trace<Self::Metric>, Self::Error> {
            let right_trace = trace.iter().map(|(time, (_, right))| (time, right.clone())).collect();

            Ok(right_trace)
        }
    }

    #[test]
    fn not() -> Result<(), ConstError> {
        let formula = Not::new(Const);
        let input = Trace::from_iter([(0, 0.0), (1, 1.0), (2, 2.0), (3, 3.0)]);
        let robustness = formula.evaluate_trace(&input)?;
        let expected = Trace::from_iter([(0, 0.0), (1, -1.0), (2, -2.0), (3, -3.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn or() -> Result<(), BinaryOperatorError<ConstError, ConstError>> {
        let formula = Or::new(ConstLeft, ConstRight);
        let left = Trace::from_iter([(0, 0.0), (1, 1.0), (2, 2.0), (3, 3.0)]);
        let right = Trace::from_iter([(0, 1.0), (1, 0.0), (2, 4.0), (3, 6.0)]);
        let input = left.zip(right);
        let robustness = formula.evaluate_trace(&input)?;
        let expected = Trace::from_iter([(0, 1.0), (1, 1.0), (2, 4.0), (3, 6.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn and() -> Result<(), BinaryOperatorError<ConstError, ConstError>> {
        let formula = And::new(ConstLeft, ConstRight);
        let left = Trace::from_iter([(0, 0.0), (1, 1.0), (2, 2.0), (3, 3.0)]);
        let right = Trace::from_iter([(0, 1.0), (1, 0.0), (2, 4.0), (3, 6.0)]);
        let input = left.zip(right);
        let robustness = formula.evaluate_trace(&input)?;
        let expected = Trace::from_iter([(0, 0.0), (1, 0.0), (2, 2.0), (3, 3.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn implies() -> Result<(), BinaryOperatorError<ConstError, ConstError>> {
        let formula = Implies::new(ConstLeft, ConstRight);
        let antecedent = Trace::from_iter([(0, 0.0), (1, 1.0), (2, -4.0), (3, 3.0)]);
        let consequent = Trace::from_iter([(0, 1.0), (1, 0.0), (2, 2.0), (3, 6.0)]);
        let input = antecedent.zip(consequent);
        let robustness = formula.evaluate_trace(&input)?;
        let expected = Trace::from_iter([(0, 1.0), (1, 0.0), (2, 4.0), (3, 6.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn always() -> Result<(), BoundedError<ConstError>> {
        let formula = Always::unbounded(Const);
        let input = Trace::from_iter([(0, 4.0), (1, 2.0), (2, 3.0), (3, 1.0), (4, 3.0)]);
        let robustness = formula.evaluate_trace(&input)?;
        let expected = Trace::from_iter([(0, 1.0), (1, 1.0), (2, 1.0), (3, 1.0), (4, 3.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn bounded_always() -> Result<(), BoundedError<ConstError>> {
        let formula = Always::bounded(0f64..=2f64, Const);
        let input = Trace::from_iter([(0, 4.0), (1, 2.0), (2, 3.0), (3, 1.0), (4, 3.0)]);
        let robustness = formula.evaluate_trace(&input)?;
        let expected = Trace::from_iter([(0, 2.0), (1, 1.0), (2, 1.0), (3, 1.0), (4, 3.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn eventually() -> Result<(), BoundedError<ConstError>> {
        let formula = Eventually::unbounded(Const);
        let input = Trace::from_iter([(0, 4.0), (1, 2.0), (2, 3.0), (3, 1.0), (4, 3.0)]);
        let robustness = formula.evaluate_trace(&input)?;
        let expected = Trace::from_iter([(0, 4.0), (1, 3.0), (2, 3.0), (3, 3.0), (4, 3.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn bounded_eventually() -> Result<(), BoundedError<ConstError>> {
        let formula = Eventually::bounded(0f64..=2f64, Const);
        let input = Trace::from_iter([(0, 4.0), (1, 2.0), (2, 1.0), (3, 5.0), (4, 3.0)]);
        let robustness = formula.evaluate_trace(&input)?;
        let expected = Trace::from_iter([(0, 4.0), (1, 5.0), (2, 5.0), (3, 5.0), (4, 3.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn next() -> Result<(), ConstError> {
        let input = Trace::from_iter([(0, 1.0), (1, 2.0), (2, 3.0), (3, 4.0)]);
        let formula = Next::new(Const);
        let robustness = formula.evaluate_trace(&input)?;
        let expected = Trace::from_iter([(0, 2.0), (1, 3.0), (2, 4.0), (3, f64::NEG_INFINITY)]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn until() -> Result<(), BinaryOperatorError<ConstError, ConstError>> {
        let formula = Until::new(ConstLeft, ConstRight);
        let left = Trace::from_iter([(0.0, 3.0), (1.0, 1.5), (2.0, 1.4), (3.0, 1.1)]);
        let right = Trace::from_iter([(0.0, -2.1), (1.0, 3.7), (2.0, 1.2), (3.0, 2.2)]);
        let input = left.zip(right);
        let robustness = formula.evaluate_trace(&input)?;

        assert_eq!(robustness[3.0], 1.1);
        assert_eq!(robustness[2.0], 1.2);
        assert_eq!(robustness[1.0], 1.5);
        assert_eq!(robustness[0.0], 1.5);

        Ok(())
    }
}
