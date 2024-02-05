use std::fmt::{Display, Formatter};
use std::ops::{Bound, RangeBounds};

use thiserror::Error;

use crate::Formula;
use crate::metrics::{Top, Bottom, Meet, Join};
use crate::trace::{Range, Trace};
use super::BinaryOperatorError;

struct ForwardIter<'a, T, F> {
    rest: Range<'a, T>,
    state: Option<(f64, T)>,
    combine: F,
}

impl<'a, T, F> ForwardIter<'a, T, F>
where
    F: Fn(&T, &T) -> T,
{
    fn new(mut range: Range<'a, T>, init: T, combine: F) -> Self {
        // If the provided range is empty, then the metric at time 0 is whatever the initial value
        // is. This behavior matches what is expected when a forward operator analyzes an empty
        // trace.
        let state = match range.next_back() {
            Some((time, value)) => Some((time, combine(&init, value))),
            None => Some((0.0, init)),
        };

        Self {
            rest: range,
            state,
            combine,
        }
    }
}

impl<'a, T, F> Iterator for ForwardIter<'a, T, F>
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

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
enum Endpoint {
    Open(f64),
    Closed(f64),
}

impl Endpoint {
    fn value(&self) -> f64 {
        match self {
            Self::Open(value) => *value,
            Self::Closed(value) => *value,
        }
    }

    fn map<F>(self, f: F) -> Endpoint
    where
        F: Fn(f64) -> f64,
    {
        match self {
            Self::Open(value) => Self::Open(f(value)),
            Self::Closed(value) => Self::Closed(f(value)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Interval {
    start: Endpoint,
    end: Endpoint,
}

impl Interval {
    fn shift(&self, amount: f64) -> Interval {
        Self {
            start: self.start.map(|start| start + amount),
            end: self.end.map(|end| end + amount),
        }
    }
}

impl Display for Interval {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let opening = match &self.start {
            Endpoint::Open(_) => '(',
            Endpoint::Closed(_) => '[',
        };

        let closing = match &self.end {
            Endpoint::Open(_) => ')',
            Endpoint::Closed(_) => ']',
        };

        write!(f, "{}{},{}{}", opening, self.start.value(), self.end.value(), closing)
    }
}

impl<T> From<std::ops::Range<T>> for Interval
where
    T: Into<f64>,
{
    fn from(std::ops::Range { start, end }: std::ops::Range<T>) -> Self {
        Self {
            start: Endpoint::Closed(start.into()),
            end: Endpoint::Open(end.into()),
        }
    }
}

impl<T> From<std::ops::RangeInclusive<T>> for Interval
where
    T: Into<f64>,
{
    fn from(range: std::ops::RangeInclusive<T>) -> Self {
        let (start, end) = range.into_inner();

        Self {
            start: Endpoint::Closed(start.into()),
            end: Endpoint::Closed(end.into()),
        }
    }
}

impl RangeBounds<f64> for Interval {
    fn contains<U>(&self, item: &U) -> bool
    where
        U: PartialOrd<f64> + ?Sized,
    {
        let within_lower = match &self.start {
            Endpoint::Open(lower) => item.gt(lower),
            Endpoint::Closed(lower) => item.ge(lower),
        };

        let within_upper = match &self.end {
            Endpoint::Open(upper) => item.lt(upper),
            Endpoint::Closed(upper) => item.le(upper),
        };

        within_lower && within_upper
    }

    fn start_bound(&self) -> Bound<&f64> {
        match &self.start {
            Endpoint::Open(start) => Bound::Excluded(start),
            Endpoint::Closed(start) => Bound::Included(start),
        }
    }

    fn end_bound(&self) -> Bound<&f64> {
        match &self.end {
            Endpoint::Open(start) => Bound::Excluded(start),
            Endpoint::Closed(start) => Bound::Included(start),
        }
    }
}

#[derive(Debug, Clone)]
struct UnaryOperator<F> {
    subformula: F,
    bounds: Option<Interval>
}

#[derive(Debug, Error)]
pub enum ForwardOperatorError<F> {
    #[error("Bounded formula error: {0}")]
    FormulaError(F),

    #[error("Subtrace evaluation for interval {0} is empty")]
    EmptySubtraceEvaluation(Interval),

    #[error("Empty interval")]
    EmptyInterval,
}

impl<F> UnaryOperator<F> {
    fn new(bounds: Option<Interval>, subformula: F) -> Self {
        Self { bounds, subformula }
    }

    fn evaluate<State, I, C, Metric>(&self, trace: &Trace<State>, init: I, combine: C) -> Result<Trace<F::Metric>, ForwardOperatorError<F::Error>>
    where
        F: Formula<State, Metric = Metric>,
        I: Fn() -> Metric,
        C: Fn(&Metric, &Metric) -> Metric,
    {
        if trace.is_empty() {
            return Ok(Trace::from_iter([(0.0, init())]));
        }

        let inner = self.subformula
            .evaluate(trace)
            .map_err(ForwardOperatorError::FormulaError)?;

        match &self.bounds {
            None => {
                let first = init();
                let range = inner.range(..);
                let result = ForwardIter::new(range, first, combine).collect();

                Ok(result)
            },
            Some(interval) => {
                let evaluate_time = |time: f64| -> Result<(f64, Metric), ForwardOperatorError<F::Error>> {
                    let shifted = interval.shift(time);
                    let range = inner.range((shifted.start_bound(), shifted.end_bound()));
                    let iter = ForwardIter::new(range, init(), &combine);

                    iter.last()
                        .ok_or(ForwardOperatorError::EmptySubtraceEvaluation(shifted))
                        .map(|(_, value)| (time, value))
                };

                inner.times().map(evaluate_time).collect()
            },
        }
    }
}

/// Temporal operator that requires its subformula to hold for every time.
///
/// The `Always` operator works by scanning forward at each time and taking the minimum of all
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
/// `Always` formulas can be created either with or without bounds like so:
///
/// ```rust
/// use banquo::predicate;
/// use banquo::operators::Always;
///
/// let unbounded = Always::unbounded(predicate!(x <= 2.0));
/// let bounded = Always::bounded(0.0..=4.0, predicate!(-1.0 * x <= -2.0));
/// ```
#[derive(Clone)]
pub struct Always<F>(UnaryOperator<F>);

impl<F> Always<F> {
    /// Create an unbounded `Always` operator.
    ///
    /// An unbounded `Always` operator analyzes to the end of the trace for each time step in the
    /// trace produced by the subformula.
    ///
    /// # Example
    ///
    /// ```rust
    /// use banquo::predicate;
    /// use banquo::operators::Always;
    ///
    /// let phi = Always::unbounded(predicate!{ rpm <= 5000.0 });
    /// ```
    pub fn unbounded(formula: F) -> Self {
        Self(UnaryOperator::new(None, formula))
    }

    /// Create a bounded `Always` operator.
    ///
    /// At any time `t` the bounded `Always` operator analyzes the sub-interval of the trace that
    /// includes all the times from `t + start bound` to `t + end bound`. The end bound can either
    /// be exclusive or inclusive.
    ///
    /// # Example
    ///
    /// ```rust
    /// use banquo::predicate;
    /// use banquo::operators::Always;
    ///
    /// let phi = Always::bounded(0.0..3.0, predicate!{ rpm <= 5.0 });   // Exclusive end bound
    /// let phi = Always::bounded(0.0..=3.0, predicate!{ rpm <= 5.0 });  // Inclusive end bound
    /// ```
    pub fn bounded<I>(interval: I, formula: F) -> Self
    where
        I: Into<Interval>,
    {
        Self(UnaryOperator::new(Some(interval.into()), formula))
    }
}

impl<State, F, M> Formula<State> for Always<F>
where
    F: Formula<State, Metric = M>,
    M: Top + Meet,
{
    type Metric = M;
    type Error = ForwardOperatorError<F::Error>;

    fn evaluate(&self, trace: &Trace<State>) -> Result<Trace<Self::Metric>, Self::Error> {
        self.0.evaluate(trace, M::top, M::min)
    }
}

/// Temporal operator that requires its subformula to hold at some time in the future.
///
/// The `Eventually` operator works by scanning forward at each time and taking the maximum of all
/// included values. In cases where negative values represent failure, this behavior ensures that
/// the value for each time is positive if any forward values are positive. For floating point
/// values, then an example evaluation would look like the following:
///
/// | time | subformula | Always |
/// | ---- | ---------- | ------ |
/// |  0.0 |        1.0 |   -5.0 |
/// |  1.0 |        2.0 |   -5.0 |
/// |  2.0 |       -5.0 |   -5.0 |
/// |  3.0 |        4.0 |    4.0 |
/// |  4.0 |        6.0 |    6.0 |
///
/// `Eventually` formulas can be created either with or without bounds like so:
///
/// ```rust
/// use banquo::predicate;
/// use banquo::operators::Eventually;
///
/// let unbounded = Eventually::unbounded(predicate!(x <= 2.0));
/// let bounded = Eventually::bounded(0.0..=4.0, predicate!(x * -1.0 <= -2.0));
/// ```
#[derive(Clone)]
pub struct Eventually<F>(UnaryOperator<F>);

impl<F> Eventually<F> {
    /// Create an unbounded `Eventually` operator.
    ///
    /// An unbounded `Eventually` operator analyzes to the end of the trace for each time step in
    /// the trace produced by the subformula.
    ///
    /// # Example
    ///
    /// ```rust
    /// use banquo::predicate;
    /// use banquo::operators::Eventually;
    ///
    /// let phi = Eventually::unbounded(predicate!{ speed <= 45.0 });
    /// ```
    pub fn unbounded(formula: F) -> Self {
        Self(UnaryOperator::new(None, formula))
    }

    /// Create a bounded `Eventually` operator.
    ///
    /// At any time `t` the bounded `Eventually` operator analyzes the sub-interval of the trace
    /// that includes all the times from `t + start bound` to `t + end bound`. The end bound can
    /// either be exclusive or inclusive.
    ///
    /// # Example
    ///
    /// ```rust
    /// use banquo::predicate;
    /// use banquo::operators::Eventually;
    ///
    /// let phi = Eventually::bounded(0.0..3.0, predicate!{ speed <= 45.0 });   // Exclusive end bound
    /// let phi = Eventually::bounded(0.0..=3.0, predicate!{ speed <= 45.0 });  // Inclusive end bound
    /// ```
    pub fn bounded<I>(interval: I, formula: F) -> Self
    where
        I: Into<Interval>,
    {
        Self(UnaryOperator::new(Some(interval.into()), formula))
    }
}

impl<State, F, M> Formula<State> for Eventually<F>
where
    F: Formula<State, Metric = M>,
    M: Bottom + Join,
{
    type Metric = M;
    type Error = ForwardOperatorError<F::Error>;

    fn evaluate(&self, trace: &Trace<State>) -> Result<Trace<Self::Metric>, Self::Error> {
        self.0.evaluate(trace, M::bottom, M::max)
    }
}

/// Temporal operator that requires its subformula to hold at next time step.
///
/// The `Next` operator is a special case of temporal operator that only operates on the next value in
/// the output of the subformula rather than a sub-trace of arbitrary length like [`Always`] and
/// [`Eventually`]. This operator can be seen to some extent as a left-shift operation, where each
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
/// Creating a `Next` operator can be accomplished like so:
///
/// ```rust
/// use banquo::predicate;
/// use banquo::operators::Next;
///
/// let formula = Next::new(predicate!(x <= 3.0));
/// ```
///
#[derive(Clone, Debug)]
pub struct Next<F> {
    subformula: F,
}

impl<F> Next<F> {
    /// Create a `Next` operator.
    ///
    /// A `Next` operator shifts the values in the trace produced by the sub-formula one time-step
    /// to the left.
    ///
    /// # Example
    ///
    /// ```rust
    /// use banquo::predicate;
    /// use banquo::operators::Next;
    ///
    /// let phi = Next::new(predicate!{ rpm <= 5000.0 });
    /// ```
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

    fn evaluate(&self, trace: &Trace<State>) -> Result<Trace<Self::Metric>, Self::Error> {
        self.subformula
            .evaluate(trace)
            .map(|inner_trace| next_op(inner_trace, |_, metric| metric))
    }
}

/// Temporal operator that requires its right subformula to hold and its left subformula to hold up
/// to and including the time the right subformula holds.
///
/// For each time in the trace, The `Until` operator evaluates the current state using the right
/// subformula, and all states from the start of the trace to current time using the left trace.
/// The operator takes the minimum of the left trace evaluation along with the state evaluation,
/// and then the maximum of the current result and the previous result. This is the equivalent of
/// evaluating for every time `t` the formula `(always[0, t] left) and (right)`. The maximum
/// operation across all times ensures that if multiple times satisfy the formula, the best
/// metric is kept.
///
/// Creating a formula using the `Until` operator can be accomplished like so:
///
/// ```rust
/// use banquo::predicate;
/// use banquo::operators::Until;
///
/// let lhs = predicate!{ x <= 3.0 };
/// let rhs = predicate!{ -1.0 * y <= -3.0 };
///
/// let formula = Until::new(lhs, rhs);
/// ```
pub struct Until<Left, Right> {
    left: Left,
    right: Right,
}

impl<Left, Right> Until<Left, Right> {
    /// Create an `Until` operator.
    ///
    /// The `Until` ensures that its left formula holds up to and inclusing the time its right
    /// formula holds.
    ///
    /// # Example
    ///
    /// ```rust
    /// use banquo::predicate;
    /// use banquo::operators::Until;
    ///
    /// let lhs = predicate!{ x <= 3.0 };
    /// let rhs = predicate!{ -1.0 * y <= -3.0 };
    ///
    /// let phi = Until::new(lhs, rhs);
    /// ```
    pub fn new(left: Left, right: Right) -> Self {
        Self { left, right }
    }
}

fn until_eval_time<M>(left: &Trace<M>, time: f64, right: M, prev: &M) -> M
where
    M: Top + Meet + Join,
{
    let left_metric = left
        .range(..=time)
        .fold(M::top(), |l, (_, r)| l.min(r)); // Minimum of left trace until time

    let combined_metric = left_metric.min(&right); // minimum of ^ and right metric
    combined_metric.max(prev) // Maximum of ^  and previous metric
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

    fn evaluate(&self, trace: &Trace<State>) -> Result<Trace<Self::Metric>, Self::Error> {
        let left_trace = self
            .left
            .evaluate(trace)
            .map_err(BinaryOperatorError::LeftError)?;

        let right_trace = self
            .right
            .evaluate(trace)
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
    use crate::Formula;
    use crate::operators::BinaryOperatorError;
    use crate::operators::test::*;
    use crate::trace::Trace;
    use super::{Always, Eventually, Next, Until, ForwardOperatorError};

    #[test]
    fn always() -> Result<(), ForwardOperatorError<ConstError>> {
        let input = Trace::from_iter([
            (0, 4.0),
            (1, 2.0),
            (2, 3.0),
            (3, 1.0),
            (4, 3.0),
        ]);

        let formula = Always::unbounded(Const);
        let robustness = formula.evaluate(&input)?;
        let expected = Trace::from_iter([
            (0, 1.0),
            (1, 1.0),
            (2, 1.0),
            (3, 1.0),
            (4, 3.0),
        ]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn bounded_always() -> Result<(), ForwardOperatorError<ConstError>> {
        let input = Trace::from_iter([
            (0, 4.0),
            (1, 2.0),
            (2, 3.0),
            (3, 1.0),
            (4, 3.0),
        ]);

        let formula = Always::bounded(0.0..=2.0, Const);
        let robustness = formula.evaluate(&input)?;
        let expected = Trace::from_iter([
            (0, 2.0),
            (1, 1.0),
            (2, 1.0),
            (3, 1.0),
            (4, 3.0),
        ]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn eventually() -> Result<(), ForwardOperatorError<ConstError>> {
        let input = Trace::from_iter([
            (0, 4.0),
            (1, 2.0),
            (2, 3.0),
            (3, 1.0),
            (4, 3.0),
        ]);

        let formula = Eventually::unbounded(Const);
        let robustness = formula.evaluate(&input)?;
        let expected = Trace::from_iter([
            (0, 4.0),
            (1, 3.0),
            (2, 3.0),
            (3, 3.0),
            (4, 3.0),
        ]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn bounded_eventually() -> Result<(), ForwardOperatorError<ConstError>> {
        let input = Trace::from_iter([
            (0, 4.0),
            (1, 2.0),
            (2, 1.0),
            (3, 5.0),
            (4, 3.0),
        ]);

        let formula = Eventually::bounded(0.0..=2.0, Const);
        let robustness = formula.evaluate(&input)?;
        let expected = Trace::from_iter([
            (0, 4.0),
            (1, 5.0),
            (2, 5.0),
            (3, 5.0),
            (4, 3.0),
        ]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn bounds() -> Result<(), ForwardOperatorError<ConstError>> {
        let input = Trace::from_iter([
            (0, 4.0),
            (1, 2.0),
            (2, 1.0),
            (3, 5.0),
            (4, 3.0),
        ]);

        let expected = Trace::from_iter([
            (0, 4.0),
            (1, 5.0),
            (2, 5.0),
            (3, 5.0),
            (4, 3.0),
        ]);

        let f1 = Eventually::bounded(0f64..=2f64, Const);
        let f2 = Eventually::bounded(0f64..3f64, Const);

        assert_eq!(f1.evaluate(&input)?, expected);
        assert_eq!(f2.evaluate(&input)?, expected);
        Ok(())
    }

    #[test]
    fn next() -> Result<(), ConstError> {
        let input = Trace::from_iter([
            (0, 1.0),
            (1, 2.0),
            (2, 3.0),
            (3, 4.0),
        ]);

        let formula = Next::new(Const);
        let robustness = formula.evaluate(&input)?;
        let expected = Trace::from_iter([
            (0, 2.0),
            (1, 3.0),
            (2, 4.0),
            (3, f64::NEG_INFINITY),
        ]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn until() -> Result<(), BinaryOperatorError<ConstError, ConstError>> {
        let input = Trace::from_iter([
            (0.0, (3.0, -2.1)),
            (1.0, (1.5, 3.7)),
            (2.0, (1.4, 1.2)),
            (3.0, (1.1, 2.2)),
        ]);

        let formula = Until::new(ConstLeft, ConstRight);
        let robustness = formula.evaluate(&input)?;

        assert_eq!(robustness[3.0], 1.1);
        assert_eq!(robustness[2.0], 1.2);
        assert_eq!(robustness[1.0], 1.5);
        assert_eq!(robustness[0.0], 1.5);

        Ok(())
    }
}
