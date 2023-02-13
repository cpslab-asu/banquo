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
use super::binary::{Join, Meet};
use crate::formulas::Formula;
use crate::trace::Trace;

pub type TimeBounds = (f64, f64);

/// Trait representing a type with a least value
///
/// When combined with the [Join] trait, this element should serve as the identity for the operator.
/// In other words, for any value x: join(x, bottom) = x. This value is sometimes referred to as
/// the zero element.
pub trait Bottom {
    fn bottom() -> Self;
}

/// Trait representing a type with a greatest value
///
/// When combined with the [Meet] trait, this element should serve as the identity for the
/// operator. In other words, for any value x: meet(x, top) = x. This value is sometimes referred
/// to as the one element.
pub trait Top {
    fn top() -> Self;
}

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
        u_trace.insert_state(prev_time, prev_u);
        prev_u = next_u;
        prev_time = time;
    }

    u_trace.insert_state(prev_time, prev_u);
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
/// let unbounded_formula = Always::new_unbounded(subformula);
///
/// let subformula = Predicate::new(Term::variable("x", -1.0), Term::constant(-2.0));
/// let bounded_formula = Always::new_bounded(subformula, (0.0, 4.0));
/// ```
#[derive(Clone, Debug)]
pub struct Always<F> {
    subformula: F,
}

impl<F> Always<F> {
    pub fn new(subformula: F) -> Self {
        Self { subformula }
    }
}

impl<State, F, M> Formula<State> for Always<F>
where
    F: Formula<State, Metric = M>,
    M: Top + for<'a> Meet<&'a M>,
{
    type Metric = M;
    type Error = F::Error;

    fn evaluate_trace(&self, trace: &Trace<State>) -> Result<Trace<Self::Metric>, Self::Error> {
        let subformula_trace = self.subformula.evaluate_trace(trace)?;
        let meet = |left: M, right: &M| left.meet(right);
        let evaluated_trace = fw_op_unbounded(subformula_trace, M::top(), meet);

        Ok(evaluated_trace)
    }
}

pub struct BoundedAlways<F> {
    bounds: TimeBounds,
    subformula: F,
}

impl<F> BoundedAlways<F> {
    pub fn new<Lower, Upper>(lower: Lower, upper: Upper, subformula: F) -> Self
    where
        Lower: Into<f64>,
        Upper: Into<f64>,
    {
        Self {
            bounds: (lower.into(), upper.into()),
            subformula,
        }
    }
}

impl<State, F, M> Formula<State> for BoundedAlways<F>
where
    F: Formula<State, Metric = M>,
    M: Top + for<'a> Meet<&'a M>,
{
    type Metric = M;
    type Error = F::Error;

    fn evaluate_trace(&self, trace: &Trace<State>) -> Result<Trace<Self::Metric>, Self::Error> {
        let subformula_trace = self.subformula.evaluate_trace(trace)?;
        let meet = |left: M, right: &M| left.meet(right);
        let evaluated_trace = fw_op_bounded(subformula_trace, self.bounds, M::top, meet);

        Ok(evaluated_trace)
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
/// let unbounded_formula = Always::new_unbounded(subformula);
///
/// let subformula = Predicate::new(Term::variable("x", -1.0), Term::constant(-2.0));
/// let bounded_formula = Always::new_bounded(subformula, (0.0, 4.0));
/// ```
#[derive(Clone, Debug)]
pub struct Eventually<F> {
    subformula: F,
}

impl<F> Eventually<F> {
    pub fn new(subformula: F) -> Self {
        Self { subformula }
    }
}

impl<State, F, M> Formula<State> for Eventually<F>
where
    F: Formula<State, Metric = M>,
    M: Bottom + for<'a> Join<&'a M>,
{
    type Metric = M;
    type Error = F::Error;

    fn evaluate_trace(&self, trace: &Trace<State>) -> Result<Trace<Self::Metric>, Self::Error> {
        let subformula_trace = self.subformula.evaluate_trace(trace)?;
        let join = |left: M, right: &M| left.join(right);
        let evaluated_trace = fw_op_unbounded(subformula_trace, M::bottom(), join);

        Ok(evaluated_trace)
    }
}

pub struct BoundedEventually<F> {
    subformula: F,
    bounds: TimeBounds,
}

impl<F> BoundedEventually<F> {
    pub fn new<Lower, Upper>(subformula: F, lower: Lower, upper: Upper) -> Self
    where
        Lower: Into<f64>,
        Upper: Into<f64>,
    {
        Self {
            subformula,
            bounds: (lower.into(), upper.into()),
        }
    }
}

impl<State, F, M> Formula<State> for BoundedEventually<F>
where
    F: Formula<State, Metric = M>,
    M: Bottom + for<'a> Join<&'a M>,
{
    type Metric = M;
    type Error = F::Error;

    fn evaluate_trace(&self, trace: &Trace<State>) -> Result<Trace<Self::Metric>, Self::Error> {
        let subformula_trace = self.subformula.evaluate_trace(trace)?;
        let join = |left: M, right: &M| left.join(right);
        let evaluated_trace = fw_op_bounded(subformula_trace, self.bounds, M::bottom, join);

        Ok(evaluated_trace)
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
        trace.insert_state(time, U::bottom());

        while let Some((prev_time, prev_metric)) = iter.next() {
            trace.insert_state(prev_time, f(&prev_metric, metric));
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

#[cfg(test)]
mod tests {
    use super::{Always, BoundedAlways, BoundedEventually, Eventually, Next};
    use crate::operators::testing::{Const, ConstError};
    use crate::trace::Trace;
    use crate::Formula;

    #[test]
    fn always_unbounded_robustness() -> Result<(), ConstError> {
        let inner = Trace::from_iter([(0, 4.0), (1, 2.0), (2, 3.0), (3, 1.0), (4, 3.0)]);
        let formula = Always::new(Const::from(inner));

        let input = Trace::default();
        let robustness = formula.evaluate_states(&input)?;
        let expected = Trace::from_iter([(0, 1.0), (1, 1.0), (2, 1.0), (3, 1.0), (4, 3.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn always_bounded_robustness() -> Result<(), ConstError> {
        let inner = Trace::from_iter([(0, 4.0), (1, 2.0), (2, 3.0), (3, 1.0), (4, 3.0)]);
        let formula = BoundedAlways::new(Const::from(inner), 0, 2);

        let input = Trace::default();
        let robustness = formula.evaluate_states(&input)?;
        let expected = Trace::from_iter([(0, 2.0), (1, 1.0), (2, 1.0), (3, 1.0), (4, 3.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn eventually_unbounded_robustness() -> Result<(), ConstError> {
        let inner = Trace::from_iter([(0, 4.0), (1, 2.0), (2, 3.0), (3, 1.0), (4, 3.0)]);
        let formula = Eventually::new(Const::from(inner));

        let input = Trace::default();
        let robustness = formula.evaluate_states(&input)?;
        let expected = Trace::from_iter([(0, 4.0), (1, 3.0), (2, 3.0), (3, 3.0), (4, 3.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn eventually_bounded_robustness() -> Result<(), ConstError> {
        let inner = Trace::from_iter([(0, 4.0), (1, 2.0), (2, 1.0), (3, 5.0), (4, 3.0)]);
        let formula = BoundedEventually::new(Const::from(inner), 0, 2);

        let input = Trace::default();
        let robustness = formula.evaluate_states(&input)?;
        let expected = Trace::from_iter([(0, 4.0), (1, 5.0), (2, 5.0), (3, 5.0), (4, 3.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn next_robustness() -> Result<(), ConstError> {
        let inner = Trace::from_iter([(0, 1.0), (1, 2.0), (2, 3.0), (3, 4.0)]);
        let formula = Next::new(Const::from(inner));

        let input = Trace::default();
        let robustness = formula.evaluate_states(&input)?;
        let expected = Trace::from_iter([(0, 2.0), (1, 3.0), (2, 4.0), (3, f64::NEG_INFINITY)]);

        assert_eq!(robustness, expected);
        Ok(())
    }
}
