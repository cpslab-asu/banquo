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

#[derive(Clone, Debug)]
pub struct ForwardOperator<F> {
    pub subformula: F,
    pub t_bounds: Option<(f64, f64)>,
}

fn fw_op<S, F, T>(inner_trace: Trace<S>, maybe_bounds: Option<(f64, f64)>, f: F) -> Trace<T>
where
    F: Fn(Trace<&S>) -> T,
{
    let eval_time = |time: f64| -> (f64, T) {
        let t_bounds = match maybe_bounds {
            Some((lower, upper)) => (time + lower)..=(time + upper),
            None => time..=f64::INFINITY,
        };
        let forward_trace = inner_trace.range(t_bounds);
        let cost = f(forward_trace);

        (time, cost)
    };

    inner_trace.times().map(eval_time).collect()
}

fn fw_fold<S, F>(inner_trace: Trace<S>, maybe_bounds: Option<(f64, f64)>, initial: S, f: F) -> Trace<S>
where
    S: Clone,
    F: Fn(S, S) -> S,
{
    let fold_subtrace = |subtrace: Trace<&S>| {
        subtrace
            .into_iter()
            .map(|(_, rob)| rob.clone())
            .fold(initial.clone(), |acc, rob| f(acc, rob))
    };

    fw_op(inner_trace, maybe_bounds, fold_subtrace)
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
    bounds: Option<TimeBounds>,
}

impl<F> Always<F> {
    pub fn unbounded(subformula: F) -> Self {
        Self {
            subformula,
            bounds: None,
        }
    }

    pub fn bounded<Lower, Upper>(subformula: F, (lower, upper): (Lower, Upper)) -> Self
    where
        Lower: Into<f64>,
        Upper: Into<f64>,
    {
        Self {
            subformula,
            bounds: Some((lower.into(), upper.into())),
        }
    }
}

impl<State, F, Metric> Formula<State> for Always<F>
where
    F: Formula<State, Metric = Metric>,
    Metric: Top + for<'a> Meet<&'a Metric>,
{
    type Metric = Metric;
    type Error = F::Error;

    fn evaluate_trace(&self, trace: &Trace<State>) -> Result<Trace<Self::Metric>, Self::Error> {
        self.subformula
            .evaluate_trace(trace)
            .map(|trace| fw_fold(trace, self.bounds, Metric::top(), Metric::meet))
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
    bounds: Option<TimeBounds>,
}

impl<F> Eventually<F> {
    pub fn unbounded(subformula: F) -> Self {
        Self {
            subformula,
            bounds: None,
        }
    }

    pub fn bounded<Lower, Upper>(subformula: F, (lower, upper): (Lower, Upper)) -> Self
    where
        Lower: Into<f64>,
        Upper: Into<f64>,
    {
        Self {
            subformula,
            bounds: Some((lower.into(), upper.into())),
        }
    }
}

impl<State, F, Metric> Formula<State> for Eventually<F>
where
    F: Formula<State, Metric = Metric>,
    Metric: Bottom + for<'a> Join<&'a Metric>,
{
    type Metric = Metric;
    type Error = F::Error;

    fn evaluate_trace(&self, trace: &Trace<State>) -> Result<Trace<Self::Metric>, Self::Error> {
        self.subformula
            .evaluate_trace(trace)
            .map(|trace| fw_fold(trace, self.bounds, Metric::bottom(), Metric::join))
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

fn fw_next_map<A, B, F>(trace: Trace<A>, f: F) -> Trace<B>
where
    F: Fn(A, Option<&A>) -> B,
{
    let mut iter = trace.into_iter().peekable();
    let mut mapped_trace = Trace::default();

    while let Some((time, value)) = iter.next() {
        let maybe_next = iter.peek().map(|(_, next_value)| next_value);
        let mapped_value = f(value, maybe_next);

        mapped_trace.insert_state(time, mapped_value);
    }

    mapped_trace
}

fn fw_next<A>(trace: Trace<A>, default: A) -> Trace<A>
where
    A: Clone,
{
    let f = move |_, next_value: Option<&A>| match next_value {
        Some(value) => value.clone(),
        None => default.clone(),
    };

    fw_next_map(trace, f)
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
            .evaluate_states(trace)
            .map(|inner_trace| fw_next(inner_trace, Metric::bottom()))
    }
}

#[cfg(test)]
mod tests {
    use super::{Always, Eventually, Next};
    use crate::operators::testing::{Const, ConstError};
    use crate::trace::Trace;
    use crate::Formula;

    #[test]
    fn always_unbounded_robustness() -> Result<(), ConstError> {
        let inner = Trace::from_iter([(0, 4.0), (1, 2.0), (2, 3.0), (3, 1.0), (4, 3.0)]);
        let formula = Always::new_unbounded(Const::from(inner));

        let input = Trace::default();
        let robustness = formula.evaluate_states(&input)?;
        let expected = Trace::from_iter([(0, 1.0), (1, 1.0), (2, 1.0), (3, 1.0), (4, 3.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn always_bounded_robustness() -> Result<(), ConstError> {
        let inner = Trace::from_iter([(0, 4.0), (1, 2.0), (2, 3.0), (3, 1.0), (4, 3.0)]);
        let formula = Always::new_bounded(Const::from(inner), (0, 2));

        let input = Trace::default();
        let robustness = formula.evaluate_states(&input)?;
        let expected = Trace::from_iter([(0, 2.0), (1, 1.0), (2, 1.0), (3, 1.0), (4, 3.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn eventually_unbounded_robustness() -> Result<(), ConstError> {
        let inner = Trace::from_iter([(0, 4.0), (1, 2.0), (2, 3.0), (3, 1.0), (4, 3.0)]);
        let formula = Eventually::new_unbounded(Const::from(inner));

        let input = Trace::default();
        let robustness = formula.evaluate_states(&input)?;
        let expected = Trace::from_iter([(0, 4.0), (1, 3.0), (2, 3.0), (3, 3.0), (4, 3.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn eventually_bounded_robustness() -> Result<(), ConstError> {
        let inner = Trace::from_iter([(0, 4.0), (1, 2.0), (2, 1.0), (3, 5.0), (4, 3.0)]);
        let formula = Eventually::new_bounded(Const::from(inner), (0, 2));

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
