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

use std::ops::Deref;
use std::rc::Rc;

use crate::formulas::{
    DebugRobustness, DebugRobustnessFormula, HybridDistance, HybridDistanceFormula, RobustnessFormula,
};
use crate::trace::Trace;
use crate::Formula;

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

impl<F> ForwardOperator<F> {
    fn robustness<S, G>(&self, trace: &Trace<S>, initial: f64, g: G) -> Result<Trace<f64>, F::Error>
    where
        F: RobustnessFormula<S>,
        G: Fn(f64, f64) -> f64,
    {
        let inner_trace = self.subformula.robustness(trace)?;
        let robustness_trace = fw_fold(inner_trace, self.t_bounds, initial, g);

        Ok(robustness_trace)
    }

    fn debug_robustness<S, G, T>(&self, trace: &Trace<S>, g: G) -> Result<Trace<DebugRobustness<T>>, F::Error>
    where
        F: DebugRobustnessFormula<S>,
        G: Fn(Trace<&Rc<DebugRobustness<F::Prev>>>) -> DebugRobustness<T>,
    {
        let inner_trace = self
            .subformula
            .debug_robustness(trace)?
            .into_iter()
            .map_states(Rc::new)
            .collect();

        let robustness_trace = fw_op(inner_trace, self.t_bounds, g);

        Ok(robustness_trace)
    }

    fn hybrid_distance<S, L, G>(
        &self,
        trace: &Trace<(S, L)>,
        initial: HybridDistance,
        g: G,
    ) -> Result<Trace<HybridDistance>, F::Error>
    where
        F: HybridDistanceFormula<S, L>,
        G: Fn(HybridDistance, HybridDistance) -> HybridDistance,
    {
        let inner_trace = self.subformula.hybrid_distance(trace)?;
        let robustness_trace = fw_fold(inner_trace, self.t_bounds, initial, g);

        Ok(robustness_trace)
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
/// use banquo::expressions::Predicate;
/// use banquo::operators::Always;
///
/// let subformula = Predicate::new(("x", 1.0), 2.0);
/// let unbounded_formula = Always::new_unbounded(subformula);
///
/// let subformula = Predicate::new(("x", -1.0), -2.0);
/// let bounded_formula = Always::new_bounded(subformula, (0.0, 4.0));
/// ```
#[derive(Clone, Debug)]
pub struct Always<F>(ForwardOperator<F>);

impl<F> Always<F> {
    pub fn new_unbounded(subformula: F) -> Self {
        let operator = ForwardOperator {
            subformula,
            t_bounds: None,
        };

        Self(operator)
    }

    pub fn new_bounded<B>(subformula: F, (lower, upper): (B, B)) -> Self
    where
        B: Into<f64>,
    {
        let t_bounds = (lower.into(), upper.into());
        let operator = ForwardOperator {
            subformula,
            t_bounds: Some(t_bounds),
        };

        Self(operator)
    }
}

impl<F> Deref for Always<F> {
    type Target = ForwardOperator<F>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<F> Formula<f64> for Always<F>
where
    F: Formula<f64>
{
    type State = F::State;
    type Error = F::Error;

    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<f64>, Self::Error> {
        self.0
            .subformula
            .evaluate_states(trace)
            .map(|trace| fw_fold(trace, self.0.t_bounds, f64::INFINITY, f64::min))
    }
}

type AlwaysDebug<FPrev> = DebugRobustness<MinOfSubtrace<FPrev>>;

impl<F, FPrev> Formula<AlwaysDebug<FPrev>> for Always<F>
where
    F: Formula<DebugRobustness<FPrev>>
{
    type State = F::State;
    type Error = F::Error;

    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<AlwaysDebug<FPrev>>, Self::Error> {
        let debug_trace_min = |subtrace: Trace<&Rc<DebugRobustness<FPrev>>>| {
            let subtrace = subtrace
                .into_iter()
                .map_states(|debug_rc| debug_rc.clone())
                .collect::<Trace<_>>();

            let trace_min = subtrace
                .iter()
                .fold(f64::INFINITY, |min, (_, debug)| f64::min(min, debug.robustness));

            DebugRobustness {
                robustness: trace_min,
                previous: MinOfSubtrace(subtrace),
            }
        };

        self.0
            .subformula
            .evaluate_states(trace)
            .map(|trace| fw_op(trace.into_shared(), self.0.t_bounds, debug_trace_min))
    }
}

impl<F> Formula<HybridDistance> for Always<F>
where
    F: Formula<HybridDistance>,
{
    type State = F::State;
    type Error = F::Error;

    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<HybridDistance>, Self::Error> {
        self.0
            .subformula
            .evaluate_states(trace)
            .map(|trace| fw_fold(trace, self.0.t_bounds, HybridDistance::Robustness(f64::INFINITY), HybridDistance::min))
    }
}

impl<S, F> RobustnessFormula<S> for Always<F>
where
    F: RobustnessFormula<S>,
{
    type Error = F::Error;

    fn robustness(&self, trace: &Trace<S>) -> Result<Trace<f64>, Self::Error> {
        self.0.robustness(trace, f64::INFINITY, f64::min)
    }
}

pub struct MinOfSubtrace<T>(Trace<Rc<DebugRobustness<T>>>);

fn make_debug_min<T>(trace: Trace<&Rc<DebugRobustness<T>>>) -> DebugRobustness<MinOfSubtrace<T>> {
    let mut min_robustness = f64::INFINITY;
    let mut debug_trace: Trace<Rc<DebugRobustness<T>>> = Trace::default();

    for (time, debug) in trace {
        min_robustness = f64::min(min_robustness, debug.robustness);
        debug_trace.insert_state(time, debug.clone());
    }

    DebugRobustness {
        robustness: min_robustness,
        previous: MinOfSubtrace(debug_trace),
    }
}

impl<S, F> DebugRobustnessFormula<S> for Always<F>
where
    F: DebugRobustnessFormula<S>,
{
    type Error = F::Error;
    type Prev = MinOfSubtrace<F::Prev>;

    fn debug_robustness(&self, trace: &Trace<S>) -> Result<Trace<DebugRobustness<Self::Prev>>, Self::Error> {
        self.0.debug_robustness(trace, make_debug_min)
    }
}

impl<S, L, F> HybridDistanceFormula<S, L> for Always<F>
where
    F: HybridDistanceFormula<S, L>,
{
    type Error = F::Error;

    fn hybrid_distance(&self, trace: &Trace<(S, L)>) -> Result<Trace<HybridDistance>, Self::Error> {
        self.0
            .hybrid_distance(trace, HybridDistance::Robustness(f64::INFINITY), HybridDistance::min)
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
/// use banquo::expressions::Predicate;
/// use banquo::operators::Always;
///
/// let subformula = Predicate::new(("x", 1.0), 2.0);
/// let unbounded_formula = Always::new_unbounded(subformula);
///
/// let subformula = Predicate::new(("x", -1.0), -2.0);
/// let bounded_formula = Always::new_bounded(subformula, (0.0, 4.0));
/// ```
#[derive(Clone, Debug)]
pub struct Eventually<F>(ForwardOperator<F>);

impl<F> Eventually<F> {
    pub fn new_unbounded(subformula: F) -> Self {
        let operator = ForwardOperator {
            subformula,
            t_bounds: None,
        };

        Self(operator)
    }

    pub fn new_bounded<B>(subformula: F, (lower, upper): (B, B)) -> Self
    where
        B: Into<f64>,
    {
        let t_bounds = (lower.into(), upper.into());
        let operator = ForwardOperator {
            subformula,
            t_bounds: Some(t_bounds),
        };

        Self(operator)
    }
}

impl<F> Formula<f64> for Eventually<F>
where
    F: Formula<f64>
{
    type State = F::State;
    type Error = F::Error;

    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<f64>, Self::Error> {
        self.0
            .subformula
            .evaluate_states(trace)
            .map(|trace| fw_fold(trace, self.0.t_bounds, f64::NEG_INFINITY, f64::max))
    }
}

type EventuallyDebug<FPrev> = DebugRobustness<MaxOfSubtrace<FPrev>>;

impl<F, FPrev> Formula<EventuallyDebug<FPrev>> for Eventually<F>
where
    F: Formula<DebugRobustness<FPrev>>
{
    type State = F::State;
    type Error = F::Error;

    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<EventuallyDebug<FPrev>>, Self::Error> {
        let debug_trace_max = |subtrace: Trace<&Rc<DebugRobustness<FPrev>>>| {
            let subtrace = subtrace
                .into_iter()
                .map_states(|debug_rc| debug_rc.clone())
                .collect::<Trace<_>>();

            let trace_max = subtrace
                .iter()
                .fold(f64::NEG_INFINITY, |max, (_, debug)| f64::max(max, debug.robustness));

            DebugRobustness {
                robustness: trace_max,
                previous: MaxOfSubtrace(subtrace),
            }
        };

        self.0
            .subformula
            .evaluate_states(trace)
            .map(|trace| fw_op(trace.into_shared(), self.0.t_bounds, debug_trace_max))
    }
}

impl<F> Formula<HybridDistance> for Eventually<F>
where
    F: Formula<HybridDistance>
{
    type State = F::State;
    type Error = F::Error;

    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<HybridDistance>, Self::Error> {
        self.0
            .subformula
            .evaluate_states(trace)
            .map(|trace| fw_fold(trace, self.0.t_bounds, HybridDistance::Infinite, HybridDistance::max))
    }
}

impl<F> Deref for Eventually<F> {
    type Target = ForwardOperator<F>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<S, F> RobustnessFormula<S> for Eventually<F>
where
    F: RobustnessFormula<S>,
{
    type Error = F::Error;

    fn robustness(&self, trace: &Trace<S>) -> Result<Trace<f64>, Self::Error> {
        self.0.robustness(trace, f64::NEG_INFINITY, f64::max)
    }
}

pub struct MaxOfSubtrace<T>(Trace<Rc<DebugRobustness<T>>>);

fn make_debug_max<T>(trace: Trace<&Rc<DebugRobustness<T>>>) -> DebugRobustness<MaxOfSubtrace<T>> {
    let mut min_robustness = f64::NEG_INFINITY;
    let mut debug_trace: Trace<Rc<DebugRobustness<T>>> = Trace::default();

    for (time, debug) in trace {
        min_robustness = f64::max(min_robustness, debug.robustness);
        debug_trace.insert_state(time, debug.clone());
    }

    DebugRobustness {
        robustness: min_robustness,
        previous: MaxOfSubtrace(debug_trace),
    }
}

impl<S, F> DebugRobustnessFormula<S> for Eventually<F>
where
    F: DebugRobustnessFormula<S>,
{
    type Error = F::Error;
    type Prev = MaxOfSubtrace<F::Prev>;

    fn debug_robustness(&self, trace: &Trace<S>) -> Result<Trace<DebugRobustness<Self::Prev>>, Self::Error> {
        self.0.debug_robustness(trace, make_debug_max)
    }
}

impl<S, L, F> HybridDistanceFormula<S, L> for Eventually<F>
where
    F: HybridDistanceFormula<S, L>,
{
    type Error = F::Error;

    fn hybrid_distance(&self, trace: &Trace<(S, L)>) -> Result<Trace<HybridDistance>, Self::Error> {
        self.0
            .hybrid_distance(trace, HybridDistance::Infinite, HybridDistance::max)
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
/// use banquo::expressions::Predicate;
/// use banquo::operators::Next;
///
/// let subformula = Predicate::new(("x", 1.0), 3.0);
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

impl<F> Formula<f64> for Next<F>
where
    F: Formula<f64>,
{
    type State = F::State;
    type Error = F::Error;

    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<f64>, Self::Error> {
        self.subformula
            .evaluate_states(trace)
            .map(|inner_trace| fw_next(inner_trace, f64::NEG_INFINITY))
    }
}

impl<F, FPrev> Formula<DebugRobustness<DebugRobustness<FPrev>>> for Next<F>
where
    F: Formula<DebugRobustness<FPrev>>,
    FPrev: Clone,
{
    type State = F::State;
    type Error = F::Error;

    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<DebugRobustness<DebugRobustness<FPrev>>>, Self::Error> {
        let shift_left = |previous: DebugRobustness<FPrev>, next: Option<&DebugRobustness<FPrev>>| {
            let robustness = match next {
                Some(debug) => debug.robustness,
                None => f64::NEG_INFINITY,
            };

            DebugRobustness { robustness, previous }
        };

        self.subformula
            .evaluate_states(trace)
            .map(|inner_trace| fw_next_map(inner_trace, shift_left))
    }
}

impl<F> Formula<HybridDistance> for Next<F>
where
    F: Formula<HybridDistance>,
{
    type State = F::State;
    type Error = F::Error;

    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<HybridDistance>, Self::Error> {
        self.subformula
            .evaluate_states(trace)
            .map(|inner_trace| fw_next(inner_trace, HybridDistance::Robustness(f64::NEG_INFINITY)))
    }
}

impl<S, F> RobustnessFormula<S> for Next<F>
where
    F: RobustnessFormula<S>,
{
    type Error = F::Error;

    fn robustness(&self, trace: &Trace<S>) -> Result<Trace<f64>, Self::Error> {
        let inner = self.subformula.robustness(trace)?;
        let trace = fw_next(inner, f64::NEG_INFINITY);

        Ok(trace)
    }
}

impl<S, F> DebugRobustnessFormula<S> for Next<F>
where
    F: DebugRobustnessFormula<S>,
    F::Prev: Clone,
{
    type Error = F::Error;
    type Prev = DebugRobustness<F::Prev>;

    fn debug_robustness(&self, trace: &Trace<S>) -> Result<Trace<DebugRobustness<Self::Prev>>, Self::Error> {
        let f = |previous: DebugRobustness<F::Prev>, next_debug: Option<&DebugRobustness<F::Prev>>| {
            let robustness = match next_debug {
                Some(d) => d.robustness,
                None => f64::NEG_INFINITY,
            };

            DebugRobustness { robustness, previous }
        };

        let inner = self.subformula.debug_robustness(trace)?;
        let trace = fw_next_map(inner, f);

        Ok(trace)
    }
}

impl<S, L, F> HybridDistanceFormula<S, L> for Next<F>
where
    F: HybridDistanceFormula<S, L>,
{
    type Error = F::Error;

    fn hybrid_distance(&self, trace: &Trace<(S, L)>) -> Result<Trace<HybridDistance>, Self::Error> {
        let inner = self.subformula.hybrid_distance(trace)?;
        let trace = fw_next(inner, HybridDistance::Robustness(f64::NEG_INFINITY));

        Ok(trace)
    }
}

#[cfg(test)]
mod tests {
    use super::{Always, Eventually, Next};
    use crate::formulas::RobustnessFormula;
    use crate::operators::{Const, ConstError};
    use crate::trace::Trace;

    #[test]
    fn always_unbounded_robustness() -> Result<(), ConstError> {
        let inner = Trace::from_iter([(0, 4.0), (1, 2.0), (2, 3.0), (3, 1.0), (4, 3.0)]);
        let formula = Always::new_unbounded(Const(inner));

        let input = Trace::default();
        let robustness = formula.robustness(&input)?;
        let expected = Trace::from_iter([(0, 1.0), (1, 1.0), (2, 1.0), (3, 1.0), (4, 3.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn always_bounded_robustness() -> Result<(), ConstError> {
        let inner = Trace::from_iter([(0, 4.0), (1, 2.0), (2, 3.0), (3, 1.0), (4, 3.0)]);
        let formula = Always::new_bounded(Const(inner), (0, 2));

        let input = Trace::default();
        let robustness = formula.robustness(&input)?;
        let expected = Trace::from_iter([(0, 2.0), (1, 1.0), (2, 1.0), (3, 1.0), (4, 3.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn eventually_unbounded_robustness() -> Result<(), ConstError> {
        let inner = Trace::from_iter([(0, 4.0), (1, 2.0), (2, 3.0), (3, 1.0), (4, 3.0)]);
        let formula = Eventually::new_unbounded(Const(inner));

        let input = Trace::default();
        let robustness = formula.robustness(&input)?;
        let expected = Trace::from_iter([(0, 4.0), (1, 3.0), (2, 3.0), (3, 3.0), (4, 3.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn eventually_bounded_robustness() -> Result<(), ConstError> {
        let inner = Trace::from_iter([(0, 4.0), (1, 2.0), (2, 1.0), (3, 5.0), (4, 3.0)]);
        let formula = Eventually::new_bounded(Const(inner), (0, 2));

        let input = Trace::default();
        let robustness = formula.robustness(&input)?;
        let expected = Trace::from_iter([(0, 4.0), (1, 5.0), (2, 5.0), (3, 5.0), (4, 3.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }

    #[test]
    fn next_robustness() -> Result<(), ConstError> {
        let inner = Trace::from_iter([(0, 1.0), (1, 2.0), (2, 3.0), (3, 4.0)]);
        let formula = Next::new(Const(inner));

        let input = Trace::default();
        let robustness = formula.robustness(&input)?;
        let expected = Trace::from_iter([(0, 2.0), (1, 3.0), (2, 4.0), (3, f64::NEG_INFINITY)]);

        assert_eq!(robustness, expected);
        Ok(())
    }
}
