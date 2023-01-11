use std::ops::Deref;
use std::rc::Rc;

use crate::formulas::{
    DebugRobustness, DebugRobustnessFormula, HybridDistance, HybridDistanceFormula, RobustnessFormula,
};
use crate::trace::Trace;

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
