
use std::ops::Neg;

use crate::formulas::{
    DebugRobustness, DebugRobustnessFormula, HybridDistance, HybridDistanceFormula, RobustnessFormula,
};
use crate::trace::Trace;
use crate::Formula;

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
/// use banquo::expressions::Predicate;
/// use banquo::operators::Not;
/// use banquo::{Trace, evaluate_robustness};
///
/// let subformula = Predicate::new(("x", 1.0), 1.0);
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

impl<F> Formula<f64> for Not<F>
where
    F: Formula<f64>
{
    type State = F::State;
    type Error = F::Error;

    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<f64>, Self::Error> {
        self.subformula.evaluate_states(trace).map(not)
    }
}

type NotDebug<FPrev> = DebugRobustness<NegOf<FPrev>>;

impl<F, FPrev> Formula<NotDebug<FPrev>> for Not<F>
where
    F: Formula<DebugRobustness<FPrev>>
{
    type State = F::State;
    type Error = F::Error;

    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<NotDebug<FPrev>>, Self::Error> {
        let debug_neg = |debug: DebugRobustness<FPrev>| {
            DebugRobustness {
                robustness: -debug.robustness,
                previous: NegOf(debug),
            }
        };

        let debug_trace = self
            .subformula
            .evaluate_states(trace)?
            .into_iter()
            .map_states(debug_neg)
            .collect();

        Ok(debug_trace)
    }
}

impl<F> Formula<HybridDistance> for Not<F>
where
    F: Formula<HybridDistance>
{
    type State = F::State;
    type Error = F::Error;

    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<HybridDistance>, Self::Error> {
        self.subformula.evaluate_states(trace).map(not)
    }
}

impl<S, F> RobustnessFormula<S> for Not<F>
where
    F: RobustnessFormula<S>,
{
    type Error = F::Error;

    fn robustness(&self, trace: &Trace<S>) -> Result<Trace<f64>, Self::Error> {
        self.subformula.robustness(trace).map(not)
    }
}

pub struct NegOf<T>(pub DebugRobustness<T>);

fn make_debug<T>((time, previous): (f64, DebugRobustness<T>)) -> (f64, DebugRobustness<NegOf<T>>) {
    let neg_robustness = DebugRobustness {
        robustness: -previous.robustness,
        previous: NegOf(previous),
    };

    (time, neg_robustness)
}

impl<S, F> DebugRobustnessFormula<S> for Not<F>
where
    F: DebugRobustnessFormula<S>,
{
    type Error = F::Error;
    type Prev = NegOf<F::Prev>;

    fn debug_robustness(&self, trace: &Trace<S>) -> Result<Trace<DebugRobustness<Self::Prev>>, Self::Error> {
        let previous = self.subformula.debug_robustness(trace)?;
        let debug_trace = previous.into_iter().map(make_debug).collect();

        Ok(debug_trace)
    }
}

impl<S, L, F> HybridDistanceFormula<S, L> for Not<F>
where
    F: HybridDistanceFormula<S, L>,
{
    type Error = F::Error;

    fn hybrid_distance(&self, trace: &Trace<(S, L)>) -> Result<Trace<HybridDistance>, Self::Error> {
        self.subformula.hybrid_distance(trace).map(not)
    }
}

#[cfg(test)]
mod tests {
    use super::Not;
    use crate::formulas::RobustnessFormula;
    use crate::operators::{Const, ConstError};
    use crate::trace::Trace;

    #[test]
    fn robustness() -> Result<(), ConstError> {
        let trace = Trace::from_iter([(0, 0.0), (1, 1.0), (2, 2.0), (3, 3.0)]);
        let formula = Not::new(Const(trace));

        let input = Trace::default();
        let robustness = formula.robustness(&input)?;
        let expected = Trace::from_iter([(0, 0.0), (1, -1.0), (2, -2.0), (3, -3.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }
}
