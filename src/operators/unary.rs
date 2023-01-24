use std::ops::Neg;

use crate::formulas::{DebugRobustness, HybridDistance};
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

impl<F> Formula<f64> for Not<F>
where
    F: Formula<f64>,
{
    type State = F::State;
    type Error = F::Error;

    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<f64>, Self::Error> {
        self.subformula.evaluate_states(trace).map(not)
    }
}

pub struct NegOf<T>(pub DebugRobustness<T>);
type NotDebug<FPrev> = DebugRobustness<NegOf<FPrev>>;

impl<F, FPrev> Formula<NotDebug<FPrev>> for Not<F>
where
    F: Formula<DebugRobustness<FPrev>>,
{
    type State = F::State;
    type Error = F::Error;

    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<NotDebug<FPrev>>, Self::Error> {
        let debug_neg = |debug: DebugRobustness<FPrev>| DebugRobustness {
            robustness: -debug.robustness,
            previous: NegOf(debug),
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
    F: Formula<HybridDistance>,
{
    type State = F::State;
    type Error = F::Error;

    fn evaluate_states(&self, trace: &Trace<Self::State>) -> Result<Trace<HybridDistance>, Self::Error> {
        self.subformula.evaluate_states(trace).map(not)
    }
}

#[cfg(test)]
mod tests {
    use super::Not;
    use crate::operators::testing::{Const, ConstError};
    use crate::trace::Trace;
    use crate::Formula;

    #[test]
    fn robustness() -> Result<(), ConstError> {
        let trace = Trace::from_iter([(0, 0.0), (1, 1.0), (2, 2.0), (3, 3.0)]);
        let formula = Not::new(Const::from(trace));

        let input = Trace::default();
        let robustness = formula.evaluate_states(&input)?;
        let expected = Trace::from_iter([(0, 0.0), (1, -1.0), (2, -2.0), (3, -3.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }
}
