use std::ops::Neg;

use crate::formulas::Formula;
use crate::trace::Trace;

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

#[cfg(test)]
mod tests {
    use super::Not;
    use crate::formulas::Formula;
    use crate::operators::testing::{Const, ConstError};
    use crate::trace::Trace;

    #[test]
    fn robustness() -> Result<(), ConstError> {
        let trace = Trace::from_iter([(0, 0.0), (1, 1.0), (2, 2.0), (3, 3.0)]);
        let formula = Not::new(Const::from(trace));

        let input: Trace<()> = Trace::default();
        let robustness = formula.evaluate_trace(&input)?;
        let expected = Trace::from_iter([(0, 0.0), (1, -1.0), (2, -2.0), (3, -3.0)]);

        assert_eq!(robustness, expected);
        Ok(())
    }
}
