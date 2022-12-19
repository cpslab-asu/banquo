use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

use crate::trace::Trace;

pub enum BinaryOperatorError<L, R> {
    LeftError(L),
    RightError(R),
}

impl<L, R> Debug for BinaryOperatorError<L, R>
where
    L: Debug,
    R: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LeftError(err) => write!(f, "LeftError({:?})", err),
            Self::RightError(err) => write!(f, "RightError({:?})", err),
        }
    }
}

impl<L, R> Display for BinaryOperatorError<L, R>
where
    L: Display,
    R: Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LeftError(err) => write!(f, "left subformula error: {}", err),
            Self::RightError(err) => write!(f, "right subformula error: {}", err),
        }
    }
}

impl<L, R> Error for BinaryOperatorError<L, R>
where
    L: Error,
    R: Error,
{
}

#[derive(Clone, Debug)]
pub struct BinaryOperator<L, R> {
    pub left: L,
    pub right: R,
}

type Result<T, L, R> = std::result::Result<Trace<T>, BinaryOperatorError<L, R>>;

impl<L, R> BinaryOperator<L, R> {
    pub fn apply<S, T1, E1, F1, T2, E2, F2, T3, F3>(
        &self,
        trace: &Trace<S>,
        get_left: F1,
        get_right: F2,
        combine: F3,
    ) -> Result<T3, E1, E2>
    where
        F1: Fn(&L, &Trace<S>) -> std::result::Result<Trace<T1>, E1>,
        F2: Fn(&R, &Trace<S>) -> std::result::Result<Trace<T2>, E2>,
        F3: Fn(T1, T2) -> T3,
    {
        let left = get_left(&self.left, trace).map_err(BinaryOperatorError::LeftError)?;
        let right = get_right(&self.right, trace).map_err(BinaryOperatorError::RightError)?;
        let trace = left
            .zip(right)
            .into_iter()
            .map_states(|(lvalue, rvalue)| combine(lvalue, rvalue))
            .collect();

        Ok(trace)
    }
}
