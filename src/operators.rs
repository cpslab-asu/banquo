use std::error::Error;
use std::fmt::{Display, Formatter};

use crate::formulas::RobustnessFormula;
use crate::trace::Trace;

struct Const(Trace<f64>);

#[derive(Debug)]
struct ConstError;

impl Display for ConstError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "ConstError")
    }
}

impl Error for ConstError {}

impl RobustnessFormula<()> for Const {
    type Error = ConstError;

    fn robustness(&self, _: &Trace<()>) -> Result<Trace<f64>, Self::Error> {
        Ok(self.0.clone())
    }
}

mod binary;
mod backwards;
mod forward;
mod unary;

pub use binary::{And, Implies, Or};
pub use forward::{Always, Eventually, Next};
pub use unary::Not;
pub use backwards::Until;
