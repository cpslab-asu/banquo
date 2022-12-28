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

mod always;
mod binary;
mod eventually;
mod next;
mod not;
mod temporal;
mod until;

pub use always::Always;
pub use binary::{And, Implies, Or};
pub use eventually::Eventually;
pub use next::Next;
pub use not::Not;
pub use until::Until;
