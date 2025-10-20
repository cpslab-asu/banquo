use std::collections::HashMap;
use std::error::Error;

use banquo::operators::{Always, And, Eventually, Implies};
use banquo::{evaluate, predicate, Trace};

#[test]
fn boolean_semantics() -> Result<(), Box<dyn Error>> {
    fn state(v1: f64, v2: f64) -> HashMap<&'static str, f64> {
        HashMap::from([("gear", v1), ("rpm", v2)])
    }

    let is_gear_3 = predicate! { gear = 3.0 };
    let is_gear_4 = predicate! { gear = 4.0 };
    let rpm_above_limit = predicate! { -1.0 * rpm <= -4.0 };
    let phi = Always::unbounded(Implies::new(
        And::new(is_gear_3, rpm_above_limit),
        Eventually::bounded(0..=3, is_gear_4),
    ));

    let t1 = Trace::from([
        (0, state(3.0, 5.0)),
        (1, state(4.0, 5.0)),
        (2, state(4.0, 5.0)),
        (3, state(4.0, 5.0)),
        (4, state(4.0, 5.0)),
    ]);

    assert_eq!(evaluate(&t1, &phi)?, f64::INFINITY);

    let t2 = Trace::from([
        (0, state(3.0, 5.0)),
        (1, state(1.0, 5.0)),
        (2, state(1.0, 5.0)),
        (3, state(1.0, 5.0)),
        (4, state(1.0, 5.0)),
    ]);

    assert_eq!(evaluate(&t2, &phi)?, -1.0);

    Ok(())
}
