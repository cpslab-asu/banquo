use std::collections::HashMap;
use std::str::FromStr;

use banquo::expressions::Predicate;
use banquo::operators::{Always, Or};
use banquo::{eval_robustness, Trace};
use csv::{Reader, StringRecord};

fn parse_row(row: StringRecord) -> (f64, HashMap<&'static str, f64>) {
    let time_str = row.get(0).expect("Could not read time column");
    let time: f64 = f64::from_str(time_str).expect("Could not convert time value to f64");

    let x_str = row.get(1).expect("Could not read x column");
    let x = f64::from_str(x_str).expect("Could not convert x value to f64");

    let mut state = HashMap::new();
    state.insert("x", x);

    (time, state)
}

fn main() {
    let mut reader = Reader::from_path("trace.csv").unwrap();
    let trace: Trace<_> = reader
        .records()
        .into_iter()
        .map(|result| parse_row(result.unwrap()))
        .collect();

    let p1 = Predicate::simple("x", 1.0, 0.0);
    let p2 = Predicate::simple("x", -1.0, 0.0);
    let formula = Always::new_unbounded(Or::new(p1, p2));
    let robustness = eval_robustness(formula, &trace).unwrap();

    println!("CSV Trace Example");
    println!("Robustness: {}", robustness);
}
