# Banquo Parser

This crate parses temporal logic formulas from strings into Banquo policies (formulas) that you can evaluate on traces.

## Testing

From the **workspace root** (the `banquo` directory):

```bash
# Run all parser unit tests
cargo test -p banquo-parser

# Run the integration test that parses strings into policies
cargo test -p banquo-parser --test parse_string_to_policy
```

## Parsing strings into policies

### Numeric / STL-style formulas

Use **`parse_formula`** for full temporal formulas (with operators like `always`, `eventually`, `and`, `not`, etc.):

```rust
use std::collections::HashMap;
use banquo_parser::{parse_formula, ParsedFormula, Trace};

// Parse a string into a policy (formula)
let formula: ParsedFormula = parse_formula("always 3.1*x <= 0.5*y").expect("parse");

// Build a trace: time -> variable map
fn state(x: f64, y: f64) -> HashMap<String, f64> {
    HashMap::from([("x".into(), x), ("y".into(), y)])
}
let trace = Trace::from([
    (0.0, state(0.1, 1.0)),
    (1.0, state(0.2, 2.0)),
]);

// Evaluate the policy on the trace
let metrics = formula.evaluate(&trace).expect("evaluate");
```

Use **`parse_predicate`** for a single inequality (no temporal operators):

```rust
use banquo_parser::parse_predicate;

let p = parse_predicate("12.0 + 3.1*x + 22.4*y <= 4.8*z").expect("parse");
// Use p as a formula on traces with variables x, y, z
```

### Supported formula syntax

- **Predicates:** `3.1*x + 22.4*y <= 12.0`, `x <= 0.5*y`, etc. (polynomials on left and right of `<=`)
- **Logical:** `and`, `or`, `not`, `->` / `implies`; also `/\`, `\/`, `!`
- **Temporal:** `always`, `eventually`, `next` / `X` / `()`, `U` (until)
- **Bounded temporal:** `always {0,10} phi`, `[]{0,10} phi`, `G{1,2}(phi)`, and similar for eventually
- **Grouping:** parentheses `( ... )`

### Hybrid formulas

Use **`parse_hybrid_formula`** to parse formulas over **named** hybrid predicates (mode + continuous state). You supply a map from names to `HybridPredicate<L>` and an automaton; the formula string refers to those names (e.g. `"always p1"`, `"p1 and p2"`).

```rust
use std::collections::HashMap;
use banquo_parser::{parse_hybrid_formula, Formula, ParsedHybridFormula, Trace};
use banquo_hybrid_distance::automaton::{Automaton, Guard};
use banquo_hybrid_distance::{HybridPredicate, HybridState};

// Build automaton and named predicates (mode label type L must be Copy + Ord + Hash)
let automaton = Automaton::from([...]);
let p1 = HybridPredicate::new(None, [Mode::On], &automaton);
let predicates = HashMap::from([("p1", p1)]);

let formula: ParsedHybridFormula<Mode> =
    parse_hybrid_formula("always p1", predicates).expect("parse");

// Evaluate on a trace of HybridState<HashMap<String, f64>, Mode>
let trace = Trace::from([(0.0, hybrid_state), ...]);
let metrics = formula.evaluate(&trace).expect("evaluate");
```

Run the hybrid integration tests:

```bash
cargo test -p banquo-parser --test parse_hybrid_formula
```

## Status

Both the **numeric formula parser** (STL-style) and the **hybrid formula parser** are implemented and tested.
