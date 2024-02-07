# Banquo

_Why, by the verities on thee made good, may they not be my oracles as well,_
_and set me up in hope_

Banquo is an offline monitor for Signal Temporal Logic (STL) written in Rust.
The name Banquo comes from a character in Shakespeare's tragedy _MacBeth_ who's
role is to observe in place of the audience.

## What is Temporal Logic?

Temporal logic is a higher-order logic which allows for reasoning about
propositions over time. This is useful in formal verification, where it is
desirable to determine if a system properties holds for a certain amount of
time. In addition to supporting the first-order logic operators, temporal logic
introduces the following temporal operators with associated semantics:

| Operator               | Symbols        | Meaning                                                            |
| ---------------------- | -------------- | ------------------------------------------------------------------ |
| Next $\varphi$         | $\bigcirc$, X  | $\varphi$ must hold at the next timestep                           | 
| Always $\varphi$       | $\square$, G   | $\varphi$ must hold at all times in the future                     |
| Eventually $\varphi$;  | $\diamond$, F  | $\varphi$ must hold at least once in the future                    |
| $\varphi$ Until $\psi$ | $\mathcal{U}$  | $\varphi$ must hold until and including the time that $\psi$ holds |

## What is STL

Signal Temporal Logic (STL) is an extension of temporal logic which allows for
_real-time_ and _real-value_ constraints. This means that, while basic temporal
logic only allows for discrete (integer) times and true/false propositions, STL
can handle times and states that are decimals. This allows us to express
constraints on systems with continous state variables - a useful feature for
evaluating real world systems.

Signal Temporal Logic also incorporates Metric Temporal Logic (MTL), which is an
extension of temporal logic that introduces support for bounded temporal
operators. Operators that are bounded only apply for the states associated with
all times within the bound. One special case is when a bounded operator is used
on the right side of an implication - in this scenario the time 0 of the
right-side bound is the time when the left side is true.

These two extensions allow us to express system requirements in a compact and
explicit manner, like so: 

$$
\Box ((gear=2 \wedge speed \geq 30) \rightarrow \diamond\{0,4\}\ gear=3)
$$

In English, this formula represents the safety requirement that whenever the car
speed exceeds 30 miles an hour in second gear then in the next 4 seconds the
gear will be changed to third.

You can read more information about temporal logics
[here](https://en.wikipedia.org/wiki/Temporal_logic).

## What is a Monitor?

Once we have a requirement for our system, we would like to be able to analyze
our system to ensure that it does not violate the requirement. To do so, we must
generate a system _trace_, which is a set that contains a set of times and the
system state at each time. Given a trace, we evaluate our formula for each time
to decide if the system violated the requirement. This type of evaluation is
called __monitoring__, and can be done offline (when the entire trace is
available) or online (when the trace is evaluated as it is generated). The
result of evaluating a trace is a single value that represents the success or
failure of the system to satisfy the requirement. Intuitively we might expect
the result of an evaluation to be a boolean value indicating success or failure,
but we can define other result values that convey additional semantics beyond
solely satisfaction.

_Currently Banquo only supports offline monitoring, but online support is
planned in the future._

## What Types of Evaluation Can Banquo Do?

Banquo supports the following monitoring semantics:

### Robustness

Robustness is a measure of how close a system came to violating a requirement,
or in the case of a violation how much the system violated the requirement. This
is represented as a floating point value, with negative values indicating a
requirement violation. As an example, given the requirement $\square x \leq 10$
and the trace:

| Time | x   |
| ---- | --- |
| 0.0  | 1.0 |
| 1.0  | 3.2 |
| 2.0  | 9.1 |
| 3.0  | 8.7 |

we can see that the closest the trace ever comes to violating the requirement is
at time `2.0`, and that the distance from violation is `0.9`.

### Hybrid Distance

Systems with both discrete and continuous states can be represented as Hybrid
Automata (HA), where the state of the system is a pair formed by the discrete
and contious states. HAs also have __guards__ which restrict the transition
between discrete states based on the continuous state variables of the system.
An example of a system representable as an HA is a traffic light, which has the
discrete states $\{GREEN, RED, YELLOW\}$ as well as a real-valued
internal timer measuring when to switch between discrete states. The guards of
the traffic light would be the following:

| From   | To     | Condition   |
| ------ | ------ | ----------- |
| Green  | Yellow | timer >= 10 |
| Yellow | Red    | timer >= 3  |
| Red    | Gree   | timer >= 12 |

Given a hybrid automaton, we can write requirements about the continuous states
of the system but we are required to specify the discrete states in which the
requirement is valid. As a consequence, it is not always possible to measure the
robustness of a requirement and we need to develop another measure to ensure
that we can provide a valid evaluation at each time. The solution to this
problem is to introduce hybrid distance, which either computes the robustness of
the formula or computes the distance to the discrete state in which the
robustness can be computed. Hybrid distance is represented as a pair where the
first component is the shortest path distance to the target state and the second
component is either the robustness if the first component is 0, or the distance
from the closest transition guard if the first component is non-zero.

## Extending Banquo

Banquo formulas are defined as traits and the implementations provided by this
crate are designed to be as general as possible to allow users to define their
own operators or expressions. An expression is the terminal part of the
formula, which evaluates to a concrete value that is passed back up the formula
chain. Below is an example of creating a new expression:

```rust
use std::collections::HashMap;

use std::error::Error;
use std::fmt;

use banquo::{Formula, Trace};

#[derive(Debug)]
struct PropositionError {
    name: String,
}

impl fmt::Display for PropositionError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Missing proposition {}", &self.name)
    }
}

struct Proposition {
    name: String,
}

impl Formula<HashMap<String, bool>> for Proposition {
    type Metric = f64;
    type Error = PropositionError;

    fn evaluate(&self, trace: &Trace<HashMap<String, bool>>) -> Result<Trace<Self::Metric>, Self::Error> {
        let mut result = Trace::new();

        for (time, state) in trace {
            let prop = state
                .get(&self.name)
                .copied()
                .ok_or_else(|| PropositionError { name: self.name.clone() })?;

            result.insert(time, if prop { f64::INFINITY } else { f64::NEG_INFINITY });
        }

        Ok(result)
    }
}
```

An operator transforms the output of its subformulas, which can be expressions
or other operators. An example of creating a new operator is as follows:

```rust

use banquo::{Bottom, Formula, Trace};

struct Prev<F> {
    subformula: F,
}

impl<F, S, M> Formula<S> for Prev<F>
where
    F: Formula<S, Metric = M>,
    M: Bottom + Clone,
{
    type Metric = M;
    type Error = F::Error;

    fn evaluate(&self, trace: &Trace<S>) -> Result<Trace<Self::Metric>, F::Error> {
        let evaluated = self.subformula.evaluate(trace)?;
        let mut iter = evaluated.into_iter().rev().peekable();
        let mut result = Trace::new();

        while let Some((time, _)) = iter.next() {
            let prev = iter
                .peek()
                .map(|(_, metric)| metric.clone())
                .unwrap_or(M::bottom());

            result.insert(time, prev);
        }

        Ok(result)
    }
}

```

While both expressions and operators implement the same trait, it is important
to note that they have different meanings. For more information on the
`Formula` trait or its semantics, please consult the documentation available on
[docs.rs](https://docs.rs/banquo).

## Installing Banquo

Banquo can be added to your project with the command `cargo add banquo`. This will insert a line
in your `Cargo.toml` file with the latest version of this library.

## Building Banquo

Banquo has few dependencies and should build on almost every system supported by
Cargo. Building can be accomplished using the command `cargo build`.
