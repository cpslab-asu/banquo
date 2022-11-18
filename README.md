# Banquo

_Why, by the verities on thee made good, may they not be my oracles as well,_
_and set me up in hope_

Banquo is an offline monitor for Metric Temporal Logic (MTL) written in Rust.
The name Banquo comes from a character in Shakespeare's tragedy _MacBeth_ who's
role is to observe in place of the audience.

## What is MTL?

Metric Temporal Logic is an extension of temporal logic which supports
real-valued state traces and bounded temporal operators. This allows us to
construct formulas like the following for a car transmission:

```
[] ((gear = 2 /\ speed >= 30) -> <>{0, 4} gear = 3)
```

In English, this formula represents the safety requirement that whenever the car
speed exceeds 30 miles an hour in second gear then in the next 4 seconds the
gear will be changed to third.

## What can Banquo do?

Banquo supports several semantics for monitoring, listed below.

### Robustness

TODO

### Debug Robustness

TODO

### Hybrid Distance

TODO

## Building Banquo

Banquo has few dependencies and should build on almost every system supported by
Cargo. Building can be accomplished using the command `cargo build`.
