# Banquo

[![Crates.io](https://img.shields.io/crates/v/banquo)](https://crates.io/crates/banquo)
[![Documentation](https://docs.rs/banquo/badge.svg)](https://docs.rs/banquo)

This repository contains the crates that make up the banquo temporal logic monitoring library. For
more information, please refer to the README in the `banquo` crate or the documentation on
[docs.rs](https://docs.rs/banquo).

## Structure

The banquo library is divided into multiple feature crates so that users can avoid including any
functionality that they are not interested in. The `banquo-core` crate provides the `Predicate`
and temporal operator definitions using the `Formula` trait. The `banquo-hybrid_distance` crate
provides the `HybridPredicate` definition for analyzing systems that can be represented as hybrid
automata. The `banquo-parser` crate provides the functionality to parse formulas from text.
