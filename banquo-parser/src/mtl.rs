use chumsky::prelude::*;

use crate::ltl;
use crate::num;

pub fn bounds<'src>() -> impl Parser<'src, &'src str, (f64, f64)> + Clone {
    num()
        .then_ignore(just(","))
        .then(num())
        .delimited_by(just("{"), just("}"))
}

pub fn always<'src>() -> impl Parser<'src, &'src str, Option<(f64, f64)>> + Clone {
    ltl::always().ignore_then(bounds()).or_not()
}

pub fn eventually<'src>() -> impl Parser<'src, &'src str, Option<(f64, f64)>> + Clone {
    ltl::eventually().ignore_then(bounds()).or_not()
}
