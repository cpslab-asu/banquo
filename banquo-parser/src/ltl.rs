use chumsky::prelude::*;

pub fn neg<'src>() -> impl Parser<'src, &'src str, &'src str> + Clone {
    choice((just("not").padded(), just("!")))
}

pub fn next<'src>() -> impl Parser<'src, &'src str, &'src str> + Clone {
    just("X").padded()
}

pub fn always<'src>() -> impl Parser<'src, &'src str, &'src str> + Clone {
    choice((just("always"), just("[]"), just("G")))
}

pub fn eventually<'src>() -> impl Parser<'src, &'src str, &'src str> + Clone {
    choice((just("eventually"), just("<>"), just("F")))
}

pub fn and<'src>() -> impl Parser<'src, &'src str, &'src str> + Clone {
    choice((just("and"), just("&&"), just(r"/\"))).padded()
}

pub fn or<'src>() -> impl Parser<'src, &'src str, &'src str> + Clone {
    choice((just("or"), just("||"), just(r"\/"))).padded()
}

pub fn implies<'src>() -> impl Parser<'src, &'src str, &'src str> + Clone {
    choice((just("->"), just("implies"))).padded()
}

pub fn until<'src>() -> impl Parser<'src, &'src str, &'src str> + Clone {
    choice((just("until"), just("U"))).padded()
}
