mod predicate;

use banquo_core::stl;
use chumsky::prelude::*;

use crate::ltl;
use crate::mtl;
use predicate::predicate;

fn formula<'src>() -> impl Parser<'src, &'src str, stl::Formula> {
    recursive(|expr| {
        let predicate = predicate()
            .padded()
            .map(|p| stl::Formula::from(stl::Symbol::Predicate(p)));
        let subformula = expr.delimited_by(just("(").padded(), just(")").padded());
        let atom = predicate.or(subformula);

        let unary_op = choice((
            ltl::neg().to(stl::Symbol::Not),
            mtl::always().map(|bounds| stl::Symbol::Always(bounds)),
            mtl::eventually().map(|bounds| stl::Symbol::Eventually(bounds)),
        ));
        let unary = unary_op.repeated().foldr(atom, |op, formula| {
            stl::Formula::from_iter(std::iter::once(op).chain(formula))
        });

        let binary_op = choice((
            ltl::and().to(stl::Symbol::And),
            ltl::or().to(stl::Symbol::Or),
            ltl::implies().to(stl::Symbol::Implies),
        ));
        let binary = unary.clone().foldl(binary_op.then(unary).repeated(), |lhs, (op, rhs)| {
            stl::Formula::from_iter(std::iter::once(op).chain(lhs).chain(rhs))
        });

        binary
    })
}

#[derive(Debug, PartialEq)]
enum ParseErrorKind {
    Unknown,
}

impl Default for ParseErrorKind {
    fn default() -> Self {
        Self::Unknown
    }
}

#[derive(Debug, Default, PartialEq)]
pub struct ParseError {
    kind: ParseErrorKind,
}

pub fn parse(phi: &str) -> Result<stl::Formula, ParseError> {
    formula().parse(phi).into_result().map_err(|_| ParseError::default())
}

#[cfg(test)]
mod tests {
    use banquo_core::{predicate, stl};

    #[test]
    fn test_predicate() {
        let p = predicate! { 3.0 * x <= 11.0 };
        let expected = stl::Formula::from([stl::Symbol::Predicate(p)]);

        assert_eq!(super::parse("3.0 * x <= 11.0"), Ok(expected));
    }

    #[test]
    fn test_negation() {
        let p = predicate! { 3.0 * x <= 11.0 };
        let expected = Ok(stl::Formula::from([stl::Symbol::Not, stl::Symbol::Predicate(p)]));

        assert_eq!(super::parse("not 3.0 * x <= 11.0"), expected);
        assert_eq!(super::parse("!(3.0 * x <= 11.0)"), expected);
    }

    #[test]
    fn test_conjunction() {
        let p1 = predicate! { 3.0 * x <= 11.0 };
        let p2 = predicate! { -2.0 * y + z <= 4.0 };
        let expected = Ok(stl::Formula::from([
            stl::Symbol::And,
            stl::Symbol::Predicate(p1.clone()),
            stl::Symbol::Predicate(p2.clone()),
        ]));

        assert_eq!(super::parse("3.0 * x <= 11.0 && -2.0 * y + z <= 4.0"), expected);
        assert_eq!(super::parse("(3.0 * x <= 11.0) and -2.0 * y + z <= 4.0"), expected);
        assert_eq!(super::parse(r"(3.0 * x <= 11.0) /\ -2.0 * y + z <= 4.0"), expected);

        let p3 = predicate! { a <= 5.0 };
        let expected = stl::Formula::from([
            stl::Symbol::And,
            stl::Symbol::And,
            stl::Symbol::Predicate(p1),
            stl::Symbol::Predicate(p2),
            stl::Symbol::Predicate(p3),
        ]);

        assert_eq!(
            super::parse("(3.0 * x <= 11.0 && -2.0 * y + z <= 4.0) and a <= 5.0"),
            Ok(expected)
        );
    }
}
