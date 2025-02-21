//! the tinylisp parser, implemented using the PEG formalism.

use peg::{error::ParseError, parser, str::LineCol};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression<'a> {
    Integer(i64),
    Name(&'a str),
    List(Vec<Expression<'a>>),
}

parser! {
    grammar tinylisp() for str {
        rule _ = quiet! { [' ' | '\n' | '\r']* }

        rule name() -> &'input str = $([^'(' | ')' | ' ' | '\n' | '\r']+)

        rule list() -> Vec<Expression<'input>> =
            "(" _ expressions:(expression() ** _) _ (![_] / ")") { expressions }

        pub rule expression() -> Expression<'input> = (
            name:name() {
                name.parse::<u64>().map_or_else(
                    |_| Expression::Name(name),
                    |i| Expression::Integer(i.try_into().unwrap()),
                )
            }
            / list:list() { Expression::List(list) }
        )

        pub rule program() -> Vec<Expression<'input>> = expression() ** _
    }
}

pub fn parse<'input>(input: &'input str) -> Result<Vec<Expression<'input>>, ParseError<LineCol>> {
    tinylisp::program(input.trim())
}

pub fn parse_expression<'input>(input: &'input str) -> Result<Expression<'input>, ParseError<LineCol>> {
    tinylisp::expression(input.trim())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn names() {
        assert_eq!(
            parse("foo 0bar -10 1234 12.1"),
            Result::Ok(vec![
                Expression::Name("foo"),
                Expression::Name("0bar"),
                Expression::Name("-10"),
                Expression::Integer(1234),
                Expression::Name("12.1"),
            ]),
        );
    }

    #[test]
    fn lists() {
        assert_eq!(
            parse("(a b c ())"),
            Result::Ok(vec![Expression::List(vec![
                Expression::Name("a"),
                Expression::Name("b"),
                Expression::Name("c"),
                Expression::List(vec![])
            ])])
        )
    }

    #[test]
    fn whitespace() {
        assert_eq!(
            parse("   ( ( a z ) )   "),
            Result::Ok(vec![Expression::List(vec![Expression::List(vec![
                Expression::Name("a"),
                Expression::Name("z")
            ])])])
        )
    }

    #[test]
    fn unclosed_parenthesis() {
        assert_eq!(
            parse("(((()(("),
            Result::Ok(vec![Expression::List(vec![Expression::List(vec![
                Expression::List(vec![
                    Expression::List(vec![]),
                    Expression::List(vec![Expression::List(vec![])])
                ])
            ])])])
        )
    }
}
