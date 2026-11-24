use chumsky::prelude::*;

use super::declare::*;

pub fn lexer<'src>()
-> impl Parser<'src, &'src str, Vec<Spanned<Token<'src>>>, extra::Err<Rich<'src, char, Span>>> {
    let float = {
        let digits = text::digits(10).to_slice();

        let frac = just('.').then(digits);

        let exp = just('e')
            .or(just('E'))
            .then(one_of("+-").or_not())
            .then(digits);

        just('-')
            .or_not()
            .then(text::int(10))
            .then(frac.or_not())
            .then(exp.or_not())
            .filter(|(((_sign, _int), frac), exp)| frac.is_some() || exp.is_some())
            .to_slice()
            .from_str()
            .unwrapped()
            .map(Token::Float)
    };

    let int = just('-')
        .or_not()
        .then(text::int(10))
        .to_slice()
        .map(|s: &str| s.parse::<i64>().map(Token::Int));

    let ctrl = choice((
        just('(').to(Token::OpenParen),
        just(')').to(Token::CloseParen),
        just('{').to(Token::OpenBrace),
        just('}').to(Token::CloseBrace),
        just(',').to(Token::Comma),
        just('.').to(Token::Dot),
    ));

    let keyword = choice((
        text::unicode::keyword("fn").to(Token::Fn),
        text::unicode::keyword("let").to(Token::Let),
        text::unicode::keyword("case").to(Token::Case),
        text::unicode::keyword("opl").to(Token::Opl),
        text::unicode::keyword("opr").to(Token::Opr),
        text::unicode::keyword("unit").to(Token::Unit),
    ));

    let ident = any()
        .filter(|c: &char| !c.is_whitespace() && !matches!(c, '(' | ')' | '{' | '}' | ',' | '.'))
        .repeated()
        .at_least(1)
        .to_slice()
        .map(|s| match s {
            "=>" => Token::PattArm,
            "=" => Token::Assign,
            _ => Token::Ident(s),
        });

    let tok = choice((
        float.map(Ok),
        int,
        ctrl.map(Ok),
        keyword.map(Ok),
        ident.map(Ok),
    ));

    let comment = just("//")
        .then(any().and_is(just('\n').not()).repeated())
        .padded();

    tok.map_with(|tok, e| Spanned::new(tok, e.span()))
        .padded_by(comment.repeated())
        .padded()
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect::<Vec<_>>()
        // Validate after tokenization so invalid integers cannot fall back to identifiers
        // or recover into executable fragments of the original literal.
        .try_map(|toks, _| {
            toks.into_iter()
                .map(|Spanned { value, span }| {
                    value.map(|tok| Spanned::new(tok, span)).map_err(|err| {
                        Rich::custom(span, format!("Invalid integer literal: {err}"))
                    })
                })
                .collect()
        })
}

#[cfg(test)]
mod tests {
    use ariadne::{Color, Label, Report, ReportKind, sources};

    use super::*;

    macro_rules! assert_lex {
        ($src:literal, $($expected:expr),* $(,)?) => {
            let src = $src;
            let expected = vec![$($expected,)*];

            let (toks, errs) = lexer().parse(src).into_output_errors();

            errs.into_iter()
                .map(|e| e.map_token(|tok| tok.to_string()))
                .for_each(|e| {
                    Report::build(ReportKind::Error, ("src", e.span().into_range()))
                        .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
                        .with_message(e.to_string())
                        .with_label(
                            Label::new(("src", e.span().into_range()))
                                .with_message(e.reason().to_string())
                                .with_color(Color::Red),
                        )
                        .with_labels(e.contexts().map(|(label, span)| {
                            Label::new(("src", span.into_range()))
                                .with_message(format!("while parsing this {label}"))
                                .with_color(Color::Yellow)
                        }))
                        .finish()
                        .print(sources([("src", src)]))
                        .unwrap()
                });

            assert_eq!(
                toks.unwrap()
                    .into_iter()
                    .map(|tok| tok.value)
                    .collect::<Vec<_>>(),
                expected,
            );
        }
    }

    #[test]
    fn test_float() {
        assert_lex!(
            "3.14 -2.718 1.0e10 3e8 -1.0E-5",
            Token::Float(3.14),
            Token::Float(-2.718),
            Token::Float(1.0e10),
            Token::Float(3e8),
            Token::Float(-1.0e-5),
        );
    }

    #[test]
    fn test_int() {
        assert_lex!(
            "123 -456 7890",
            Token::Int(123),
            Token::Int(-456),
            Token::Int(7890),
        );
    }

    #[test]
    fn test_int_boundaries() {
        assert_lex!(
            "9223372036854775807 -9223372036854775808 0 -0",
            Token::Int(i64::MAX),
            Token::Int(i64::MIN),
            Token::Int(0),
            Token::Int(0),
        );
    }

    #[test]
    fn test_int_out_of_range() {
        for literal in [
            "9223372036854775808",
            "-9223372036854775809",
            "18446744073709551615",
            "99999999999999999999999999999999999999999999999999",
            "-99999999999999999999999999999999999999999999999999",
        ] {
            for (prefix, suffix) in [("", ""), ("print(", ")."), ("let x = ", " 42")] {
                let src = format!("{prefix}{literal}{suffix}");
                let (toks, errs) = lexer().parse(&src).into_output_errors();

                assert!(toks.is_none(), "Invalid integer produced tokens: {src}");
                assert_eq!(errs.len(), 1, "{src}: {errs:?}");
                assert_eq!(
                    errs[0].span().into_range(),
                    prefix.len()..prefix.len() + literal.len(),
                    "{src}",
                );
                assert!(
                    errs[0]
                        .reason()
                        .to_string()
                        .contains("Invalid integer literal"),
                    "{src}: {:?}",
                    errs[0],
                );
            }
        }
    }

    #[test]
    fn test_large_float_literals_remain_valid() {
        assert_lex!(
            "9223372036854775808.0 -9223372036854775809.0 1e999",
            Token::Float(9223372036854775808.0),
            Token::Float(-9223372036854775809.0),
            Token::Float(f64::INFINITY),
        );
    }

    #[test]
    fn test_ident() {
        assert_lex!(
            "foo bar _baz abc123 🦀 ::<> a+b",
            Token::Ident("foo"),
            Token::Ident("bar"),
            Token::Ident("_baz"),
            Token::Ident("abc123"),
            Token::Ident("🦀"),
            Token::Ident("::<>"),
            Token::Ident("a+b"),
        );
    }
}
