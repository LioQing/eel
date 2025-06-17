use chumsky::prelude::*;

use crate::declare::*;

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
        .from_str()
        .unwrapped()
        .map(Token::Int);

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

    let tok = choice((float, int, ctrl, keyword, ident));

    let comment = just("//")
        .then(any().and_is(just('\n').not()).repeated())
        .padded();

    tok.map_with(|tok, e| (tok, e.span()))
        .padded_by(comment.repeated())
        .padded()
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
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
                    .map(|(tok, _)| tok)
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
