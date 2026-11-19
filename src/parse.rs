use chumsky::{input::ValueInput, prelude::*};

use crate::declare::*;

pub fn expr_parser<'toks, 'src: 'toks, I: ValueInput<'toks, Token = Token<'src>, Span = Span>>()
-> impl Parser<
    'toks,
    I,
    Spanned<SyntaxExpr<'src>>,
    extra::Full<Rich<'toks, Token<'src>, Span>, OpSpecs<'src>, ()>,
> + Clone {
    recursive(|expr| {
        let value = select! {
            Token::Float(v) => Value::Float(v),
            Token::Int(v) => Value::Int(v),
            Token::Unit => Value::Unit,
        }
        .labelled("value");

        let lit = value
            .map(|v| SyntaxExpr::Atom(Expr::Lit(v)))
            .labelled("literal");

        let ident = select! { Token::Ident(x) => x }.labelled("identifier");

        let var = ident
            .map(|x| SyntaxExpr::Atom(Expr::Var(x)))
            .labelled("variable");

        let mut inline = Recursive::declare();
        let mut op = Recursive::declare();

        let block = expr
            .clone()
            .delimited_by(just(Token::OpenBrace), just(Token::CloseBrace))
            .recover_with(via_parser(nested_delimiters(
                Token::OpenBrace,
                Token::CloseBrace,
                [(Token::OpenParen, Token::CloseParen)],
                |span| (SyntaxExpr::Atom(Expr::Err), span),
            )))
            .labelled("block expression");

        let paren = op
            .clone()
            .or(inline.clone())
            .delimited_by(just(Token::OpenParen), just(Token::CloseParen))
            .recover_with(via_parser(nested_delimiters(
                Token::OpenParen,
                Token::CloseParen,
                [(Token::OpenBrace, Token::CloseBrace)],
                |span| (SyntaxExpr::Atom(Expr::Err), span),
            )))
            .labelled("parenthesized expression");

        let atom = choice((lit, var))
            .map_with(|expr, e| (expr, e.span()))
            .or(choice((block, paren)));

        let call = atom
            .clone()
            .foldl_with(
                op.clone()
                    .or(inline.clone())
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::OpenParen), just(Token::CloseParen))
                    .or(just(Token::Dot).map(|_| vec![]))
                    .map_with(|args, e| (args, e.span()))
                    .repeated(),
                |f, args, e| (SyntaxExpr::Call(Box::new(f), args), e.span()),
            )
            .labelled("call expression");

        let r#fn = just(Token::Fn)
            .ignore_then(
                ident
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::OpenParen), just(Token::CloseParen))
                    .map_with(|params, e| (params, e.span())),
            )
            .then(inline.clone())
            .map_with(|(params, body), e| (SyntaxExpr::Fn(params, Box::new(body)), e.span()))
            .labelled("function expression");

        let patt = recursive(|case_patt| {
            let r#struct = ident
                .then(
                    case_patt
                        .clone()
                        .separated_by(just(Token::Comma))
                        .allow_trailing()
                        .collect::<Vec<_>>()
                        .delimited_by(just(Token::OpenParen), just(Token::CloseParen))
                        .map_with(|patt, e| (patt, e.span())),
                )
                .map(|(name, fields)| Patt::Struct(name, fields))
                .labelled("struct pattern");
            let var = ident.map(Patt::Var).labelled("variable pattern");
            let lit = value.map(Patt::Lit).labelled("literal pattern");

            choice((r#struct, var, lit))
                .map_with(|patt, e| (patt, e.span()))
                .labelled("pattern")
        });

        let case_arm = patt
            .then_ignore(just(Token::PattArm))
            .then(inline.clone())
            .map(|(patt, body)| (patt, Box::new(body)))
            .labelled("case expression arm");

        let case = just(Token::Case)
            .ignore_then(inline.clone())
            .then(
                case_arm
                    .repeated()
                    .at_least(1)
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::OpenBrace), just(Token::CloseBrace))
                    .map_with(|arms, e| (arms, e.span())),
            )
            .map_with(|(expr, arms), e| (SyntaxExpr::Case(Box::new(expr), arms), e.span()))
            .labelled("case expression");

        inline.define(choice((call, r#fn, case)));

        op.define(
            inline
                .clone()
                .then(atom.then(inline.clone()).repeated().at_least(1).collect())
                .map_with(|(first, rest), e| (SyntaxExpr::Ops(Box::new(first), rest), e.span())),
        );

        let r#let = just(Token::Let)
            .ignore_then(
                just(Token::Opl)
                    .to(OpAssoc::Left)
                    .or(just(Token::Opr).to(OpAssoc::Right))
                    .then(select! { Token::Int(v) => v })
                    .or_not(),
            )
            .then(ident)
            .then_ignore(just(Token::Assign))
            .then(inline.clone())
            .then(expr.clone())
            .map(|(((op_spec, x), assign), body)| {
                SyntaxExpr::Let(op_spec, x, Box::new(assign), Box::new(body))
            })
            .map_with(|expr, e| (expr, e.span()))
            .labelled("let expression");

        choice((op, inline, r#let)).labelled("expression")
    })
}
