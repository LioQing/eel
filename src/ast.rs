use chumsky::{input::ValueInput, prelude::*};

use crate::declare::*;

pub fn expr_parser<'toks, 'src: 'toks, I: ValueInput<'toks, Token = Token<'src>, Span = Span>>()
-> impl Parser<
    'toks,
    I,
    Spanned<Expr<'src>>,
    extra::Full<Rich<'toks, Token<'src>, Span>, OpSpecs<'src>, ()>,
> + Clone {
    recursive(|expr| {
        let value = select! {
            Token::Float(v) => Value::Float(v),
            Token::Int(v) => Value::Int(v),
            Token::Unit => Value::Unit,
        }
        .labelled("value");

        let lit = value.map(Expr::Lit).labelled("literal");

        let ident = select! { Token::Ident(x) => x }.labelled("identifier");

        let var = ident.map(Expr::Var).labelled("variable");

        let mut inline = Recursive::declare();
        let mut op = Recursive::declare();

        let block = expr
            .clone()
            .delimited_by(just(Token::OpenBrace), just(Token::CloseBrace))
            .recover_with(via_parser(nested_delimiters(
                Token::OpenBrace,
                Token::CloseBrace,
                [(Token::OpenParen, Token::CloseParen)],
                |span| (Expr::Err, span),
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
                |span| (Expr::Err, span),
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
                |f, args, e| (Expr::Call(Box::new(f), args), e.span()),
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
            .map_with(|(params, body), e| (Expr::Fn(params, Box::new(body)), e.span()))
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
            .map_with(|(expr, arms), e| (Expr::Case(Box::new(expr), arms), e.span()))
            .labelled("case expression");

        inline.define(choice((call, r#fn, case)));

        op.define(
            inline
                .clone()
                .then(atom)
                .then(op.clone().or(inline.clone()))
                .map_with(|((l, op), r), e| {
                    (Expr::Op(Box::new(l), Box::new(op), Box::new(r)), e.span())
                }),
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
                Expr::Let(op_spec, x, Box::new(assign), Box::new(body))
            })
            .map_with(|expr, e| (expr, e.span()))
            .labelled("let expression");

        choice((op, inline, r#let)).labelled("expression")
    })
}

#[cfg(test)]
mod tests {
    use ariadne::{Color, Label, Report, ReportKind, sources};

    use super::*;

    use crate::lex;

    fn spanned<T: Clone>(value: T) -> Spanned<T> {
        Spanned::new(value, 0..0)
    }

    macro_rules! assert_ast {
        ($src:literal, $expected:expr $(,)?) => {
            let src = $src;

            let toks = lex::lexer().parse(src).unwrap();

            let (ast, errs) = expr_parser()
                .parse_with_state(
                    toks.as_slice()
                        .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
                    &mut extra::SimpleState(vec![]),
                )
                .into_output_errors();

            errs.into_iter()
                .map(|e| e.map_token(|tok| tok.to_string()))
                .for_each(|e| {
                    Report::build(ReportKind::Error, ("src", e.span().into_range()))
                        .with_config(
                            ariadne::Config::new().with_index_type(ariadne::IndexType::Byte),
                        )
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

            let ast = ast.unwrap();
            let expected = $expected;

            assert!(
                ast.spanless_eq(&expected),
                "AST does not match expected:\nExpected: {expected:?}\nGot: {ast:?}\nTokens: {toks:?}"
            );
        };
    }

    #[test]
    fn test_lit_positive_int() {
        assert_ast!("42", spanned(Expr::Lit(Value::Int(42))));
    }

    #[test]
    fn test_lit_negative_int() {
        assert_ast!("-42", spanned(Expr::Lit(Value::Int(-42))));
    }

    #[test]
    fn test_lit_negative_float() {
        assert_ast!("-2.718", spanned(Expr::Lit(Value::Float(-2.718))));
    }

    #[test]
    fn test_lit_positive_float() {
        assert_ast!("3.14", spanned(Expr::Lit(Value::Float(3.14))));
    }

    #[test]
    fn test_lit_float_scientific_notation_e() {
        assert_ast!("3e8", spanned(Expr::Lit(Value::Float(3e8))));
    }

    #[test]
    fn test_lit_float_scientific_notation_e_with_decimal() {
        assert_ast!("1.0e10", spanned(Expr::Lit(Value::Float(1.0e10))));
    }

    #[test]
    fn test_lit_float_scientific_notation_negative_exponent() {
        assert_ast!("-1.0E-5", spanned(Expr::Lit(Value::Float(-1.0e-5))));
    }

    #[test]
    fn test_var_simple_foo() {
        assert_ast!("foo", spanned(Expr::Var("foo")));
    }

    #[test]
    fn test_var_simple_bar() {
        assert_ast!("bar", spanned(Expr::Var("bar")));
    }

    #[test]
    fn test_var_with_leading_underscore() {
        assert_ast!("_baz", spanned(Expr::Var("_baz")));
    }

    #[test]
    fn test_var_alphanumeric() {
        assert_ast!("abc123", spanned(Expr::Var("abc123")));
    }

    #[test]
    fn test_var_emoji() {
        assert_ast!("🦀", spanned(Expr::Var("🦀")));
    }

    #[test]
    fn test_var_symbols() {
        assert_ast!("::<>", spanned(Expr::Var("::<>")));
    }

    #[test]
    fn test_call_no_args_parentheses() {
        assert_ast!(
            "foo()",
            spanned(Expr::Call(
                Box::new(spanned(Expr::Var("foo"))),
                spanned(vec![])
            )),
        );
    }

    #[test]
    fn test_call_no_args_dot() {
        assert_ast!(
            "bar.",
            spanned(Expr::Call(
                Box::new(spanned(Expr::Var("bar"))),
                spanned(vec![]),
            )),
        );
    }

    #[test]
    fn test_call_multiple_arg_types() {
        assert_ast!(
            "foo(1, 2.0, bar)",
            spanned(Expr::Call(
                Box::new(spanned(Expr::Var("foo"))),
                spanned(vec![
                    spanned(Expr::Lit(Value::Int(1))),
                    spanned(Expr::Lit(Value::Float(2.0))),
                    spanned(Expr::Var("bar")),
                ]),
            )),
        );
    }

    #[test]
    fn test_call_single_int_arg() {
        assert_ast!(
            "bar(42)",
            spanned(Expr::Call(
                Box::new(spanned(Expr::Var("bar"))),
                spanned(vec![spanned(Expr::Lit(Value::Int(42)))]),
            )),
        );
    }

    #[test]
    fn test_call_chained_calls() {
        assert_ast!(
            "foo(bar)(baz)",
            spanned(Expr::Call(
                Box::new(spanned(Expr::Call(
                    Box::new(spanned(Expr::Var("foo"))),
                    spanned(vec![spanned(Expr::Var("bar"))]),
                ))),
                spanned(vec![spanned(Expr::Var("baz"))]),
            )),
        );
    }

    #[test]
    fn test_call_chained_call_with_dot() {
        assert_ast!(
            "foo(bar).",
            spanned(Expr::Call(
                Box::new(spanned(Expr::Call(
                    Box::new(spanned(Expr::Var("foo"))),
                    spanned(vec![spanned(Expr::Var("bar"))]),
                ))),
                spanned(vec![]),
            )),
        );
    }

    #[test]
    fn test_call_nested_call_as_arg() {
        assert_ast!(
            "foo(bar, baz(42))",
            spanned(Expr::Call(
                Box::new(spanned(Expr::Var("foo"))),
                spanned(vec![
                    spanned(Expr::Var("bar")),
                    spanned(Expr::Call(
                        Box::new(spanned(Expr::Var("baz"))),
                        spanned(vec![spanned(Expr::Lit(Value::Int(42)))]),
                    )),
                ]),
            )),
        );
    }

    #[test]
    fn test_fn_no_params_block_body() {
        assert_ast!(
            "fn() { 42 }",
            spanned(Expr::Fn(
                spanned(vec![]),
                Box::new(spanned(Expr::Lit(Value::Int(42)))),
            )),
        );
    }

    #[test]
    fn test_fn_single_param_expr_body() {
        assert_ast!(
            "fn(x) x",
            spanned(Expr::Fn(
                spanned(vec!["x"]),
                Box::new(spanned(Expr::Var("x"))),
            )),
        );
    }

    #[test]
    fn test_fn_nested_block_body() {
        assert_ast!(
            "fn(x) { fn(y) x(y) }",
            spanned(Expr::Fn(
                spanned(vec!["x"]),
                Box::new(spanned(Expr::Fn(
                    spanned(vec!["y"]),
                    Box::new(spanned(Expr::Call(
                        Box::new(spanned(Expr::Var("x"))),
                        spanned(vec![spanned(Expr::Var("y"))]),
                    ))),
                ))),
            )),
        );
    }

    #[test]
    fn test_fn_nested_expr_body() {
        assert_ast!(
            "fn(x) fn(y) x(y)",
            spanned(Expr::Fn(
                spanned(vec!["x"]),
                Box::new(spanned(Expr::Fn(
                    spanned(vec!["y"]),
                    Box::new(spanned(Expr::Call(
                        Box::new(spanned(Expr::Var("x"))),
                        spanned(vec![spanned(Expr::Var("y"))]),
                    ))),
                ))),
            )),
        );
    }

    #[test]
    fn test_fn_multiple_params_block_body() {
        assert_ast!(
            "fn(x, y) { x(y) }",
            spanned(Expr::Fn(
                spanned(vec!["x", "y"]),
                Box::new(spanned(Expr::Call(
                    Box::new(spanned(Expr::Var("x"))),
                    spanned(vec![spanned(Expr::Var("y"))]),
                ))),
            )),
        );
    }

    #[test]
    fn test_case_single_arm_lit_pattern() {
        assert_ast!(
            "case x { 42 => y }",
            spanned(Expr::Case(
                Box::new(spanned(Expr::Var("x"))),
                spanned(vec![(
                    spanned(Patt::Lit(Value::Int(42))),
                    Box::new(spanned(Expr::Var("y"))),
                )]),
            )),
        );
    }

    #[test]
    fn test_case_single_arm_var_pattern() {
        assert_ast!(
            "case x { y => z }",
            spanned(Expr::Case(
                Box::new(spanned(Expr::Var("x"))),
                spanned(vec![(
                    spanned(Patt::Var("y")),
                    Box::new(spanned(Expr::Var("z"))),
                )]),
            )),
        );
    }

    #[test]
    fn test_case_single_arm_struct_pattern() {
        assert_ast!(
            "case x { y(z) => w }",
            spanned(Expr::Case(
                Box::new(spanned(Expr::Var("x"))),
                spanned(vec![(
                    spanned(Patt::Struct("y", spanned(vec![spanned(Patt::Var("z"))]))),
                    Box::new(spanned(Expr::Var("w"))),
                )]),
            )),
        );
    }

    #[test]
    fn test_case_multiple_arms() {
        assert_ast!(
            "
            case x {
                w => 42
                42 => x
                y(z) => f(z)
            }
            ",
            spanned(Expr::Case(
                Box::new(spanned(Expr::Var("x"))),
                spanned(vec![
                    (
                        spanned(Patt::Var("w")),
                        Box::new(spanned(Expr::Lit(Value::Int(42)))),
                    ),
                    (
                        spanned(Patt::Lit(Value::Int(42))),
                        Box::new(spanned(Expr::Var("x"))),
                    ),
                    (
                        spanned(Patt::Struct("y", spanned(vec![spanned(Patt::Var("z"))]))),
                        Box::new(spanned(Expr::Call(
                            Box::new(spanned(Expr::Var("f"))),
                            spanned(vec![spanned(Expr::Var("z"))]),
                        ))),
                    ),
                ]),
            )),
        );
    }

    #[test]
    fn test_case_op() {
        assert_ast!(
            "case (x + y) { 42 => z }",
            spanned(Expr::Case(
                Box::new(spanned(Expr::Op(
                    Box::new(spanned(Expr::Var("x"))),
                    Box::new(spanned(Expr::Var("+"))),
                    Box::new(spanned(Expr::Var("y"))),
                ))),
                spanned(vec![(
                    spanned(Patt::Lit(Value::Int(42))),
                    Box::new(spanned(Expr::Var("z"))),
                )]),
            )),
        );
    }

    #[test]
    fn test_op_simple_addition() {
        assert_ast!(
            "x + y",
            spanned(Expr::Op(
                Box::new(spanned(Expr::Var("x"))),
                Box::new(spanned(Expr::Var("+"))),
                Box::new(spanned(Expr::Var("y"))),
            )),
        );
    }

    #[test]
    fn test_op_precedence_mul_then_add() {
        assert_ast!(
            "x + y * z",
            spanned(Expr::Op(
                Box::new(spanned(Expr::Var("x"))),
                Box::new(spanned(Expr::Var("+"))),
                Box::new(spanned(Expr::Op(
                    Box::new(spanned(Expr::Var("y"))),
                    Box::new(spanned(Expr::Var("*"))),
                    Box::new(spanned(Expr::Var("z"))),
                ))),
            )),
        );
    }

    #[test]
    fn test_op_precedence_parentheses_override() {
        assert_ast!(
            "(x + y) * z",
            spanned(Expr::Op(
                Box::new(spanned(Expr::Op(
                    Box::new(spanned(Expr::Var("x"))),
                    Box::new(spanned(Expr::Var("+"))),
                    Box::new(spanned(Expr::Var("y"))),
                ))),
                Box::new(spanned(Expr::Var("*"))),
                Box::new(spanned(Expr::Var("z"))),
            )),
        );
    }

    #[test]
    fn test_let_simple_int_assignment() {
        assert_ast!(
            "
            let x = 42
            x
            ",
            spanned(Expr::Let(
                None,
                "x",
                Box::new(spanned(Expr::Lit(Value::Int(42)))),
                Box::new(spanned(Expr::Var("x"))),
            )),
        );
    }

    #[test]
    fn test_let_simple_var_assignment() {
        assert_ast!(
            "
            let x = y
            y
            ",
            spanned(Expr::Let(
                None,
                "x",
                Box::new(spanned(Expr::Var("y"))),
                Box::new(spanned(Expr::Var("y"))),
            )),
        );
    }

    #[test]
    fn test_let_block_assignment_and_usage() {
        assert_ast!(
            "
            let x = {
                let y = 42
                y
            }
            x
            ",
            spanned(Expr::Let(
                None,
                "x",
                Box::new(spanned(Expr::Let(
                    None,
                    "y",
                    Box::new(spanned(Expr::Lit(Value::Int(42)))),
                    Box::new(spanned(Expr::Var("y"))),
                ))),
                Box::new(spanned(Expr::Var("x"))),
            )),
        );
    }

    #[test]
    fn test_let_with_op_assoc_and_precedence() {
        assert_ast!(
            "
            let opl 0 x = 42
            let opr 1 y = x
            y
            ",
            spanned(Expr::Let(
                Some((OpAssoc::Left, 0)),
                "x",
                Box::new(spanned(Expr::Lit(Value::Int(42)))),
                Box::new(spanned(Expr::Let(
                    Some((OpAssoc::Right, 1)),
                    "y",
                    Box::new(spanned(Expr::Var("x"))),
                    Box::new(spanned(Expr::Var("y"))),
                ))),
            )),
        );
    }
}
