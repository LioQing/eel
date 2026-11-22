use std::collections::HashSet;

use chumsky::{input::ValueInput, prelude::*};

use crate::declare::*;

/// Builds the expression and REPL item parsers from the same grammar.
pub fn expr_and_item_parsers<
    'toks,
    'src: 'toks,
    I: ValueInput<'toks, Token = Token<'src>, Span = Span>,
>() -> (
    impl Parser<'toks, I, Spanned<SyntaxExpr<'src>>, extra::Err<Rich<'toks, Token<'src>, Span>>> + Clone,
    impl Parser<'toks, I, ReplItem<'src>, extra::Err<Rich<'toks, Token<'src>, Span>>>,
) {
    let ident = select! { Token::Ident(x) => x }.labelled("identifier");

    let value = select! {
        Token::Float(v) => Value::Float(v),
        Token::Int(v) => Value::Int(v),
        Token::Unit => Value::Unit,
    }
    .labelled("value");

    let lit = value
        .map(|v| SyntaxExpr::Atom(Expr::Lit(v)))
        .labelled("literal");

    let var = ident
        .map(|x| SyntaxExpr::Atom(Expr::Var(x)))
        .labelled("variable");

    let binding_head = just(Token::Opl)
        .to(OpAssoc::Left)
        .or(just(Token::Opr).to(OpAssoc::Right))
        .then(select! { Token::Int(v) => v })
        .or_not()
        .then(ident.map_with(|name, e| (name, e.span())))
        .then_ignore(just(Token::Assign));

    let let_head = just(Token::Let)
        .ignore_then(binding_head.clone())
        .map(|(fixity, (name, _))| (fixity, name));

    let mut let_rec_head = Recursive::declare();
    let expr = recursive(|expr| {
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

        let fn_parts = just(Token::Fn)
            .ignore_then(
                ident
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::OpenParen), just(Token::CloseParen))
                    .map_with(|params, e| (params, e.span())),
            )
            .then(inline.clone())
            .labelled("function expression");

        let r#fn = fn_parts
            .clone()
            .map_with(|(params, body), e| (SyntaxExpr::Fn(params, Box::new(body)), e.span()));

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
                .map(|(name, args)| Patt::Struct(name, args))
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

        let r#let = let_head
            .clone()
            .then(inline.clone())
            .then(expr.clone())
            .map(|(((op_spec, x), assign), body)| {
                SyntaxExpr::Let(op_spec, x, Box::new(assign), Box::new(body))
            })
            .map_with(|expr, e| (expr, e.span()))
            .labelled("let expression");

        let rec_fn = binding_head
            .then(fn_parts)
            .map(|((fixity, name), (params, body))| RecFn {
                name,
                fixity,
                params,
                body,
            });

        let_rec_head.define(
            just(Token::Let).ignore_then(
                rec_fn
                    .repeated()
                    .at_least(1)
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::OpenBrace), just(Token::CloseBrace))
                    .validate(|bindings, _, emitter| {
                        let mut names = HashSet::new();
                        for binding in &bindings {
                            let (name, span) = binding.name;
                            if !names.insert(name) {
                                emitter.emit(Rich::custom(
                                    span,
                                    format!("Duplicate name `{name}` in recursive let group"),
                                ));
                            }
                        }
                        bindings
                    }),
            ),
        );

        let let_rec = let_rec_head
            .clone()
            .then(expr)
            .map_with(|(bindings, body), e| {
                (SyntaxExpr::LetRec(bindings, Box::new(body)), e.span())
            })
            .labelled("recursive let expression");

        choice((op, inline, r#let, let_rec)).labelled("expression")
    });

    let let_item = let_head
        .then(expr.clone())
        .then_ignore(end())
        .map(|((op_spec, name), expr)| ReplItem::Let(op_spec, name, expr))
        .labelled("let item");

    let let_rec_item = let_rec_head
        .then_ignore(end())
        .map(ReplItem::LetRec)
        .labelled("recursive let item");

    let item = choice((let_item, let_rec_item, expr.clone().map(ReplItem::Expr)));

    (expr, item)
}

#[cfg(test)]
mod tests {
    use ariadne::{Color, Label, Report, ReportKind, sources};

    use super::*;

    use crate::lex;

    fn spanned<T: Clone>(value: T) -> Spanned<T> {
        Spanned::new(value, 0..0)
    }

    fn atom(expr: Expr<'_>) -> Spanned<SyntaxExpr<'_>> {
        spanned(SyntaxExpr::Atom(expr))
    }

    fn ops<'src>(
        first: Spanned<SyntaxExpr<'src>>,
        rest: Vec<(&'src str, Spanned<SyntaxExpr<'src>>)>,
    ) -> Spanned<SyntaxExpr<'src>> {
        spanned(SyntaxExpr::Ops(
            Box::new(first),
            rest.into_iter()
                .map(|(op, right)| (atom(Expr::Var(op)), right))
                .collect(),
        ))
    }

    macro_rules! assert_parse {
        ($src:expr, $expected:expr $(,)?) => {
            let src = $src;

            let toks = lex::lexer().parse(src).unwrap();

            let (expr, _) = expr_and_item_parsers();
            let (ast, errs) = expr
                .parse(
                    toks.as_slice()
                        .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
                )
                .into_output_errors();

            let has_errors = !errs.is_empty();
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

            assert!(!has_errors, "Unexpected parse errors in {src:?}");
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
        assert_parse!("42", atom(Expr::Lit(Value::Int(42))));
    }

    #[test]
    fn test_lit_negative_int() {
        assert_parse!("-42", atom(Expr::Lit(Value::Int(-42))));
    }

    #[test]
    fn test_lit_negative_float() {
        assert_parse!("-2.718", atom(Expr::Lit(Value::Float(-2.718))));
    }

    #[test]
    fn test_lit_positive_float() {
        assert_parse!("3.14", atom(Expr::Lit(Value::Float(3.14))));
    }

    #[test]
    fn test_lit_float_scientific_notation_e() {
        assert_parse!("3e8", atom(Expr::Lit(Value::Float(3e8))));
    }

    #[test]
    fn test_lit_float_scientific_notation_e_with_decimal() {
        assert_parse!("1.0e10", atom(Expr::Lit(Value::Float(1.0e10))));
    }

    #[test]
    fn test_lit_float_scientific_notation_negative_exponent() {
        assert_parse!("-1.0E-5", atom(Expr::Lit(Value::Float(-1.0e-5))));
    }

    #[test]
    fn test_var_simple_foo() {
        assert_parse!("foo", atom(Expr::Var("foo")));
    }

    #[test]
    fn test_var_simple_bar() {
        assert_parse!("bar", atom(Expr::Var("bar")));
    }

    #[test]
    fn test_var_with_leading_underscore() {
        assert_parse!("_baz", atom(Expr::Var("_baz")));
    }

    #[test]
    fn test_var_alphanumeric() {
        assert_parse!("abc123", atom(Expr::Var("abc123")));
    }

    #[test]
    fn test_var_emoji() {
        assert_parse!("🦀", atom(Expr::Var("🦀")));
    }

    #[test]
    fn test_var_symbols() {
        assert_parse!("::<>", atom(Expr::Var("::<>")));
    }

    #[test]
    fn test_call_no_args_parentheses() {
        assert_parse!(
            "foo()",
            spanned(SyntaxExpr::Call(
                Box::new(atom(Expr::Var("foo"))),
                spanned(vec![])
            )),
        );
    }

    #[test]
    fn test_call_no_args_dot() {
        assert_parse!(
            "bar.",
            spanned(SyntaxExpr::Call(
                Box::new(atom(Expr::Var("bar"))),
                spanned(vec![]),
            )),
        );
    }

    #[test]
    fn test_call_multiple_arg_types() {
        assert_parse!(
            "foo(1, 2.0, bar)",
            spanned(SyntaxExpr::Call(
                Box::new(atom(Expr::Var("foo"))),
                spanned(vec![
                    atom(Expr::Lit(Value::Int(1))),
                    atom(Expr::Lit(Value::Float(2.0))),
                    atom(Expr::Var("bar")),
                ]),
            )),
        );
    }

    #[test]
    fn test_call_single_int_arg() {
        assert_parse!(
            "bar(42)",
            spanned(SyntaxExpr::Call(
                Box::new(atom(Expr::Var("bar"))),
                spanned(vec![atom(Expr::Lit(Value::Int(42)))]),
            )),
        );
    }

    #[test]
    fn test_call_chained_calls() {
        assert_parse!(
            "foo(bar)(baz)",
            spanned(SyntaxExpr::Call(
                Box::new(spanned(SyntaxExpr::Call(
                    Box::new(atom(Expr::Var("foo"))),
                    spanned(vec![atom(Expr::Var("bar"))]),
                ))),
                spanned(vec![atom(Expr::Var("baz"))]),
            )),
        );
    }

    #[test]
    fn test_call_chained_call_with_dot() {
        assert_parse!(
            "foo(bar).",
            spanned(SyntaxExpr::Call(
                Box::new(spanned(SyntaxExpr::Call(
                    Box::new(atom(Expr::Var("foo"))),
                    spanned(vec![atom(Expr::Var("bar"))]),
                ))),
                spanned(vec![]),
            )),
        );
    }

    #[test]
    fn test_call_nested_call_as_arg() {
        assert_parse!(
            "foo(bar, baz(42))",
            spanned(SyntaxExpr::Call(
                Box::new(atom(Expr::Var("foo"))),
                spanned(vec![
                    atom(Expr::Var("bar")),
                    spanned(SyntaxExpr::Call(
                        Box::new(atom(Expr::Var("baz"))),
                        spanned(vec![atom(Expr::Lit(Value::Int(42)))]),
                    )),
                ]),
            )),
        );
    }

    #[test]
    fn test_fn_no_params_block_body() {
        assert_parse!(
            "fn() { 42 }",
            spanned(SyntaxExpr::Fn(
                spanned(vec![]),
                Box::new(atom(Expr::Lit(Value::Int(42)))),
            )),
        );
    }

    #[test]
    fn test_fn_single_param_expr_body() {
        assert_parse!(
            "fn(x) x",
            spanned(SyntaxExpr::Fn(
                spanned(vec!["x"]),
                Box::new(atom(Expr::Var("x"))),
            )),
        );
    }

    #[test]
    fn test_fn_nested_block_body() {
        assert_parse!(
            "fn(x) { fn(y) x(y) }",
            spanned(SyntaxExpr::Fn(
                spanned(vec!["x"]),
                Box::new(spanned(SyntaxExpr::Fn(
                    spanned(vec!["y"]),
                    Box::new(spanned(SyntaxExpr::Call(
                        Box::new(atom(Expr::Var("x"))),
                        spanned(vec![atom(Expr::Var("y"))]),
                    ))),
                ))),
            )),
        );
    }

    #[test]
    fn test_fn_nested_expr_body() {
        assert_parse!(
            "fn(x) fn(y) x(y)",
            spanned(SyntaxExpr::Fn(
                spanned(vec!["x"]),
                Box::new(spanned(SyntaxExpr::Fn(
                    spanned(vec!["y"]),
                    Box::new(spanned(SyntaxExpr::Call(
                        Box::new(atom(Expr::Var("x"))),
                        spanned(vec![atom(Expr::Var("y"))]),
                    ))),
                ))),
            )),
        );
    }

    #[test]
    fn test_fn_multiple_params_block_body() {
        assert_parse!(
            "fn(x, y) { x(y) }",
            spanned(SyntaxExpr::Fn(
                spanned(vec!["x", "y"]),
                Box::new(spanned(SyntaxExpr::Call(
                    Box::new(atom(Expr::Var("x"))),
                    spanned(vec![atom(Expr::Var("y"))]),
                ))),
            )),
        );
    }

    #[test]
    fn test_case_single_arm_lit_pattern() {
        assert_parse!(
            "case x { 42 => y }",
            spanned(SyntaxExpr::Case(
                Box::new(atom(Expr::Var("x"))),
                spanned(vec![(
                    spanned(Patt::Lit(Value::Int(42))),
                    Box::new(atom(Expr::Var("y"))),
                )]),
            )),
        );
    }

    #[test]
    fn test_case_single_arm_var_pattern() {
        assert_parse!(
            "case x { y => z }",
            spanned(SyntaxExpr::Case(
                Box::new(atom(Expr::Var("x"))),
                spanned(vec![(
                    spanned(Patt::Var("y")),
                    Box::new(atom(Expr::Var("z"))),
                )]),
            )),
        );
    }

    #[test]
    fn test_case_single_arm_struct_pattern() {
        assert_parse!(
            "case x { y(z) => w }",
            spanned(SyntaxExpr::Case(
                Box::new(atom(Expr::Var("x"))),
                spanned(vec![(
                    spanned(Patt::Struct("y", spanned(vec![spanned(Patt::Var("z"))]))),
                    Box::new(atom(Expr::Var("w"))),
                )]),
            )),
        );
    }

    #[test]
    fn test_case_multiple_arms() {
        assert_parse!(
            "
            case x {
                w => 42
                42 => x
                y(z) => f(z)
            }
            ",
            spanned(SyntaxExpr::Case(
                Box::new(atom(Expr::Var("x"))),
                spanned(vec![
                    (
                        spanned(Patt::Var("w")),
                        Box::new(atom(Expr::Lit(Value::Int(42)))),
                    ),
                    (
                        spanned(Patt::Lit(Value::Int(42))),
                        Box::new(atom(Expr::Var("x"))),
                    ),
                    (
                        spanned(Patt::Struct("y", spanned(vec![spanned(Patt::Var("z"))]))),
                        Box::new(spanned(SyntaxExpr::Call(
                            Box::new(atom(Expr::Var("f"))),
                            spanned(vec![atom(Expr::Var("z"))]),
                        ))),
                    ),
                ]),
            )),
        );
    }

    #[test]
    fn test_case_op() {
        assert_parse!(
            "case (x + y) { 42 => z }",
            spanned(SyntaxExpr::Case(
                Box::new(ops(atom(Expr::Var("x")), vec![("+", atom(Expr::Var("y")))])),
                spanned(vec![(
                    spanned(Patt::Lit(Value::Int(42))),
                    Box::new(atom(Expr::Var("z"))),
                )]),
            )),
        );
    }

    #[test]
    fn test_op_simple_addition() {
        assert_parse!(
            "x + y",
            ops(atom(Expr::Var("x")), vec![("+", atom(Expr::Var("y")))]),
        );
    }

    #[test]
    fn test_op_chain_stays_flat() {
        assert_parse!(
            "x + y * z",
            ops(
                atom(Expr::Var("x")),
                vec![("+", atom(Expr::Var("y"))), ("*", atom(Expr::Var("z")))],
            ),
        );
    }

    #[test]
    fn test_op_parentheses_preserve_grouping() {
        let (x, y, z) = (
            atom(Expr::Var("x")),
            atom(Expr::Var("y")),
            atom(Expr::Var("z")),
        );
        assert_parse!(
            "(x + y) * z",
            ops(
                ops(x.clone(), vec![("+", y.clone())]),
                vec![("*", z.clone())]
            ),
        );
        assert_parse!("x * (y + z)", ops(x, vec![("*", ops(y, vec![("+", z)]))]));
    }

    #[test]
    fn test_calls_bind_more_tightly_than_operators() {
        let (two, three, four) = (
            atom(Expr::Lit(Value::Int(2))),
            atom(Expr::Lit(Value::Int(3))),
            atom(Expr::Lit(Value::Int(4))),
        );
        let call_id = |arg| {
            spanned(SyntaxExpr::Call(
                Box::new(spanned(SyntaxExpr::Call(
                    Box::new(atom(Expr::Var("id"))),
                    spanned(vec![arg]),
                ))),
                spanned(vec![]),
            ))
        };
        assert_parse!(
            "2 * id(3). + id(4).",
            ops(
                two.clone(),
                vec![("*", call_id(three.clone())), ("+", call_id(four.clone()))],
            ),
        );
        assert_parse!(
            "id(2 * 3 + 4).",
            call_id(ops(two, vec![("*", three), ("+", four)],)),
        );
    }

    #[test]
    fn test_let_simple_int_assignment() {
        assert_parse!(
            "
            let x = 42
            x
            ",
            spanned(SyntaxExpr::Let(
                None,
                "x",
                Box::new(atom(Expr::Lit(Value::Int(42)))),
                Box::new(atom(Expr::Var("x"))),
            )),
        );
    }

    #[test]
    fn test_let_simple_var_assignment() {
        assert_parse!(
            "
            let x = y
            y
            ",
            spanned(SyntaxExpr::Let(
                None,
                "x",
                Box::new(atom(Expr::Var("y"))),
                Box::new(atom(Expr::Var("y"))),
            )),
        );
    }

    #[test]
    fn test_let_block_assignment_and_usage() {
        assert_parse!(
            "
            let x = {
                let y = 42
                y
            }
            x
            ",
            spanned(SyntaxExpr::Let(
                None,
                "x",
                Box::new(spanned(SyntaxExpr::Let(
                    None,
                    "y",
                    Box::new(atom(Expr::Lit(Value::Int(42)))),
                    Box::new(atom(Expr::Var("y"))),
                ))),
                Box::new(atom(Expr::Var("x"))),
            )),
        );
    }

    #[test]
    fn test_let_with_op_assoc_and_precedence() {
        assert_parse!(
            "
            let opl 0 x = 42
            let opr 1 y = x
            y
            ",
            spanned(SyntaxExpr::Let(
                Some((OpAssoc::Left, 0)),
                "x",
                Box::new(atom(Expr::Lit(Value::Int(42)))),
                Box::new(spanned(SyntaxExpr::Let(
                    Some((OpAssoc::Right, 1)),
                    "y",
                    Box::new(atom(Expr::Var("x"))),
                    Box::new(atom(Expr::Var("y"))),
                ))),
            )),
        );
    }

    #[test]
    fn test_recursive_let_parses_members_without_separators() {
        assert_parse!(
            "let { first = fn(n) later opl 4 left = fn(a, b) { a }
                   opr 2 later = fn(a, b) b } first",
            spanned(SyntaxExpr::LetRec(
                vec![
                    RecFn {
                        name: spanned("first"),
                        fixity: None,
                        params: spanned(vec!["n"]),
                        body: atom(Expr::Var("later")),
                    },
                    RecFn {
                        name: spanned("left"),
                        fixity: Some((OpAssoc::Left, 4)),
                        params: spanned(vec!["a", "b"]),
                        body: atom(Expr::Var("a")),
                    },
                    RecFn {
                        name: spanned("later"),
                        fixity: Some((OpAssoc::Right, 2)),
                        params: spanned(vec!["a", "b"]),
                        body: atom(Expr::Var("b")),
                    },
                ],
                Box::new(atom(Expr::Var("first"))),
            )),
        );
    }

    #[test]
    fn test_recursive_let_rejects_invalid_groups() {
        for src in [
            "let {} unit",
            "let { f = 42 } unit",
            "let { f = other } unit",
            "let { f = fn() unit, g = fn() unit } unit",
            "let { f = fn() unit }",
        ] {
            let toks = lex::lexer().parse(src).unwrap();
            let (expr, _) = expr_and_item_parsers();
            assert!(
                expr.parse(
                    toks.as_slice()
                        .map((src.len()..src.len()).into(), |(t, s)| (t, s))
                )
                .has_errors(),
                "{src}"
            );
        }
    }

    #[test]
    fn test_recursive_let_reports_duplicate_at_second_name() {
        let src = "let { f = fn() unit opr 2 f = fn(a, b) a } unit";
        let duplicate = src.find("f = fn(a").unwrap();
        let toks = lex::lexer().parse(src).unwrap();
        let (expr, _) = expr_and_item_parsers();
        let errors = expr
            .parse(
                toks.as_slice()
                    .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
            )
            .into_errors();
        assert!(
            errors.iter().any(|error| {
                error.span().into_range() == (duplicate..duplicate + 1)
                    && error.to_string().contains("Duplicate name `f`")
            }),
            "{errors:?}"
        );
    }

    #[test]
    fn test_recursive_let_allows_shadowing_across_group_boundaries() {
        let src = "let f = 0 let { f = fn() { let { f = fn() 1 } f } } let f = 2 f";
        let toks = lex::lexer().parse(src).unwrap();
        let (expr, _) = expr_and_item_parsers();
        expr.parse(
            toks.as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .unwrap();
    }

    fn parse_repl_item(src: &str) -> Result<ReplItem<'_>, Vec<String>> {
        let toks = lex::lexer().parse(src).unwrap();
        let (_, item) = expr_and_item_parsers();
        item.parse(
            toks.as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result()
        .map_err(|errors| errors.into_iter().map(|error| error.to_string()).collect())
    }

    #[test]
    fn test_repl_item_accepts_standalone_let_with_infix_expr() {
        let item = parse_repl_item("let x = 2 + 3 * 4").unwrap();
        let ReplItem::Let(op_spec, name, expr) = item else {
            panic!("Expected declaration, got {item:?}");
        };
        assert_eq!(op_spec, None);
        assert_eq!(name, "x");
        assert!(expr.spanless_eq(&ops(
            atom(Expr::Lit(Value::Int(2))),
            vec![
                ("+", atom(Expr::Lit(Value::Int(3)))),
                ("*", atom(Expr::Lit(Value::Int(4))))
            ],
        )));
    }

    #[test]
    fn test_repl_item_accepts_operator_declaration() {
        let item = parse_repl_item("let opr 3 sub = fn(a, b) {\n    a - b\n}").unwrap();
        let ReplItem::Let(op_spec, name, expr) = item else {
            panic!("Expected declaration, got {item:?}");
        };
        assert_eq!(op_spec, Some((OpAssoc::Right, 3)));
        assert_eq!(name, "sub");
        assert!(matches!(expr.0, SyntaxExpr::Fn(..)));
    }

    #[test]
    fn test_repl_distinguishes_recursive_declarations_from_expressions() {
        let declaration = "let { f = fn() { let x = 42 x } }";
        let item = parse_repl_item(declaration).unwrap();
        let ReplItem::LetRec(members) = item else {
            panic!("Expected recursive let declaration, got {item:?}");
        };
        assert_eq!(members[0].name.0, "f");
        assert!(matches!(members[0].body.0, SyntaxExpr::Let(..)));

        let expression = format!("{declaration} f().");
        assert!(matches!(
            parse_repl_item(&expression),
            Ok(ReplItem::Expr((SyntaxExpr::LetRec(..), _)))
        ));
    }

    #[test]
    fn test_repl_item_accepts_ordinary_expressions_and_lets_with_bodies() {
        for (src, expected) in [
            ("42", atom(Expr::Lit(Value::Int(42)))),
            (
                "let x = 42\nx",
                spanned(SyntaxExpr::Let(
                    None,
                    "x",
                    Box::new(atom(Expr::Lit(Value::Int(42)))),
                    Box::new(atom(Expr::Var("x"))),
                )),
            ),
        ] {
            let item = parse_repl_item(src).unwrap();
            let ReplItem::Expr(expr) = item else {
                panic!("Expected expression, got {item:?}");
            };
            assert!(expr.spanless_eq(&expected));
        }
    }

    #[test]
    fn test_standalone_let_is_only_valid_at_the_repl_root() {
        let src = "let x = 42";
        assert!(matches!(parse_repl_item(src), Ok(ReplItem::Let { .. })));
        let toks = lex::lexer().parse(src).unwrap();
        let (expr, _) = expr_and_item_parsers();
        assert!(
            expr.parse(
                toks.as_slice()
                    .map((src.len()..src.len()).into(), |(t, s)| (t, s))
            )
            .has_errors()
        );

        for src in ["let x =", "{ let x = 42 }", "let x = 42 }"] {
            assert!(parse_repl_item(src).is_err(), "{src}");
        }
    }
}
