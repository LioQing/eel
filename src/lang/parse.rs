use std::collections::HashSet;

use chumsky::{input::ValueInput, prelude::*};

use super::declare::*;

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
        .map(|(assoc, precedence)| Fixity { assoc, precedence })
        .or_not()
        .then(ident.map_with(|name, e| Spanned::new(name, e.span())))
        .then_ignore(just(Token::Assign));

    let let_head = just(Token::Let)
        .ignore_then(binding_head.clone())
        .map(|(fixity, name)| (fixity, name.value));

    let mut let_rec_head = Recursive::declare();
    let mut let_bind = Recursive::declare();
    let mut expr = Recursive::declare();
    let mut inline = Recursive::declare();
    let mut op = Recursive::declare();

    let block = expr
        .clone()
        .delimited_by(just(Token::OpenBrace), just(Token::CloseBrace))
        .recover_with(via_parser(nested_delimiters(
            Token::OpenBrace,
            Token::CloseBrace,
            [(Token::OpenParen, Token::CloseParen)],
            |span| Spanned::new(SyntaxExpr::Atom(Expr::Err), span),
        )))
        .labelled("block expression");

    let paren = op
        .clone()
        .or(inline.clone())
        .or(let_bind.clone())
        .delimited_by(just(Token::OpenParen), just(Token::CloseParen))
        .recover_with(via_parser(nested_delimiters(
            Token::OpenParen,
            Token::CloseParen,
            [(Token::OpenBrace, Token::CloseBrace)],
            |span| Spanned::new(SyntaxExpr::Atom(Expr::Err), span),
        )))
        .labelled("parenthesized expression");

    let atom = choice((lit, var))
        .map_with(|expr, e| Spanned::new(expr, e.span()))
        .or(choice((block, paren)));

    let postfix = atom
        .clone()
        .foldl_with(
            op.clone()
                .or(inline.clone())
                .or(let_bind.clone())
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::OpenParen), just(Token::CloseParen))
                .map_with(|args, e| Some(Spanned::new(args, e.span())))
                .or(just(Token::Dot).to(None))
                .repeated(),
            |operand, args, e| {
                let expr = match args {
                    Some(args) => SyntaxExpr::Call(Call {
                        callee: Box::new(operand),
                        args,
                    }),
                    None => SyntaxExpr::Eval(Box::new(operand)),
                };
                Spanned::new(expr, e.span())
            },
        )
        .labelled("postfix expression");

    let fn_parts = just(Token::Fn)
        .ignore_then(
            ident
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::OpenParen), just(Token::CloseParen))
                .map_with(|params, e| Spanned::new(params, e.span())),
        )
        .then(inline.clone())
        .labelled("function expression");

    let r#fn = fn_parts.clone().map_with(|(params, body), e| {
        Spanned::new(
            SyntaxExpr::Fn(FnExpr {
                params,
                body: Box::new(body),
            }),
            e.span(),
        )
    });

    let patt = recursive(|case_patt| {
        let r#struct = ident
            .then(
                case_patt
                    .clone()
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::OpenParen), just(Token::CloseParen))
                    .map_with(|patt, e| Spanned::new(patt, e.span())),
            )
            .map(|(name, args)| Patt::Struct(StructPatt { name, args }))
            .labelled("struct pattern");
        let var = ident.map(Patt::Var).labelled("variable pattern");
        let lit = value.map(Patt::Lit).labelled("literal pattern");

        choice((r#struct, var, lit))
            .map_with(|patt, e| Spanned::new(patt, e.span()))
            .labelled("pattern")
    });

    let case_arm = patt
        .then_ignore(just(Token::PattArm))
        .then(inline.clone())
        .map(|(patt, body)| CaseArm {
            patt,
            body: Box::new(body),
        })
        .labelled("case expression arm");

    let case = just(Token::Case)
        .ignore_then(inline.clone())
        .then(
            case_arm
                .repeated()
                .at_least(1)
                .collect::<Vec<_>>()
                .delimited_by(just(Token::OpenBrace), just(Token::CloseBrace))
                .map_with(|arms, e| Spanned::new(arms, e.span())),
        )
        .map_with(|(scrutinee, arms), e| {
            Spanned::new(
                SyntaxExpr::Case(Case {
                    scrutinee: Box::new(scrutinee),
                    arms,
                }),
                e.span(),
            )
        })
        .labelled("case expression");

    inline.define(choice((postfix, r#fn, case)));

    op.define(
        inline
            .clone()
            .then(
                atom.then(inline.clone())
                    .map(|(op, rhs)| OpStep { op, rhs })
                    .repeated()
                    .at_least(1)
                    .collect(),
            )
            .map_with(|(first, rest), e| {
                Spanned::new(
                    SyntaxExpr::Ops(Ops {
                        first: Box::new(first),
                        rest,
                    }),
                    e.span(),
                )
            }),
    );

    let r#let = let_head
        .clone()
        .then(inline.clone())
        .then(expr.clone())
        .map(|(((fixity, name), init), body)| {
            SyntaxExpr::Let(Let {
                fixity,
                name,
                init: Box::new(init),
                body: Box::new(body),
            })
        })
        .map_with(|expr, e| Spanned::new(expr, e.span()))
        .labelled("let expression");

    let rec_fn = binding_head
        .then(fn_parts)
        .map(|((fixity, name), (params, body))| RecFn {
            name,
            fixity,
            params,
            body,
        });

    let_bind.define(
        just(Token::Let)
            .ignore_then(ident.map_with(|name, e| Spanned::new(name, e.span())))
            .then(just(Token::BindArrow).map_with(|_, e| e.span()))
            .then(inline.clone())
            .then(expr.clone())
            .map_with(|(((name, arrow), action), body), e| {
                Spanned::new(
                    SyntaxExpr::LetBind(LetBind {
                        name,
                        arrow,
                        action: Box::new(action),
                        body: Box::new(body),
                    }),
                    e.span(),
                )
            })
            .labelled("let bind expression"),
    );

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
                        let Spanned { value: name, span } = binding.name;
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
        .then(expr.clone())
        .map_with(|(bindings, body), e| {
            Spanned::new(
                SyntaxExpr::LetRec(LetRec {
                    bindings,
                    body: Box::new(body),
                }),
                e.span(),
            )
        })
        .labelled("recursive let expression");

    expr.define(choice((op, inline, r#let, let_bind, let_rec)).labelled("expression"));

    let let_item = let_head
        .then(expr.clone())
        .then_ignore(end())
        .map(|((fixity, name), init)| ReplItem::Let(LetBinding { fixity, name, init }))
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

    use crate::lang::lex;

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
        spanned(SyntaxExpr::Ops(Ops {
            first: Box::new(first),
            rest: rest
                .into_iter()
                .map(|(op, rhs)| OpStep {
                    op: atom(Expr::Var(op)),
                    rhs,
                })
                .collect(),
        }))
    }

    fn var(name: &str) -> Spanned<SyntaxExpr<'_>> {
        atom(Expr::Var(name))
    }

    fn int(value: i64) -> Spanned<SyntaxExpr<'static>> {
        atom(Expr::Lit(Value::Int(value)))
    }

    fn call<'src>(
        callee: Spanned<SyntaxExpr<'src>>,
        args: Vec<Spanned<SyntaxExpr<'src>>>,
    ) -> Spanned<SyntaxExpr<'src>> {
        spanned(SyntaxExpr::Call(Call {
            callee: Box::new(callee),
            args: spanned(args),
        }))
    }

    fn evaluate(operand: Spanned<SyntaxExpr<'_>>) -> Spanned<SyntaxExpr<'_>> {
        spanned(SyntaxExpr::Eval(Box::new(operand)))
    }

    fn function<'src>(
        params: Vec<&'src str>,
        body: Spanned<SyntaxExpr<'src>>,
    ) -> Spanned<SyntaxExpr<'src>> {
        spanned(SyntaxExpr::Fn(FnExpr {
            params: spanned(params),
            body: Box::new(body),
        }))
    }

    fn case<'src>(
        scrutinee: Spanned<SyntaxExpr<'src>>,
        arms: Vec<(Patt<'src>, Spanned<SyntaxExpr<'src>>)>,
    ) -> Spanned<SyntaxExpr<'src>> {
        spanned(SyntaxExpr::Case(Case {
            scrutinee: Box::new(scrutinee),
            arms: spanned(
                arms.into_iter()
                    .map(|(patt, body)| CaseArm {
                        patt: spanned(patt),
                        body: Box::new(body),
                    })
                    .collect(),
            ),
        }))
    }

    fn struct_patt<'src>(name: &'src str, args: Vec<Spanned<Patt<'src>>>) -> Patt<'src> {
        Patt::Struct(StructPatt {
            name,
            args: spanned(args),
        })
    }

    fn binding<'src>(
        fixity: Option<Fixity>,
        name: &'src str,
        init: Spanned<SyntaxExpr<'src>>,
        body: Spanned<SyntaxExpr<'src>>,
    ) -> Spanned<SyntaxExpr<'src>> {
        spanned(SyntaxExpr::Let(Let {
            fixity,
            name,
            init: Box::new(init),
            body: Box::new(body),
        }))
    }

    macro_rules! assert_parse {
        ($src:expr, $expected:expr $(,)?) => {
            let src = $src;

            let toks = lex::lexer().parse(src).unwrap();

            let (expr, _) = expr_and_item_parsers();
            let (ast, errs) = expr
                .parse(
                    toks.as_slice()
                        .map((src.len()..src.len()).into(), Spanned::as_parts),
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
        assert_parse!("foo()", call(var("foo"), vec![]));
    }

    #[test]
    fn test_eval() {
        assert_parse!("bar.", evaluate(var("bar")));
    }

    #[test]
    fn test_call_multiple_arg_types() {
        assert_parse!(
            "foo(1, 2.0, bar)",
            call(
                var("foo"),
                vec![int(1), atom(Expr::Lit(Value::Float(2.0))), var("bar"),],
            ),
        );
    }

    #[test]
    fn test_call_single_int_arg() {
        assert_parse!("bar(42)", call(var("bar"), vec![int(42)]));
    }

    #[test]
    fn test_call_chained_calls() {
        assert_parse!(
            "foo(bar)(baz)",
            call(call(var("foo"), vec![var("bar")]), vec![var("baz")]),
        );
    }

    #[test]
    fn test_call_and_eval_chain() {
        assert_parse!(
            "foo(bar).().",
            evaluate(call(evaluate(call(var("foo"), vec![var("bar")])), vec![])),
        );
    }

    #[test]
    fn test_call_nested_call_as_arg() {
        assert_parse!(
            "foo(bar, baz(42))",
            call(
                var("foo"),
                vec![var("bar"), call(var("baz"), vec![int(42)])]
            ),
        );
    }

    #[test]
    fn test_fn_no_params_block_body() {
        assert_parse!("fn() { 42 }", function(vec![], int(42)));
    }

    #[test]
    fn test_fn_single_param_expr_body() {
        assert_parse!("fn(x) x", function(vec!["x"], var("x")));
    }

    #[test]
    fn test_fn_nested_block_body() {
        assert_parse!(
            "fn(x) { fn(y) x(y) }",
            function(
                vec!["x"],
                function(vec!["y"], call(var("x"), vec![var("y")]))
            ),
        );
    }

    #[test]
    fn test_fn_nested_expr_body() {
        assert_parse!(
            "fn(x) fn(y) x(y)",
            function(
                vec!["x"],
                function(vec!["y"], call(var("x"), vec![var("y")]))
            ),
        );
    }

    #[test]
    fn test_fn_multiple_params_block_body() {
        assert_parse!(
            "fn(x, y) { x(y) }",
            function(vec!["x", "y"], call(var("x"), vec![var("y")])),
        );
    }

    #[test]
    fn test_case_single_arm_lit_pattern() {
        assert_parse!(
            "case x { 42 => y }",
            case(var("x"), vec![(Patt::Lit(Value::Int(42)), var("y"))]),
        );
    }

    #[test]
    fn test_case_single_arm_var_pattern() {
        assert_parse!(
            "case x { y => z }",
            case(var("x"), vec![(Patt::Var("y"), var("z"))]),
        );
    }

    #[test]
    fn test_case_single_arm_struct_pattern() {
        assert_parse!(
            "case x { y(z) => w }",
            case(
                var("x"),
                vec![(struct_patt("y", vec![spanned(Patt::Var("z"))]), var("w"))]
            ),
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
            case(
                var("x"),
                vec![
                    (Patt::Var("w"), int(42)),
                    (Patt::Lit(Value::Int(42)), var("x")),
                    (
                        struct_patt("y", vec![spanned(Patt::Var("z"))]),
                        call(var("f"), vec![var("z")]),
                    ),
                ],
            ),
        );
    }

    #[test]
    fn test_case_op() {
        assert_parse!(
            "case (x + y) { 42 => z }",
            case(
                ops(var("x"), vec![("+", var("y"))]),
                vec![(Patt::Lit(Value::Int(42)), var("z"))]
            ),
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
    fn test_postfix_expressions_bind_more_tightly_than_operators() {
        let (two, three, four) = (
            atom(Expr::Lit(Value::Int(2))),
            atom(Expr::Lit(Value::Int(3))),
            atom(Expr::Lit(Value::Int(4))),
        );
        let call_id = |arg| evaluate(call(var("id"), vec![arg]));
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
            binding(None, "x", int(42), var("x")),
        );
    }

    #[test]
    fn test_let_simple_var_assignment() {
        assert_parse!(
            "
            let x = y
            y
            ",
            binding(None, "x", var("y"), var("y")),
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
            binding(None, "x", binding(None, "y", int(42), var("y")), var("x")),
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
            binding(
                Some(Fixity {
                    assoc: OpAssoc::Left,
                    precedence: 0
                }),
                "x",
                int(42),
                binding(
                    Some(Fixity {
                        assoc: OpAssoc::Right,
                        precedence: 1
                    }),
                    "y",
                    var("x"),
                    var("y"),
                ),
            ),
        );
    }

    #[test]
    fn test_recursive_let_parses_members_without_separators() {
        assert_parse!(
            "let { first = fn(n) later opl 4 left = fn(a, b) { a }
                   opr 2 later = fn(a, b) b } first",
            spanned(SyntaxExpr::LetRec(LetRec {
                bindings: vec![
                    RecFn {
                        name: spanned("first"),
                        fixity: None,
                        params: spanned(vec!["n"]),
                        body: atom(Expr::Var("later")),
                    },
                    RecFn {
                        name: spanned("left"),
                        fixity: Some(Fixity {
                            assoc: OpAssoc::Left,
                            precedence: 4
                        }),
                        params: spanned(vec!["a", "b"]),
                        body: atom(Expr::Var("a")),
                    },
                    RecFn {
                        name: spanned("later"),
                        fixity: Some(Fixity {
                            assoc: OpAssoc::Right,
                            precedence: 2
                        }),
                        params: spanned(vec!["a", "b"]),
                        body: atom(Expr::Var("b")),
                    },
                ],
                body: Box::new(var("first")),
            })),
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
                        .map((src.len()..src.len()).into(), Spanned::as_parts)
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
                    .map((src.len()..src.len()).into(), Spanned::as_parts),
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
                .map((src.len()..src.len()).into(), Spanned::as_parts),
        )
        .unwrap();
    }

    fn parse_repl_item(src: &str) -> Result<ReplItem<'_>, Vec<String>> {
        let toks = lex::lexer().parse(src).unwrap();
        let (_, item) = expr_and_item_parsers();
        item.parse(
            toks.as_slice()
                .map((src.len()..src.len()).into(), Spanned::as_parts),
        )
        .into_result()
        .map_err(|errors| errors.into_iter().map(|error| error.to_string()).collect())
    }

    #[test]
    fn test_repl_item_accepts_standalone_let_with_infix_expr() {
        let item = parse_repl_item("let x = 2 + 3 * 4").unwrap();
        let expected = ReplItem::Let(LetBinding {
            fixity: None,
            name: "x",
            init: ops(
                atom(Expr::Lit(Value::Int(2))),
                vec![
                    ("+", atom(Expr::Lit(Value::Int(3)))),
                    ("*", atom(Expr::Lit(Value::Int(4)))),
                ],
            ),
        });
        assert!(item.spanless_eq(&expected), "{item:?}");
    }

    #[test]
    fn test_repl_item_accepts_operator_declaration() {
        let item = parse_repl_item("let opr 3 sub = fn(a, b) {\n    a - b\n}").unwrap();
        let ReplItem::Let(LetBinding { fixity, name, init }) = item else {
            panic!("Expected declaration, got {item:?}");
        };
        assert_eq!(
            fixity,
            Some(Fixity {
                assoc: OpAssoc::Right,
                precedence: 3
            })
        );
        assert_eq!(name, "sub");
        assert!(matches!(init.value, SyntaxExpr::Fn(_)));
    }

    #[test]
    fn test_repl_distinguishes_recursive_declarations_from_expressions() {
        let declaration = "let { f = fn() { let x = 42 x } }";
        let item = parse_repl_item(declaration).unwrap();
        let ReplItem::LetRec(members) = item else {
            panic!("Expected recursive let declaration, got {item:?}");
        };
        assert_eq!(members[0].name.value, "f");
        assert!(matches!(members[0].body.value, SyntaxExpr::Let(_)));

        let expression = format!("{declaration} f().");
        assert!(matches!(
            parse_repl_item(&expression),
            Ok(ReplItem::Expr(Spanned {
                value: SyntaxExpr::LetRec(_),
                ..
            }))
        ));
    }

    #[test]
    fn test_repl_item_accepts_ordinary_expressions_and_lets_with_bodies() {
        for (src, expected) in [
            ("42", atom(Expr::Lit(Value::Int(42)))),
            ("let x = 42\nx", binding(None, "x", int(42), var("x"))),
        ] {
            let item = parse_repl_item(src).unwrap();
            assert!(item.spanless_eq(&ReplItem::Expr(expected)), "{item:?}");
        }
    }

    #[test]
    fn test_repl_item_display() {
        for (src, expected) in [
            ("1 + 2", "(1 + 2)"),
            ("let  x = 42", "let x = 42"),
            (
                "let opr 3 sub = fn(a,b){a - b}",
                "let opr 3 sub = fn(a, b) {(a - b)}",
            ),
            (
                "let {f = fn(){42} opl 2 g = fn(x){x}}",
                "let { f = fn() { 42 } opl 2 g = fn(x) { x } }",
            ),
        ] {
            assert_eq!(parse_repl_item(src).unwrap().to_string(), expected, "{src}");
        }
    }

    #[test]
    fn test_repl_item_spanless_eq_preserves_structure() {
        let src = "let { f = fn(x) x opl 2 g = fn() f(42) }";
        let shifted = format!("  {src}");
        let item = parse_repl_item(src).unwrap();
        assert!(item.spanless_eq(&parse_repl_item(&shifted).unwrap()));

        for different in [
            "let { f = fn(x) x opr 2 g = fn() f(42) }",
            "let { f = fn(x) x opl 2 g = fn() f(43) }",
            "let { opl 2 g = fn() f(42) f = fn(x) x }",
            "let { f = fn(x) x opl 2 g = fn() f(42) } g.",
        ] {
            assert!(
                !item.spanless_eq(&parse_repl_item(different).unwrap()),
                "{different}"
            );
        }
    }

    #[test]
    fn test_standalone_let_is_only_valid_at_the_repl_root() {
        let src = "let x = 42";
        assert!(matches!(parse_repl_item(src), Ok(ReplItem::Let(_))));
        let toks = lex::lexer().parse(src).unwrap();
        let (expr, _) = expr_and_item_parsers();
        assert!(
            expr.parse(
                toks.as_slice()
                    .map((src.len()..src.len()).into(), Spanned::as_parts)
            )
            .has_errors()
        );

        for src in ["let x =", "{ let x = 42 }", "let x = 42 }"] {
            assert!(parse_repl_item(src).is_err(), "{src}");
        }
    }
}
