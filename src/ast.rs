use chumsky::{input::ValueInput, prelude::*};

use crate::declare::*;

// Keep each syntactic operator chain separate until its lexical declarations are known.
// In particular, a parenthesized chain must never be flattened into its parent chain.
enum ParsedExpr<'src> {
    Atom(Expr<'src>),
    Call(Box<Spanned<Self>>, Spanned<Vec<Spanned<Self>>>),
    Fn(Spanned<Vec<&'src str>>, Box<Spanned<Self>>),
    Case(
        Box<Spanned<Self>>,
        Spanned<Vec<(Spanned<Patt<'src>>, Box<Spanned<Self>>)>>,
    ),
    Ops(Box<Spanned<Self>>, Vec<(Spanned<Self>, Spanned<Self>)>),
    Let(
        Option<(OpAssoc, i64)>,
        &'src str,
        Box<Spanned<Self>>,
        Box<Spanned<Self>>,
    ),
}

fn op_spec(op: &Spanned<Expr<'_>>, specs: &[OpSpec<'_>]) -> (OpAssoc, i64) {
    if let Expr::Var(name) = &op.0 {
        if let Some((_, assoc, prec)) = specs.iter().rev().find(|(x, _, _)| x == name) {
            return (*assoc, *prec);
        }

        let prec = match *name {
            "==" | "!=" => 1,
            "<" | "<=" | ">" | ">=" => 2,
            "+" | "-" => 3,
            "*" | "/" | "%" => 4,
            _ => 0,
        };
        (OpAssoc::Left, prec)
    } else {
        (OpAssoc::Left, 0)
    }
}

fn reduce_op<'src>(operands: &mut Vec<Spanned<Expr<'src>>>, op: Spanned<Expr<'src>>) {
    let right = operands.pop().unwrap();
    let left = operands.pop().unwrap();
    let span = (left.1.start..right.1.end).into();
    operands.push((
        Expr::Op(Box::new(left), Box::new(op), Box::new(right)),
        span,
    ));
}

fn shadow_patt<'src>(patt: &Patt<'src>, specs: &mut Vec<OpSpec<'src>>) {
    match patt {
        Patt::Var(name) => specs.push((name, OpAssoc::Left, 0)),
        Patt::Struct(_, fields) => {
            for (patt, _) in &fields.0 {
                shadow_patt(patt, specs);
            }
        }
        Patt::Lit(_) => {}
    }
}

fn resolve_ops<'src>(
    (expr, span): Spanned<ParsedExpr<'src>>,
    specs: &[OpSpec<'src>],
) -> Spanned<Expr<'src>> {
    let expr = match expr {
        ParsedExpr::Atom(expr) => expr,
        ParsedExpr::Call(func, (args, args_span)) => Expr::Call(
            Box::new(resolve_ops(*func, specs)),
            (
                args.into_iter()
                    .map(|arg| resolve_ops(arg, specs))
                    .collect(),
                args_span,
            ),
        ),
        ParsedExpr::Fn(params, body) => {
            let mut scoped_specs = specs.to_vec();
            scoped_specs.extend(params.0.iter().map(|name| (*name, OpAssoc::Left, 0)));
            Expr::Fn(params, Box::new(resolve_ops(*body, &scoped_specs)))
        }
        ParsedExpr::Case(expr, (arms, arms_span)) => Expr::Case(
            Box::new(resolve_ops(*expr, specs)),
            (
                arms.into_iter()
                    .map(|(patt, body)| {
                        let mut scoped_specs = specs.to_vec();
                        shadow_patt(&patt.0, &mut scoped_specs);
                        (patt, Box::new(resolve_ops(*body, &scoped_specs)))
                    })
                    .collect(),
                arms_span,
            ),
        ),
        ParsedExpr::Ops(first, rest) => {
            let mut operands = vec![resolve_ops(*first, specs)];
            let mut operators = Vec::new();
            for (op, right) in rest {
                let op = resolve_ops(op, specs);
                let (assoc, prec) = op_spec(&op, specs);
                while operators.last().is_some_and(|prev| {
                    let (_, prev_prec) = op_spec(prev, specs);
                    prev_prec > prec || (prev_prec == prec && assoc == OpAssoc::Left)
                }) {
                    reduce_op(&mut operands, operators.pop().unwrap());
                }
                operators.push(op);
                operands.push(resolve_ops(right, specs));
            }
            while let Some(op) = operators.pop() {
                reduce_op(&mut operands, op);
            }
            return (operands.pop().unwrap().0, span);
        }
        ParsedExpr::Let(spec, name, assign, body) => {
            let mut scoped_specs = specs.to_vec();
            let (assoc, prec) = spec.unwrap_or((OpAssoc::Left, 0));
            scoped_specs.push((name, assoc, prec));
            // A function binding is recursive, but an ordinary initializer still sees
            // the previous binding, just as it does during evaluation.
            let assign_specs = if matches!(&assign.0, ParsedExpr::Fn(..)) {
                scoped_specs.as_slice()
            } else {
                specs
            };
            Expr::Let(
                spec,
                name,
                Box::new(resolve_ops(*assign, assign_specs)),
                Box::new(resolve_ops(*body, &scoped_specs)),
            )
        }
    };
    (expr, span)
}

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

        let lit = value
            .map(|v| ParsedExpr::Atom(Expr::Lit(v)))
            .labelled("literal");

        let ident = select! { Token::Ident(x) => x }.labelled("identifier");

        let var = ident
            .map(|x| ParsedExpr::Atom(Expr::Var(x)))
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
                |span| (ParsedExpr::Atom(Expr::Err), span),
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
                |span| (ParsedExpr::Atom(Expr::Err), span),
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
                |f, args, e| (ParsedExpr::Call(Box::new(f), args), e.span()),
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
            .map_with(|(params, body), e| (ParsedExpr::Fn(params, Box::new(body)), e.span()))
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
            .map_with(|(expr, arms), e| (ParsedExpr::Case(Box::new(expr), arms), e.span()))
            .labelled("case expression");

        inline.define(choice((call, r#fn, case)));

        op.define(
            inline
                .clone()
                .then(atom.then(inline.clone()).repeated().at_least(1).collect())
                .map_with(|(first, rest), e| (ParsedExpr::Ops(Box::new(first), rest), e.span())),
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
                ParsedExpr::Let(op_spec, x, Box::new(assign), Box::new(body))
            })
            .map_with(|expr, e| (expr, e.span()))
            .labelled("let expression");

        choice((op, inline, r#let)).labelled("expression")
    })
    .map_with(|expr, e| {
        let specs: &mut OpSpecs<'src> = e.state();
        resolve_ops(expr, &specs.0)
    })
}

#[cfg(test)]
mod tests {
    use ariadne::{Color, Label, Report, ReportKind, sources};

    use super::*;

    use crate::{builtin, interp, lex};

    fn spanned<T: Clone>(value: T) -> Spanned<T> {
        Spanned::new(value, 0..0)
    }

    fn parse_source(src: &str) -> Spanned<Expr<'_>> {
        let toks = lex::lexer().parse(src).unwrap();
        expr_parser()
            .parse_with_state(
                toks.as_slice()
                    .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
                &mut extra::SimpleState(vec![]),
            )
            .into_result()
            .unwrap_or_else(|errs| panic!("source: {src}\nparse errors: {errs:?}"))
    }

    fn assert_source_eval(src: &str, expected: i64) {
        let expr = parse_source(src);
        let result = interp::eval(&expr, builtin::env());
        assert_eq!(result, Ok(Value::Int(expected)), "source: {src}");
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

    #[test]
    fn test_builtin_arithmetic_precedence_and_associativity() {
        for (src, expected) in [
            ("2 * 3 + 4", 10),
            ("2 + 3 * 4", 14),
            ("20 - 3 - 2", 15),
            ("24 / 3 / 2", 4),
            ("20 / 2 * 3", 30),
            ("20 % 6 * 2", 4),
            ("20 - 3 + 2", 19),
            ("2 + 3 * 4 - 6 / 2", 11),
        ] {
            assert_source_eval(src, expected);
        }
    }

    #[test]
    fn test_builtin_comparison_precedence_and_associativity() {
        for (src, expected) in [
            ("2 * 3 + 4 == 10", 1),
            ("10 == 2 + 3 * 4", 0),
            ("1 + 2 < 4 * 2", 1),
            ("2 * 3 >= 4 + 2", 1),
            ("1 == 2 < 3", 1),
            ("1 < 2 == 3 < 4", 1),
            ("4 < 3 < 2", 1),
            ("2 == 2 == 1", 1),
        ] {
            assert_source_eval(src, expected);
        }
        // Check inequality grouping independently of its runtime implementation.
        assert_eq!(
            parse_source("1 + 2 != 3 < 4").0.to_string(),
            "((1 + 2) != (3 < 4))",
        );
    }

    #[test]
    fn test_declared_left_and_right_associativity() {
        assert_source_eval("let opl 1 sub = fn(a, b) { a - b } 10 sub 3 sub 2", 5);
        assert_source_eval("let opr 1 sub = fn(a, b) { a - b } 10 sub 3 sub 2", 9);
        assert_source_eval(
            "let opr 9223372036854775807 sub = fn(a, b) { a - b } 10 sub 3 sub 2",
            9,
        );
        assert_source_eval(
            "let opl -9223372036854775808 sub = fn(a, b) { a - b } 10 sub 3 sub 2",
            5,
        );
    }

    #[test]
    fn test_custom_precedence_in_both_operator_orders() {
        for (chain, expected) in [
            ("2 mul 3 add 4", 10),
            ("2 add 3 mul 4", 14),
            ("2 mul 3 add 4 mul 5 add 6", 32),
        ] {
            assert_source_eval(
                &format!(
                    "let opl 5 add = fn(a, b) {{ a + b }}
                     let opl 6 mul = fn(a, b) {{ a * b }} {chain}"
                ),
                expected,
            );
        }
        for (chain, expected) in [("2 mul 3 add 4", 14), ("2 add 3 mul 4", 20)] {
            assert_source_eval(
                &format!(
                    "let opl 6 add = fn(a, b) {{ a + b }}
                     let opl 5 mul = fn(a, b) {{ a * b }} {chain}"
                ),
                expected,
            );
        }
    }

    #[test]
    fn test_custom_and_builtin_precedence_in_both_operator_orders() {
        for (src, expected) in [
            ("let opl 0 add = fn(a, b) { a + b } 2 * 3 add 4", 10),
            ("let opl 0 add = fn(a, b) { a + b } 2 add 3 * 4", 14),
            ("let opl 5 add = fn(a, b) { a + b } 2 * 3 add 4", 14),
            ("let opl 5 add = fn(a, b) { a + b } 2 add 3 * 4", 20),
        ] {
            assert_source_eval(src, expected);
        }
    }

    #[test]
    fn test_equal_precedence_uses_following_operator_associativity() {
        for (sub_assoc, add_assoc, expected) in [
            ("opl", "opl", 9),
            ("opr", "opr", 5),
            ("opl", "opr", 5),
            ("opr", "opl", 9),
        ] {
            assert_source_eval(
                &format!(
                    "let {sub_assoc} 5 sub = fn(a, b) {{ a - b }}
                     let {add_assoc} 5 add = fn(a, b) {{ a + b }} 10 sub 3 add 2"
                ),
                expected,
            );
        }
    }

    #[test]
    fn test_parentheses_and_nested_grouping_override_fixity() {
        for (src, expected) in [
            ("2 * (3 + 4)", 14),
            ("(2 * 3) + 4", 10),
            ("100 - (20 - (5 - 2))", 83),
            ("((20 - 3) - 2)", 15),
            ("20 - (3 - 2)", 19),
            ("2 * ((3 + 4) * (5 - (3 - 1))) + 1", 43),
            ("let opl 1 sub = fn(a, b) { a - b } 10 sub (3 sub 2)", 9),
            ("let opr 1 sub = fn(a, b) { a - b } { (10 sub 3) sub 2 }", 5),
            (
                "let opl 5 add = fn(a, b) { a + b }
                 let opl 6 mul = fn(a, b) { a * b } { (2 add 3) mul 4 }",
                20,
            ),
        ] {
            assert_source_eval(src, expected);
        }
    }

    #[test]
    fn test_calls_bind_more_tightly_than_operators() {
        assert_source_eval("let id = fn(x) { x } 2 * id(3). + id(4).", 10);
        assert_source_eval("let id = fn(x) { x } id(2 * 3 + 4).", 10);
    }

    #[test]
    fn test_nested_declaration_does_not_leak_into_outer_chain() {
        assert_source_eval(
            "let opl 1 sub = fn(a, b) { a - b }
             { let opr 1 sub = fn(a, b) { a - b } 10 sub 3 sub 2 } sub 3 sub 2",
            4,
        );
        assert_source_eval(
            "let opl 0 add = fn(a, b) { a + b }
             let nested = { let opl 5 add = fn(a, b) { a + b } 2 add 3 * 4 }
             nested + (2 add 3 * 4)",
            34,
        );
    }

    #[test]
    fn test_nested_declaration_does_not_leak_into_siblings() {
        assert_source_eval(
            "let opl 1 sub = fn(a, b) { a - b }
             let pair = fn(a, b) { a * 100 + b }
             pair({ let opr 1 sub = fn(a, b) { a - b } 10 sub 3 sub 2 },
                  10 sub 3 sub 2).",
            905,
        );
        assert_source_eval(
            "let opl 1 sub = fn(a, b) { a - b }
             case 1 {
                 0 => { let opr 1 sub = fn(a, b) { a - b } 10 sub 3 sub 2 }
                 _ => { 10 sub 3 sub 2 }
             }",
            5,
        );
    }

    #[test]
    fn test_function_fixity_is_lexical_not_call_site_dependent() {
        assert_source_eval(
            "let opl 1 sub = fn(a, b) { a - b }
             let f = fn() { 10 sub 3 sub 2 }
             let opr 1 sub = fn(a, b) { a - b }
             f().",
            5,
        );
        assert_source_eval(
            "let opl 1 sub = fn(a, b) { a - b }
             let f = fn() { let opr 1 sub = fn(a, b) { a - b } 10 sub 3 sub 2 }
             f(). * 100 + (10 sub 3 sub 2)",
            905,
        );
    }

    #[test]
    fn test_plain_bindings_shadow_operator_fixity() {
        assert_source_eval("let sub = fn(a, b) { a - b } 10 sub 3 sub 2", 5);
        assert_source_eval(
            "let opr 5 sub = fn(a, b) { a - b }
             let sub = fn(a, b) { a - b } 10 sub 3 sub 2",
            5,
        );
        assert_source_eval(
            "let opr 5 sub = fn(a, b) { a - b }
             let f = fn(sub) { 10 sub 3 sub 2 }
             f(fn(a, b) { a - b }).",
            5,
        );
        assert_source_eval(
            "let opr 5 sub = fn(a, b) { a - b }
             let ordinary = fn(a, b) { a - b }
             case ordinary { sub => { 10 sub 3 sub 2 } }",
            5,
        );
        assert_source_eval(
            "let opr 5 sub = fn(a, b) { a - b }
             let wrap = fn(value) { unit }
             case wrap(fn(a, b) { a - b }) { wrap(sub) => { 10 sub 3 sub 2 } }",
            5,
        );
    }

    #[test]
    fn test_initializers_use_previous_binding_fixity() {
        assert_source_eval(
            "let opr 1 sub = fn(a, b) { a - b }
             let sub = { 10 sub 3 sub 2 } sub",
            9,
        );
        assert_source_eval(
            "let opl 1 sub = fn(a, b) { a - b }
             let opr 1 sub = { 10 sub 3 sub 2 } sub",
            5,
        );
    }

    #[test]
    fn test_recursive_function_body_uses_its_own_declared_fixity() {
        for (assoc, expected) in [("opl", "((10 sub 3) sub 2)"), ("opr", "(10 sub (3 sub 2))")] {
            let src = format!("let {assoc} 1 sub = fn(a, b) {{ 10 sub 3 sub 2 }} sub");
            let expr = parse_source(&src);
            let Expr::Let(_, _, assign, _) = expr.0 else {
                panic!("expected let expression");
            };
            let Expr::Fn(_, body) = assign.0 else {
                panic!("expected function expression");
            };
            assert_eq!(body.0.to_string(), expected);
        }
    }

    #[test]
    fn test_builtin_operator_fixity_can_be_shadowed() {
        assert_source_eval("let subtract = - let opr 4 - = subtract 10 - 3 - 2", 9);
        assert_source_eval("let add = + let opl 5 + = add 2 + 3 * 4", 20);
    }

    #[test]
    fn test_supplied_operator_specs_are_used_without_scope_mutation() {
        let src = "let opl 0 unused = fn(a, b) { a } 10 sub 3 sub 2";
        let toks = lex::lexer().parse(src).unwrap();
        let mut specs = extra::SimpleState(vec![("sub", OpAssoc::Right, 7)]);
        let expr = expr_parser()
            .parse_with_state(
                toks.as_slice()
                    .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
                &mut specs,
            )
            .unwrap();
        assert_eq!(specs.0, vec![("sub", OpAssoc::Right, 7)]);
        let Expr::Let(_, _, _, body) = expr.0 else {
            panic!("expected let expression");
        };
        assert_eq!(body.0.to_string(), "(10 sub (3 sub 2))");
    }
}
