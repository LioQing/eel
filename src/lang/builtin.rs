use std::rc::Rc;

use super::declare::*;

macro_rules! builtin {
    ($name:literal $expr:ident $env:ident [$($param:ident),* $(,)?] => $body:expr) => {
        (
            $name,
            Value::Closure(Closure {
                name: Some($name),
                group: Rc::new(ClosureGroup {
                    env: Default::default(),
                    members: Box::new([ClosureDef {
                        id: DefinitionId::Builtin($name),
                        rec_name: None,
                        params: vec![$(stringify!($param)),*],
                        body: Rc::new(Spanned::new(Expr::Builtin(|expr, env| {
                            #[allow(unused_variables)]
                            let $expr = expr;
                            #[allow(unused_variables)]
                            let $env = env;
                            $(
                                #[allow(unused_variables)]
                                let $param = env.get(stringify!($param))
                                    .map(Rc::as_ref)
                                    .ok_or_else(|| Error {
                                        msg: format!("Variable `{}` not found", stringify!($param)),
                                        span: expr.span,
                                    })?;
                            )*
                            $body
                        }), 0..0)),
                    }]),
                }),
                member: 0,
            }),
        )
    };
    ($name:literal $expr:ident $env:ident [$($param:ident),* $(,)?] in $body:literal) => {{
        let src = $body;

        let toks = crate::lang::lex::lexer().parse(src).unwrap();

        let (expr, _) = crate::lang::parse::expr_and_item_parsers();
        let syntax = expr
            .parse(
                toks.as_slice()
                    .map((src.len()..src.len()).into(), Spanned::as_parts),
            )
            .unwrap();
        let ast = crate::lang::fixity::resolve(syntax, &[]);

        (
            $name,
            Value::Closure(Closure {
                name: Some($name),
                group: Rc::new(ClosureGroup {
                    env: Default::default(),
                    members: Box::new([ClosureDef {
                        id: DefinitionId::Builtin($name),
                        rec_name: None,
                        params: vec![$(stringify!($param)),*],
                        body: Rc::new(Spanned::new(ast.value, 0..0)),
                    }]),
                }),
                member: 0,
            }),
        )
    }}
}

pub fn with_span<'src>(mut body: Rc<Spanned<Expr<'src>>>, span: Span) -> Rc<Spanned<Expr<'src>>> {
    fn set_expr_span(expr: &mut Spanned<Expr<'_>>, span: Span) {
        expr.span = span;
        match &mut expr.value {
            Expr::Call(Call { callee, args }) => {
                set_expr_span(callee, span);
                args.span = span;
                for arg in &mut args.value {
                    set_expr_span(arg, span);
                }
            }
            Expr::Eval(operand) => set_expr_span(operand, span),
            Expr::Fn(FnExpr { params, body }) => {
                params.span = span;
                set_expr_span(Rc::make_mut(body), span);
            }
            Expr::Case(Case { scrutinee, arms }) => {
                set_expr_span(scrutinee, span);
                arms.span = span;
                for CaseArm { patt, body } in &mut arms.value {
                    set_patt_span(patt, span);
                    set_expr_span(body, span);
                }
            }
            Expr::Op(Op { lhs, op, rhs }) => {
                set_expr_span(lhs, span);
                set_expr_span(op, span);
                set_expr_span(rhs, span);
            }
            Expr::Let(Let { init, body, .. }) => {
                set_expr_span(init, span);
                set_expr_span(body, span);
            }
            Expr::LetRec(LetRec { bindings, body }) => {
                for binding in bindings {
                    binding.name.span = span;
                    binding.params.span = span;
                    set_expr_span(Rc::make_mut(&mut binding.body), span);
                }
                set_expr_span(body, span);
            }
            Expr::Err | Expr::Lit(_) | Expr::Var(_) | Expr::Builtin(_) => {}
        }
    }

    fn set_patt_span(patt: &mut Spanned<Patt<'_>>, span: Span) {
        patt.span = span;
        match &mut patt.value {
            Patt::Struct(StructPatt { args, .. }) => {
                args.span = span;
                for arg in &mut args.value {
                    set_patt_span(arg, span);
                }
            }
            Patt::Var(_) | Patt::Lit(_) => {}
        }
    }

    // Builtin bodies use an empty outer span to mark their internal source.
    // None of their descendant spans may be interpreted as user-source offsets.
    if body.span.end == 0 {
        set_expr_span(Rc::make_mut(&mut body), span);
    }

    body
}

pub fn env<'src>() -> Env<'src> {
    fn action<'src>(name: &'static str, body_span: Span, args: StructArgs<'src>) -> Value<'src> {
        let applied_env = args
            .iter()
            .map(|arg| (arg.name, Rc::clone(&arg.value)))
            .collect();
        Value::Struct(Struct {
            ctor: DefinitionId::Builtin(name),
            name: Some(name),
            body: Rc::new(Spanned::new(Expr::Lit(Value::Unit), body_span)),
            args,
            applied_env,
        })
    }

    [
        builtin!("Io" expr env [plan] => Ok(plan.clone())),
        // These are also the constructors of plans returned by the lowercase helpers.
        builtin!("Return" expr env [value] => Ok(Value::Unit)),
        builtin!("Print" expr env [value] => Ok(Value::Unit)),
        builtin!("Eprint" expr env [value] => Ok(Value::Unit)),
        builtin!("Bind" expr env [plan, next] => Ok(Value::Unit)),
        builtin!("return" expr env [value] => Ok(action("Return", expr.span, Rc::from([
            Binding { name: "value", value: Rc::new(value.clone()) },
        ])))),
        builtin!("print" expr env [value] => Ok(action("Print", expr.span, Rc::from([
            Binding { name: "value", value: Rc::new(value.clone()) },
        ])))),
        builtin!("eprint" expr env [value] => Ok(action("Eprint", expr.span, Rc::from([
            Binding { name: "value", value: Rc::new(value.clone()) },
        ])))),
        builtin!("bind" expr env [plan, next] => Ok(action("Bind", expr.span, Rc::from([
            Binding { name: "plan", value: Rc::new(plan.clone()) },
            Binding { name: "next", value: Rc::new(next.clone()) },
        ])))),
        builtin!("+" expr env [a, b] => {
            match (a, b) {
                (Value::Int(a), Value::Int(b)) => a.checked_add(*b)
                    .map(Value::Int)
                    .ok_or_else(|| Error {
                        msg: format!("Integer overflow: {a} + {b}"),
                        span: expr.span,
                    }),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
                _ => Err(Error {
                    msg: format!("Cannot add {a} and {b}"),
                    span: expr.span,
                }),
            }
        }),
        builtin!("-" expr env [a, b] => {
            match (a, b) {
                (Value::Int(a), Value::Int(b)) => a.checked_sub(*b)
                    .map(Value::Int)
                    .ok_or_else(|| Error {
                        msg: format!("Integer overflow: {a} - {b}"),
                        span: expr.span,
                    }),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
                _ => Err(Error {
                    msg: format!("Cannot subtract {b} from {a}"),
                    span: expr.span,
                }),
            }
        }),
        builtin!("*" expr env [a, b] => {
            match (a, b) {
                (Value::Int(a), Value::Int(b)) => a.checked_mul(*b)
                    .map(Value::Int)
                    .ok_or_else(|| Error {
                        msg: format!("Integer overflow: {a} * {b}"),
                        span: expr.span,
                    }),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
                _ => Err(Error {
                    msg: format!("Cannot multiply {a} and {b}"),
                    span: expr.span,
                }),
            }
        }),
        builtin!("/" expr env [a, b] => {
            match (a, b) {
                (Value::Int(a), Value::Int(b)) if *b != 0 => a.checked_div(*b)
                    .map(Value::Int)
                    .ok_or_else(|| Error {
                        msg: format!("Integer overflow: {a} / {b}"),
                        span: expr.span,
                    }),
                (Value::Float(a), Value::Float(b)) if *b != 0.0 => Ok(Value::Float(a / b)),
                _ => Err(Error {
                    msg: format!("Cannot divide {a} by {b}"),
                    span: expr.span,
                }),
            }
        }),
        builtin!("%" expr env [a, b] => {
            match (a, b) {
                (Value::Int(a), Value::Int(b)) if *b != 0 => a.checked_rem(*b)
                    .map(Value::Int)
                    .ok_or_else(|| Error {
                        msg: format!("Integer overflow: {a} % {b}"),
                        span: expr.span,
                    }),
                _ => Err(Error {
                    msg: format!("Cannot modulo {a} by {b}"),
                    span: expr.span,
                }),
            }
        }),
        builtin!("==" expr env [a, b] => {
            match (a, b) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(if a == b { 1 } else { 0 })),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Int(if a == b { 1 } else { 0 })),
                (Value::Unit, Value::Unit) => Ok(Value::Int(1)),
                _ => Err(Error {
                    msg: format!("Cannot compare {a} and {b} for equality"),
                    span: expr.span,
                }),
            }
        }),
        builtin!("!=" expr env [a, b] => {
            match (a, b) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(if a != b { 1 } else { 0 })),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Int(if a != b { 1 } else { 0 })),
                (Value::Unit, Value::Unit) => Ok(Value::Int(0)),
                _ => Err(Error {
                    msg: format!("Cannot compare {a} and {b} for inequality"),
                    span: expr.span,
                }),
            }
        }),
        builtin!(">" expr env [a, b] => {
            match (a, b) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(if a > b { 1 } else { 0 })),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Int(if a > b { 1 } else { 0 })),
                _ => Err(Error {
                    msg: format!("Cannot compare {a} and {b} for greater than"),
                    span: expr.span,
                }),
            }
        }),
        builtin!("<" expr env [a, b] => {
            match (a, b) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(if a < b { 1 } else { 0 })),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Int(if a < b { 1 } else { 0 })),
                _ => Err(Error {
                    msg: format!("Cannot compare {a} and {b} for less than"),
                    span: expr.span,
                }),
            }
        }),
        builtin!(">=" expr env [a, b] => {
            match (a, b) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(if a >= b { 1 } else { 0 })),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Int(if a >= b { 1 } else { 0 })),
                _ => Err(Error {
                    msg: format!("Cannot compare {a} and {b} for greater than or equal"),
                    span: expr.span,
                }),
            }
        }),
        builtin!("<=" expr env [a, b] => {
            match (a, b) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(if a <= b { 1 } else { 0 })),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Int(if a <= b { 1 } else { 0 })),
                _ => Err(Error {
                    msg: format!("Cannot compare {a} and {b} for less than or equal"),
                    span: expr.span,
                }),
            }
        }),
    ]
    .into_iter()
    .map(|(name, value)| (name, Rc::new(value)))
    .collect()
}

#[cfg(test)]
mod tests {
    use ariadne::{Label, Report, ReportKind, sources};
    use chumsky::prelude::*;

    use super::*;
    use crate::lang::interp;

    fn spanned<T: Clone>(value: T) -> Spanned<T> {
        Spanned::new(value, 0..0)
    }

    fn call<'src>(
        func: Spanned<Expr<'src>>,
        args: Vec<Spanned<Expr<'src>>>,
    ) -> Spanned<Expr<'src>> {
        spanned(Expr::Call(Call {
            callee: Box::new(func),
            args: spanned(args),
        }))
    }

    fn evaluate(operand: Spanned<Expr<'_>>) -> Spanned<Expr<'_>> {
        spanned(Expr::Eval(Box::new(operand)))
    }

    fn var(name: &str, span: std::ops::Range<usize>) -> Spanned<Expr<'_>> {
        Spanned::new(Expr::Var(name), span)
    }

    fn op<'src>(
        lhs: Spanned<Expr<'src>>,
        op: Spanned<Expr<'src>>,
        rhs: Spanned<Expr<'src>>,
    ) -> Expr<'src> {
        Expr::Op(Op {
            lhs: Box::new(lhs),
            op: Box::new(op),
            rhs: Box::new(rhs),
        })
    }

    fn binding<'src>(
        name: &'src str,
        init: Spanned<Expr<'src>>,
        body: Spanned<Expr<'src>>,
    ) -> Expr<'src> {
        Expr::Let(Let {
            fixity: None,
            name,
            init: Box::new(init),
            body: Box::new(body),
        })
    }

    fn function<'src>(params: Spanned<Vec<&'src str>>, body: Spanned<Expr<'src>>) -> Expr<'src> {
        Expr::Fn(FnExpr {
            params,
            body: Rc::new(body),
        })
    }

    fn case<'src>(
        scrutinee: Spanned<Expr<'src>>,
        arms: Spanned<Vec<CaseArm<'src, Expr<'src>>>>,
    ) -> Expr<'src> {
        Expr::Case(Case {
            scrutinee: Box::new(scrutinee),
            arms,
        })
    }

    fn arm<'src>(
        patt: Spanned<Patt<'src>>,
        body: Spanned<Expr<'src>>,
    ) -> CaseArm<'src, Expr<'src>> {
        CaseArm {
            patt,
            body: Box::new(body),
        }
    }

    fn eel_builtin<'src>(
        name: &'src str,
        params: Vec<&'src str>,
        body: Spanned<Expr<'src>>,
    ) -> (&'src str, Value<'src>) {
        (
            name,
            Value::Closure(Closure {
                name: Some(name),
                group: Rc::new(ClosureGroup {
                    env: Default::default(),
                    members: Box::new([ClosureDef {
                        id: DefinitionId::Builtin(name),
                        rec_name: None,
                        params,
                        body: Rc::new(Spanned::new(body.value, 0..0)),
                    }]),
                }),
                member: 0,
            }),
        )
    }

    fn applications<'src>(
        name: &'src str,
        a: Value<'src>,
        b: Value<'src>,
    ) -> [(Spanned<Expr<'src>>, Span); 2] {
        let infix_span = (10..10 + name.len()).into();
        let prefix_span = (0..name.len()).into();
        let args = vec![spanned(Expr::Lit(a)), spanned(Expr::Lit(b))];
        let infix = spanned(op(
            args[0].clone(),
            Spanned::new(Expr::Var(name), infix_span),
            args[1].clone(),
        ));
        let prefix = evaluate(call(Spanned::new(Expr::Var(name), prefix_span), args));
        [(infix, infix_span), (prefix, prefix_span)]
    }

    fn assert_call_error<'src>(
        src: &'src str,
        expr: Spanned<Expr<'src>>,
        builtin: (&'src str, Value<'src>),
        expected: Span,
        msg: &str,
    ) {
        let env = Env::new().insert(builtin.0, Rc::new(builtin.1));
        let error = interp::eval(&expr, env).unwrap_err();
        assert_eq!(error.msg, msg);
        assert_eq!(error.span, expected);
        assert!(src.get(error.span.into_range()).is_some());

        // Exercise Ariadne's byte-indexed rendering, not only the error payload.
        let mut output = Vec::new();
        Report::build(ReportKind::Error, ("caller", error.span.into_range()))
            .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
            .with_message(&error.msg)
            .with_label(Label::new(("caller", error.span.into_range())).with_message(&error.msg))
            .finish()
            .write(sources([("caller", src)]), &mut output)
            .unwrap();
        assert!(String::from_utf8(output).unwrap().contains(msg));
    }

    #[test]
    fn test_eel_builtin_marks_internal_body() {
        let (name, value) = builtin!("b" expr env [a, b] in "a + b");
        let expected = spanned(op(var("a", 0..1), var("+", 2..3), var("b", 4..5)));
        assert_eq!(name, "b");
        assert_eq!(
            value,
            Value::Closure(Closure {
                name: Some("b"),
                group: Rc::new(ClosureGroup {
                    env: Default::default(),
                    members: Box::new([ClosureDef {
                        id: DefinitionId::Builtin("b"),
                        rec_name: None,
                        params: vec!["a", "b"],
                        body: Rc::new(expected),
                    }]),
                }),
                member: 0,
            })
        );
    }

    #[test]
    fn test_eel_builtin_errors_use_infix_and_prefix_call_sites() {
        let infix = spanned(op(
            spanned(Expr::Lit(Value::Int(1))),
            var("b", 2..3),
            spanned(Expr::Lit(Value::Int(2))),
        ));
        let prefix = evaluate(call(
            var("b", 0..1),
            vec![
                spanned(Expr::Lit(Value::Int(1))),
                spanned(Expr::Lit(Value::Int(2))),
            ],
        ));
        for (src, expr, span) in [("1 b 2", infix, 2..3), ("b(1, 2).", prefix, 0..1)] {
            let body = spanned(case(
                var("a", 5..6),
                Spanned::new(
                    vec![arm(
                        Spanned::new(Patt::Var("_"), 9..10),
                        var("missing_internal", 14..30),
                    )],
                    7..32,
                ),
            ));
            assert_call_error(
                src,
                expr,
                eel_builtin("b", vec!["a", "b"], body),
                span.into(),
                "Variable `missing_internal` not found",
            );
        }
    }

    #[test]
    fn test_eel_builtin_errors_use_utf8_call_site_boundaries() {
        let infix = spanned(op(
            spanned(Expr::Lit(Value::Int(1))),
            var("🦀", 2..6),
            spanned(Expr::Lit(Value::Int(2))),
        ));
        let prefix = evaluate(call(
            var("🦀", 0..4),
            vec![
                spanned(Expr::Lit(Value::Int(1))),
                spanned(Expr::Lit(Value::Int(2))),
            ],
        ));
        for (src, expr, span) in [("1 🦀 2", infix, 2..6), ("🦀(1, 2).", prefix, 0..4)] {
            // The internal operator's 2..5 span would split the caller's emoji.
            assert_call_error(
                src,
                expr,
                eel_builtin(
                    "🦀",
                    vec!["a", "b"],
                    spanned(op(var("a", 0..1), var("bad", 2..5), var("b", 6..7))),
                ),
                span.into(),
                "Variable `bad` not found",
            );
        }
    }

    #[test]
    fn test_eel_builtin_remaps_every_ast_span() {
        fn body(span: Span) -> Spanned<Expr<'static>> {
            let var = |name| Spanned::new(Expr::Var(name), span);
            let patt = |value| Spanned::new(value, span);
            let tail = patt(Patt::Struct(StructPatt {
                name: "tail",
                args: Spanned::new(vec![patt(Patt::Var("rest"))], span),
            }));
            let pair = patt(Patt::Struct(StructPatt {
                name: "pair",
                args: Spanned::new(vec![patt(Patt::Var("head")), tail], span),
            }));
            let sum = Spanned::new(op(var("y"), var("+"), var("head")), span);
            let case = Spanned::new(
                case(
                    var("x"),
                    Spanned::new(
                        vec![
                            arm(pair, sum),
                            arm(
                                patt(Patt::Lit(Value::Int(0))),
                                Spanned::new(Expr::Lit(Value::Int(1)), span),
                            ),
                            arm(patt(Patt::Var("_")), var("y")),
                        ],
                        span,
                    ),
                ),
                span,
            );
            let function = Spanned::new(function(Spanned::new(vec!["x", "y"], span), case), span);
            let apply = Spanned::new(
                Expr::Call(Call {
                    callee: Box::new(var("combine")),
                    args: Spanned::new(vec![var("a"), var("b")], span),
                }),
                span,
            );
            let force = Spanned::new(Expr::Eval(Box::new(apply)), span);
            Spanned::new(binding("combine", function, force), span)
        }
        let mut original = body((50..60).into());
        original.span = (0..0).into();
        let original = Rc::new(original);
        let call_span = (13..17).into();

        assert_eq!(*with_span(Rc::clone(&original), call_span), body(call_span),);
        assert_eq!(original.span, (0..0).into());
        for expr in [
            Expr::Err,
            Expr::Lit(Value::Unit),
            Expr::Builtin(|_, _| Ok(Value::Unit)),
        ] {
            let remapped = with_span(Rc::new(spanned(expr)), call_span);
            assert_eq!(remapped.span, call_span);
        }
    }

    #[test]
    fn test_user_function_body_spans_are_preserved() {
        let body = Rc::new(Spanned::new(
            function(
                Spanned::new(vec!["x"], 2..5),
                Spanned::new(
                    Expr::Call(Call {
                        callee: Box::new(var("missing", 8..15)),
                        args: Spanned::new(vec![var("x", 16..17)], 15..18),
                    }),
                    8..18,
                ),
            ),
            0..20,
        ));
        let call_body = with_span(Rc::clone(&body), (30..34).into());
        assert!(Rc::ptr_eq(&call_body, &body));
    }

    #[test]
    fn test_returned_eel_builtin_function_keeps_original_call_site() {
        let expr = spanned(binding(
            "f",
            evaluate(call(var("b", 8..9), vec![])),
            evaluate(call(
                var("f", 13..14),
                vec![spanned(Expr::Lit(Value::Int(0)))],
            )),
        ));
        let body = spanned(function(
            Spanned::new(vec!["x"], 2..5),
            Spanned::new(
                Expr::Call(Call {
                    callee: Box::new(var("missing_internal", 8..24)),
                    args: Spanned::new(vec![var("x", 25..26)], 24..27),
                }),
                8..27,
            ),
        ));
        assert_call_error(
            "let f = b(). f(0).",
            expr,
            eel_builtin("b", vec![], body),
            (8..9).into(),
            "Variable `missing_internal` not found",
        );
    }

    #[test]
    fn test_eel_builtin_calls_do_not_reuse_previous_call_spans() {
        let expr = spanned(binding(
            "_",
            evaluate(call(
                var("b", 8..9),
                vec![
                    spanned(Expr::Lit(Value::Int(0))),
                    spanned(Expr::Lit(Value::Int(2))),
                ],
            )),
            evaluate(call(
                var("b", 17..18),
                vec![
                    spanned(Expr::Lit(Value::Int(1))),
                    spanned(Expr::Lit(Value::Int(2))),
                ],
            )),
        ));
        let body = spanned(case(
            var("a", 5..6),
            Spanned::new(
                vec![
                    arm(
                        Spanned::new(Patt::Lit(Value::Int(0)), 9..10),
                        Spanned::new(Expr::Lit(Value::Int(0)), 14..15),
                    ),
                    arm(
                        Spanned::new(Patt::Var("_"), 16..17),
                        var("missing_internal", 21..37),
                    ),
                ],
                7..39,
            ),
        ));
        assert_call_error(
            "let _ = b(0, 2). b(1, 2).",
            expr,
            eel_builtin("b", vec!["a", "b"], body),
            (17..18).into(),
            "Variable `missing_internal` not found",
        );
    }

    #[test]
    fn test_integer_arithmetic_boundaries() {
        for (a, op, b, expected) in [
            (i64::MAX, "+", 0, i64::MAX),
            (i64::MIN, "+", 0, i64::MIN),
            (i64::MAX, "+", i64::MIN, -1),
            (i64::MAX, "+", -1, i64::MAX - 1),
            (i64::MIN, "+", 1, i64::MIN + 1),
            (i64::MAX, "-", 0, i64::MAX),
            (i64::MIN, "-", 0, i64::MIN),
            (-1, "-", i64::MAX, i64::MIN),
            (i64::MIN, "-", -1, i64::MIN + 1),
            (i64::MAX, "-", i64::MAX, 0),
            (i64::MAX, "*", 1, i64::MAX),
            (i64::MIN, "*", 1, i64::MIN),
            (i64::MAX, "*", 0, 0),
            (i64::MIN, "*", 0, 0),
            (i64::MAX, "*", -1, -i64::MAX),
            (i64::MIN, "/", 1, i64::MIN),
            (i64::MAX, "/", -1, -i64::MAX),
            (i64::MIN, "/", i64::MIN, 1),
            (0, "/", -1, 0),
            (7, "/", -3, -2),
            (-7, "/", 3, -2),
            (-7, "/", -3, 2),
            (i64::MIN, "%", 1, 0),
            (i64::MIN, "%", i64::MIN, 0),
            (i64::MAX, "%", -1, 0),
            (0, "%", -1, 0),
            (-7, "%", 3, -1),
            (7, "%", -3, 1),
        ] {
            for (expr, _) in applications(op, Value::Int(a), Value::Int(b)) {
                assert_eq!(
                    interp::eval(&expr, env()),
                    Ok(Value::Int(expected)),
                    "{a} {op} {b}"
                );
            }
        }
    }

    #[test]
    fn test_integer_arithmetic_overflow() {
        for (a, op, b) in [
            (i64::MAX, "+", 1),
            (1, "+", i64::MAX),
            (i64::MIN, "+", -1),
            (-1, "+", i64::MIN),
            (i64::MAX, "+", i64::MAX),
            (i64::MIN, "+", i64::MIN),
            (i64::MIN, "-", 1),
            (i64::MAX, "-", -1),
            (0, "-", i64::MIN),
            (i64::MAX, "-", i64::MIN),
            (i64::MAX, "*", 2),
            (2, "*", i64::MAX),
            (i64::MIN, "*", 2),
            (i64::MIN, "*", -1),
            (-1, "*", i64::MIN),
            (i64::MIN, "*", i64::MIN),
            (i64::MIN, "/", -1),
            (i64::MIN, "%", -1),
        ] {
            for (expr, span) in applications(op, Value::Int(a), Value::Int(b)) {
                assert_eq!(
                    interp::eval(&expr, env()),
                    Err(Error {
                        msg: format!("Integer overflow: {a} {op} {b}"),
                        span,
                    }),
                    "{a} {op} {b}",
                );
            }
        }
    }

    #[test]
    fn test_integer_zero_divisors() {
        for a in [i64::MIN, -1, 0, 1, i64::MAX] {
            for (op, action) in [("/", "divide"), ("%", "modulo")] {
                for (expr, span) in applications(op, Value::Int(a), Value::Int(0)) {
                    let err = interp::eval(&expr, env()).unwrap_err();
                    assert_eq!(err.msg, format!("Cannot {action} {a} by 0"), "{a} {op} 0");
                    assert_eq!(err.span, span);
                }
            }
        }
    }

    #[test]
    fn test_inequality_supported_values() {
        for (a, b, unequal) in [
            (Value::Int(1), Value::Int(2), 1),
            (Value::Int(2), Value::Int(1), 1),
            (Value::Int(1), Value::Int(1), 0),
            (Value::Int(-2), Value::Int(-2), 0),
            (Value::Int(-2), Value::Int(2), 1),
            (Value::Float(1.0), Value::Float(2.0), 1),
            (Value::Float(1.0), Value::Float(1.0), 0),
            (Value::Float(0.0), Value::Float(-0.0), 0),
            (Value::Unit, Value::Unit, 0),
        ] {
            for (op, expected) in [("!=", unequal), ("==", 1 - unequal)] {
                for (expr, _) in applications(op, a.clone(), b.clone()) {
                    assert_eq!(
                        interp::eval(&expr, env()),
                        Ok(Value::Int(expected)),
                        "{a} {op} {b}"
                    );
                }
            }
        }
    }

    #[test]
    fn test_overflow_in_deferred_and_nested_evaluation() {
        let max = spanned(Expr::Lit(Value::Int(i64::MAX)));
        let one = spanned(Expr::Lit(Value::Int(1)));
        let overflow = applications("+", Value::Int(i64::MAX), Value::Int(1))[0]
            .0
            .clone();
        let deferred = spanned(binding(
            "sum",
            call(var("+", 5..6), vec![max.clone(), one.clone()]),
            evaluate(spanned(Expr::Var("sum"))),
        ));
        let variables = spanned(binding(
            "x",
            max.clone(),
            spanned(binding(
                "y",
                one,
                spanned(op(
                    spanned(Expr::Var("x")),
                    var("+", 10..11),
                    spanned(Expr::Var("y")),
                )),
            )),
        ));
        let function = spanned(binding(
            "f",
            Spanned::new(
                function(
                    spanned(vec!["x"]),
                    Spanned::new(
                        op(
                            spanned(Expr::Var("x")),
                            var("*", 20..21),
                            spanned(Expr::Lit(Value::Int(2))),
                        ),
                        18..23,
                    ),
                ),
                10..25,
            ),
            evaluate(call(spanned(Expr::Var("f")), vec![max])),
        ));
        let nested = spanned(op(
            spanned(Expr::Lit(Value::Int(0))),
            var("+", 2..3),
            overflow,
        ));
        for expr in [variables, deferred, function, nested] {
            let err = interp::eval(&expr, env()).unwrap_err();
            assert!(
                err.msg.starts_with("Integer overflow:"),
                "{expr:?}: {err:?}"
            );
            assert!(err.span.start < err.span.end, "{expr:?}: {err:?}");
        }
    }

    #[test]
    fn test_float_arithmetic_unchanged() {
        for (a, op, b, expected) in [
            (1.5, "+", 2.5, 4.0),
            (1.5, "-", 2.5, -1.0),
            (1.5, "*", 2.0, 3.0),
            (7.0, "/", -2.0, -3.5),
            (1e308, "*", 1e308, f64::INFINITY),
            (-1e308, "*", 1e308, f64::NEG_INFINITY),
        ] {
            for (expr, _) in applications(op, Value::Float(a), Value::Float(b)) {
                assert_eq!(
                    interp::eval(&expr, env()),
                    Ok(Value::Float(expected)),
                    "{a} {op} {b}"
                );
            }
        }

        assert!(matches!(
            interp::eval(&applications("-", Value::Float(f64::INFINITY), Value::Float(f64::INFINITY))[0].0, env()),
            Ok(Value::Float(value)) if value.is_nan()
        ));

        for (a, b) in [(1.0, 0.0), (1.0, -0.0), (0.0, 0.0)] {
            for (expr, span) in applications("/", Value::Float(a), Value::Float(b)) {
                let err = interp::eval(&expr, env()).unwrap_err();
                assert!(err.msg.starts_with("Cannot divide"), "{a} / {b}: {err:?}");
                assert_eq!(err.span, span);
            }
        }
    }

    #[test]
    fn test_arithmetic_type_policy_unchanged() {
        for (op, action) in [
            ("+", "add"),
            ("-", "subtract"),
            ("*", "multiply"),
            ("/", "divide"),
            ("%", "modulo"),
        ] {
            for (a, b) in [
                (Value::Int(1), Value::Float(2.0)),
                (Value::Float(1.0), Value::Int(2)),
                (Value::Unit, Value::Int(1)),
                (Value::Int(1), Value::Unit),
            ] {
                for (expr, span) in applications(op, a.clone(), b.clone()) {
                    let err = interp::eval(&expr, env()).unwrap_err();
                    assert!(
                        err.msg.starts_with(&format!("Cannot {action}")),
                        "{a} {op} {b}: {err:?}"
                    );
                    assert_eq!(err.span, span);
                }
            }
        }

        assert_eq!(
            interp::eval(
                &applications("%", Value::Float(1.0), Value::Float(2.0))[0].0,
                env()
            )
            .unwrap_err()
            .msg,
            "Cannot modulo 1 by 2",
        );
    }

    #[test]
    fn test_inequality_rejects_unsupported_values() {
        let closure = Value::Closure(Closure {
            name: None,
            group: Rc::new(ClosureGroup {
                env: Default::default(),
                members: Box::new([ClosureDef {
                    id: DefinitionId::Source((0..0).into()),
                    rec_name: None,
                    params: vec!["x"],
                    body: Rc::new(spanned(Expr::Var("x"))),
                }]),
            }),
            member: 0,
        });
        let args: StructArgs<'_> = Rc::from([
            Binding {
                name: "a",
                value: Rc::new(Value::Int(1)),
            },
            Binding {
                name: "b",
                value: Rc::new(Value::Int(2)),
            },
        ]);
        let applied_env = args
            .iter()
            .map(|arg| (arg.name, Rc::clone(&arg.value)))
            .collect();
        let deferred = Value::Struct(Struct {
            ctor: DefinitionId::Builtin("+"),
            name: Some("+"),
            body: Rc::new(spanned(Expr::Lit(Value::Int(3)))),
            args,
            applied_env,
        });
        for (a, b) in [
            (Value::Int(1), Value::Float(1.0)),
            (Value::Float(1.0), Value::Int(1)),
            (Value::Unit, Value::Int(1)),
            (Value::Int(1), Value::Unit),
            (Value::Unit, Value::Float(1.0)),
            (Value::Float(1.0), Value::Unit),
            (closure.clone(), closure.clone()),
            (deferred.clone(), deferred.clone()),
            (closure, Value::Int(1)),
            (Value::Int(1), deferred),
        ] {
            for (op, comparison) in [("!=", "inequality"), ("==", "equality")] {
                for (expr, span) in applications(op, a.clone(), b.clone()) {
                    let err = interp::eval(&expr, env()).unwrap_err();
                    assert!(
                        err.msg.starts_with("Cannot compare "),
                        "{a} {op} {b}: {err:?}"
                    );
                    assert!(
                        err.msg.ends_with(&format!(" for {comparison}")),
                        "{a} {op} {b}: {err:?}"
                    );
                    assert_eq!(err.span, span);
                }
            }
        }
    }

    #[test]
    fn test_inequality_application_remains_explicit() {
        for value in [Value::Int(2), Value::Float(1.0)] {
            let expr = call(
                spanned(Expr::Var("!=")),
                vec![
                    spanned(Expr::Lit(Value::Int(1))),
                    spanned(Expr::Lit(value.clone())),
                ],
            );
            let result = interp::eval(&expr, env());
            assert!(
                matches!(result, Ok(Value::Struct(Struct { name: Some("!="), ref args, .. }))
                    if args.iter().map(|arg| (arg.name, arg.value.as_ref()))
                        .eq([("a", &Value::Int(1)), ("b", &value)])),
                "{expr:?}: {result:?}"
            );
        }
    }

    #[test]
    fn test_inequality_does_not_depend_on_shadowed_equality() {
        let equality = spanned(function(
            spanned(vec!["a", "b"]),
            spanned(Expr::Lit(Value::Int(0))),
        ));
        let inequality = applications("!=", Value::Int(1), Value::Int(1))[0]
            .0
            .clone();
        let expr = spanned(binding("==", equality, inequality));
        assert_eq!(interp::eval(&expr, env()), Ok(Value::Int(0)));
    }
}
