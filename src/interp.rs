use std::rc::Rc;

use crate::declare::*;

fn closure_env<'src>(closure: &Value<'src>) -> Env<'src> {
    let Value::Closure(name, _, _, captured_env) = closure else {
        unreachable!();
    };

    let mut env = captured_env.as_ref().clone();
    if let Some(name) = name {
        // Recreate the recursive binding when applying the closure, so its
        // retained environment never needs to contain a cycle back to itself.
        env.push((name, closure.clone()));
    }
    env
}

pub fn eval<'src>(expr: &Spanned<Expr<'src>>, env: Env<'src>) -> Result<Value<'src>, Error> {
    Ok(match &expr.0 {
        Expr::Err => unreachable!(), // Valid AST does not contain Expr::Err
        Expr::Lit(v) => v.clone(),
        Expr::Var(x) => env
            .iter()
            .rev()
            .find(|(y, _)| y == x)
            .map(|(_, v)| v.clone())
            .ok_or_else(|| Error {
                msg: format!("Variable `{x}` not found"),
                span: expr.1.clone(),
            })?,
        Expr::Call(f, args) => match eval(f, env.clone())? {
            closure @ Value::Closure(..) => {
                let captured_env = Rc::new(closure_env(&closure));
                let Value::Closure(name, params, body, _) = closure else {
                    unreachable!();
                };
                if params.len() != args.0.len() {
                    return Err(Error {
                        msg: format!(
                            "Expected {} arguments for function {}, but got {}",
                            params.len(),
                            name.map(|n| format!("`{n}`"))
                                .unwrap_or_else(|| "<anonymous>".to_string()),
                            args.0.len(),
                        ),
                        span: expr.1.clone(),
                    });
                }

                let body = crate::builtin::body_at_call_site(body, f.1);

                let args_env = params
                    .iter()
                    .zip(args.0.iter())
                    .map(|(param, arg)| Ok((*param, eval(arg, env.clone())?)))
                    .collect::<Result<_, _>>()?;

                Value::Struct(name, body, args_env, captured_env)
            }
            Value::Struct(name, body, args_env, captured_env) => {
                if !args.0.is_empty() {
                    return Err(Error {
                        msg: format!(
                            "Expected no argument for struct {}, but got {}",
                            name.map(|n| format!("`{n}`"))
                                .unwrap_or_else(|| "<anonymous>".to_string()),
                            args.0.len()
                        ),
                        span: expr.1.clone(),
                    });
                }

                let mut new_env = captured_env.as_ref().clone();
                new_env.extend(args_env);

                eval(&body, new_env)?
            }
            f => {
                return Err(Error {
                    msg: format!("Expected a function or struct, but got {f}"),
                    span: expr.1.clone(),
                });
            }
        },
        Expr::Fn(params, body) => {
            Value::Closure(None, params.0.clone(), body.clone(), Rc::new(env))
        }
        Expr::Case(e, arms) => {
            let v = eval(e, env.clone())?;

            let (patt_env, body) = arms
                .0
                .iter()
                .find_map(|(patt, body)| match_patt(&patt.0, &v).map(|env| (env, body)))
                .ok_or_else(|| Error {
                    msg: format!("No pattern matched for value {v}"),
                    span: expr.1.clone(),
                })?;

            let mut new_env = env;
            new_env.extend(patt_env);

            eval(body, new_env)?
        }
        Expr::Op(e1, op, e2) => match eval(op, env.clone())? {
            closure @ Value::Closure(..) => {
                let mut new_env = closure_env(&closure);
                let Value::Closure(name, params, body, _) = closure else {
                    unreachable!();
                };
                if params.len() != 2 {
                    return Err(Error {
                        msg: format!(
                            "Expected 2 parameters for operator {}, but got {}",
                            name.map(|n| format!("`{n}`"))
                                .unwrap_or_else(|| "<anonymous>".to_string()),
                            params.len(),
                        ),
                        span: op.1,
                    });
                }

                let body = crate::builtin::body_at_call_site(body, op.1);

                let arg1 = eval(e1, env.clone())?;
                let arg2 = eval(e2, env.clone())?;

                new_env.extend([(params[0], arg1), (params[1], arg2)]);

                eval(&body, new_env)?
            }
            v => {
                return Err(Error {
                    msg: format!("Expected an operator, but got {v}"),
                    span: op.1,
                });
            }
        },
        Expr::Let(_, x, e, body) => {
            let mut v = eval(e, env.clone())?;

            let recursive_binding = match &v {
                Value::Closure(Some(name), _, _, _) if name != x => Some((*name, v.clone())),
                _ => None,
            };
            if let Value::Closure(name, _, _, captured_env) = &mut v {
                // An alias keeps the original function's recursive binding,
                // even though its display and struct name follow the new let.
                if let Some(binding) = recursive_binding {
                    Rc::make_mut(captured_env).push(binding);
                }
                *name = Some(x);
            }

            let mut new_env = env;
            new_env.push((x, v));

            eval(body, new_env)?
        }
        Expr::Builtin(f) => f(&expr, &env)?,
    })
}

pub fn match_patt<'src>(patt: &Patt<'src>, value: &Value<'src>) -> Option<Env<'src>> {
    match (patt, value) {
        (Patt::Lit(p), v) if p == v => Some(vec![]),
        (Patt::Var(x), _) => Some(vec![(x, value.clone())]),
        (Patt::Struct(x, ps), Value::Struct(y, _, args_env, _)) if Some(*x) == *y => {
            if ps.0.len() != args_env.len() {
                return None;
            }

            let mut env = vec![];
            for ((p, _), (_, v)) in ps.0.iter().zip(args_env.iter()) {
                if let Some(new_env) = match_patt(p, v) {
                    env.extend(new_env);
                } else {
                    return None;
                }
            }

            Some(env)
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use chumsky::prelude::*;

    use crate::builtin;

    use super::*;

    fn spanned<T: Clone>(value: T) -> Spanned<T> {
        Spanned::new(value, 0..0)
    }

    fn assert_eval<'src>(expr: Spanned<Expr<'src>>, env: Env<'src>, expected: Value<'src>) {
        let value = match eval(&expr, env) {
            Ok(value) => value,
            Err(e) => panic!("{}", e.msg),
        };

        assert_eq!(value, expected);
    }

    fn int<'src>(value: i64) -> Spanned<Expr<'src>> {
        spanned(Expr::Lit(Value::Int(value)))
    }

    fn var(name: &str) -> Spanned<Expr<'_>> {
        spanned(Expr::Var(name))
    }

    fn function<'src>(params: Vec<&'src str>, body: Spanned<Expr<'src>>) -> Spanned<Expr<'src>> {
        spanned(Expr::Fn(spanned(params), Box::new(body)))
    }

    fn call<'src>(
        func: Spanned<Expr<'src>>,
        args: Vec<Spanned<Expr<'src>>>,
    ) -> Spanned<Expr<'src>> {
        spanned(Expr::Call(Box::new(func), spanned(args)))
    }

    fn binding<'src>(
        name: &'src str,
        assign: Spanned<Expr<'src>>,
        body: Spanned<Expr<'src>>,
    ) -> Spanned<Expr<'src>> {
        spanned(Expr::Let(None, name, Box::new(assign), Box::new(body)))
    }

    fn op<'src>(
        left: Spanned<Expr<'src>>,
        name: &'src str,
        right: Spanned<Expr<'src>>,
    ) -> Spanned<Expr<'src>> {
        spanned(Expr::Op(
            Box::new(left),
            Box::new(var(name)),
            Box::new(right),
        ))
    }

    fn case<'src>(
        value: Spanned<Expr<'src>>,
        arms: Vec<(Spanned<Patt<'src>>, Spanned<Expr<'src>>)>,
    ) -> Spanned<Expr<'src>> {
        spanned(Expr::Case(
            Box::new(value),
            spanned(
                arms.into_iter()
                    .map(|(patt, body)| (patt, Box::new(body)))
                    .collect(),
            ),
        ))
    }

    fn struct_value<'src>(name: &'src str, fields: Env<'src>) -> Value<'src> {
        Value::Struct(
            Some(name),
            Box::new(spanned(Expr::Lit(Value::Unit))),
            fields,
            Default::default(),
        )
    }

    fn struct_pattern<'src>(
        name: &'src str,
        fields: Vec<Spanned<Patt<'src>>>,
    ) -> Spanned<Patt<'src>> {
        spanned(Patt::Struct(name, spanned(fields)))
    }

    #[test]
    fn test_lit() {
        let env = vec![("x", Value::Int(42)), ("y", Value::Float(3.14))];

        // 42
        let expr = spanned(Expr::Lit(Value::Int(43)));

        assert_eval(expr, env, Value::Int(43));
    }

    #[test]
    fn test_var() {
        let env = vec![("x", Value::Int(42)), ("y", Value::Float(3.14))];

        // x
        let expr = spanned(Expr::Var("x"));

        assert_eval(expr, env, Value::Int(42));
    }

    #[test]
    fn test_call() {
        let env = vec![
            ("x", Value::Int(42)),
            ("y", Value::Float(3.14)),
            (
                "id",
                Value::Closure(
                    Some("id"),
                    vec!["y"],
                    Box::new(spanned(Expr::Var("y"))),
                    Default::default(),
                ),
            ),
        ];

        // id(x)()
        let expr = spanned(Expr::Call(
            Box::new(spanned(Expr::Call(
                Box::new(spanned(Expr::Var("id"))),
                spanned(vec![spanned(Expr::Var("x"))]),
            ))),
            spanned(vec![]),
        ));

        assert_eval(expr, env, Value::Int(42));
    }

    #[test]
    fn test_call_closure() {
        let id = Value::Closure(
            Some("id"),
            vec!["y"],
            Box::new(spanned(Expr::Var("y"))),
            Default::default(),
        );
        let env = vec![
            ("x", Value::Int(42)),
            ("y", Value::Float(3.14)),
            ("id", id.clone()),
        ];

        // id(x)
        let expr = spanned(Expr::Call(
            Box::new(spanned(Expr::Var("id"))),
            spanned(vec![spanned(Expr::Var("x"))]),
        ));

        assert_eval(
            expr,
            env,
            Value::Struct(
                Some("id"),
                Box::new(spanned(Expr::Var("y"))),
                vec![("y", Value::Int(42))],
                Rc::new(vec![("id", id)]),
            ),
        );
    }

    #[test]
    fn test_call_struct() {
        let env = vec![
            ("x", Value::Int(42)),
            ("y", Value::Float(3.14)),
            (
                "x_struct",
                Value::Struct(
                    Some("x_struct"),
                    Box::new(spanned(Expr::Var("x"))),
                    vec![("x", Value::Int(43))],
                    Rc::new(vec![("x", Value::Int(42)), ("y", Value::Float(3.14))]),
                ),
            ),
        ];

        // x_struct()
        let expr = spanned(Expr::Call(
            Box::new(spanned(Expr::Var("x_struct"))),
            spanned(vec![]),
        ));

        assert_eval(expr, env, Value::Int(43));
    }

    #[test]
    fn test_fn() {
        let env = vec![("x", Value::Int(42)), ("y", Value::Float(3.14))];

        // fn(y) { y }
        let expr = spanned(Expr::Fn(
            spanned(vec!["y"]),
            Box::new(spanned(Expr::Var("y"))),
        ));

        assert_eval(
            expr,
            env.clone(),
            Value::Closure(
                None,
                vec!["y"],
                Box::new(spanned(Expr::Var("y"))),
                Rc::new(env),
            ),
        );
    }

    #[test]
    fn test_case() {
        let env = vec![("x", Value::Int(42)), ("y", Value::Float(3.14))];

        // case x {
        //     42 => y
        //     _ => x
        // }
        let expr = spanned(Expr::Case(
            Box::new(spanned(Expr::Var("x"))),
            spanned(vec![
                (
                    spanned(Patt::Lit(Value::Int(42))),
                    Box::new(spanned(Expr::Var("y"))),
                ),
                (spanned(Patt::Var("_")), Box::new(spanned(Expr::Var("x")))),
            ]),
        ));

        assert_eval(expr, env, Value::Float(3.14));
    }

    #[test]
    fn test_op() {
        let env = vec![
            ("x", Value::Int(42)),
            ("y", Value::Int(3)),
            builtin::env().into_iter().find(|(k, _)| *k == "+").unwrap(),
        ];

        // x + y
        let expr = spanned(Expr::Op(
            Box::new(spanned(Expr::Var("x"))),
            Box::new(spanned(Expr::Var("+"))),
            Box::new(spanned(Expr::Var("y"))),
        ));

        assert_eval(expr, env, Value::Int(42 + 3));
    }

    #[test]
    fn test_op_ident() {
        let env = vec![
            ("x", Value::Int(42)),
            ("y", Value::Int(3)),
            builtin::env()
                .into_iter()
                .find(|(k, _)| *k == "+")
                .map(|(_, v)| ("add", v))
                .unwrap(),
        ];

        // x add y
        let expr = spanned(Expr::Op(
            Box::new(spanned(Expr::Var("x"))),
            Box::new(spanned(Expr::Var("add"))),
            Box::new(spanned(Expr::Var("y"))),
        ));

        assert_eval(expr, env, Value::Int(42 + 3));
    }

    #[test]
    fn test_let_fn() {
        let env = vec![("x", Value::Int(42)), ("y", Value::Float(3.14))];

        // let id = fn(y) { y } id
        let expr = spanned(Expr::Let(
            None,
            "id",
            Box::new(spanned(Expr::Fn(
                spanned(vec!["y"]),
                Box::new(spanned(Expr::Var("y"))),
            ))),
            Box::new(spanned(Expr::Var("id"))),
        ));

        assert_eval(
            expr,
            env.clone(),
            Value::Closure(
                Some("id"),
                vec!["y"],
                Box::new(spanned(Expr::Var("y"))),
                Rc::new(env),
            ),
        );
    }

    #[test]
    fn test_let() {
        let env = vec![("x", Value::Int(42)), ("y", Value::Float(3.14))];

        // let z = x z
        let expr = spanned(Expr::Let(
            None,
            "z",
            Box::new(spanned(Expr::Var("x"))),
            Box::new(spanned(Expr::Var("z"))),
        ));

        assert_eval(expr, env, Value::Int(42));
    }

    #[test]
    fn test_builtin() {
        let mut env = builtin::env();
        env.extend([("x", Value::Int(42)), ("y", Value::Int(3))]);

        // +(x, y)()
        let expr = spanned(Expr::Call(
            Box::new(spanned(Expr::Call(
                Box::new(spanned(Expr::Var("+"))),
                spanned(vec![spanned(Expr::Var("x")), spanned(Expr::Var("y"))]),
            ))),
            spanned(vec![]),
        ));

        assert_eval(expr, env, Value::Int(45));
    }

    #[test]
    fn test_returned_closure_retains_parameter() {
        let make = function(vec!["x"], function(vec![], var("x")));
        let expr = binding(
            "make",
            make,
            binding(
                "f",
                call(call(var("make"), vec![int(42)]), vec![]),
                call(call(var("f"), vec![]), vec![]),
            ),
        );
        assert_eval(expr, vec![], Value::Int(42));
    }

    #[test]
    fn test_block_closure_retains_shadowed_binding() {
        let expr = binding(
            "x",
            int(0),
            binding(
                "f",
                binding("x", int(42), function(vec![], var("x"))),
                binding("padding", int(99), call(call(var("f"), vec![]), vec![])),
            ),
        );
        assert_eval(expr, vec![], Value::Int(42));
    }

    #[test]
    fn test_separate_calls_retain_separate_environments() {
        let make = function(vec!["x"], function(vec![], var("x")));
        let expr = binding(
            "make",
            make,
            binding(
                "first",
                call(call(var("make"), vec![int(20)]), vec![]),
                binding(
                    "second",
                    call(call(var("make"), vec![int(22)]), vec![]),
                    op(
                        call(call(var("first"), vec![]), vec![]),
                        "+",
                        call(call(var("second"), vec![]), vec![]),
                    ),
                ),
            ),
        );
        assert_eval(expr, builtin::env(), Value::Int(42));
    }

    #[test]
    fn test_escaped_struct_retains_parameter() {
        let make = function(
            vec!["x"],
            binding("get", function(vec![], var("x")), call(var("get"), vec![])),
        );
        let expr = binding(
            "make",
            make,
            binding(
                "value",
                call(call(var("make"), vec![int(42)]), vec![]),
                call(var("value"), vec![]),
            ),
        );
        assert_eval(expr, vec![], Value::Int(42));
    }

    #[test]
    fn test_block_struct_retains_shadowed_binding() {
        let value = binding(
            "x",
            int(42),
            binding("get", function(vec![], var("x")), call(var("get"), vec![])),
        );
        let expr = binding(
            "x",
            int(0),
            binding(
                "value",
                value,
                binding("padding", int(99), call(var("value"), vec![])),
            ),
        );
        assert_eval(expr, vec![], Value::Int(42));
    }

    #[test]
    fn test_caller_bindings_do_not_replace_lexical_bindings() {
        let closure = Value::Closure(
            Some("f"),
            vec![],
            Box::new(var("x")),
            Rc::new(vec![("x", Value::Int(42))]),
        );
        let expr = call(call(var("f"), vec![]), vec![]);
        assert_eval(
            expr,
            vec![("f", closure), ("x", Value::Int(99))],
            Value::Int(42),
        );
    }

    #[test]
    fn test_parameters_shadow_captured_bindings() {
        let closure = Value::Closure(
            Some("make"),
            vec!["x"],
            Box::new(function(vec![], var("x"))),
            Rc::new(vec![("x", Value::Int(99))]),
        );
        let expr = binding(
            "f",
            call(call(var("make"), vec![int(42)]), vec![]),
            call(call(var("f"), vec![]), vec![]),
        );
        assert_eval(expr, vec![("make", closure)], Value::Int(42));
    }

    #[test]
    fn test_returned_operator_retains_lexical_environment() {
        let expr = binding(
            "op",
            binding(
                "x",
                int(40),
                function(
                    vec!["a", "b"],
                    op(var("x"), "+", op(var("a"), "+", var("b"))),
                ),
            ),
            op(int(1), "op", int(1)),
        );
        assert_eval(expr, builtin::env(), Value::Int(42));
    }

    #[test]
    fn test_returned_closure_retains_pattern_bindings() {
        let value = struct_value("wrap", vec![("x", Value::Int(42))]);
        let expr = binding(
            "f",
            case(
                spanned(Expr::Lit(value)),
                vec![(
                    struct_pattern("wrap", vec![spanned(Patt::Var("x"))]),
                    function(vec![], var("x")),
                )],
            ),
            call(call(var("f"), vec![]), vec![]),
        );
        assert_eval(expr, vec![], Value::Int(42));
    }

    fn factorial<'src>() -> Spanned<Expr<'src>> {
        function(
            vec!["n"],
            case(
                var("n"),
                vec![
                    (spanned(Patt::Lit(Value::Int(0))), int(1)),
                    (
                        spanned(Patt::Var("_")),
                        op(
                            var("n"),
                            "*",
                            call(call(var("fact"), vec![op(var("n"), "-", int(1))]), vec![]),
                        ),
                    ),
                ],
            ),
        )
    }

    #[test]
    fn test_recursive_function_retains_escaped_environment() {
        let sum = function(
            vec!["n"],
            case(
                var("n"),
                vec![
                    (spanned(Patt::Lit(Value::Int(0))), var("base")),
                    (
                        spanned(Patt::Var("_")),
                        op(
                            var("n"),
                            "+",
                            call(call(var("sum"), vec![op(var("n"), "-", int(1))]), vec![]),
                        ),
                    ),
                ],
            ),
        );
        let make = function(vec!["base"], binding("sum", sum, var("sum")));
        let expr = binding(
            "make",
            make,
            binding(
                "sum",
                call(call(var("make"), vec![int(27)]), vec![]),
                call(call(var("sum"), vec![int(5)]), vec![]),
            ),
        );
        assert_eval(expr, builtin::env(), Value::Int(42));
    }

    #[test]
    fn test_recursive_alias_keeps_original_recursive_binding() {
        let expr = binding(
            "fact",
            factorial(),
            binding(
                "alias",
                var("fact"),
                binding(
                    "fact",
                    function(vec!["n"], int(0)),
                    call(call(var("alias"), vec![int(5)]), vec![]),
                ),
            ),
        );
        assert_eval(expr, builtin::env(), Value::Int(120));
    }

    #[test]
    fn test_escaped_struct_retains_recursive_binding() {
        let expr = binding(
            "value",
            binding("fact", factorial(), call(var("fact"), vec![int(5)])),
            call(var("value"), vec![]),
        );
        assert_eval(expr, builtin::env(), Value::Int(120));
    }

    #[test]
    fn test_parameters_shadow_recursive_binding() {
        let expr = binding(
            "f",
            function(vec!["f"], var("f")),
            call(call(var("f"), vec![int(42)]), vec![]),
        );
        assert_eval(expr, vec![], Value::Int(42));
    }

    #[test]
    fn test_builtin_alias_evaluates_arguments_in_caller_environment() {
        let expr = binding(
            "add",
            var("+"),
            binding(
                "x",
                int(42),
                call(call(var("add"), vec![var("x"), int(0)]), vec![]),
            ),
        );
        assert_eval(expr, builtin::env(), Value::Int(42));
    }

    #[test]
    fn test_caller_cannot_supply_missing_lexical_binding() {
        let closure = Value::Closure(
            Some("f"),
            vec![],
            Box::new((Expr::Var("x"), (12..13).into())),
            Default::default(),
        );
        let expr = call(call(var("f"), vec![]), vec![]);
        assert_eq!(
            eval(&expr, vec![("f", closure), ("x", Value::Int(42))]),
            Err(Error {
                msg: "Variable `x` not found".into(),
                span: (12..13).into()
            })
        );
    }

    #[test]
    fn test_struct_pattern_requires_exact_field_arity() {
        for (name, fields, patterns) in [
            (
                "pair",
                vec![("x", Value::Int(42)), ("y", Value::Int(7))],
                vec![vec![spanned(Patt::Var("a"))]],
            ),
            (
                "box",
                vec![("x", Value::Int(42))],
                vec![
                    vec![],
                    vec![spanned(Patt::Var("a")), spanned(Patt::Var("b"))],
                ],
            ),
            ("empty", vec![], vec![vec![spanned(Patt::Var("a"))]]),
        ] {
            let value = struct_value(name, fields);
            for fields in patterns {
                let pattern = struct_pattern(name, fields);
                assert_eq!(match_patt(&pattern.0, &value), None);
                let expr = case(
                    spanned(Expr::Lit(value.clone())),
                    vec![(pattern, int(1)), (spanned(Patt::Var("_")), int(0))],
                );
                assert_eval(expr, vec![], Value::Int(0));
            }
        }
    }

    #[test]
    fn test_struct_pattern_matches_zero_fields() {
        let value = struct_value("empty", vec![]);
        let pattern = struct_pattern("empty", vec![]);
        assert_eq!(match_patt(&pattern.0, &value), Some(vec![]));
        assert_eval(
            case(spanned(Expr::Lit(value)), vec![(pattern, int(1))]),
            vec![],
            Value::Int(1),
        );
    }

    #[test]
    fn test_struct_pattern_matches_exact_arity_and_binds_fields() {
        let value = struct_value("pair", vec![("x", Value::Int(42)), ("y", Value::Int(7))]);
        let pattern = struct_pattern(
            "pair",
            vec![spanned(Patt::Var("a")), spanned(Patt::Var("b"))],
        );
        assert_eq!(
            match_patt(&pattern.0, &value),
            Some(vec![("a", Value::Int(42)), ("b", Value::Int(7))])
        );
        assert_eval(
            case(
                spanned(Expr::Lit(value)),
                vec![(pattern, op(var("a"), "-", var("b")))],
            ),
            builtin::env(),
            Value::Int(35),
        );
    }

    #[test]
    fn test_struct_pattern_matches_nested_fields_and_binds_values() {
        let value = struct_value(
            "pair",
            vec![
                ("x", Value::Int(7)),
                ("y", struct_value("box", vec![("x", Value::Int(42))])),
            ],
        );
        let pattern = struct_pattern(
            "pair",
            vec![
                spanned(Patt::Var("a")),
                struct_pattern("box", vec![spanned(Patt::Var("b"))]),
            ],
        );
        assert_eq!(
            match_patt(&pattern.0, &value),
            Some(vec![("a", Value::Int(7)), ("b", Value::Int(42))])
        );
        assert_eval(
            case(
                spanned(Expr::Lit(value)),
                vec![(pattern, op(var("a"), "+", var("b")))],
            ),
            builtin::env(),
            Value::Int(49),
        );
    }

    #[test]
    fn test_struct_pattern_nested_arity_mismatch_preserves_fallback_bindings() {
        let value = struct_value(
            "outer",
            vec![
                ("tag", Value::Int(7)),
                ("item", struct_value("inner", vec![("x", Value::Int(42))])),
            ],
        );
        for fields in [
            vec![],
            vec![spanned(Patt::Var("a")), spanned(Patt::Var("b"))],
        ] {
            let pattern = struct_pattern(
                "outer",
                vec![spanned(Patt::Var("tag")), struct_pattern("inner", fields)],
            );
            assert_eq!(match_patt(&pattern.0, &value), None);
            let expr = case(
                spanned(Expr::Lit(value.clone())),
                vec![(pattern, int(1)), (spanned(Patt::Var("_")), var("tag"))],
            );
            assert_eval(expr, vec![("tag", Value::Int(99))], Value::Int(99));
        }
    }

    #[test]
    fn test_struct_pattern_falls_through_to_matching_arm() {
        let value = struct_value("box", vec![("x", Value::Int(42))]);
        let expr = case(
            spanned(Expr::Lit(value)),
            vec![
                (struct_pattern("box", vec![]), int(0)),
                (
                    struct_pattern(
                        "box",
                        vec![spanned(Patt::Var("a")), spanned(Patt::Var("b"))],
                    ),
                    int(1),
                ),
                (
                    struct_pattern("box", vec![spanned(Patt::Lit(Value::Int(41)))]),
                    int(2),
                ),
                (
                    struct_pattern("box", vec![spanned(Patt::Var("a"))]),
                    var("a"),
                ),
                (spanned(Patt::Var("_")), int(3)),
            ],
        );
        assert_eval(expr, vec![], Value::Int(42));
    }
}
