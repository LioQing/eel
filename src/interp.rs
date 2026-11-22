use std::rc::Rc;

use crate::declare::*;

fn insert_group<'src>(group: &Rc<ClosureGroup<'src>>, env: &mut Env<'src>) {
    for (member, def) in group.members.iter().enumerate() {
        if let Some(name) = def.rec_name {
            let closure = Value::Closure(Closure {
                name: Some(name),
                group: Rc::clone(group),
                member,
            });
            env.insert_mut(name, Rc::new(closure));
        }
    }
}

fn closure_env<'src>(closure: &Closure<'src>) -> Env<'src> {
    // The group owns definitions and the pre-binding snapshot, never its own
    // closures. Recreate bindings with their original names, even through aliases.
    let mut env = closure.group.env.clone();
    insert_group(&closure.group, &mut env);
    env
}

fn extend_env<'src>(
    env: &mut Env<'src>,
    bindings: impl IntoIterator<Item = (&'src str, Value<'src>)>,
) {
    for (name, value) in bindings {
        env.insert_mut(name, Rc::new(value));
    }
}

/// Evaluates the expression in the old environment and installs it on success.
pub fn bind<'src>(
    x: &'src str,
    expr: &Spanned<Expr<'src>>,
    env: &mut Env<'src>,
) -> Result<(), Error> {
    let v = match &expr.0 {
        Expr::Fn(params, body) => Value::Closure(Closure {
            name: Some(x),
            group: Rc::new(ClosureGroup {
                env: env.clone(),
                members: Box::new([ClosureDef {
                    rec_name: Some(x),
                    params: params.0.clone(),
                    body: Rc::clone(body),
                }]),
            }),
            member: 0,
        }),
        _ => {
            let mut v = eval(expr, env.clone())?;
            // An alias changes the display and struct name, not the scope.
            if let Value::Closure(closure) = &mut v {
                closure.name = Some(x);
            }
            v
        }
    };
    env.insert_mut(x, Rc::new(v));
    Ok(())
}

pub fn bind_rec_group<'src>(members: &[RecFn<'src, Rc<Spanned<Expr<'src>>>>], env: &mut Env<'src>) {
    let group = Rc::new(ClosureGroup {
        env: env.clone(),
        members: members
            .iter()
            .map(|member| ClosureDef {
                rec_name: Some(member.name.0),
                params: member.params.0.clone(),
                body: Rc::clone(&member.body),
            })
            .collect(),
    });
    insert_group(&group, env);
}

pub fn eval<'src>(expr: &Spanned<Expr<'src>>, env: Env<'src>) -> Result<Value<'src>, Error> {
    Ok(match &expr.0 {
        Expr::Err => unreachable!(), // Valid AST does not contain Expr::Err
        Expr::Lit(v) => v.clone(),
        Expr::Var(x) => env
            .get(*x)
            .map(|v| v.as_ref().clone())
            .ok_or_else(|| Error {
                msg: format!("Variable `{x}` not found"),
                span: expr.1.clone(),
            })?,
        Expr::Call(f, args) => match eval(f, env.clone())? {
            Value::Closure(closure) => {
                let name = closure.name;
                let ClosureDef { params, body, .. } = closure.definition();
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

                let body = crate::builtin::body_at_call_site(Rc::clone(body), f.1);

                let struct_args: StructArgs<'src> = params
                    .iter()
                    .zip(args.0.iter())
                    .map(|(param, arg)| Ok((*param, Rc::new(eval(arg, env.clone())?))))
                    .collect::<Result<_, _>>()?;

                let mut applied_env = closure_env(&closure);
                for (param, value) in struct_args.iter() {
                    applied_env.insert_mut(*param, Rc::clone(value));
                }
                Value::Struct(name, body, struct_args, applied_env)
            }
            Value::Struct(name, body, _, applied_env) => {
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

                eval(&body, applied_env)?
            }
            f => {
                return Err(Error {
                    msg: format!("Expected a function or struct, but got {f}"),
                    span: expr.1.clone(),
                });
            }
        },
        Expr::Fn(params, body) => Value::Closure(Closure {
            name: None,
            group: Rc::new(ClosureGroup {
                env,
                members: Box::new([ClosureDef {
                    rec_name: None,
                    params: params.0.clone(),
                    body: Rc::clone(body),
                }]),
            }),
            member: 0,
        }),
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
            extend_env(&mut new_env, patt_env);

            eval(body, new_env)?
        }
        Expr::Op(e1, op, e2) => match eval(op, env.clone())? {
            Value::Closure(closure) => {
                let name = closure.name;
                let ClosureDef { params, body, .. } = closure.definition();
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

                let body = crate::builtin::body_at_call_site(Rc::clone(body), op.1);

                let arg1 = eval(e1, env.clone())?;
                let arg2 = eval(e2, env.clone())?;

                let mut new_env = closure_env(&closure);
                extend_env(&mut new_env, [(params[0], arg1), (params[1], arg2)]);

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
            let mut new_env = env;
            bind(x, e, &mut new_env)?;
            eval(body, new_env)?
        }
        Expr::LetRec(members, body) => {
            let mut new_env = env;
            bind_rec_group(members, &mut new_env);
            eval(body, new_env)?
        }
        Expr::Builtin(f) => f(&expr, &env)?,
    })
}

pub fn match_patt<'src>(patt: &Patt<'src>, value: &Value<'src>) -> Option<Bindings<'src>> {
    match (patt, value) {
        (Patt::Lit(p), v) if p == v => Some(vec![]),
        (Patt::Var(x), _) => Some(vec![(x, value.clone())]),
        (Patt::Struct(x, ps), Value::Struct(y, _, args, _)) if Some(*x) == *y => {
            if ps.0.len() != args.len() {
                return None;
            }

            let mut env = vec![];
            for ((p, _), (_, v)) in ps.0.iter().zip(args.iter()) {
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

    fn env_from<'src>(bindings: impl IntoIterator<Item = (&'src str, Value<'src>)>) -> Env<'src> {
        let mut env = Env::new();
        extend_env(&mut env, bindings);
        env
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
        spanned(Expr::Fn(spanned(params), Rc::new(body)))
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

    fn struct_value<'src>(name: &'src str, args: Bindings<'src>) -> Value<'src> {
        let args: StructArgs<'src> = args
            .into_iter()
            .map(|(name, v)| (name, Rc::new(v)))
            .collect();
        let env = args.iter().cloned().collect();
        Value::Struct(
            Some(name),
            Rc::new(spanned(Expr::Lit(Value::Unit))),
            args,
            env,
        )
    }

    fn struct_pattern<'src>(
        name: &'src str,
        args: Vec<Spanned<Patt<'src>>>,
    ) -> Spanned<Patt<'src>> {
        spanned(Patt::Struct(name, spanned(args)))
    }

    #[test]
    fn test_lit() {
        let env = env_from([("x", Value::Int(42)), ("y", Value::Float(3.14))]);

        // 42
        let expr = spanned(Expr::Lit(Value::Int(43)));

        assert_eval(expr, env, Value::Int(43));
    }

    #[test]
    fn test_var() {
        let env = env_from([("x", Value::Int(42)), ("y", Value::Float(3.14))]);

        // x
        let expr = spanned(Expr::Var("x"));

        assert_eval(expr, env, Value::Int(42));
    }

    #[test]
    fn test_bind_preserves_previous_binding_when_expression_fails() {
        let mut env = env_from([("x", Value::Int(42))]);

        assert!(bind("x", &var("missing"), &mut env).is_err());
        assert_eval(var("x"), env, Value::Int(42));
    }

    #[test]
    fn test_call() {
        let env = env_from([
            ("x", Value::Int(42)),
            ("y", Value::Float(3.14)),
            (
                "id",
                Value::Closure(Closure {
                    name: Some("id"),
                    group: Rc::new(ClosureGroup {
                        env: Default::default(),
                        members: Box::new([ClosureDef {
                            rec_name: None,
                            params: vec!["y"],
                            body: Rc::new(spanned(Expr::Var("y"))),
                        }]),
                    }),
                    member: 0,
                }),
            ),
        ]);

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
        let body = Rc::new((Expr::Var("y"), (8..9).into()));
        let id = Value::Closure(Closure {
            name: Some("id"),
            group: Rc::new(ClosureGroup {
                env: Default::default(),
                members: Box::new([ClosureDef {
                    rec_name: Some("id"),
                    params: vec!["y"],
                    body: Rc::clone(&body),
                }]),
            }),
            member: 0,
        });
        let env = env_from([
            ("x", Value::Int(42)),
            ("y", Value::Float(3.14)),
            ("id", id.clone()),
        ]);

        // id(x)
        let expr = spanned(Expr::Call(
            Box::new(spanned(Expr::Var("id"))),
            spanned(vec![spanned(Expr::Var("x"))]),
        ));

        let value = eval(&expr, env).unwrap();
        assert_eq!(
            value,
            Value::Struct(
                Some("id"),
                Rc::clone(&body),
                Rc::from([("y", Rc::new(Value::Int(42)))]),
                env_from([("id", id), ("y", Value::Int(42))]),
            ),
        );
        let Value::Struct(_, applied_body, ..) = value.clone() else {
            panic!("expected a struct");
        };
        assert!(Rc::ptr_eq(&applied_body, &body));
    }

    #[test]
    fn test_call_struct() {
        let x_arg = Rc::new(Value::Int(43));
        let applied_env = env_from([("y", Value::Float(3.14))]).insert("x", Rc::clone(&x_arg));
        let env = env_from([
            ("x", Value::Int(42)),
            ("y", Value::Float(3.14)),
            (
                "x_struct",
                Value::Struct(
                    Some("x_struct"),
                    Rc::new(spanned(Expr::Var("x"))),
                    Rc::from([("x", x_arg)]),
                    applied_env,
                ),
            ),
        ]);

        // x_struct()
        let expr = spanned(Expr::Call(
            Box::new(spanned(Expr::Var("x_struct"))),
            spanned(vec![]),
        ));

        assert_eval(expr, env, Value::Int(43));
    }

    #[test]
    fn test_fn() {
        let env = env_from([("x", Value::Int(42)), ("y", Value::Float(3.14))]);

        // fn(y) { y }
        let expr = spanned(Expr::Fn(
            spanned(vec!["y"]),
            Rc::new(spanned(Expr::Var("y"))),
        ));

        assert_eval(
            expr,
            env.clone(),
            Value::Closure(Closure {
                name: None,
                group: Rc::new(ClosureGroup {
                    env,
                    members: Box::new([ClosureDef {
                        rec_name: None,
                        params: vec!["y"],
                        body: Rc::new(spanned(Expr::Var("y"))),
                    }]),
                }),
                member: 0,
            }),
        );
    }

    #[test]
    fn test_closures_share_snapshot_until_shadowing() {
        let env = env_from([("x", Value::Int(1)), ("y", Value::Int(2))]);
        let function = function(vec![], var("x"));
        let Value::Closure(first) = eval(&function, env.clone()).unwrap() else {
            panic!("expected a closure");
        };
        let Value::Closure(second) = eval(&function, env.clone()).unwrap() else {
            panic!("expected a closure");
        };
        assert!(Rc::ptr_eq(
            &first.definition().body,
            &second.definition().body
        ));
        let first = &first.group.env;
        assert!(first.ptr_eq(&env));
        assert!(second.group.env.ptr_eq(first));

        let shadow = binding("x", int(42), function);
        let Value::Closure(shadowed) = eval(&shadow, env.clone()).unwrap() else {
            panic!("expected a closure");
        };
        let shadowed = &shadowed.group.env;
        assert!(!shadowed.ptr_eq(first));
        assert_eq!(first.get("x").unwrap().as_ref(), &Value::Int(1));
        assert_eq!(shadowed.get("x").unwrap().as_ref(), &Value::Int(42));
        assert!(Rc::ptr_eq(
            first.get("y").unwrap(),
            shadowed.get("y").unwrap()
        ));
    }

    #[test]
    fn test_struct_reuses_applied_snapshot_and_shares_args() {
        let make = function(vec!["x"], function(vec![], var("x")));
        let expr = call(make, vec![int(42)]);
        let value = eval(&expr, Env::new()).unwrap();
        let Value::Struct(_, _, args, applied_env) = &value else {
            panic!("expected a struct");
        };
        assert!(Rc::ptr_eq(&args[0].1, applied_env.get("x").unwrap()));

        let env = env_from([("value", value.clone())]);
        let force = call(var("value"), vec![]);
        for _ in 0..2 {
            let Value::Closure(closure) = eval(&force, env.clone()).unwrap() else {
                panic!("expected a closure");
            };
            assert!(closure.group.env.ptr_eq(applied_env));
        }
    }

    #[test]
    fn test_case() {
        let env = env_from([("x", Value::Int(42)), ("y", Value::Float(3.14))]);

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
        let mut env = builtin::env();
        extend_env(&mut env, [("x", Value::Int(42)), ("y", Value::Int(3))]);

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
        let mut env = env_from([("x", Value::Int(42)), ("y", Value::Int(3))]);
        env.insert_mut("add", builtin::env().get("+").unwrap().clone());

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
        let env = env_from([("x", Value::Int(42)), ("y", Value::Float(3.14))]);

        // let id = fn(y) { y } id
        let expr = spanned(Expr::Let(
            None,
            "id",
            Box::new(spanned(Expr::Fn(
                spanned(vec!["y"]),
                Rc::new(spanned(Expr::Var("y"))),
            ))),
            Box::new(spanned(Expr::Var("id"))),
        ));

        assert_eval(
            expr,
            env.clone(),
            Value::Closure(Closure {
                name: Some("id"),
                group: Rc::new(ClosureGroup {
                    env,
                    members: Box::new([ClosureDef {
                        rec_name: Some("id"),
                        params: vec!["y"],
                        body: Rc::new(spanned(Expr::Var("y"))),
                    }]),
                }),
                member: 0,
            }),
        );
    }

    #[test]
    fn test_let() {
        let env = env_from([("x", Value::Int(42)), ("y", Value::Float(3.14))]);

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
        extend_env(&mut env, [("x", Value::Int(42)), ("y", Value::Int(3))]);

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
        assert_eval(expr, Env::new(), Value::Int(42));
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
        assert_eval(expr, Env::new(), Value::Int(42));
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
        assert_eval(expr, Env::new(), Value::Int(42));
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
        assert_eval(expr, Env::new(), Value::Int(42));
    }

    #[test]
    fn test_caller_bindings_do_not_replace_lexical_bindings() {
        let closure = Value::Closure(Closure {
            name: Some("f"),
            group: Rc::new(ClosureGroup {
                env: env_from([("x", Value::Int(42))]),
                members: Box::new([ClosureDef {
                    rec_name: None,
                    params: vec![],
                    body: Rc::new(var("x")),
                }]),
            }),
            member: 0,
        });
        let expr = call(call(var("f"), vec![]), vec![]);
        assert_eval(
            expr,
            env_from([("f", closure), ("x", Value::Int(99))]),
            Value::Int(42),
        );
    }

    #[test]
    fn test_parameters_shadow_captured_bindings() {
        let closure = Value::Closure(Closure {
            name: Some("make"),
            group: Rc::new(ClosureGroup {
                env: env_from([("x", Value::Int(99))]),
                members: Box::new([ClosureDef {
                    rec_name: None,
                    params: vec!["x"],
                    body: Rc::new(function(vec![], var("x"))),
                }]),
            }),
            member: 0,
        });
        let expr = binding(
            "f",
            call(call(var("make"), vec![int(42)]), vec![]),
            call(call(var("f"), vec![]), vec![]),
        );
        assert_eval(expr, env_from([("make", closure)]), Value::Int(42));
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
        assert_eval(expr, Env::new(), Value::Int(42));
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
    fn test_recursive_group_outlives_scope_but_drops_with_last_reference() {
        let members = [
            RecFn {
                name: spanned("first"),
                fixity: None,
                params: spanned(vec![]),
                body: Rc::new(call(call(var("second"), vec![]), vec![])),
            },
            RecFn {
                name: spanned("second"),
                fixity: None,
                params: spanned(vec![]),
                body: Rc::new(var("base")),
            },
        ];
        let mut env = env_from([("base", Value::Int(42))]);
        bind_rec_group(&members, &mut env);
        let Value::Closure(first) = env.get("first").unwrap().as_ref() else {
            panic!("expected a group member");
        };
        let Value::Closure(second) = env.get("second").unwrap().as_ref() else {
            panic!("expected a group member");
        };
        assert!(Rc::ptr_eq(&first.group, &second.group));
        assert!(Rc::ptr_eq(&first.definition().body, &members[0].body));
        assert!(first.group.env.get("first").is_none());
        assert!(first.group.env.get("second").is_none());
        let group = Rc::downgrade(&first.group);

        let deferred = eval(&call(var("first"), vec![]), env).unwrap();
        assert!(group.upgrade().is_some());
        assert_eval(
            call(var("deferred"), vec![]),
            env_from([("deferred", deferred)]),
            Value::Int(42),
        );
        assert!(group.upgrade().is_none());
    }

    #[test]
    fn test_parameters_shadow_recursive_binding() {
        let expr = binding(
            "f",
            function(vec!["f"], var("f")),
            call(call(var("f"), vec![int(42)]), vec![]),
        );
        assert_eval(expr, Env::new(), Value::Int(42));
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
        let closure = Value::Closure(Closure {
            name: Some("f"),
            group: Rc::new(ClosureGroup {
                env: Default::default(),
                members: Box::new([ClosureDef {
                    rec_name: None,
                    params: vec![],
                    body: Rc::new((Expr::Var("x"), (12..13).into())),
                }]),
            }),
            member: 0,
        });
        let expr = call(call(var("f"), vec![]), vec![]);
        assert_eq!(
            eval(&expr, env_from([("f", closure), ("x", Value::Int(42))])),
            Err(Error {
                msg: "Variable `x` not found".into(),
                span: (12..13).into()
            })
        );
    }

    #[test]
    fn test_struct_pattern_requires_exact_arg_arity() {
        for (name, args, patterns) in [
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
            let value = struct_value(name, args);
            for arg_patterns in patterns {
                let pattern = struct_pattern(name, arg_patterns);
                assert_eq!(match_patt(&pattern.0, &value), None);
                let expr = case(
                    spanned(Expr::Lit(value.clone())),
                    vec![(pattern, int(1)), (spanned(Patt::Var("_")), int(0))],
                );
                assert_eval(expr, Env::new(), Value::Int(0));
            }
        }
    }

    #[test]
    fn test_struct_pattern_matches_zero_args() {
        let value = struct_value("empty", vec![]);
        let pattern = struct_pattern("empty", vec![]);
        assert_eq!(match_patt(&pattern.0, &value), Some(vec![]));
        assert_eval(
            case(spanned(Expr::Lit(value)), vec![(pattern, int(1))]),
            Env::new(),
            Value::Int(1),
        );
    }

    #[test]
    fn test_struct_pattern_matches_exact_arity_and_binds_args() {
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
    fn test_struct_pattern_matches_nested_args_and_binds_values() {
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
        for arg_patterns in [
            vec![],
            vec![spanned(Patt::Var("a")), spanned(Patt::Var("b"))],
        ] {
            let pattern = struct_pattern(
                "outer",
                vec![
                    spanned(Patt::Var("tag")),
                    struct_pattern("inner", arg_patterns),
                ],
            );
            assert_eq!(match_patt(&pattern.0, &value), None);
            let expr = case(
                spanned(Expr::Lit(value.clone())),
                vec![(pattern, int(1)), (spanned(Patt::Var("_")), var("tag"))],
            );
            assert_eval(expr, env_from([("tag", Value::Int(99))]), Value::Int(99));
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
        assert_eval(expr, Env::new(), Value::Int(42));
    }
}
