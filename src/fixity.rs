use std::rc::Rc;

use crate::declare::*;

fn op_fixity(op: &Spanned<Expr<'_>>, specs: &[OpSpec<'_>]) -> Fixity {
    if let Expr::Var(name) = &op.value {
        if let Some(spec) = specs.iter().rev().find(|spec| spec.name == *name) {
            return spec.fixity;
        }

        let precedence = match *name {
            "==" | "!=" => 1,
            "<" | "<=" | ">" | ">=" => 2,
            "+" | "-" => 3,
            "*" | "/" | "%" => 4,
            _ => 0,
        };
        Fixity {
            assoc: OpAssoc::Left,
            precedence,
        }
    } else {
        Fixity::default()
    }
}

fn reduce_op<'src>(operands: &mut Vec<Spanned<Expr<'src>>>, op: Spanned<Expr<'src>>) {
    let rhs = operands.pop().unwrap();
    let lhs = operands.pop().unwrap();
    let span = lhs.span.start..rhs.span.end;
    operands.push(Spanned::new(
        Expr::Op(Op {
            lhs: Box::new(lhs),
            op: Box::new(op),
            rhs: Box::new(rhs),
        }),
        span,
    ));
}

fn shadow_patt<'src>(patt: &Patt<'src>, specs: &mut Vec<OpSpec<'src>>) {
    match patt {
        Patt::Var(name) => specs.push(OpSpec {
            name,
            fixity: Fixity::default(),
        }),
        Patt::Struct(StructPatt { args, .. }) => {
            for patt in &args.value {
                shadow_patt(&patt.value, specs);
            }
        }
        Patt::Lit(_) => {}
    }
}

fn resolve_fn_body<'src>(
    params: &[&'src str],
    body: Spanned<SyntaxExpr<'src>>,
    specs: &[OpSpec<'src>],
) -> Rc<Spanned<Expr<'src>>> {
    let mut scoped_specs = specs.to_vec();
    scoped_specs.extend(params.iter().map(|name| OpSpec {
        name,
        fixity: Fixity::default(),
    }));
    Rc::new(resolve(body, &scoped_specs))
}

/// Resolves the expression and returns the fixities in scope after the binding.
pub fn resolve_let<'src>(
    name: &'src str,
    fixity: Option<Fixity>,
    expr: Spanned<SyntaxExpr<'src>>,
    specs: &[OpSpec<'src>],
) -> (Spanned<Expr<'src>>, Vec<OpSpec<'src>>) {
    let mut scoped_specs = specs.to_vec();
    scoped_specs.push(OpSpec {
        name,
        fixity: fixity.unwrap_or_default(),
    });

    // Only a function definition sees the new binding from its expression.
    let expr_specs = if matches!(&expr.value, SyntaxExpr::Fn(_)) {
        scoped_specs.as_slice()
    } else {
        specs
    };

    (resolve(expr, expr_specs), scoped_specs)
}

/// Resolves all members and returns the fixities in scope after the group.
pub fn resolve_let_rec<'src>(
    members: Vec<RecFn<'src, Spanned<SyntaxExpr<'src>>>>,
    specs: &[OpSpec<'src>],
) -> (Vec<RecFn<'src, Rc<Spanned<Expr<'src>>>>>, Vec<OpSpec<'src>>) {
    let mut scoped_specs = specs.to_vec();
    scoped_specs.extend(members.iter().map(|member| OpSpec {
        name: member.name.value,
        fixity: member.fixity.unwrap_or_default(),
    }));

    let members = members
        .into_iter()
        .map(|member| RecFn {
            name: member.name,
            fixity: member.fixity,
            body: resolve_fn_body(&member.params.value, member.body, &scoped_specs),
            params: member.params,
        })
        .collect();

    (members, scoped_specs)
}

pub fn resolve<'src>(
    Spanned { value: expr, span }: Spanned<SyntaxExpr<'src>>,
    specs: &[OpSpec<'src>],
) -> Spanned<Expr<'src>> {
    let expr = match expr {
        SyntaxExpr::Atom(expr) => expr,
        SyntaxExpr::Call(Call { callee, args }) => Expr::Call(Call {
            callee: Box::new(resolve(*callee, specs)),
            args: args.map(|args| args.into_iter().map(|arg| resolve(arg, specs)).collect()),
        }),
        SyntaxExpr::Fn(FnExpr { params, body }) => {
            let body = resolve_fn_body(&params.value, *body, specs);
            Expr::Fn(FnExpr { params, body })
        }
        SyntaxExpr::Case(Case { scrutinee, arms }) => Expr::Case(Case {
            scrutinee: Box::new(resolve(*scrutinee, specs)),
            arms: arms.map(|arms| {
                arms.into_iter()
                    .map(|CaseArm { patt, body }| {
                        let mut scoped_specs = specs.to_vec();
                        shadow_patt(&patt.value, &mut scoped_specs);
                        CaseArm {
                            patt,
                            body: Box::new(resolve(*body, &scoped_specs)),
                        }
                    })
                    .collect()
            }),
        }),
        SyntaxExpr::Ops(Ops { first, rest }) => {
            let mut operands = vec![resolve(*first, specs)];
            let mut operators = Vec::new();
            for OpStep { op, rhs } in rest {
                let op = resolve(op, specs);
                let Fixity { assoc, precedence } = op_fixity(&op, specs);
                while operators.last().is_some_and(|prev| {
                    let prev_prec = op_fixity(prev, specs).precedence;
                    prev_prec > precedence || (prev_prec == precedence && assoc == OpAssoc::Left)
                }) {
                    reduce_op(&mut operands, operators.pop().unwrap());
                }
                operators.push(op);
                operands.push(resolve(rhs, specs));
            }
            while let Some(op) = operators.pop() {
                reduce_op(&mut operands, op);
            }
            operands.pop().unwrap().value
        }
        SyntaxExpr::Let(Let {
            fixity,
            name,
            init,
            body,
        }) => {
            let (init, scoped_specs) = resolve_let(name, fixity, *init, specs);
            Expr::Let(Let {
                fixity,
                name,
                init: Box::new(init),
                body: Box::new(resolve(*body, &scoped_specs)),
            })
        }
        SyntaxExpr::LetRec(LetRec { bindings, body }) => {
            let (bindings, scoped_specs) = resolve_let_rec(bindings, specs);
            Expr::LetRec(LetRec {
                bindings,
                body: Box::new(resolve(*body, &scoped_specs)),
            })
        }
    };
    Spanned::new(expr, span)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn spanned<T: Clone>(value: T) -> Spanned<T> {
        Spanned::new(value, 0..0)
    }

    fn int<'src>(value: i64) -> Spanned<Expr<'src>> {
        spanned(Expr::Lit(Value::Int(value)))
    }

    fn var(name: &str) -> Spanned<Expr<'_>> {
        spanned(Expr::Var(name))
    }

    fn op<'src>(
        left: Spanned<Expr<'src>>,
        name: &'src str,
        right: Spanned<Expr<'src>>,
    ) -> Spanned<Expr<'src>> {
        spanned(Expr::Op(Op {
            lhs: Box::new(left),
            op: Box::new(var(name)),
            rhs: Box::new(right),
        }))
    }

    fn declared(assoc: OpAssoc, precedence: i64) -> Option<Fixity> {
        Some(Fixity { assoc, precedence })
    }

    fn spec(name: &str, assoc: OpAssoc, precedence: i64) -> OpSpec<'_> {
        OpSpec {
            name,
            fixity: Fixity { assoc, precedence },
        }
    }

    fn binding<'src>(
        fixity: Option<Fixity>,
        name: &'src str,
        assign: Spanned<Expr<'src>>,
        body: Spanned<Expr<'src>>,
    ) -> Spanned<Expr<'src>> {
        spanned(Expr::Let(Let {
            fixity,
            name,
            init: Box::new(assign),
            body: Box::new(body),
        }))
    }

    fn function<'src>(params: Vec<&'src str>, body: Spanned<Expr<'src>>) -> Spanned<Expr<'src>> {
        spanned(Expr::Fn(FnExpr {
            params: spanned(params),
            body: Rc::new(body),
        }))
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

    fn case<'src>(
        value: Spanned<Expr<'src>>,
        arms: Vec<(Spanned<Patt<'src>>, Spanned<Expr<'src>>)>,
    ) -> Spanned<Expr<'src>> {
        spanned(Expr::Case(Case {
            scrutinee: Box::new(value),
            arms: spanned(
                arms.into_iter()
                    .map(|(patt, body)| CaseArm {
                        patt,
                        body: Box::new(body),
                    })
                    .collect(),
            ),
        }))
    }

    fn syntax_int<'src>(value: i64) -> Spanned<SyntaxExpr<'src>> {
        spanned(SyntaxExpr::Atom(Expr::Lit(Value::Int(value))))
    }

    fn syntax_var(name: &str) -> Spanned<SyntaxExpr<'_>> {
        spanned(SyntaxExpr::Atom(Expr::Var(name)))
    }

    fn syntax_ops<'src>(
        first: Spanned<SyntaxExpr<'src>>,
        rest: Vec<(&'src str, Spanned<SyntaxExpr<'src>>)>,
    ) -> Spanned<SyntaxExpr<'src>> {
        spanned(SyntaxExpr::Ops(Ops {
            first: Box::new(first),
            rest: rest
                .into_iter()
                .map(|(op, rhs)| OpStep {
                    op: syntax_var(op),
                    rhs,
                })
                .collect(),
        }))
    }

    fn syntax_binding<'src>(
        fixity: Option<Fixity>,
        name: &'src str,
        assign: Spanned<SyntaxExpr<'src>>,
        body: Spanned<SyntaxExpr<'src>>,
    ) -> Spanned<SyntaxExpr<'src>> {
        spanned(SyntaxExpr::Let(Let {
            fixity,
            name,
            init: Box::new(assign),
            body: Box::new(body),
        }))
    }

    fn syntax_function<'src>(
        params: Vec<&'src str>,
        body: Spanned<SyntaxExpr<'src>>,
    ) -> Spanned<SyntaxExpr<'src>> {
        spanned(SyntaxExpr::Fn(FnExpr {
            params: spanned(params),
            body: Box::new(body),
        }))
    }

    fn syntax_call<'src>(
        func: Spanned<SyntaxExpr<'src>>,
        args: Vec<Spanned<SyntaxExpr<'src>>>,
    ) -> Spanned<SyntaxExpr<'src>> {
        spanned(SyntaxExpr::Call(Call {
            callee: Box::new(func),
            args: spanned(args),
        }))
    }

    fn syntax_case<'src>(
        value: Spanned<SyntaxExpr<'src>>,
        arms: Vec<(Spanned<Patt<'src>>, Spanned<SyntaxExpr<'src>>)>,
    ) -> Spanned<SyntaxExpr<'src>> {
        spanned(SyntaxExpr::Case(Case {
            scrutinee: Box::new(value),
            arms: spanned(
                arms.into_iter()
                    .map(|(patt, body)| CaseArm {
                        patt,
                        body: Box::new(body),
                    })
                    .collect(),
            ),
        }))
    }

    fn assert_resolve<'src>(syntax: Spanned<SyntaxExpr<'src>>, expected: Spanned<Expr<'src>>) {
        assert_eq!(resolve(syntax, &[]), expected);
    }

    #[test]
    fn test_resolve_preserves_nested_call_and_case_spans() {
        use chumsky::prelude::*;

        let src = "case f(1) { x => x }";
        let tokens = crate::lex::lexer().parse(src).unwrap();
        let (parser, _) = crate::parse::expr_and_item_parsers();
        let syntax = parser
            .parse(
                tokens
                    .as_slice()
                    .map((src.len()..src.len()).into(), Spanned::as_parts),
            )
            .unwrap();
        let expr = resolve(syntax, &[]);
        assert_eq!(expr.span.into_range(), 0..20);
        let Expr::Case(Case { scrutinee, arms }) = expr.value else {
            panic!("Expected case expression");
        };
        assert_eq!(scrutinee.span.into_range(), 5..9);
        let Expr::Call(Call { callee, args }) = scrutinee.value else {
            panic!("Expected call expression");
        };
        assert_eq!(callee.span.into_range(), 5..6);
        assert_eq!(args.span.into_range(), 6..9);
        assert_eq!(args.value[0].span.into_range(), 7..8);
        assert_eq!(arms.span.into_range(), 10..20);
        assert_eq!(arms.value[0].patt.span.into_range(), 12..13);
        assert_eq!(arms.value[0].body.span.into_range(), 17..18);
    }

    fn syntax_int_ops<'src>(first: i64, rest: &[(&'src str, i64)]) -> Spanned<SyntaxExpr<'src>> {
        syntax_ops(
            syntax_int(first),
            rest.iter().map(|&(op, n)| (op, syntax_int(n))).collect(),
        )
    }

    fn sub_chain() -> Spanned<SyntaxExpr<'static>> {
        syntax_int_ops(10, &[("sub", 3), ("sub", 2)])
    }

    fn left_sub_chain() -> Spanned<Expr<'static>> {
        op(op(int(10), "sub", int(3)), "sub", int(2))
    }

    fn right_sub_chain() -> Spanned<Expr<'static>> {
        op(int(10), "sub", op(int(3), "sub", int(2)))
    }

    #[test]
    fn test_builtin_arithmetic_precedence_and_associativity() {
        for (first, rest, expected) in [
            (
                2,
                vec![("*", 3), ("+", 4)],
                op(op(int(2), "*", int(3)), "+", int(4)),
            ),
            (
                2,
                vec![("+", 3), ("*", 4)],
                op(int(2), "+", op(int(3), "*", int(4))),
            ),
            (
                20,
                vec![("-", 3), ("-", 2)],
                op(op(int(20), "-", int(3)), "-", int(2)),
            ),
            (
                24,
                vec![("/", 3), ("/", 2)],
                op(op(int(24), "/", int(3)), "/", int(2)),
            ),
            (
                20,
                vec![("/", 2), ("*", 3)],
                op(op(int(20), "/", int(2)), "*", int(3)),
            ),
            (
                20,
                vec![("%", 6), ("*", 2)],
                op(op(int(20), "%", int(6)), "*", int(2)),
            ),
            (
                20,
                vec![("-", 3), ("+", 2)],
                op(op(int(20), "-", int(3)), "+", int(2)),
            ),
            (
                2,
                vec![("+", 3), ("*", 4), ("-", 6), ("/", 2)],
                op(
                    op(int(2), "+", op(int(3), "*", int(4))),
                    "-",
                    op(int(6), "/", int(2)),
                ),
            ),
        ] {
            assert_resolve(syntax_int_ops(first, &rest), expected);
        }
    }

    #[test]
    fn test_builtin_comparison_precedence_and_associativity() {
        for equality in ["==", "!="] {
            for (first, rest, expected) in [
                (
                    2,
                    vec![("*", 3), ("+", 4), (equality, 10)],
                    op(op(op(int(2), "*", int(3)), "+", int(4)), equality, int(10)),
                ),
                (
                    10,
                    vec![(equality, 2), ("+", 3), ("*", 4)],
                    op(int(10), equality, op(int(2), "+", op(int(3), "*", int(4)))),
                ),
                (
                    1,
                    vec![("+", 2), (equality, 4), ("*", 2)],
                    op(op(int(1), "+", int(2)), equality, op(int(4), "*", int(2))),
                ),
                (
                    1,
                    vec![(equality, 2), ("<", 3)],
                    op(int(1), equality, op(int(2), "<", int(3))),
                ),
                (
                    1,
                    vec![("<", 2), (equality, 3), ("<", 4)],
                    op(op(int(1), "<", int(2)), equality, op(int(3), "<", int(4))),
                ),
                (
                    2,
                    vec![(equality, 2), (equality, 1)],
                    op(op(int(2), equality, int(2)), equality, int(1)),
                ),
            ] {
                assert_resolve(syntax_int_ops(first, &rest), expected);
            }
        }
        for (first, rest, expected) in [
            (
                1,
                vec![("+", 2), ("<", 4), ("*", 2)],
                op(op(int(1), "+", int(2)), "<", op(int(4), "*", int(2))),
            ),
            (
                4,
                vec![("<", 3), ("<", 2)],
                op(op(int(4), "<", int(3)), "<", int(2)),
            ),
            (
                2,
                vec![("==", 2), ("!=", 0)],
                op(op(int(2), "==", int(2)), "!=", int(0)),
            ),
            (
                1,
                vec![("!=", 2), ("==", 0)],
                op(op(int(1), "!=", int(2)), "==", int(0)),
            ),
            (
                2,
                vec![("*", 3), (">=", 4), ("+", 2)],
                op(op(int(2), "*", int(3)), ">=", op(int(4), "+", int(2))),
            ),
            (
                4,
                vec![("<", 3), ("!=", 2), ("+", 1)],
                op(op(int(4), "<", int(3)), "!=", op(int(2), "+", int(1))),
            ),
            (
                1,
                vec![("+", 2), ("!=", 3), ("<", 4)],
                op(op(int(1), "+", int(2)), "!=", op(int(3), "<", int(4))),
            ),
        ] {
            assert_resolve(syntax_int_ops(first, &rest), expected);
        }

        let syntax_float = |value| spanned(SyntaxExpr::Atom(Expr::Lit(Value::Float(value))));
        let float = |value| spanned(Expr::Lit(Value::Float(value)));
        assert_resolve(
            syntax_ops(
                syntax_float(1.0),
                vec![
                    ("+", syntax_float(2.0)),
                    ("!=", syntax_float(3.0)),
                    ("*", syntax_float(1.0)),
                ],
            ),
            op(
                op(float(1.0), "+", float(2.0)),
                "!=",
                op(float(3.0), "*", float(1.0)),
            ),
        );
    }

    #[test]
    fn test_declared_left_and_right_associativity() {
        for (assoc, prec) in [
            (OpAssoc::Left, 1),
            (OpAssoc::Right, 1),
            (OpAssoc::Right, i64::MAX),
            (OpAssoc::Left, i64::MIN),
        ] {
            let spec = declared(assoc, prec);
            let body = match assoc {
                OpAssoc::Left => left_sub_chain(),
                OpAssoc::Right => right_sub_chain(),
            };
            assert_resolve(
                syntax_binding(spec, "sub", syntax_var("-"), sub_chain()),
                binding(spec, "sub", var("-"), body),
            );
        }
    }

    #[test]
    fn test_custom_precedence_in_both_operator_orders() {
        for (add_prec, mul_prec, rest, expected) in [
            (
                5,
                6,
                vec![("mul", 3), ("add", 4)],
                op(op(int(2), "mul", int(3)), "add", int(4)),
            ),
            (
                5,
                6,
                vec![("add", 3), ("mul", 4)],
                op(int(2), "add", op(int(3), "mul", int(4))),
            ),
            (
                5,
                6,
                vec![("mul", 3), ("add", 4), ("mul", 5), ("add", 6)],
                op(
                    op(op(int(2), "mul", int(3)), "add", op(int(4), "mul", int(5))),
                    "add",
                    int(6),
                ),
            ),
            (
                6,
                5,
                vec![("mul", 3), ("add", 4)],
                op(int(2), "mul", op(int(3), "add", int(4))),
            ),
            (
                6,
                5,
                vec![("add", 3), ("mul", 4)],
                op(op(int(2), "add", int(3)), "mul", int(4)),
            ),
        ] {
            let add = declared(OpAssoc::Left, add_prec);
            let mul = declared(OpAssoc::Left, mul_prec);
            assert_resolve(
                syntax_binding(
                    add,
                    "add",
                    syntax_var("+"),
                    syntax_binding(mul, "mul", syntax_var("*"), syntax_int_ops(2, &rest)),
                ),
                binding(
                    add,
                    "add",
                    var("+"),
                    binding(mul, "mul", var("*"), expected),
                ),
            );
        }
    }

    #[test]
    fn test_custom_and_builtin_precedence_in_both_operator_orders() {
        for (prec, rest, expected) in [
            (
                0,
                vec![("*", 3), ("add", 4)],
                op(op(int(2), "*", int(3)), "add", int(4)),
            ),
            (
                0,
                vec![("add", 3), ("*", 4)],
                op(int(2), "add", op(int(3), "*", int(4))),
            ),
            (
                5,
                vec![("*", 3), ("add", 4)],
                op(int(2), "*", op(int(3), "add", int(4))),
            ),
            (
                5,
                vec![("add", 3), ("*", 4)],
                op(op(int(2), "add", int(3)), "*", int(4)),
            ),
        ] {
            let spec = declared(OpAssoc::Left, prec);
            assert_resolve(
                syntax_binding(spec, "add", syntax_var("+"), syntax_int_ops(2, &rest)),
                binding(spec, "add", var("+"), expected),
            );
        }
    }

    #[test]
    fn test_equal_precedence_uses_following_operator_associativity() {
        for sub_assoc in [OpAssoc::Left, OpAssoc::Right] {
            for add_assoc in [OpAssoc::Left, OpAssoc::Right] {
                let sub = declared(sub_assoc, 5);
                let add = declared(add_assoc, 5);
                let body = match add_assoc {
                    OpAssoc::Left => op(op(int(10), "sub", int(3)), "add", int(2)),
                    OpAssoc::Right => op(int(10), "sub", op(int(3), "add", int(2))),
                };
                assert_resolve(
                    syntax_binding(
                        sub,
                        "sub",
                        syntax_var("-"),
                        syntax_binding(
                            add,
                            "add",
                            syntax_var("+"),
                            syntax_int_ops(10, &[("sub", 3), ("add", 2)]),
                        ),
                    ),
                    binding(sub, "sub", var("-"), binding(add, "add", var("+"), body)),
                );
            }
        }
    }

    #[test]
    fn test_nested_operator_groups_resolve_independently() {
        for (syntax, expected) in [
            (
                syntax_ops(syntax_int(2), vec![("*", syntax_int_ops(3, &[("+", 4)]))]),
                op(int(2), "*", op(int(3), "+", int(4))),
            ),
            (
                syntax_ops(
                    syntax_int(2),
                    vec![
                        ("*", syntax_int_ops(3, &[("+", 4)])),
                        ("!=", syntax_int(10)),
                    ],
                ),
                op(op(int(2), "*", op(int(3), "+", int(4))), "!=", int(10)),
            ),
            (
                syntax_ops(syntax_int_ops(2, &[("*", 3)]), vec![("+", syntax_int(4))]),
                op(op(int(2), "*", int(3)), "+", int(4)),
            ),
            (
                syntax_ops(
                    syntax_int(100),
                    vec![(
                        "-",
                        syntax_ops(syntax_int(20), vec![("-", syntax_int_ops(5, &[("-", 2)]))]),
                    )],
                ),
                op(int(100), "-", op(int(20), "-", op(int(5), "-", int(2)))),
            ),
            (
                syntax_ops(syntax_int_ops(20, &[("-", 3)]), vec![("-", syntax_int(2))]),
                op(op(int(20), "-", int(3)), "-", int(2)),
            ),
            (
                syntax_ops(syntax_int(20), vec![("-", syntax_int_ops(3, &[("-", 2)]))]),
                op(int(20), "-", op(int(3), "-", int(2))),
            ),
            (
                syntax_ops(
                    syntax_int(2),
                    vec![
                        (
                            "*",
                            syntax_ops(
                                syntax_int_ops(3, &[("+", 4)]),
                                vec![(
                                    "*",
                                    syntax_ops(
                                        syntax_int(5),
                                        vec![("-", syntax_int_ops(3, &[("-", 1)]))],
                                    ),
                                )],
                            ),
                        ),
                        ("+", syntax_int(1)),
                    ],
                ),
                op(
                    op(
                        int(2),
                        "*",
                        op(
                            op(int(3), "+", int(4)),
                            "*",
                            op(int(5), "-", op(int(3), "-", int(1))),
                        ),
                    ),
                    "+",
                    int(1),
                ),
            ),
        ] {
            assert_resolve(syntax, expected);
        }

        let left = declared(OpAssoc::Left, 1);
        let right = declared(OpAssoc::Right, 1);
        assert_resolve(
            syntax_binding(
                left,
                "sub",
                syntax_var("-"),
                syntax_ops(
                    syntax_int(10),
                    vec![("sub", syntax_int_ops(3, &[("sub", 2)]))],
                ),
            ),
            binding(left, "sub", var("-"), right_sub_chain()),
        );
        assert_resolve(
            syntax_binding(
                right,
                "sub",
                syntax_var("-"),
                syntax_ops(
                    syntax_int_ops(10, &[("sub", 3)]),
                    vec![("sub", syntax_int(2))],
                ),
            ),
            binding(right, "sub", var("-"), left_sub_chain()),
        );
        let add = declared(OpAssoc::Left, 5);
        let mul = declared(OpAssoc::Left, 6);
        assert_resolve(
            syntax_binding(
                add,
                "add",
                syntax_var("+"),
                syntax_binding(
                    mul,
                    "mul",
                    syntax_var("*"),
                    syntax_ops(
                        syntax_int_ops(2, &[("add", 3)]),
                        vec![("mul", syntax_int(4))],
                    ),
                ),
            ),
            binding(
                add,
                "add",
                var("+"),
                binding(
                    mul,
                    "mul",
                    var("*"),
                    op(op(int(2), "add", int(3)), "mul", int(4)),
                ),
            ),
        );
    }

    #[test]
    fn test_calls_contain_independent_operator_chains() {
        let syntax_id = |arg| syntax_call(syntax_call(syntax_var("id"), vec![arg]), vec![]);
        let id = |arg| call(call(var("id"), vec![arg]), vec![]);
        assert_resolve(
            syntax_ops(
                syntax_int(2),
                vec![
                    ("*", syntax_id(syntax_int(3))),
                    ("+", syntax_id(syntax_int(4))),
                ],
            ),
            op(op(int(2), "*", id(int(3))), "+", id(int(4))),
        );
        assert_resolve(
            syntax_id(syntax_int_ops(2, &[("*", 3), ("+", 4)])),
            id(op(op(int(2), "*", int(3)), "+", int(4))),
        );
        assert_resolve(
            syntax_call(
                syntax_call(
                    syntax_var("!="),
                    vec![syntax_int_ops(2, &[("*", 3), ("+", 4)]), syntax_int(10)],
                ),
                vec![],
            ),
            call(
                call(
                    var("!="),
                    vec![op(op(int(2), "*", int(3)), "+", int(4)), int(10)],
                ),
                vec![],
            ),
        );
    }

    #[test]
    fn test_nested_declaration_does_not_leak_into_outer_chain() {
        let left = declared(OpAssoc::Left, 1);
        let right = declared(OpAssoc::Right, 1);
        assert_resolve(
            syntax_binding(
                left,
                "sub",
                syntax_var("-"),
                syntax_ops(
                    syntax_binding(right, "sub", syntax_var("-"), sub_chain()),
                    vec![("sub", syntax_int(3)), ("sub", syntax_int(2))],
                ),
            ),
            binding(
                left,
                "sub",
                var("-"),
                op(
                    op(
                        binding(right, "sub", var("-"), right_sub_chain()),
                        "sub",
                        int(3),
                    ),
                    "sub",
                    int(2),
                ),
            ),
        );
        let low = declared(OpAssoc::Left, 0);
        let high = declared(OpAssoc::Left, 5);
        let chain = syntax_int_ops(2, &[("add", 3), ("*", 4)]);
        assert_resolve(
            syntax_binding(
                low,
                "add",
                syntax_var("+"),
                syntax_binding(
                    None,
                    "nested",
                    syntax_binding(high, "add", syntax_var("+"), chain.clone()),
                    syntax_ops(syntax_var("nested"), vec![("+", chain)]),
                ),
            ),
            binding(
                low,
                "add",
                var("+"),
                binding(
                    None,
                    "nested",
                    binding(
                        high,
                        "add",
                        var("+"),
                        op(op(int(2), "add", int(3)), "*", int(4)),
                    ),
                    op(
                        var("nested"),
                        "+",
                        op(int(2), "add", op(int(3), "*", int(4))),
                    ),
                ),
            ),
        );
    }

    #[test]
    fn test_nested_declaration_does_not_leak_into_siblings() {
        let left = declared(OpAssoc::Left, 1);
        let right = declared(OpAssoc::Right, 1);
        let nested = syntax_binding(right, "sub", syntax_var("-"), sub_chain());
        let expected_nested = binding(right, "sub", var("-"), right_sub_chain());
        assert_resolve(
            syntax_binding(
                left,
                "sub",
                syntax_var("-"),
                syntax_call(syntax_var("pair"), vec![nested.clone(), sub_chain()]),
            ),
            binding(
                left,
                "sub",
                var("-"),
                call(var("pair"), vec![expected_nested.clone(), left_sub_chain()]),
            ),
        );
        let zero = spanned(Patt::Lit(Value::Int(0)));
        let wildcard = spanned(Patt::Var("_"));
        assert_resolve(
            syntax_binding(
                left,
                "sub",
                syntax_var("-"),
                syntax_case(
                    syntax_int(1),
                    vec![(zero.clone(), nested), (wildcard.clone(), sub_chain())],
                ),
            ),
            binding(
                left,
                "sub",
                var("-"),
                case(
                    int(1),
                    vec![(zero, expected_nested), (wildcard, left_sub_chain())],
                ),
            ),
        );
    }

    #[test]
    fn test_function_fixity_is_lexical_not_call_site_dependent() {
        let left = declared(OpAssoc::Left, 1);
        let right = declared(OpAssoc::Right, 1);
        let syntax_f = syntax_call(syntax_call(syntax_var("f"), vec![]), vec![]);
        let f = call(call(var("f"), vec![]), vec![]);
        assert_resolve(
            syntax_binding(
                left,
                "sub",
                syntax_var("-"),
                syntax_binding(
                    None,
                    "f",
                    syntax_function(vec![], sub_chain()),
                    syntax_binding(right, "sub", syntax_var("-"), syntax_f.clone()),
                ),
            ),
            binding(
                left,
                "sub",
                var("-"),
                binding(
                    None,
                    "f",
                    function(vec![], left_sub_chain()),
                    binding(right, "sub", var("-"), f.clone()),
                ),
            ),
        );
        assert_resolve(
            syntax_binding(
                left,
                "sub",
                syntax_var("-"),
                syntax_binding(
                    None,
                    "f",
                    syntax_function(
                        vec![],
                        syntax_binding(right, "sub", syntax_var("-"), sub_chain()),
                    ),
                    syntax_ops(syntax_f, vec![("*", syntax_int(100)), ("+", sub_chain())]),
                ),
            ),
            binding(
                left,
                "sub",
                var("-"),
                binding(
                    None,
                    "f",
                    function(vec![], binding(right, "sub", var("-"), right_sub_chain())),
                    op(op(f, "*", int(100)), "+", left_sub_chain()),
                ),
            ),
        );
    }

    #[test]
    fn test_plain_bindings_shadow_operator_fixity() {
        let right = declared(OpAssoc::Right, 5);
        assert_resolve(
            syntax_binding(None, "sub", syntax_var("-"), sub_chain()),
            binding(None, "sub", var("-"), left_sub_chain()),
        );
        for (syntax, expected) in [
            (
                syntax_binding(None, "sub", syntax_var("-"), sub_chain()),
                binding(None, "sub", var("-"), left_sub_chain()),
            ),
            (
                syntax_function(vec!["sub"], sub_chain()),
                function(vec!["sub"], left_sub_chain()),
            ),
            {
                let sub = spanned(Patt::Var("sub"));
                (
                    syntax_case(syntax_var("ordinary"), vec![(sub.clone(), sub_chain())]),
                    case(var("ordinary"), vec![(sub, left_sub_chain())]),
                )
            },
            {
                let wrap = spanned(Patt::Struct(StructPatt {
                    name: "wrap",
                    args: spanned(vec![spanned(Patt::Var("sub"))]),
                }));
                (
                    syntax_case(syntax_var("wrapped"), vec![(wrap.clone(), sub_chain())]),
                    case(var("wrapped"), vec![(wrap, left_sub_chain())]),
                )
            },
        ] {
            assert_resolve(
                syntax_binding(right, "sub", syntax_var("-"), syntax),
                binding(right, "sub", var("-"), expected),
            );
        }
    }

    #[test]
    fn test_expressions_use_previous_binding_fixity() {
        let left = declared(OpAssoc::Left, 1);
        let right = declared(OpAssoc::Right, 1);
        for (old, new, expected) in [
            (right, None, right_sub_chain()),
            (left, right, left_sub_chain()),
        ] {
            assert_resolve(
                syntax_binding(
                    old,
                    "sub",
                    syntax_var("-"),
                    syntax_binding(new, "sub", sub_chain(), syntax_var("sub")),
                ),
                binding(
                    old,
                    "sub",
                    var("-"),
                    binding(new, "sub", expected, var("sub")),
                ),
            );
        }
    }

    #[test]
    fn test_recursive_function_body_uses_its_own_declared_fixity() {
        for (assoc, expected) in [
            (OpAssoc::Left, left_sub_chain()),
            (OpAssoc::Right, right_sub_chain()),
        ] {
            let spec = declared(assoc, 1);
            assert_resolve(
                syntax_binding(
                    spec,
                    "sub",
                    syntax_function(vec!["a", "b"], sub_chain()),
                    syntax_var("sub"),
                ),
                binding(spec, "sub", function(vec!["a", "b"], expected), var("sub")),
            );
        }
    }

    #[test]
    fn test_recursive_group_resolves_forward_fixities_in_members_and_continuation() {
        let chain = syntax_int_ops(10, &[("sub", 3), ("sub", 2), ("join", 5), ("join", 1)]);
        let members = vec![
            RecFn {
                name: spanned("probe"),
                fixity: None,
                params: spanned(vec![]),
                body: chain.clone(),
            },
            RecFn {
                name: spanned("sub"),
                fixity: declared(OpAssoc::Right, 4),
                params: spanned(vec!["a", "b"]),
                body: syntax_var("a"),
            },
            RecFn {
                name: spanned("join"),
                fixity: declared(OpAssoc::Left, 2),
                params: spanned(vec!["a", "b"]),
                body: syntax_var("a"),
            },
        ];
        let syntax = spanned(SyntaxExpr::LetRec(LetRec {
            bindings: members,
            body: Box::new(chain),
        }));
        let expr = resolve(
            syntax,
            &[
                spec("sub", OpAssoc::Left, 1),
                spec("join", OpAssoc::Right, 7),
            ],
        );
        let Expr::LetRec(LetRec { bindings, body }) = expr.value else {
            panic!("Expected recursive let, got {expr:?}");
        };
        let expected = op(op(right_sub_chain(), "join", int(5)), "join", int(1));
        assert_eq!(*bindings[0].body, expected);
        assert_eq!(*body, expected);
    }

    #[test]
    fn test_recursive_group_fixities_follow_binding_and_parameter_shadowing() {
        for (fixity, peer_body) in [
            (None, left_sub_chain()),
            (declared(OpAssoc::Right, 4), right_sub_chain()),
        ] {
            let functions = [
                ("shadow", vec!["sub"]),
                ("peer", vec![]),
                ("sub", vec!["a", "b"]),
            ]
            .into_iter()
            .map(|(name, params)| RecFn {
                name: spanned(name),
                fixity: if name == "sub" { fixity } else { None },
                params: spanned(params),
                body: sub_chain(),
            })
            .collect();
            let (members, specs) = resolve_let_rec(functions, &[spec("sub", OpAssoc::Right, 7)]);
            assert_eq!(*members[0].body, left_sub_chain());
            assert_eq!(*members[1].body, peer_body);
            assert_eq!(*members[2].body, peer_body);
            assert_eq!(resolve(sub_chain(), &specs), peer_body);
        }
    }

    #[test]
    fn test_builtin_operator_fixity_can_be_shadowed() {
        let right = declared(OpAssoc::Right, 4);
        let left = declared(OpAssoc::Left, 5);
        assert_resolve(
            syntax_binding(
                None,
                "subtract",
                syntax_var("-"),
                syntax_binding(
                    right,
                    "-",
                    syntax_var("subtract"),
                    syntax_int_ops(10, &[("-", 3), ("-", 2)]),
                ),
            ),
            binding(
                None,
                "subtract",
                var("-"),
                binding(
                    right,
                    "-",
                    var("subtract"),
                    op(int(10), "-", op(int(3), "-", int(2))),
                ),
            ),
        );
        assert_resolve(
            syntax_binding(
                None,
                "add",
                syntax_var("+"),
                syntax_binding(
                    left,
                    "+",
                    syntax_var("add"),
                    syntax_int_ops(2, &[("+", 3), ("*", 4)]),
                ),
            ),
            binding(
                None,
                "add",
                var("+"),
                binding(
                    left,
                    "+",
                    var("add"),
                    op(op(int(2), "+", int(3)), "*", int(4)),
                ),
            ),
        );
    }

    #[test]
    fn test_supplied_operator_specs_are_used() {
        let fixity = declared(OpAssoc::Left, 0);
        let syntax = syntax_binding(
            fixity,
            "unused",
            syntax_function(vec!["a", "b"], syntax_var("a")),
            sub_chain(),
        );
        let expr = resolve(syntax, &[spec("sub", OpAssoc::Right, 7)]);

        assert_eq!(
            expr,
            binding(
                fixity,
                "unused",
                function(vec!["a", "b"], var("a")),
                right_sub_chain()
            )
        );
    }
}
