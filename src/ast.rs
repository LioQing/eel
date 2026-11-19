use chumsky::{input::ValueInput, prelude::*};

use crate::declare::*;
use crate::fixity::resolve;
use crate::parse::expr_parser;

#[cfg(test)]
mod tests {
    use ariadne::{Color, Label, Report, ReportKind, sources};

    use super::*;

    use crate::lex;

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
        spanned(Expr::Op(
            Box::new(left),
            Box::new(var(name)),
            Box::new(right),
        ))
    }

    fn binding<'src>(
        spec: Option<(OpAssoc, i64)>,
        name: &'src str,
        assign: Spanned<Expr<'src>>,
        body: Spanned<Expr<'src>>,
    ) -> Spanned<Expr<'src>> {
        spanned(Expr::Let(spec, name, Box::new(assign), Box::new(body)))
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

    macro_rules! assert_ast {
        ($src:expr, $expected:expr $(,)?) => {
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
            ("2 * 3 + 4", op(op(int(2), "*", int(3)), "+", int(4))),
            ("2 + 3 * 4", op(int(2), "+", op(int(3), "*", int(4)))),
            ("20 - 3 - 2", op(op(int(20), "-", int(3)), "-", int(2))),
            ("24 / 3 / 2", op(op(int(24), "/", int(3)), "/", int(2))),
            ("20 / 2 * 3", op(op(int(20), "/", int(2)), "*", int(3))),
            ("20 % 6 * 2", op(op(int(20), "%", int(6)), "*", int(2))),
            ("20 - 3 + 2", op(op(int(20), "-", int(3)), "+", int(2))),
            (
                "2 + 3 * 4 - 6 / 2",
                op(
                    op(int(2), "+", op(int(3), "*", int(4))),
                    "-",
                    op(int(6), "/", int(2)),
                ),
            ),
        ] {
            assert_ast!(src, expected);
        }
    }

    #[test]
    fn test_builtin_comparison_precedence_and_associativity() {
        for (src, expected) in [
            (
                "2 * 3 + 4 == 10",
                op(op(op(int(2), "*", int(3)), "+", int(4)), "==", int(10)),
            ),
            (
                "10 == 2 + 3 * 4",
                op(int(10), "==", op(int(2), "+", op(int(3), "*", int(4)))),
            ),
            (
                "1 + 2 < 4 * 2",
                op(op(int(1), "+", int(2)), "<", op(int(4), "*", int(2))),
            ),
            (
                "2 * 3 >= 4 + 2",
                op(op(int(2), "*", int(3)), ">=", op(int(4), "+", int(2))),
            ),
            ("1 == 2 < 3", op(int(1), "==", op(int(2), "<", int(3)))),
            (
                "1 < 2 == 3 < 4",
                op(op(int(1), "<", int(2)), "==", op(int(3), "<", int(4))),
            ),
            ("4 < 3 < 2", op(op(int(4), "<", int(3)), "<", int(2))),
            ("2 == 2 == 1", op(op(int(2), "==", int(2)), "==", int(1))),
        ] {
            assert_ast!(src, expected);
        }
        assert_ast!(
            "1 + 2 != 3 < 4",
            op(op(int(1), "+", int(2)), "!=", op(int(3), "<", int(4))),
        );
    }

    #[test]
    fn test_inequality_mixed_precedence_and_associativity() {
        for (src, expected) in [
            (
                "2 * 3 + 4 != 10",
                op(op(op(int(2), "*", int(3)), "+", int(4)), "!=", int(10)),
            ),
            (
                "10 != 2 + 3 * 4",
                op(int(10), "!=", op(int(2), "+", op(int(3), "*", int(4)))),
            ),
            (
                "1 + 2 != 4 * 2",
                op(op(int(1), "+", int(2)), "!=", op(int(4), "*", int(2))),
            ),
            ("1 != 2 < 3", op(int(1), "!=", op(int(2), "<", int(3)))),
            (
                "1 < 2 != 3 < 4",
                op(op(int(1), "<", int(2)), "!=", op(int(3), "<", int(4))),
            ),
            (
                "4 < 3 != 2 + 1",
                op(op(int(4), "<", int(3)), "!=", op(int(2), "+", int(1))),
            ),
            ("2 != 2 != 0", op(op(int(2), "!=", int(2)), "!=", int(0))),
            ("2 == 2 != 0", op(op(int(2), "==", int(2)), "!=", int(0))),
            ("1 != 2 == 0", op(op(int(1), "!=", int(2)), "==", int(0))),
            (
                "2 * (3 + 4) != 10",
                op(op(int(2), "*", op(int(3), "+", int(4))), "!=", int(10)),
            ),
            (
                "!=(2 * 3 + 4, 10).",
                call(
                    call(
                        var("!="),
                        vec![op(op(int(2), "*", int(3)), "+", int(4)), int(10)],
                    ),
                    vec![],
                ),
            ),
        ] {
            assert_ast!(src, expected);
        }
        let float = |value| spanned(Expr::Lit(Value::Float(value)));
        assert_ast!(
            "1.0 + 2.0 != 3.0 * 1.0",
            op(
                op(float(1.0), "+", float(2.0)),
                "!=",
                op(float(3.0), "*", float(1.0))
            )
        );
    }

    #[test]
    fn test_declared_left_and_right_associativity() {
        for (keyword, assoc, prec) in [
            ("opl", OpAssoc::Left, 1),
            ("opr", OpAssoc::Right, 1),
            ("opr", OpAssoc::Right, i64::MAX),
            ("opl", OpAssoc::Left, i64::MIN),
        ] {
            let src = format!("let {keyword} {prec} sub = fn(a, b) {{ a - b }} 10 sub 3 sub 2");
            let body = match assoc {
                OpAssoc::Left => op(op(int(10), "sub", int(3)), "sub", int(2)),
                OpAssoc::Right => op(int(10), "sub", op(int(3), "sub", int(2))),
            };
            assert_ast!(
                src.as_str(),
                binding(
                    Some((assoc, prec)),
                    "sub",
                    function(vec!["a", "b"], op(var("a"), "-", var("b"))),
                    body
                )
            );
        }
    }

    #[test]
    fn test_custom_precedence_in_both_operator_orders() {
        for (add_prec, mul_prec, src, expected) in [
            (
                5,
                6,
                "let opl 5 add = fn(a, b) { a + b } let opl 6 mul = fn(a, b) { a * b } 2 mul 3 add 4",
                op(op(int(2), "mul", int(3)), "add", int(4)),
            ),
            (
                5,
                6,
                "let opl 5 add = fn(a, b) { a + b } let opl 6 mul = fn(a, b) { a * b } 2 add 3 mul 4",
                op(int(2), "add", op(int(3), "mul", int(4))),
            ),
            (
                5,
                6,
                "let opl 5 add = fn(a, b) { a + b } let opl 6 mul = fn(a, b) { a * b } 2 mul 3 add 4 mul 5 add 6",
                op(
                    op(op(int(2), "mul", int(3)), "add", op(int(4), "mul", int(5))),
                    "add",
                    int(6),
                ),
            ),
            (
                6,
                5,
                "let opl 6 add = fn(a, b) { a + b } let opl 5 mul = fn(a, b) { a * b } 2 mul 3 add 4",
                op(int(2), "mul", op(int(3), "add", int(4))),
            ),
            (
                6,
                5,
                "let opl 6 add = fn(a, b) { a + b } let opl 5 mul = fn(a, b) { a * b } 2 add 3 mul 4",
                op(op(int(2), "add", int(3)), "mul", int(4)),
            ),
        ] {
            assert_ast!(
                src,
                binding(
                    Some((OpAssoc::Left, add_prec)),
                    "add",
                    function(vec!["a", "b"], op(var("a"), "+", var("b"))),
                    binding(
                        Some((OpAssoc::Left, mul_prec)),
                        "mul",
                        function(vec!["a", "b"], op(var("a"), "*", var("b"))),
                        expected
                    )
                )
            );
        }
    }

    #[test]
    fn test_custom_and_builtin_precedence_in_both_operator_orders() {
        for (prec, src, expected) in [
            (
                0,
                "let opl 0 add = fn(a, b) { a + b } 2 * 3 add 4",
                op(op(int(2), "*", int(3)), "add", int(4)),
            ),
            (
                0,
                "let opl 0 add = fn(a, b) { a + b } 2 add 3 * 4",
                op(int(2), "add", op(int(3), "*", int(4))),
            ),
            (
                5,
                "let opl 5 add = fn(a, b) { a + b } 2 * 3 add 4",
                op(int(2), "*", op(int(3), "add", int(4))),
            ),
            (
                5,
                "let opl 5 add = fn(a, b) { a + b } 2 add 3 * 4",
                op(op(int(2), "add", int(3)), "*", int(4)),
            ),
        ] {
            assert_ast!(
                src,
                binding(
                    Some((OpAssoc::Left, prec)),
                    "add",
                    function(vec!["a", "b"], op(var("a"), "+", var("b"))),
                    expected
                )
            );
        }
    }

    #[test]
    fn test_equal_precedence_uses_following_operator_associativity() {
        for (sub_keyword, sub_assoc) in [("opl", OpAssoc::Left), ("opr", OpAssoc::Right)] {
            for (add_keyword, add_assoc) in [("opl", OpAssoc::Left), ("opr", OpAssoc::Right)] {
                let src = format!(
                    "let {sub_keyword} 5 sub = fn(a, b) {{ a - b }} let {add_keyword} 5 add = fn(a, b) {{ a + b }} 10 sub 3 add 2"
                );
                let body = match add_assoc {
                    OpAssoc::Left => op(op(int(10), "sub", int(3)), "add", int(2)),
                    OpAssoc::Right => op(int(10), "sub", op(int(3), "add", int(2))),
                };
                assert_ast!(
                    src.as_str(),
                    binding(
                        Some((sub_assoc, 5)),
                        "sub",
                        function(vec!["a", "b"], op(var("a"), "-", var("b"))),
                        binding(
                            Some((add_assoc, 5)),
                            "add",
                            function(vec!["a", "b"], op(var("a"), "+", var("b"))),
                            body
                        )
                    )
                );
            }
        }
    }

    #[test]
    fn test_parentheses_and_nested_grouping_override_fixity() {
        for (src, expected) in [
            ("2 * (3 + 4)", op(int(2), "*", op(int(3), "+", int(4)))),
            ("(2 * 3) + 4", op(op(int(2), "*", int(3)), "+", int(4))),
            (
                "100 - (20 - (5 - 2))",
                op(int(100), "-", op(int(20), "-", op(int(5), "-", int(2)))),
            ),
            ("((20 - 3) - 2)", op(op(int(20), "-", int(3)), "-", int(2))),
            ("20 - (3 - 2)", op(int(20), "-", op(int(3), "-", int(2)))),
            (
                "2 * ((3 + 4) * (5 - (3 - 1))) + 1",
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
            assert_ast!(src, expected);
        }
        assert_ast!(
            "let opl 1 sub = fn(a, b) { a - b } 10 sub (3 sub 2)",
            binding(
                Some((OpAssoc::Left, 1)),
                "sub",
                function(vec!["a", "b"], op(var("a"), "-", var("b"))),
                op(int(10), "sub", op(int(3), "sub", int(2)))
            )
        );
        assert_ast!(
            "let opr 1 sub = fn(a, b) { a - b } { (10 sub 3) sub 2 }",
            binding(
                Some((OpAssoc::Right, 1)),
                "sub",
                function(vec!["a", "b"], op(var("a"), "-", var("b"))),
                op(op(int(10), "sub", int(3)), "sub", int(2))
            )
        );
        assert_ast!(
            "let opl 5 add = fn(a, b) { a + b } let opl 6 mul = fn(a, b) { a * b } { (2 add 3) mul 4 }",
            binding(
                Some((OpAssoc::Left, 5)),
                "add",
                function(vec!["a", "b"], op(var("a"), "+", var("b"))),
                binding(
                    Some((OpAssoc::Left, 6)),
                    "mul",
                    function(vec!["a", "b"], op(var("a"), "*", var("b"))),
                    op(op(int(2), "add", int(3)), "mul", int(4))
                )
            )
        );
    }

    #[test]
    fn test_calls_bind_more_tightly_than_operators() {
        assert_ast!(
            "let id = fn(x) { x } 2 * id(3). + id(4).",
            binding(
                None,
                "id",
                function(vec!["x"], var("x")),
                op(
                    op(int(2), "*", call(call(var("id"), vec![int(3)]), vec![])),
                    "+",
                    call(call(var("id"), vec![int(4)]), vec![])
                )
            )
        );
        assert_ast!(
            "let id = fn(x) { x } id(2 * 3 + 4).",
            binding(
                None,
                "id",
                function(vec!["x"], var("x")),
                call(
                    call(var("id"), vec![op(op(int(2), "*", int(3)), "+", int(4))]),
                    vec![]
                )
            )
        );
    }

    #[test]
    fn test_nested_declaration_does_not_leak_into_outer_chain() {
        assert_ast!(
            "let opl 1 sub = fn(a, b) { a - b }
             { let opr 1 sub = fn(a, b) { a - b } 10 sub 3 sub 2 } sub 3 sub 2",
            binding(
                Some((OpAssoc::Left, 1)),
                "sub",
                function(vec!["a", "b"], op(var("a"), "-", var("b"))),
                op(
                    op(
                        binding(
                            Some((OpAssoc::Right, 1)),
                            "sub",
                            function(vec!["a", "b"], op(var("a"), "-", var("b"))),
                            op(int(10), "sub", op(int(3), "sub", int(2)))
                        ),
                        "sub",
                        int(3)
                    ),
                    "sub",
                    int(2)
                )
            ),
        );
        assert_ast!(
            "let opl 0 add = fn(a, b) { a + b }
             let nested = { let opl 5 add = fn(a, b) { a + b } 2 add 3 * 4 }
             nested + (2 add 3 * 4)",
            binding(
                Some((OpAssoc::Left, 0)),
                "add",
                function(vec!["a", "b"], op(var("a"), "+", var("b"))),
                binding(
                    None,
                    "nested",
                    binding(
                        Some((OpAssoc::Left, 5)),
                        "add",
                        function(vec!["a", "b"], op(var("a"), "+", var("b"))),
                        op(op(int(2), "add", int(3)), "*", int(4))
                    ),
                    op(
                        var("nested"),
                        "+",
                        op(int(2), "add", op(int(3), "*", int(4)))
                    )
                )
            ),
        );
    }

    #[test]
    fn test_nested_declaration_does_not_leak_into_siblings() {
        assert_ast!(
            "let opl 1 sub = fn(a, b) { a - b }
             let pair = fn(a, b) { a * 100 + b }
             pair({ let opr 1 sub = fn(a, b) { a - b } 10 sub 3 sub 2 },
                  10 sub 3 sub 2).",
            binding(
                Some((OpAssoc::Left, 1)),
                "sub",
                function(vec!["a", "b"], op(var("a"), "-", var("b"))),
                binding(
                    None,
                    "pair",
                    function(
                        vec!["a", "b"],
                        op(op(var("a"), "*", int(100)), "+", var("b"))
                    ),
                    call(
                        call(
                            var("pair"),
                            vec![
                                binding(
                                    Some((OpAssoc::Right, 1)),
                                    "sub",
                                    function(vec!["a", "b"], op(var("a"), "-", var("b"))),
                                    op(int(10), "sub", op(int(3), "sub", int(2)))
                                ),
                                op(op(int(10), "sub", int(3)), "sub", int(2))
                            ]
                        ),
                        vec![]
                    )
                )
            ),
        );
        assert_ast!(
            "let opl 1 sub = fn(a, b) { a - b }
             case 1 {
                 0 => { let opr 1 sub = fn(a, b) { a - b } 10 sub 3 sub 2 }
                 _ => { 10 sub 3 sub 2 }
             }",
            binding(
                Some((OpAssoc::Left, 1)),
                "sub",
                function(vec!["a", "b"], op(var("a"), "-", var("b"))),
                spanned(Expr::Case(
                    Box::new(int(1)),
                    spanned(vec![
                        (
                            spanned(Patt::Lit(Value::Int(0))),
                            Box::new(binding(
                                Some((OpAssoc::Right, 1)),
                                "sub",
                                function(vec!["a", "b"], op(var("a"), "-", var("b"))),
                                op(int(10), "sub", op(int(3), "sub", int(2)))
                            ))
                        ),
                        (
                            spanned(Patt::Var("_")),
                            Box::new(op(op(int(10), "sub", int(3)), "sub", int(2)))
                        )
                    ])
                ))
            ),
        );
    }

    #[test]
    fn test_function_fixity_is_lexical_not_call_site_dependent() {
        assert_ast!(
            "let opl 1 sub = fn(a, b) { a - b }
             let f = fn() { 10 sub 3 sub 2 }
             let opr 1 sub = fn(a, b) { a - b }
             f().",
            binding(
                Some((OpAssoc::Left, 1)),
                "sub",
                function(vec!["a", "b"], op(var("a"), "-", var("b"))),
                binding(
                    None,
                    "f",
                    function(vec![], op(op(int(10), "sub", int(3)), "sub", int(2))),
                    binding(
                        Some((OpAssoc::Right, 1)),
                        "sub",
                        function(vec!["a", "b"], op(var("a"), "-", var("b"))),
                        call(call(var("f"), vec![]), vec![])
                    )
                )
            ),
        );
        assert_ast!(
            "let opl 1 sub = fn(a, b) { a - b }
             let f = fn() { let opr 1 sub = fn(a, b) { a - b } 10 sub 3 sub 2 }
             f(). * 100 + (10 sub 3 sub 2)",
            binding(
                Some((OpAssoc::Left, 1)),
                "sub",
                function(vec!["a", "b"], op(var("a"), "-", var("b"))),
                binding(
                    None,
                    "f",
                    function(
                        vec![],
                        binding(
                            Some((OpAssoc::Right, 1)),
                            "sub",
                            function(vec!["a", "b"], op(var("a"), "-", var("b"))),
                            op(int(10), "sub", op(int(3), "sub", int(2)))
                        )
                    ),
                    op(
                        op(call(call(var("f"), vec![]), vec![]), "*", int(100)),
                        "+",
                        op(op(int(10), "sub", int(3)), "sub", int(2))
                    )
                )
            ),
        );
    }

    #[test]
    fn test_plain_bindings_shadow_operator_fixity() {
        let subtract = function(vec!["a", "b"], op(var("a"), "-", var("b")));
        let chain = op(op(int(10), "sub", int(3)), "sub", int(2));
        assert_ast!(
            "let sub = fn(a, b) { a - b } 10 sub 3 sub 2",
            binding(None, "sub", subtract.clone(), chain.clone())
        );
        assert_ast!(
            "let opr 5 sub = fn(a, b) { a - b }
             let sub = fn(a, b) { a - b } 10 sub 3 sub 2",
            binding(
                Some((OpAssoc::Right, 5)),
                "sub",
                subtract.clone(),
                binding(None, "sub", subtract.clone(), chain.clone())
            ),
        );
        assert_ast!(
            "let opr 5 sub = fn(a, b) { a - b }
             let f = fn(sub) { 10 sub 3 sub 2 }
             f(fn(a, b) { a - b }).",
            binding(
                Some((OpAssoc::Right, 5)),
                "sub",
                subtract.clone(),
                binding(
                    None,
                    "f",
                    function(vec!["sub"], chain.clone()),
                    call(call(var("f"), vec![subtract.clone()]), vec![])
                )
            ),
        );
        assert_ast!(
            "let opr 5 sub = fn(a, b) { a - b }
             let ordinary = fn(a, b) { a - b }
             case ordinary { sub => { 10 sub 3 sub 2 } }",
            binding(
                Some((OpAssoc::Right, 5)),
                "sub",
                subtract.clone(),
                binding(
                    None,
                    "ordinary",
                    subtract.clone(),
                    spanned(Expr::Case(
                        Box::new(var("ordinary")),
                        spanned(vec![(spanned(Patt::Var("sub")), Box::new(chain.clone()))])
                    ))
                )
            ),
        );
        assert_ast!(
            "let opr 5 sub = fn(a, b) { a - b }
             let wrap = fn(value) { unit }
             case wrap(fn(a, b) { a - b }) { wrap(sub) => { 10 sub 3 sub 2 } }",
            binding(
                Some((OpAssoc::Right, 5)),
                "sub",
                subtract.clone(),
                binding(
                    None,
                    "wrap",
                    function(vec!["value"], spanned(Expr::Lit(Value::Unit))),
                    spanned(Expr::Case(
                        Box::new(call(var("wrap"), vec![subtract])),
                        spanned(vec![(
                            spanned(Patt::Struct(
                                "wrap",
                                spanned(vec![spanned(Patt::Var("sub"))])
                            )),
                            Box::new(chain)
                        )])
                    ))
                )
            ),
        );
    }

    #[test]
    fn test_initializers_use_previous_binding_fixity() {
        assert_ast!(
            "let opr 1 sub = fn(a, b) { a - b }
             let sub = { 10 sub 3 sub 2 } sub",
            binding(
                Some((OpAssoc::Right, 1)),
                "sub",
                function(vec!["a", "b"], op(var("a"), "-", var("b"))),
                binding(
                    None,
                    "sub",
                    op(int(10), "sub", op(int(3), "sub", int(2))),
                    var("sub")
                )
            ),
        );
        assert_ast!(
            "let opl 1 sub = fn(a, b) { a - b }
             let opr 1 sub = { 10 sub 3 sub 2 } sub",
            binding(
                Some((OpAssoc::Left, 1)),
                "sub",
                function(vec!["a", "b"], op(var("a"), "-", var("b"))),
                binding(
                    Some((OpAssoc::Right, 1)),
                    "sub",
                    op(op(int(10), "sub", int(3)), "sub", int(2)),
                    var("sub")
                )
            ),
        );
    }

    #[test]
    fn test_recursive_function_body_uses_its_own_declared_fixity() {
        for (keyword, assoc) in [("opl", OpAssoc::Left), ("opr", OpAssoc::Right)] {
            let src = format!("let {keyword} 1 sub = fn(a, b) {{ 10 sub 3 sub 2 }} sub");
            let body = match assoc {
                OpAssoc::Left => op(op(int(10), "sub", int(3)), "sub", int(2)),
                OpAssoc::Right => op(int(10), "sub", op(int(3), "sub", int(2))),
            };
            assert_ast!(
                src.as_str(),
                binding(
                    Some((assoc, 1)),
                    "sub",
                    function(vec!["a", "b"], body),
                    var("sub")
                )
            );
        }
    }

    #[test]
    fn test_builtin_operator_fixity_can_be_shadowed() {
        assert_ast!(
            "let subtract = - let opr 4 - = subtract 10 - 3 - 2",
            binding(
                None,
                "subtract",
                var("-"),
                binding(
                    Some((OpAssoc::Right, 4)),
                    "-",
                    var("subtract"),
                    op(int(10), "-", op(int(3), "-", int(2)))
                )
            )
        );
        assert_ast!(
            "let add = + let opl 5 + = add 2 + 3 * 4",
            binding(
                None,
                "add",
                var("+"),
                binding(
                    Some((OpAssoc::Left, 5)),
                    "+",
                    var("add"),
                    op(op(int(2), "+", int(3)), "*", int(4))
                )
            )
        );
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
        let expected = binding(
            Some((OpAssoc::Left, 0)),
            "unused",
            function(vec!["a", "b"], var("a")),
            op(int(10), "sub", op(int(3), "sub", int(2))),
        );
        assert!(
            expr.spanless_eq(&expected),
            "Expected: {expected:?}\nGot: {expr:?}"
        );
    }
}
