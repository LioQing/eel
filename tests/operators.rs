mod common;

use common::{SourceFile, assert_failure, assert_returned, assert_source_diagnostic};

#[test]
fn test_builtin_infix_and_prefix_application_syntax() {
    for (a, op, b, expected) in [
        ("40", "+", "2", 42),
        ("44", "-", "2", 42),
        ("21", "*", "2", 42),
        ("84", "/", "2", 42),
        ("43", "%", "42", 1),
        ("1", "!=", "2", 1),
        ("1", "!=", "1", 0),
        ("1.0", "!=", "2.0", 1),
        ("0.0", "!=", "-0.0", 0),
        ("unit", "!=", "unit", 0),
        ("1", "==", "1", 1),
    ] {
        for src in [
            format!("{a} {op} {b}"),
            format!("{op}({a}, {b})."),
            format!("{op}({a}, {b})()"),
        ] {
            assert_returned(&src, expected);
        }
    }
}

#[test]
fn test_comparison_type_errors_in_infix_and_prefix_calls() {
    for op in ["!=", "=="] {
        for src in [
            format!("1 {op} 1.0"),
            format!("{op}(1, 1.0)."),
            format!("{op}(1, 1.0)()"),
        ] {
            let file = SourceFile::new(src);
            let output = file.run();
            assert_failure(&output, "Cannot compare");
            assert_source_diagnostic(&output, &file.0);
        }
    }
}

#[test]
fn test_builtin_precedence_and_associativity() {
    for (src, expected) in [
        ("2 * 3 + 4", 10),
        ("2 + 3 * 4", 14),
        ("20 - 3 - 2", 15),
        ("24 / 3 / 2", 4),
        ("20 / 2 * 3", 30),
        ("20 % 6 * 2", 4),
        ("20 - 3 + 2", 19),
        ("2 + 3 * 4 - 6 / 2", 11),
        ("2 * 3 + 4 == 10", 1),
        ("10 == 2 + 3 * 4", 0),
        ("1 + 2 < 4 * 2", 1),
        ("2 * 3 >= 4 + 2", 1),
        ("1 == 2 < 3", 1),
        ("1 < 2 == 3 < 4", 1),
        ("4 < 3 < 2", 1),
        ("2 == 2 == 1", 1),
    ] {
        assert_returned(src, expected);
    }
}

#[test]
fn test_inequality_mixed_precedence_and_associativity() {
    for (src, expected) in [
        ("2 * 3 + 4 != 10", 0),
        ("10 != 2 + 3 * 4", 1),
        ("1 + 2 != 4 * 2", 1),
        ("1 != 2 < 3", 0),
        ("1 < 2 != 3 < 4", 0),
        ("4 < 3 != 2 + 1", 1),
        ("2 != 2 != 0", 0),
        ("2 == 2 != 0", 1),
        ("1 != 2 == 0", 0),
        ("1.0 + 2.0 != 3.0 * 1.0", 0),
        ("2 * (3 + 4) != 10", 1),
        ("!=(2 * 3 + 4, 10).", 0),
    ] {
        assert_returned(src, expected);
    }
}

#[test]
fn test_declared_associativity_and_precedence_limits() {
    for (src, expected) in [
        ("let opl 1 sub = fn(a, b) { a - b } 10 sub 3 sub 2", 5),
        ("let opr 1 sub = fn(a, b) { a - b } 10 sub 3 sub 2", 9),
        (
            "let opr 9223372036854775807 sub = fn(a, b) { a - b } 10 sub 3 sub 2",
            9,
        ),
        (
            "let opl -9223372036854775808 sub = fn(a, b) { a - b } 10 sub 3 sub 2",
            5,
        ),
    ] {
        assert_returned(src, expected);
    }
}

#[test]
fn test_custom_precedence_in_both_operator_orders() {
    for (add_prec, mul_prec, chain, expected) in [
        (5, 6, "2 mul 3 add 4", 10),
        (5, 6, "2 add 3 mul 4", 14),
        (5, 6, "2 mul 3 add 4 mul 5 add 6", 32),
        (6, 5, "2 mul 3 add 4", 14),
        (6, 5, "2 add 3 mul 4", 20),
    ] {
        assert_returned(
            &format!(
                "let opl {add_prec} add = fn(a, b) {{ a + b }}
             let opl {mul_prec} mul = fn(a, b) {{ a * b }} {chain}"
            ),
            expected,
        );
    }
    for (src, expected) in [
        ("let opl 0 add = fn(a, b) { a + b } 2 * 3 add 4", 10),
        ("let opl 0 add = fn(a, b) { a + b } 2 add 3 * 4", 14),
        ("let opl 5 add = fn(a, b) { a + b } 2 * 3 add 4", 14),
        ("let opl 5 add = fn(a, b) { a + b } 2 add 3 * 4", 20),
    ] {
        assert_returned(src, expected);
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
        assert_returned(
            &format!(
                "let {sub_assoc} 5 sub = fn(a, b) {{ a - b }}
             let {add_assoc} 5 add = fn(a, b) {{ a + b }} 10 sub 3 add 2"
            ),
            expected,
        );
    }
}

#[test]
fn test_parentheses_and_calls_override_fixity() {
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
        ("let id = fn(x) { x } 2 * id(3). + id(4).", 10),
        ("let id = fn(x) { x } id(2 * 3 + 4).", 10),
    ] {
        assert_returned(src, expected);
    }
}

#[test]
fn test_fixity_scopes_are_lexical() {
    for (src, expected) in [
        (
            "let opl 1 sub = fn(a, b) { a - b }
          { let opr 1 sub = fn(a, b) { a - b } 10 sub 3 sub 2 } sub 3 sub 2",
            4,
        ),
        (
            "let opl 0 add = fn(a, b) { a + b }
          let nested = { let opl 5 add = fn(a, b) { a + b } 2 add 3 * 4 }
          nested + (2 add 3 * 4)",
            34,
        ),
        (
            "let opl 1 sub = fn(a, b) { a - b }
          let pair = fn(a, b) { a * 100 + b }
          pair({ let opr 1 sub = fn(a, b) { a - b } 10 sub 3 sub 2 },
               10 sub 3 sub 2).",
            905,
        ),
        (
            "let opl 1 sub = fn(a, b) { a - b }
          case 1 { 0 => { let opr 1 sub = fn(a, b) { a - b } 10 sub 3 sub 2 }
                   _ => { 10 sub 3 sub 2 } }",
            5,
        ),
        (
            "let opl 1 sub = fn(a, b) { a - b }
          let f = fn() { 10 sub 3 sub 2 }
          let opr 1 sub = fn(a, b) { a - b } f().",
            5,
        ),
        (
            "let opl 1 sub = fn(a, b) { a - b }
          let f = fn() { let opr 1 sub = fn(a, b) { a - b } 10 sub 3 sub 2 }
          f(). * 100 + (10 sub 3 sub 2)",
            905,
        ),
    ] {
        assert_returned(src, expected);
    }
}

#[test]
fn test_plain_bindings_shadow_operator_fixity() {
    for (src, expected) in [
        ("let sub = fn(a, b) { a - b } 10 sub 3 sub 2", 5),
        (
            "let opr 5 sub = fn(a, b) { a - b }
          let sub = fn(a, b) { a - b } 10 sub 3 sub 2",
            5,
        ),
        (
            "let opr 5 sub = fn(a, b) { a - b }
          let f = fn(sub) { 10 sub 3 sub 2 } f(fn(a, b) { a - b }).",
            5,
        ),
        (
            "let opr 5 sub = fn(a, b) { a - b }
          let ordinary = fn(a, b) { a - b }
          case ordinary { sub => { 10 sub 3 sub 2 } }",
            5,
        ),
        (
            "let opr 5 sub = fn(a, b) { a - b }
          let wrap = fn(value) { unit }
          case wrap(fn(a, b) { a - b }) { wrap(sub) => { 10 sub 3 sub 2 } }",
            5,
        ),
        ("let subtract = - let opr 4 - = subtract 10 - 3 - 2", 9),
        ("let add = + let opl 5 + = add 2 + 3 * 4", 20),
    ] {
        assert_returned(src, expected);
    }
}

#[test]
fn test_expressions_use_previous_binding_fixity() {
    for (src, expected) in [
        (
            "let opr 1 sub = fn(a, b) { a - b }
          let sub = { 10 sub 3 sub 2 } sub",
            9,
        ),
        (
            "let opl 1 sub = fn(a, b) { a - b }
          let opr 1 sub = { 10 sub 3 sub 2 } sub",
            5,
        ),
    ] {
        assert_returned(src, expected);
    }
}
