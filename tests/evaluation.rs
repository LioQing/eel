mod common;

use common::{SourceFile, assert_failure, assert_returned};

#[test]
fn test_call_creates_a_struct_without_evaluating_its_body() {
    assert_returned("let f = fn() { missing } case f() { f() => 42 }", 42);
}

#[test]
fn test_call_accepts_let_expressions_as_arguments() {
    assert_returned(
        "let n = 2
         let sum = fn(a, b, c) { a + b + c }
         sum(let (x, y) = (10, 20) x + y, let n = 10 n, n,).",
        42,
    );
}

#[test]
fn test_call_accepts_a_recursive_let_group_as_an_argument() {
    assert_returned(
        "let sum = fn(a, b) { a + b }
         sum(let { first = fn() { second(). } second = fn() 40 } first()., 2).",
        42,
    );
}

#[test]
fn test_dot_evaluates_a_struct() {
    assert_returned("let f = fn() { 42 } let value = f() value.", 42);
    assert_returned("let f = fn(x) { fn() { x } } f(42).().", 42);
}

#[test]
fn test_field_access_reads_parameters_and_body_bindings() {
    assert_returned(
        "let n = 5
         let Box = fn(value) unit
         let f = fn(Box((y, z))) {
             let a = (n + y)
             let (b, c) = (20, 6)
             missing_result
         }
         let s = f(Box((6, 1)))
         s.a + s.y + s.b + s.c - s.z",
        42,
    );
    assert_returned(
        "let f = fn() {
             let Monad(bind, return) = IoMonad
             unit
         }
         f().return(42)..value",
        42,
    );
}

#[test]
fn test_field_access_can_return_recursive_closures_and_unforced_structs() {
    assert_returned(
        "let f = fn(n) {
             let { first = fn() { second(). } second = fn() n }
             missing_result
         }
         f(42).first().",
        42,
    );
    assert_returned(
        "let Box = fn(value) missing_result
         let f = fn() { let box = Box(42) unit }
         f().box.value",
        42,
    );
}

#[test]
fn test_field_access_uses_the_latest_local_binding() {
    assert_returned("let f = fn(x, x) x f(1, 42).x", 42);
    assert_returned(
        "let x = 1
         let f = fn(x) {
             let { x = fn() 0 }
             let x = 42
             { let x = 99 x }
         }
         f(2).x",
        42,
    );
    assert_returned("let f = fn(x) { let { x = fn() 42 } unit } f(0).x().", 42);
}

#[test]
fn test_field_access_excludes_captures_and_nested_bindings() {
    for (source, field) in [
        ("let n = 42 let f = fn() n f().n", "n"),
        ("let f = fn() f f().f", "f"),
        ("let { f = fn() g g = fn() 42 } f().g", "g"),
        ("let f = fn() { { let n = 42 n } } f().n", "n"),
        ("let f = fn() ({ let n = 42 n }) f().n", "n"),
        ("let f = fn() { let x = { let n = 42 n } x } f().n", "n"),
        ("let f = fn(y) { case y { 1 => 0 z => z } } f(2).z", "z"),
        (
            "let f = fn() { case 1 { z => { let n = z n } } } f().n",
            "n",
        ),
    ] {
        assert_failure(
            &SourceFile::new(source).run(),
            &format!("Struct has no field `{field}`"),
        );
    }
    assert_returned("let f = fn() { { let n = 42 n } } f().", 42);
    assert_returned(
        "let f = (fn(n) { case n { 0 => 42 _ => f(0). } }) f(1).",
        42,
    );
}

#[test]
fn test_field_access_stops_before_a_let_bind() {
    assert_returned(
        "let f = fn(y) {
             let a = (y + 1)
             let x <- missing_action
             let a = x
             a
         }
         f(41).a",
        42,
    );
    let source = "let bind = fn(v, next) next(v).
                  let f = fn() { let x <- 42 let after = x after }";
    for field in ["x", "after"] {
        assert_failure(
            &SourceFile::new(format!("{source} f().{field}")).run(),
            &format!("Struct has no field `{field}`"),
        );
    }
    assert_returned(&format!("{source} f()."), 42);
}

#[test]
fn test_field_access_reports_initializer_errors_but_skips_the_result() {
    let source = "let f = fn(x) { let a = (x + 1) missing_result }";
    assert_returned(&format!("{source} f(41).a"), 42);
    assert_failure(
        &SourceFile::new(format!("{source} f(41).")).run(),
        "Variable `missing_result` not found",
    );
    assert_failure(
        &SourceFile::new("let f = fn(x) { let a = missing_initializer x } f(42).x").run(),
        "Variable `missing_initializer` not found",
    );
}

#[test]
fn test_structs_cannot_be_called() {
    for source in [
        "let f = fn() { 42 } f()()",
        "let f = fn(x) { x } let value = f(42) value()",
        "let f = fn() { missing_body } let value = f() value(missing_arg)",
    ] {
        assert_failure(&SourceFile::new(source).run(), "Use `.` to evaluate it");
    }
}

#[test]
fn test_dot_requires_a_struct() {
    for source in [
        "let f = fn() { 42 } f.",
        "(42).",
        "let f = fn() 42 f.x",
        "(42).x",
    ] {
        assert_failure(&SourceFile::new(source).run(), "Expected a struct");
    }
}
