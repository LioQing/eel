mod common;

use common::{SourceFile, assert_failure, assert_returned, assert_success};

#[test]
fn test_tuples_evaluate_and_display() {
    let output = SourceFile::new(
        "let make = fn(x) { x + (2 * 3) }
         Io(print(((), (1,), make(36)., (2.5, 3,))).)",
    )
    .run();
    assert_success(&output, "(unit, (1,), 42, (2.5, 3))\n", "");
}

#[test]
fn test_tuple_elements_evaluate_left_to_right() {
    let output = SourceFile::new("(missing_first, missing_second)").run();
    assert_failure(&output, "Variable `missing_first` not found");
}

#[test]
fn test_tuple_keeps_unforced_structs() {
    assert_returned(
        "let delayed = fn(x) missing
         case (delayed(42),) { (delayed(x),) => x _ => 0 }",
        42,
    );
}

#[test]
fn test_tuple_pattern_wrong_arity_does_not_leak_bindings() {
    assert_returned(
        "let tag = 42
         case (7, (1, 2)) { (tag, (_,)) => 0 _ => tag }",
        42,
    );
}

#[test]
fn test_tuple_pattern_does_not_match_same_arity_struct() {
    assert_returned(
        "let Pair = fn(x, y) unit
         case Pair(1, 2) { (_, _) => 0 Pair(a, b) => { a + b } }",
        3,
    );
}

#[test]
fn test_tuple_element_fixity_and_nested_pattern_shadowing() {
    assert_returned(
        "let opl 5 sub = -
         case (10 sub 3 * 2, (sub,)) {
             (14, (sub,)) => { 10 sub 3 * 2 }
             _ => 0
         }",
        4,
    );
}

#[test]
fn test_unit_expression_and_pattern() {
    assert_success(&SourceFile::new("()").run(), "", "");
    assert_returned("case unit { () => 42 _ => 0 }", 42);
}

#[test]
fn test_tuple_is_not_forceable_or_an_exit_code() {
    assert_failure(&SourceFile::new("(1, 2).").run(), "Expected a struct");
    assert_failure(
        &SourceFile::new("(1, 2)").run(),
        "Expected an integer exit code or `unit`",
    );
}
