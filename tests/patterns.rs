mod common;

use common::{SourceFile, assert_failure, assert_returned};

#[test]
fn test_let_destructures_against_outer_constructor_without_forcing() {
    assert_returned(
        "let Box = fn(x) { missing }
         let Alias = Box
         let (Alias, Alias((x, (), 7))) = (20, Box((22, unit, 7)))
         Alias + x",
        42,
    );
}

#[test]
fn test_let_pattern_requires_a_complete_match() {
    for source in [
        "let (x, y) = (42,) missing",
        "let (x, 0) = (42, 1) missing",
        "let Box = fn(x) x let old = Box(42) let Box = fn(x) x let Box(y) = old missing",
    ] {
        let output = SourceFile::new(source).run();
        assert_failure(&output, "did not match value");
        assert_failure(&output, "Initializer supplied here");
    }
}

#[test]
fn test_parameter_patterns_match_at_application_and_keep_positional_args() {
    let output = SourceFile::new("let f = fn((x, y)) missing let unused = f(0) 42").run();
    assert_failure(&output, "Pattern (x, y) did not match value 0");
    assert_failure(&output, "Argument passed here");
    assert_returned(
        "let f = fn((x, y), 7) missing
         let f((a, b), tag) = f((20, 15), 7)
         a + b + tag",
        42,
    );
}

#[test]
fn test_argument_evaluation_error_does_not_get_a_pattern_label() {
    let output = SourceFile::new("let f = fn((x, y)) x f(missing).").run();
    assert_failure(&output, "Variable `missing` not found");
    let stderr = std::str::from_utf8(&output.stderr).unwrap();
    assert!(!stderr.contains("Argument passed here"), "{stderr}");
}

#[test]
fn test_parameter_constructor_lookup_uses_definition_scope() {
    assert_returned(
        "let Box = fn(x) missing
         let old = Box(22)
         let f = fn(Box, Box(x)) { Box + x }
         let Box = fn(x) missing
         f(20, old).",
        42,
    );
}

#[test]
fn test_recursive_member_patterns_use_peers_and_nested_operator_fixity() {
    assert_returned(
        "let {
            apply = fn(Tag((opr 1 sub, n))) { n sub 3 sub 2 }
            Tag = fn(x) missing
         }
         apply(Tag((-, 10))).",
        9,
    );
}

#[test]
fn test_struct_pattern_rejects_too_few_args() {
    assert_returned(
        "let pair = fn(x, y) { unit } case pair(42, 7) { pair(a) => 1 _ => 0 }",
        0,
    );
}

#[test]
fn test_struct_pattern_rejects_too_many_args() {
    assert_returned(
        "let box = fn(x) { unit } case box(42) { box(a, b) => 1 _ => 0 }",
        0,
    );
}

#[test]
fn test_struct_pattern_matches_zero_args() {
    assert_returned(
        "let empty = fn() { unit } case empty() { empty() => 1 _ => 0 }",
        1,
    );
}

#[test]
fn test_struct_pattern_rejects_empty_pattern_for_nonempty_struct() {
    assert_returned(
        "let box = fn(x) { unit } case box(42) { box() => 1 _ => 0 }",
        0,
    );
}

#[test]
fn test_struct_pattern_rejects_nonempty_pattern_for_empty_struct() {
    assert_returned(
        "let empty = fn() { unit } case empty() { empty(a) => 1 _ => 0 }",
        0,
    );
}

#[test]
fn test_struct_pattern_matches_exact_arity_and_binds_args() {
    assert_returned(
        "let pair = fn(x, y) { unit } case pair(42, 7) { pair(a, b) => { a - b } _ => 0 }",
        35,
    );
}

#[test]
fn test_struct_pattern_matches_nested_args_and_binds_values() {
    assert_returned(
        "
        let box = fn(x) { unit }
        let pair = fn(x, y) { unit }
        case pair(7, box(42)) {
            pair(a, box(b)) => { a + b }
            _ => 0
        }
        ",
        49,
    );
}

#[test]
fn test_struct_pattern_nested_arity_mismatch_preserves_fallback_bindings() {
    for pattern in ["outer(tag, inner())", "outer(tag, inner(a, b))"] {
        let src = format!(
            "
            let tag = 99
            let inner = fn(x) {{ unit }}
            let outer = fn(tag, item) {{ unit }}
            case outer(7, inner(42)) {{
                {pattern} => 1
                _ => tag
            }}
            "
        );

        assert_returned(&src, 99);
    }
}

#[test]
fn test_struct_pattern_falls_through_to_matching_arm() {
    assert_returned(
        "
        let box = fn(x) { unit }
        case box(42) {
            box() => 0
            box(a, b) => 1
            box(41) => 2
            box(a) => a
            _ => 3
        }
        ",
        42,
    );
}
