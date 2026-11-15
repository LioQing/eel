mod common;

use common::{SourceFile, assert_failure, assert_returned, assert_source_diagnostic};

#[test]
fn test_numeric_runtime_failures_are_language_errors() {
    for (src, message) in [
        ("9223372036854775807 + 1", "Integer overflow"),
        ("-9223372036854775808 - 1", "Integer overflow"),
        ("9223372036854775807 * 2", "Integer overflow"),
        ("-9223372036854775808 / -1", "Integer overflow"),
        ("-9223372036854775808 % -1", "Integer overflow"),
        ("-9223372036854775808 / 0", "Cannot divide"),
        ("-9223372036854775808 % 0", "Cannot modulo"),
    ] {
        let file = SourceFile::new(src);
        let output = file.run();
        assert_failure(&output, message);
        assert_source_diagnostic(&output, &file.0);
        assert!(String::from_utf8_lossy(&output.stderr).contains("Evaluation failed"));
    }
}

#[test]
fn test_integer_limits_are_valid_results() {
    for (src, expected) in [
        ("9223372036854775807 + 0", i64::MAX),
        ("-9223372036854775808 / 1", i64::MIN),
    ] {
        assert_returned(src, expected);
    }
}

#[test]
fn test_overflow_in_deferred_and_nested_evaluation() {
    for src in [
        "let x = 9223372036854775807 let y = 1 x + y",
        "let add = + let sum = add(9223372036854775807, 1) sum.",
        "let f = fn(x) { x * 2 } f(9223372036854775807).",
        "0 + (9223372036854775807 + 1)",
    ] {
        let file = SourceFile::new(src);
        let output = file.run();
        assert_failure(&output, "Integer overflow");
        assert_source_diagnostic(&output, &file.0);
    }
}
