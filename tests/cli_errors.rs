mod common;

use common::{SourceFile, assert_failure, assert_source_diagnostic, assert_success, cli};

#[test]
fn test_missing_file_argument_fails_without_panicking() {
    let output = cli().output().unwrap();
    assert_failure(&output, "Expected file argument");
    assert!(String::from_utf8_lossy(&output.stderr).contains("Usage:"));
}

#[test]
fn test_extra_file_argument_fails_before_evaluation() {
    let file = SourceFile::new("let _ = print(99). 0");
    let output = cli().arg(&file.0).arg("unexpected").output().unwrap();
    assert_failure(&output, "Expected exactly one file argument");
}

#[test]
fn test_missing_source_file_fails_without_panicking() {
    let file = SourceFile::new("0");
    let missing = file.0.with_extension("missing");
    let output = cli().arg(&missing).output().unwrap();
    assert_failure(&output, "Failed to read source file");
    assert_source_diagnostic(&output, &missing);
}

#[test]
fn test_directory_source_fails_without_panicking() {
    let output = cli().arg(std::env::temp_dir()).output().unwrap();
    assert_failure(&output, "Failed to read source file");
}

#[test]
fn test_non_utf8_source_fails_without_panicking() {
    let file = SourceFile::new([0xff, 0xfe]);
    let output = file.run();
    assert_failure(&output, "Failed to read source file");
    assert_source_diagnostic(&output, &file.0);
}

#[test]
fn test_out_of_range_integer_fails_with_source_diagnostic() {
    for literal in [
        "9223372036854775808",
        "-9223372036854775809",
        "99999999999999999999999999999999999999999999999999",
    ] {
        let file = SourceFile::new(literal);
        let output = file.run();
        assert_failure(&output, "Invalid integer literal");
        assert_source_diagnostic(&output, &file.0);
        assert!(String::from_utf8_lossy(&output.stderr).contains("Lexing failed"));
    }
}

#[test]
fn test_lex_error_prevents_all_program_execution() {
    for source in [
        "let _ = print(123456789). 9223372036854775808",
        "let x = 9223372036854775808 let _ = print(123456789). 0",
        "let _ = eprint(123456789). -9223372036854775809",
    ] {
        let file = SourceFile::new(source);
        let output = file.run();
        assert_failure(&output, "Invalid integer literal");
        assert_source_diagnostic(&output, &file.0);
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(stderr.contains("Lexing failed"), "{stderr}");
        assert!(
            !stderr.lines().any(|line| line.trim() == "123456789"),
            "{stderr}"
        );
    }
}

#[test]
fn test_syntax_error_fails_with_source_diagnostic() {
    let file = SourceFile::new("let x =");
    let output = file.run();
    assert_failure(&output, "Parsing failed");
    assert_source_diagnostic(&output, &file.0);
    assert!(String::from_utf8_lossy(&output.stderr).contains("expected"));
}

#[test]
fn test_empty_source_fails() {
    let output = SourceFile::new("").run();
    assert_failure(&output, "Parsing failed");
}

#[test]
fn test_recovered_ast_is_not_evaluated() {
    let file = SourceFile::new("let _ = print(99). { let x = }");
    let output = file.run();
    assert_failure(&output, "Parsing failed");
    assert_source_diagnostic(&output, &file.0);
}

#[test]
fn test_runtime_error_fails_with_source_diagnostic() {
    let file = SourceFile::new("missing_variable");
    let output = file.run();
    assert_failure(&output, "Variable `missing_variable` not found");
    assert_source_diagnostic(&output, &file.0);
}

#[test]
fn test_type_error_fails_with_source_diagnostic() {
    let file = SourceFile::new("1 + unit");
    let output = file.run();
    assert_failure(&output, "Cannot add");
    assert_source_diagnostic(&output, &file.0);
}

#[test]
fn test_division_by_zero_fails_with_source_diagnostic() {
    for source in ["1 / 0", "1.0 / 0.0"] {
        let file = SourceFile::new(source);
        let output = file.run();
        assert_failure(&output, "Cannot divide");
        assert_source_diagnostic(&output, &file.0);
    }
}

#[test]
fn test_success_preserves_returned_output_and_zero_status() {
    let output = SourceFile::new("42").run();
    assert_success(&output, "Returned: 42\n", "");
}

#[test]
fn test_success_preserves_program_output() {
    let output = SourceFile::new("let _ = print(99). 0").run();
    assert_success(&output, "99\nReturned: 0\n", "");
}

#[test]
fn test_stderr_program_output_does_not_count_as_failure() {
    let output = SourceFile::new("let _ = eprint(99). 0").run();
    assert_success(&output, "Returned: 0\n", "99\n");
}

#[test]
fn test_run_api_exposes_failure_stages() {
    let invalid = SourceFile::new("let x =");
    assert!(matches!(eel::run(&invalid.0), Err(eel::RunError::Parse)));
    assert!(matches!(
        eel::run(invalid.0.with_extension("missing")),
        Err(eel::RunError::Source { .. })
    ));
    let runtime_error = SourceFile::new("missing_variable");
    assert!(matches!(
        eel::run(&runtime_error.0),
        Err(eel::RunError::Runtime)
    ));
    let lex_error = SourceFile::new("9223372036854775808");
    assert!(matches!(eel::run(&lex_error.0), Err(eel::RunError::Lex)));
}
