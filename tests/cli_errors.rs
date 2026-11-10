use std::{
    fs,
    path::{Path, PathBuf},
    process::{Command, Output},
    sync::atomic::{AtomicU64, Ordering},
    time::{SystemTime, UNIX_EPOCH},
};

static NEXT_FILE: AtomicU64 = AtomicU64::new(0);

struct SourceFile(PathBuf);

impl SourceFile {
    fn new(source: impl AsRef<[u8]>) -> Self {
        let timestamp = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();
        let path = std::env::temp_dir().join(format!(
            "eel cli errors 🦀-{}-{}-{}.eel",
            std::process::id(),
            timestamp.as_nanos(),
            NEXT_FILE.fetch_add(1, Ordering::Relaxed),
        ));
        fs::write(&path, source).unwrap();
        Self(path)
    }

    fn run(&self) -> Output {
        cli().arg(&self.0).output().unwrap()
    }
}

impl Drop for SourceFile {
    fn drop(&mut self) {
        let _ = fs::remove_file(&self.0);
    }
}

fn cli() -> Command {
    Command::new(env!("CARGO_BIN_EXE_eel"))
}

fn assert_failure(output: &Output, diagnostic: &str) {
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_eq!(output.status.code(), Some(1), "{output:?}");
    assert!(stderr.contains(diagnostic), "{stderr}");
    assert!(!stderr.contains("panicked"), "{stderr}");
    assert!(output.stdout.is_empty(), "{output:?}");
}

fn assert_source_diagnostic(output: &Output, path: &Path) {
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains(&*path.to_string_lossy()), "{stderr}");
}

#[test]
fn missing_file_argument_fails_without_panicking() {
    let output = cli().output().unwrap();
    assert_failure(&output, "Expected file argument");
    assert!(String::from_utf8_lossy(&output.stderr).contains("Usage:"));
}

#[test]
fn extra_file_argument_fails_before_evaluation() {
    let file = SourceFile::new("let _ = print(99). 0");
    let output = cli().arg(&file.0).arg("unexpected").output().unwrap();
    assert_failure(&output, "Expected exactly one file argument");
}

#[test]
fn missing_source_file_fails_without_panicking() {
    let file = SourceFile::new("0");
    let missing = file.0.with_extension("missing");
    let output = cli().arg(&missing).output().unwrap();
    assert_failure(&output, "Failed to read source file");
    assert_source_diagnostic(&output, &missing);
}

#[test]
fn directory_source_fails_without_panicking() {
    let output = cli().arg(std::env::temp_dir()).output().unwrap();
    assert_failure(&output, "Failed to read source file");
}

#[test]
fn non_utf8_source_fails_without_panicking() {
    let file = SourceFile::new([0xff, 0xfe]);
    let output = file.run();
    assert_failure(&output, "Failed to read source file");
    assert_source_diagnostic(&output, &file.0);
}

#[test]
fn syntax_error_fails_with_source_diagnostic() {
    let file = SourceFile::new("let x =");
    let output = file.run();
    assert_failure(&output, "Parsing failed");
    assert_source_diagnostic(&output, &file.0);
    assert!(String::from_utf8_lossy(&output.stderr).contains("expected"));
}

#[test]
fn empty_source_fails() {
    let output = SourceFile::new("").run();
    assert_failure(&output, "Parsing failed");
}

#[test]
fn recovered_ast_is_not_evaluated() {
    let file = SourceFile::new("let _ = print(99). { let x = }");
    let output = file.run();
    assert_failure(&output, "Parsing failed");
    assert_source_diagnostic(&output, &file.0);
}

#[test]
fn runtime_error_fails_with_source_diagnostic() {
    let file = SourceFile::new("missing_variable");
    let output = file.run();
    assert_failure(&output, "Variable `missing_variable` not found");
    assert_source_diagnostic(&output, &file.0);
}

#[test]
fn type_error_fails_with_source_diagnostic() {
    let file = SourceFile::new("1 + unit");
    let output = file.run();
    assert_failure(&output, "Cannot add");
    assert_source_diagnostic(&output, &file.0);
}

#[test]
fn division_by_zero_fails_with_source_diagnostic() {
    for source in ["1 / 0", "1.0 / 0.0"] {
        let file = SourceFile::new(source);
        let output = file.run();
        assert_failure(&output, "Cannot divide");
        assert_source_diagnostic(&output, &file.0);
    }
}

#[test]
fn success_preserves_returned_output_and_zero_status() {
    let output = SourceFile::new("42").run();
    assert_eq!(output.status.code(), Some(0), "{output:?}");
    assert_eq!(String::from_utf8_lossy(&output.stdout), "Returned: 42\n");
    assert!(output.stderr.is_empty(), "{output:?}");
}

#[test]
fn success_preserves_program_output() {
    let output = SourceFile::new("let _ = print(99). 0").run();
    assert_eq!(output.status.code(), Some(0), "{output:?}");
    assert_eq!(String::from_utf8_lossy(&output.stdout), "99\nReturned: 0\n");
    assert!(output.stderr.is_empty(), "{output:?}");
}

#[test]
fn stderr_program_output_does_not_count_as_failure() {
    let output = SourceFile::new("let _ = eprint(99). 0").run();
    assert_eq!(output.status.code(), Some(0), "{output:?}");
    assert_eq!(String::from_utf8_lossy(&output.stdout), "Returned: 0\n");
    assert_eq!(String::from_utf8_lossy(&output.stderr), "99\n");
}

#[test]
fn run_api_exposes_source_parse_and_runtime_failure() {
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
}
