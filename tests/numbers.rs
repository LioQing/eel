use std::{
    fs::{self, OpenOptions},
    io::Write,
    process::Command,
    sync::atomic::{AtomicU64, Ordering},
};

fn run_src(src: &str) -> String {
    static NEXT_FILE: AtomicU64 = AtomicU64::new(0);
    let path = std::env::temp_dir().join(format!(
        "eel-numbers-{}-{}.eel",
        std::process::id(),
        NEXT_FILE.fetch_add(1, Ordering::Relaxed),
    ));
    let mut file = OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(&path)
        .unwrap();
    file.write_all(src.as_bytes()).unwrap();
    drop(file);

    let output = Command::new(env!("CARGO_BIN_EXE_eel")).arg(&path).output();
    fs::remove_file(&path).unwrap();
    let output = output.unwrap();

    // Numeric diagnostics should not depend on the CLI's exit-status or output-stream policy.
    format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    )
}

#[test]
fn numeric_failures_are_language_errors() {
    for (src, message) in [
        ("9223372036854775808", "Invalid integer literal"),
        ("-9223372036854775809", "Invalid integer literal"),
        ("9223372036854775807 + 1", "Integer overflow"),
        ("-9223372036854775808 - 1", "Integer overflow"),
        ("9223372036854775807 * 2", "Integer overflow"),
        ("-9223372036854775808 / -1", "Integer overflow"),
        ("-9223372036854775808 % -1", "Integer overflow"),
        ("-9223372036854775808 / 0", "Cannot divide"),
        ("-9223372036854775808 % 0", "Cannot modulo"),
    ] {
        let output = run_src(src);
        assert!(output.contains(message), "{src}: {output}");
        assert!(!output.contains("panicked at"), "{src}: {output}");
        assert!(!output.contains("Returned:"), "{src}: {output}");
    }
}

#[test]
fn invalid_integer_prevents_evaluation() {
    let output = run_src("let _ = print(123456789). 9223372036854775808");
    assert!(output.contains("Invalid integer literal"), "{output}");
    assert!(!output.lines().any(|line| line.trim() == "123456789"));
    assert!(!output.contains("Returned:"), "{output}");
}

#[test]
fn integer_limits_are_valid_results() {
    for (src, expected) in [
        ("9223372036854775807 + 0", "Returned: 9223372036854775807"),
        ("-9223372036854775808 / 1", "Returned: -9223372036854775808"),
    ] {
        let output = run_src(src);
        assert!(output.contains(expected), "{src}: {output}");
    }
}
