// Each integration-test crate uses a different subset of these helpers.
#![allow(dead_code)]

use std::{
    fs::{self, OpenOptions},
    io::Write,
    path::{Path, PathBuf},
    process::{Command, Output},
    sync::atomic::{AtomicU64, Ordering},
    time::{SystemTime, UNIX_EPOCH},
};

static NEXT_FILE: AtomicU64 = AtomicU64::new(0);

pub struct SourceFile(pub PathBuf);

impl SourceFile {
    pub fn new(source: impl AsRef<[u8]>) -> Self {
        let timestamp = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();
        let path = std::env::temp_dir().join(format!(
            "eel tests 🦀-{}-{}-{}.eel",
            std::process::id(),
            timestamp.as_nanos(),
            NEXT_FILE.fetch_add(1, Ordering::Relaxed),
        ));
        let mut file = OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&path)
            .unwrap();
        let source_file = Self(path);
        file.write_all(source.as_ref()).unwrap();
        source_file
    }

    pub fn run(&self) -> Output {
        run_file(&self.0)
    }
}

impl Drop for SourceFile {
    fn drop(&mut self) {
        let _ = fs::remove_file(&self.0);
    }
}

pub fn cli() -> Command {
    Command::new(env!("CARGO_BIN_EXE_eel"))
}

pub fn run_file(path: impl AsRef<Path>) -> Output {
    cli()
        .arg(path.as_ref())
        .output()
        .expect("failed to run eel")
}

#[track_caller]
pub fn assert_success(output: &Output, expected_stdout: &str, expected_stderr: &str) {
    assert_eq!(output.status.code(), Some(0), "{output:?}");
    assert_eq!(
        std::str::from_utf8(&output.stdout).unwrap(),
        expected_stdout,
        "{output:?}"
    );
    assert_eq!(
        std::str::from_utf8(&output.stderr).unwrap(),
        expected_stderr,
        "{output:?}"
    );
}

#[track_caller]
pub fn assert_failure(output: &Output, diagnostic: &str) {
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_eq!(output.status.code(), Some(1), "{output:?}");
    assert!(stderr.contains(diagnostic), "{stderr}");
    assert!(!stderr.contains("panicked"), "{stderr}");
    assert!(!stderr.contains("Returned:"), "{stderr}");
    assert!(output.stdout.is_empty(), "{output:?}");
}

#[track_caller]
pub fn assert_source_diagnostic(output: &Output, path: &Path) {
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains(&*path.to_string_lossy()), "{stderr}");
}

#[track_caller]
pub fn assert_returned(source: &str, expected: i64) {
    let output = SourceFile::new(source).run();
    assert_success(&output, &format!("Returned: {expected}\n"), "");
}
