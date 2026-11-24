use std::{
    io::{self, IsTerminal},
    path::Path,
};

mod lang;
mod runner;

pub use runner::RunError;

/// Runs a source file and any returned `Io` action, then returns the exit code.
pub fn run_file(filename: impl AsRef<Path>) -> Result<u8, RunError> {
    runner::run_file(filename.as_ref())
}

/// Runs the REPL on standard input, showing prompts only on a terminal.
pub fn run_repl() -> io::Result<()> {
    let stdin = io::stdin();
    let stdout = io::stdout();
    let show_prompt = stdin.is_terminal() && stdout.is_terminal();
    runner::run_repl(stdin.lock(), stdout, io::stderr(), show_prompt)
}
