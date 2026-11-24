//! File and interactive runners, including diagnostics and I/O effects.

mod file;
mod io_action;
mod repl;
mod source;

pub use file::RunError;
pub(super) use file::run as run_file;
pub(super) use repl::run as run_repl;
