use std::{env, process::ExitCode};

fn main() -> ExitCode {
    let mut args = env::args_os().skip(1);
    let Some(filename) = args.next() else {
        eprintln!("Expected file argument\nUsage: eel <path-to-file>");
        return ExitCode::FAILURE;
    };
    if args.next().is_some() {
        eprintln!("Expected exactly one file argument\nUsage: eel <path-to-file>");
        return ExitCode::FAILURE;
    }

    match eel::run(filename) {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            eprintln!("{error}");
            ExitCode::FAILURE
        }
    }
}
