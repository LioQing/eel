use std::{env, fmt::Display, process::ExitCode};

fn main() -> ExitCode {
    let mut args = env::args_os().skip(1);

    let Some(filename) = args.next() else {
        return exit_status(eel::run_repl());
    };

    if args.next().is_some() {
        return exit_status(Err(
            "Expected exactly one file argument\nUsage: eel [<path-to-file>]",
        ));
    }

    exit_status(eel::run(filename))
}

fn exit_status(result: Result<(), impl Display>) -> ExitCode {
    match result {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            eprintln!("{error}");
            ExitCode::FAILURE
        }
    }
}
