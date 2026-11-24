use std::{env, fmt::Display, process::ExitCode};

fn main() -> ExitCode {
    let mut args = env::args_os().skip(1);

    let Some(filename) = args.next() else {
        return exit_status(eel::run_repl().map(|()| ExitCode::SUCCESS));
    };

    if args.next().is_some() {
        return exit_status(Err(
            "Expected exactly one file argument\nUsage: eel [<path-to-file>]",
        ));
    }

    exit_status(eel::run_file(filename).map(ExitCode::from))
}

fn exit_status(result: Result<ExitCode, impl Display>) -> ExitCode {
    match result {
        Ok(code) => code,
        Err(error) => {
            eprintln!("{error}");
            ExitCode::FAILURE
        }
    }
}
