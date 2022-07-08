use std::{env, process};

/// Open the file at location given as the only shell/command line parameter. No
/// security checks about the file path. Parse the input, calculate and output
/// one critical path etc.
fn main() {
    let args = env::args().collect::<Vec<_>>();
    if args.len() != 2 {
        panic!(
            "Invoked with {} argument(s). Please invoke with exactly one argument.",
            args.len() - 1
        );
    }

    let result = schedule_tasks::process_file_path(&args[1]);
    match result {
        Err(e) => {
            eprintln!("{}", e);
            process::exit(1)
        }
        Ok(tasks_map) => {
            println!("{}", tasks_map)
        }
    }
}
