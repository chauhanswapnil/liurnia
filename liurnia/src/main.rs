use liurnia_frontend::parse;
use std::io::Write;
use std::{env, io};

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();

    if args.len() == 0 {
        println!("Welcome to Liurnia REPL. Press Ctrl+D to exit.");

        loop {
            print!("liurnia> ");
            io::stdout().flush().unwrap();
            let mut input = String::new();
            if io::stdin().read_line(&mut input).is_ok() {
                let input = input.trim();
                if input.is_empty() {
                    continue; // Skip empty lines
                }
                parse(&input);
            } else {
                break; // Exit on error
            }
        }
    } else if args.len() == 1 {
        let path = args.first().unwrap();
        // Check if the file path has a ".li" extension
        if !path.ends_with(".li") {
            eprintln!("Error: Liurnia needs a .li file.");
            return;
        }
        // Read file content
        match std::fs::read_to_string(path) {
            Ok(data) => {
                parse(&data);
            }
            Err(err) => {
                eprintln!("Error reading file: {}", err);
            }
        }
    } else {
        eprintln!("Usage: liurnia [filepath.li]");
        return;
    }
}
