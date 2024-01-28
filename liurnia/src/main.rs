use liurnia_frontend::parse;
use std::env;

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();

    if args.len() != 1 {
        eprintln!("Usage: liurnia [filepath.li]");
        return;
    }

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
}
