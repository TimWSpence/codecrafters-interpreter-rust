use anyhow::Result;
use std::env;
use std::fs;
use std::io::{self, Write};

mod scanner;

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        writeln!(io::stderr(), "Usage: {} tokenize <filename>", args[0])?;
        todo!("Replace with clap")
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            let input = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });

            let mut valid = true;
            let tokens = match scanner::scan(&input) {
                Err(ts) => {
                    valid = false;
                    ts
                }
                Ok(ts) => ts,
            };
            for t in tokens {
                println!("{}", t);
            }
            if !valid {
                std::process::exit(65)
            } else {
                Ok(())
            }
        }
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command)?;
            todo!("Replace with clap")
        }
    }
}
