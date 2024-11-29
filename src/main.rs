use anyhow::{anyhow, Result};
use clap::*;
use std::fs;

mod ast;
mod parser;
mod scanner;

fn main() -> Result<()> {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Tokenize { file } => {
            let input = fs::read_to_string(file).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", file);
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
        Commands::Parse { file } => {
            let input = fs::read_to_string(file).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", file);
                String::new()
            });

            let tokens = match scanner::scan(&input) {
                Err(_) => Err(anyhow!("Could not lex {}", input)),
                Ok(ts) => Ok(ts),
            }?;

            let p = parser::parse(&tokens)?;

            println!("{}", p);
            Ok(())
        }
    }
}

#[derive(Parser)]
#[command(version, about, long_about = None)]
#[command(propagate_version = true)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Tokenize { file: String },
    Parse { file: String },
}
