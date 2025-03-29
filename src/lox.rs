use crate::error::ErrorReporter;
use crate::scanner::{self, Scanner};
use anyhow::Result;
use log::debug;
use std::fs;
use std::io::{self, Write};

pub struct Lox {
    had_error: bool,
    had_runtime_error: bool,
}
impl Lox {
    pub fn new() -> Lox {
        Lox {
            had_error: false,
            had_runtime_error: false,
        }
    }
    pub fn run_file(&mut self, path: String) -> Result<()> {
        debug!("Source File: {}", path);
        let source = fs::read_to_string(path)?;
        let scanner = Scanner::new(source, self);
        todo!();
    }
    pub fn had_error(&mut self) -> bool {
        self.had_error
    }
    pub fn run_prompt(&mut self) -> Result<()> {
        debug!("Running prompt!");
        let mut input = String::new();
        let stdin = io::stdin();
        loop {
            print!("> ");
            io::stdout().flush().unwrap();
            input.clear();
            if stdin.read_line(&mut input).is_err() {
                self.report(0, "Error reading input.");
                //TODO this seems sloppy
                std::process::exit(1)
            }

            let input = input.trim();
            if input.is_empty() {
                break;
            }
            self.run(input);
        }
        Ok(())
    }
    pub fn run(&mut self, input: &str) {
        let mut scanner = Scanner::new(input.to_string(),self);
        let tokens = scanner.scan_tokens();
        for i in tokens{
            println!("{}",i);
        }
    }
}
impl ErrorReporter for Lox {
    fn report(&mut self, line: usize, message: &str) {
        eprintln!("[line {}] Error: {}", line, message);
        self.had_error = true;
    }
    fn had_error(&self) -> bool {
        self.had_error
    }
}
