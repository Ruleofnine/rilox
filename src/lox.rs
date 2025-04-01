use crate::ast_printer::print_ast;
use crate::error::ErrorReporter;
use crate::parser::Parser;
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
        self.run(&source);
        Ok(())
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
        let mut scanner = Scanner::new(input.to_string(), self);
        let tokens = scanner.scan_tokens();
        debug!("Tokenizer compeleted");
        let mut parser = Parser::new(tokens, self);
        debug!("Parsing compeleted");
        match parser.parse() {
            Ok(expr) => {
                debug!("Printing AST");
                println!("{expr}");
            }
            Err(_) => {
                eprintln!("Parsing failed. See above for error.");
            }
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
