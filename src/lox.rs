use crate::error::ErrorReporter;
use crate::expr::LiteralValue;
use crate::interpreter::{Interpreter, evaluate};
use crate::parser::Parser;
use crate::scanner::Scanner;
use crate::stmt::Stmt;
use ansi_term::{
    Color::{Red, Yellow},
    Style,
};
use anyhow::Result;
use log::debug;
use std::fs;
use std::io::{self, Write};

pub struct Lox {
    had_error: bool,
    had_runtime_error: bool,
}
impl Default for Lox {
    fn default() -> Self {
        Self::new()
    }
}
impl Lox {
    pub fn new() -> Self {
        Lox {
            had_error: false,
            had_runtime_error: false,
        }
    }
    pub fn run_file(&mut self, path: String) -> Result<()> {
        debug!("Source File: {}", path);
        let source = fs::read_to_string(path)?;
        self.run(&source, false);
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
            if input.is_empty() || input == "exit" {
                debug!("Exiting REPL");
                break;
            }
            self.run(input, true);
        }
        Ok(())
    }
    pub fn run(&mut self, input: &str, repl_mode: bool) {
        let mut scanner = Scanner::new(input.to_string(), self);
        let tokens = scanner.scan_tokens();
        debug!("Tokenizer completed");
        let mut parser = Parser::new(tokens, self);
        let mut interpreter = Interpreter::new();
        debug!("Parsing started");
        while !parser.is_at_end() {
            debug!("Starting new parse");
            if !repl_mode {
                debug!("File mode");
                match parser.parse() {
                    Ok(stmt) => {
                        if let Err(err) = interpreter.execute(&stmt) {
                            let reset = Style::new();
                            eprintln!(
                                "{} {} {}{}",
                                Red.paint("Runtime"),
                                Yellow.paint("Error:"),
                                reset.prefix(),
                                err
                            );
                        }
                    }
                    Err(_) => {
                        parser.syncronize_expr();
                    }
                }
            } else {
                match parser.expression() {
                    Ok(expr) => match evaluate(&expr) {
                        Ok(value) => println!("{}", value),
                        Err(err) => eprintln!("Runtime error: {}", err),
                    },
                    Err(_) => parser.syncronize_expr(),
                }
            }
        }
    }
}
impl ErrorReporter for Lox {
    fn report(&mut self, line: usize, message: &str) {
        eprintln!(
            "{} {} {}",
            Red.paint(format!("[line {}]", line)),
            Yellow.paint("Error:"),
            message
        );

        self.had_error = true;
    }
    fn had_error(&self) -> bool {
        self.had_error
    }
}
