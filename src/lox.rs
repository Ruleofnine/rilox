use crate::error::ErrorReporter;
use crate::expr::LiteralValue;
use crate::interpreter::Interpreter;
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

const EXIT_SUCCESS: i32 = 0;
const EXIT_COMPILE_ERROR: i32 = 65;
const EXIT_RUNTIME_ERROR: i32 = 70;
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
    pub fn run_file(&mut self, path: String) -> Result<i32> {
        debug!("Source File: {}", path);
        let source = fs::read_to_string(path)?;
        let exit_code = self.run(&source, false)?;
        Ok(exit_code)
    }
    pub fn had_error(&mut self) -> bool {
        self.had_error
    }

    pub fn had_runtime_error(&mut self) -> bool {
        self.had_error
    }
    pub fn run_prompt(&mut self) -> Result<i32> {
        debug!("Running prompt!");
        let mut input = String::new();
        let stdin = io::stdin();
        loop {
            //TODO make sure you can add ";" at the end of line without erroring in REPL
            //TODO same thing with print
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
            let _ = self.run(input, true);
        }
        Ok(0)
    }
    pub fn run(&mut self, input: &str, repl_mode: bool) -> Result<i32> {
        let mut scanner = Scanner::new(input.to_string(), self);
        let tokens = scanner.scan_tokens();
        debug!("Tokenizer completed");
        let mut parser = Parser::new(tokens, self);
        let mut interpreter = Interpreter::new();
        debug!("Parsing started");
        while !parser.is_at_end() {
            debug!("Starting new parse");
            if repl_mode {
                match parser.expression() {
                    Ok(expr) => match interpreter.evaluate(&expr) {
                        Ok(value) => println!("{}", value),
                        Err(err) => eprintln!("Runtime error: {}", err),
                    },
                    Err(_) => parser.syncronize_stmt(),
                }
            } else {
                match parser.parse() {
                    Ok(stmt) => {
                        if let Err(err) = interpreter.statement(&stmt) {
                            debug!("had error");
                            let reset = Style::new();
                            eprintln!(
                                "{} {} {}{}",
                                Red.paint("Runtime"),
                                Yellow.paint("Error:"),
                                reset.prefix(),
                                err
                            );
                            return Ok(EXIT_RUNTIME_ERROR);
                        }
                    }
                    Err(_) => {
                        parser.syncronize_stmt();
                        return Ok(EXIT_COMPILE_ERROR);
                    }
                }
            }
        }
        Ok(EXIT_SUCCESS)
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
