use crate::error::ErrorReporter;
use crate::token::{Literal, Token};
use crate::token_type::TokenType;
use log::debug;
pub struct Scanner<'a> {
    source: String,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    reporter: &'a mut dyn ErrorReporter,
}
impl<'a> Scanner<'a> {
    pub fn new(source: String, reporter: &'a mut dyn ErrorReporter) -> Self {
        Scanner {
            source,
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
            reporter,
        }
    }
    pub fn error(&mut self, message: &str) {
        self.reporter.report(self.line, message);
    }
    pub fn scan_tokens(&mut self) -> Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }
        self.start = self.current;
        self.add_token(TokenType::Eof);
        std::mem::take(&mut self.tokens)
    }

    pub fn add_token(&mut self, token_type: TokenType) {
        self.add_token_with_literal(token_type, None);
    }
    pub fn add_token_with_literal(&mut self, token_type: TokenType, literal: Option<Literal>) {
        let text = &self.source[self.start..self.current];
        self.tokens
            .push(Token::new(token_type, text.to_string(), literal, self.line));
    }
    fn add_token_if_matches(&mut self, expected: char, matched: TokenType, not_matched: TokenType) {
        let token_type = if self.match_char(expected) {
            matched
        } else {
            not_matched
        };
        self.add_token(token_type);
    }
    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }
    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }

        if self.source[self.current..].chars().next().unwrap() != expected {
            return false;
        }

        // Consume the char
        self.current += expected.len_utf8();
        true
    }
    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source[self.current..].chars().next().unwrap()
        }
    }

    fn peek_next(&self) -> char {
        let mut iter = self.source[self.current..].chars();
        iter.next(); // Skip current char
        iter.next().unwrap_or('\0') // Return next char or '\0' if at end
    }

    pub fn scan_token(&mut self) {
        let c = self.advance();

        match c {
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => self.add_token(TokenType::Minus),
            '+' => self.add_token(TokenType::Plus),
            ';' => self.add_token(TokenType::Semicolon),
            '*' => self.add_token(TokenType::Star),
            '?' => self.add_token(TokenType::Question),
            ':' => self.add_token(TokenType::Colon),
            '!' => self.add_token_if_matches('=', TokenType::BangEqual, TokenType::Bang),
            '=' => self.add_token_if_matches('=', TokenType::EqualEqual, TokenType::Equal),
            '<' => self.add_token_if_matches('=', TokenType::LessEqual, TokenType::Less),
            '>' => self.add_token_if_matches('=', TokenType::GreaterEqual, TokenType::Greater),
            '/' => {
                if self.match_char('/') {
                    // Skip the entire line if you see a line comment
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else if self.match_char('*') {
                    // we are in a muilti-line comment
                    while !(self.is_at_end() || self.peek() == '*' && self.peek_next() == '/') {
                        if self.peek() == '\n' {
                            self.line += 1;
                        }
                        self.advance();
                    }
                    if self.is_at_end() {
                        self.error("Unterminated multi-line string.");
                        return;
                    }
                    self.advance();
                    self.advance();
                } else {
                    self.add_token(TokenType::Slash)
                }
            }
            ' ' | '\r' | '\t' => {} //Ignore whitespace
            '\n' => self.line += 1,
            '"' => self.string(),
            _ => {
                if c.is_ascii_digit() {
                    self.number()
                } else if c.is_ascii_alphabetic() {
                    self.identifier()
                } else {
                    self.error(&format!("Unexpected character: {}", c));
                }
            }
        }
    }
    fn string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }
        if self.is_at_end() {
            self.error("Unterminated string.");
            return;
        }
        self.advance();
        let start = self.start + 1;
        let end = self.current - 1;
        if let Some(value) = self.source.get(start..end) {
            println!("adding string literal token: {}", &value);
            self.add_token_with_literal(
                TokenType::String,
                Some(Literal::String(value.to_string())),
            );
        } else {
            self.error("Invalid UTF-8 in string literal.");
        }
    }
    fn number(&mut self) {
        while self.peek().is_ascii_digit() {
            self.advance();
        }
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            self.advance();
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }
        let num_to_parse = &self.source[self.start..self.current];
        if let Ok(num) = num_to_parse.parse::<f64>() {
            self.add_token_with_literal(TokenType::Number, Some(Literal::Number(num)));
        } else {
            self.error(&format!("Could not parse number: {}", num_to_parse));
        };
    }
    fn identifier(&mut self) {
        while self.peek().is_ascii_alphanumeric() || self.peek() == '_' {
            self.advance();
        }
        let text = &self.source[self.start..self.current];
        let token_type = identifier_type(text);
        self.add_token(token_type);
    }
    pub fn advance(&mut self) -> char {
        let c = self.source[self.current..].chars().next().unwrap();
        self.current += c.len_utf8();
        c
    }
}
fn identifier_type(name: &str) -> TokenType {
    match name {
        "and" => TokenType::And,
        "class" => TokenType::Class,
        "else" => TokenType::Else,
        "false" => TokenType::False,
        "for" => TokenType::For,
        "fun" => TokenType::Fun,
        "if" => TokenType::If,
        "nil" => TokenType::Nil,
        "or" => TokenType::Or,
        "print" => TokenType::Print,
        "return" => TokenType::Return,
        "super" => TokenType::Super,
        "this" => TokenType::This,
        "true" => TokenType::True,
        "var" => TokenType::Var,
        "while" => TokenType::While,
        _ => TokenType::Identifier,
    }
}
