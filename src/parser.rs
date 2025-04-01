use crate::error::ErrorReporter;
use crate::expr::{Expr, LiteralValue};
use crate::token::Token;
use crate::token_type::TokenType;
pub struct ParseError;
pub struct Parser<'a> {
    tokens: Vec<Token>,
    current: usize,
    reporter: &'a mut dyn ErrorReporter,
}
impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>, reporter: &'a mut dyn ErrorReporter) -> Parser<'a> {
        Parser {
            tokens,
            current: 0,
            reporter,
        }
    }
    pub fn parse(&mut self) -> Result<Expr, ParseError> {
        self.expression()
    }
    pub fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }
    pub fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::Eof
    }
    pub fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }
    pub fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            //move current ahead "consuming" the next token
            self.current += 1
        }
        // return the previously consumed token
        self.previous()
    }
    pub fn expression(&mut self) -> Result<Expr, ParseError> {
        self.equality()
    }
    pub fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.comparison()?;
        while self.match_any(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.previous().clone();
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }
    pub fn comparison(&mut self) -> Result<Expr, ParseError> {
        //left expr
        let mut expr = self.term()?;
        while self.match_any(&[
            TokenType::Less,
            TokenType::LessEqual,
            TokenType::Greater,
            TokenType::GreaterEqual,
        ]) {
            let operator = self.previous().clone();
            let right = self.term()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right))
        }
        Ok(expr)
    }

    pub fn consume(&mut self, expected: TokenType, message: &str) -> Result<Token, ParseError> {
        if self.check(expected) {
            return Ok(self.advance().clone());
        }
        let token = self.peek().clone();
        self.error(&token, message);
        Err(ParseError)
    }

    pub fn primary(&mut self) -> Result<Expr, ParseError> {
        match self.peek().token_type {
            TokenType::False => {
                self.advance();
                Ok(Expr::Literal(LiteralValue::Bool(false)))
            }
            TokenType::True => {
                self.advance();
                Ok(Expr::Literal(LiteralValue::Bool(true)))
            }
            TokenType::Nil => {
                self.advance();
                Ok(Expr::Literal(LiteralValue::Nil))
            }
            TokenType::Number => {
                let token = self.advance().clone();
                let value = token.lexeme.parse::<f64>().map_err(|_| {
                    self.error(&token, "Invalid number format.");
                    ParseError
                })?;
                Ok(Expr::Literal(LiteralValue::Number(value)))
            }
            TokenType::String => {
                let token = self.advance().clone();
                let s = token.lexeme.clone();
                Ok(Expr::Literal(LiteralValue::String(s)))
            }
            TokenType::LeftParen => {
                self.advance(); // consume '('
                let expr = self.expression()?;
                self.consume(TokenType::RightParen, "Expect ')' after expression.")?;
                Ok(Expr::Grouping(Box::new(expr)))
            }
            _ => {
                let token = self.peek().clone();
                self.error(&token, "Expected expression.");
                Err(ParseError)
            }
        }
    }
    pub fn unary(&mut self) -> Result<Expr, ParseError> {
        if self.match_any(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            return Ok(Expr::Unary(operator, Box::new(right)));
        }
        self.primary()
    }
    pub fn factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.unary()?;
        while self.match_any(&[TokenType::Slash, TokenType::Star]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right))
        }
        Ok(expr)
    }
    pub fn term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.factor()?;
        while self.match_any(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous().clone();
            let right = self.factor()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right))
        }
        Ok(expr)
    }
    pub fn match_any(&mut self, types: &[TokenType]) -> bool {
        for token_type in types {
            if self.check(*token_type) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check(&self, token_type: TokenType) -> bool {
        !self.is_at_end() && self.peek().token_type == token_type
    }
    pub fn error(&mut self, token: &Token, message: &str) {
        let location = if token.token_type == TokenType::Eof {
            " at end".to_string()
        } else {
            format!("at '{}'", token.lexeme)
        };
        self.reporter
            .report(token.line, &format!("{} {}", location, message));
    }
    pub fn syncronize(&mut self) {
        self.advance();
        while !self.is_at_end() {
            if self.previous().token_type == TokenType::Semicolon {
                return;
            }
            match self.peek().token_type {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => {}
            }
            self.advance();
        }
    }
}
