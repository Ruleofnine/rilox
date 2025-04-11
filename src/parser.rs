use crate::error::ErrorReporter;
use crate::expr::{Expr, LiteralValue};
use crate::stmt::Stmt;
use crate::token::{Literal, Token};
use crate::token_type::TokenType;
use log::{debug, error};

/*
program        → delcaration* EOF

declaration    → varDecl | statement

varDecl        → "var" IDENTIFIER ( "=" expression )? ";"

statement      → exprStmt
               | printStmt
               | block
               | ifStmt
               | whileStmt
               | mutationStmt


exprStmt       → expression ";"
printStmt      → "print" expression ";"
block          → "{" delcaration* "}"
ifStmt         → "if" "(" expression ")"  block ( else" block )?
whileStmt      → "while" "(" expression ")" statement
mutationStmt   → IDENTIFIER ( "+=" | "-=" ) expression ";"
                 | ( "++" | "--" ) IDENTIFIER ";"
                 | IDENTIFIER ( "++" | "--" ) ";"

expression     → assignment

assignment     → IDENTIFIER "=" assignment
               | comma

comma          → ternary ( "," ternary )*

ternary        → equality ( "?" ternary ":" ternary )?

equality       → comparison ( ( "==" | "!=" ) comparison )*
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )*
term           → factor ( ( "-" | "+" ) factor )*
factor         → power ( ( "/" | "*" | "%") power )*
power          → unary ( "^" power )?

unary          → ( "!" | "-" ) unary | primary

primary        → NUMBER | STRING | "true" | "false" | "nil" | IDENTIFIER | "(" expression ")"
*/

#[derive(Debug)]
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
    pub fn parse(&mut self) -> Result<Stmt, ParseError> {
        self.declaration()
    }
    pub fn declaration(&mut self) -> Result<Stmt, ParseError> {
        if self.match_any(&[TokenType::Var]) {
            self.var_declaration()
        } else {
            self.statement()
        }
    }
    pub fn if_block(&mut self) -> Result<Stmt, ParseError> {
        self.consume(
            TokenType::LeftParen,
            "Expected '(' after keyword 'if' and before statement.",
        )?;
        let condition = self.expression()?;
        self.consume(
            TokenType::RightParen,
            "Expected ')' after keyword 'if' and before statement.",
        )?;
        self.consume(
            TokenType::LeftBrace,
            "Expected '{' after expression and before statement.",
        )?;
        let if_branch = Box::new(self.block()?);
        let mut else_branch = None;
        if self.match_any(&[TokenType::Else]) {
            //when we call self.block next it is going to consume the ending '}'
            self.consume(
                TokenType::LeftBrace,
                "Expected '{' after 'else' and before statement.",
            )?;
            else_branch = Some(Box::new(self.block()?));
        }
        Ok(Stmt::If {
            condition,
            if_branch,
            else_branch,
        })
    }
    pub fn while_block(&mut self) -> Result<Stmt, ParseError> {
        self.consume(
            TokenType::LeftParen,
            "Expected '(' after keyword 'if' and before statement.",
        )?;
        let condition = self.expression()?;
        self.consume(
            TokenType::RightParen,
            "Expected ')' after keyword 'if' and before statement.",
        )?;
        self.consume(
            TokenType::LeftBrace,
            "Expected '{' after expression and before statement.",
        )?;
        let expr = self.block()?;
        Ok(Stmt::While(condition, Box::new(expr)))
    }
    pub fn block(&mut self) -> Result<Stmt, ParseError> {
        let mut stmts = Vec::new();
        while !self.is_at_end() && !self.check(TokenType::RightBrace) {
            let decl = self.declaration()?;
            stmts.push(decl)
        }
        self.consume(
            TokenType::RightBrace,
            "Expected '}' or value to delcare block statement",
        )?;
        Ok(Stmt::Block(stmts))
    }
    pub fn var_declaration(&mut self) -> Result<Stmt, ParseError> {
        if self.match_any(&[TokenType::Identifier]) {
            let token = self.previous().clone();
            let expr = match self.peek().token_type {
                TokenType::Equal => {
                    self.advance();
                    Ok(Some(self.expression()?))
                }
                TokenType::Semicolon => Ok(None),
                _ => {
                    self.error(
                        &self.peek().clone(),
                        "Expected ';' or value to delcare varaible",
                    );
                    Err(ParseError)
                }
            };
            self.consume(TokenType::Semicolon, "Expect ';' after value")?;
            Ok(Stmt::Var(token, expr?))
        } else {
            self.error(
                &self.peek().clone(),
                "Expected Identifier after variable declaration",
            );
            Err(ParseError)
        }
    }
    pub fn mutation_stmt(&mut self) -> Result<Stmt, ParseError> {
        let starting_token = self.peek().clone();
        match starting_token.token_type {
            TokenType::PlusPlus | TokenType::MinusMinus => {
                self.advance();
                self.consume(
                    TokenType::Identifier,
                    &format!("Expected 'Identifier' after {}", starting_token.lexeme),
                )?;
                let variable = self.previous().clone();
            }
            equal_ops => {}
        }

        todo!()
        /*Ok(Stmt::Mutation {
                            name: self.previous().clone(),
                            operator: TokenType::Plus,
                            value: 1,
                        }
        )
                */
    }
    pub fn statement(&mut self) -> Result<Stmt, ParseError> {
        //TODO this seems odd that we advance in each, but keepeing it flexible
        //might be smart
        match self.peek().token_type {
            TokenType::Print => {
                self.advance();
                let expr = self.expression()?;
                self.consume(TokenType::Semicolon, "Expect ';' after value")?;
                Ok(Stmt::Print(expr))
            }
            TokenType::LeftBrace => {
                self.advance();
                self.block()
            }
            TokenType::If => {
                self.advance();
                self.if_block()
            }
            TokenType::While => {
                self.advance();
                self.while_block()
            }
            TokenType::Identifier
            | TokenType::PlusPlus
            | TokenType::MinusMinus
            | TokenType::MinusEqual
            | TokenType::PlusEqual => self.mutation_stmt(),
            _ => {
                let expr = self.expression()?;
                self.consume(TokenType::Semicolon, "Expect ';' after value")?;
                Ok(Stmt::Expression(expr))
            }
        }
    }
    pub fn expression(&mut self) -> Result<Expr, ParseError> {
        self.comma()
    }
    pub fn comma(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.ternary()?;
        while self.match_any(&[TokenType::Comma]) {
            let right = self.ternary()?;
            expr = Expr::Comma(Box::new(expr), Box::new(right));
        }

        Ok(expr)
    }
    pub fn ternary(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.assignment()?;

        if self.match_any(&[TokenType::Question]) {
            let expr_on_true = self.ternary().unwrap_or_else(|_| {
                let token = self.peek().clone();
                self.error(&token, "Missing expression in true branch of ternary.");
                Expr::Error
            });
            if self.match_any(&[TokenType::Colon]) {
                let expr_on_false = self.ternary().unwrap_or_else(|_| {
                    let token = self.peek().clone();
                    self.error(&token, "Missing expression in false branch of ternary.");
                    Expr::Error
                });
                expr = Expr::Ternary(
                    Box::new(expr), // Condition Expr
                    Box::new(expr_on_true),
                    Box::new(expr_on_false),
                );
            } else {
                let token = self.peek().clone();
                self.error(&token, "Missing ':' in Ternary Oprerator");
                return Err(ParseError);
            }
        }
        Ok(expr)
    }
    pub fn assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.equality()?;
        if self.match_any(&[TokenType::Equal]) {
            let eqauls = self.previous().clone();
            let value = self.assignment()?;
            if let Expr::Variable(name) = expr {
                return Ok(Expr::Assign(name, Box::new(value)));
            }
            self.error(&eqauls, "Invalid assignment target.");
            return Err(ParseError);
        }
        Ok(expr)
    }
    pub fn equality(&mut self) -> Result<Expr, ParseError> {
        self.parse_binary_expr(
            &[TokenType::EqualEqual, TokenType::BangEqual],
            Parser::comparison,
        )
    }
    pub fn comparison(&mut self) -> Result<Expr, ParseError> {
        self.parse_binary_expr(
            &[
                TokenType::Less,
                TokenType::LessEqual,
                TokenType::Greater,
                TokenType::GreaterEqual,
            ],
            Parser::term,
        )
    }
    pub fn term(&mut self) -> Result<Expr, ParseError> {
        self.parse_binary_expr(&[TokenType::Plus, TokenType::Minus], Parser::factor)
    }

    pub fn factor(&mut self) -> Result<Expr, ParseError> {
        self.parse_binary_expr(
            &[TokenType::Star, TokenType::Slash, TokenType::Percent],
            Parser::exponents,
        )
    }
    pub fn exponents(&mut self) -> Result<Expr, ParseError> {
        self.parse_binary_expr(&[TokenType::Caret], Parser::unary)
    }
    pub fn unary(&mut self) -> Result<Expr, ParseError> {
        if self.match_any(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous().clone();
            let right = self.unary()?;

            return Ok(Expr::Unary(operator, Box::new(right)));
        }
        self.primary()
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
                if let Some(Literal::String(s)) = token.literal {
                    Ok(Expr::Literal(LiteralValue::String(s)))
                } else {
                    self.error(&token, "Expected String Literal.");
                    Err(ParseError)
                }
            }
            TokenType::LeftParen => {
                self.advance(); // consume '('
                let expr = self.expression()?;
                self.consume(TokenType::RightParen, "Expect ')' after expression.")?;
                Ok(Expr::Grouping(Box::new(expr)))
            }
            TokenType::Identifier => {
                let token = self.advance().clone();
                Ok(Expr::Variable(token))
            }
            _ => {
                let token = self.peek().clone();
                error!("{}", token);
                self.error(&token, "Expected expression.");
                Err(ParseError)
            }
        }
    }
    // ------ Helper functions

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
    pub fn consume(&mut self, expected: TokenType, message: &str) -> Result<Token, ParseError> {
        if self.check(expected) {
            return Ok(self.advance().clone());
        }
        let token = self.peek().clone();
        self.error(&token, message);
        Err(ParseError)
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
            "at end".to_string()
        } else {
            format!("at '{}'", token.lexeme)
        };
        self.reporter
            .report(token.line, &format!("{} {}", location, message));
    }
    pub fn syncronize_expr(&mut self) {
        self.advance();
        while !self.is_at_end() {
            self.advance();
        }
    }

    pub fn syncronize_stmt(&mut self) {
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

    fn parse_binary_expr<F>(
        &mut self,
        operators: &[TokenType],
        parse_rhs: F,
    ) -> Result<Expr, ParseError>
    where
        F: Fn(&mut Self) -> Result<Expr, ParseError>,
    {
        // Parse the left-hand side normally
        let mut expr = parse_rhs(self)?;

        // If LHS is already an error, bail out early (avoid cascading errors)
        if matches!(expr, Expr::Error) {
            return Ok(expr);
        }

        // Parse as many operator-right-hand pairs as we have
        while self.match_any(operators) {
            if matches!(expr, Expr::Error) {
                break;
            }
            let operator = self.previous().clone();
            let rhs = parse_rhs(self).unwrap_or(Expr::Error);
            expr = Expr::Binary(Box::new(expr), operator, Box::new(rhs));
        }

        Ok(expr)
    }
}
