use crate::expr::LiteralValue;
use crate::token_type::TokenType;
use std::fmt;
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,                //raw data i.e "my string"
    pub literal: Option<LiteralValue>, //parsed data i.e my string ; no quotes since it's parsed
    pub line: usize,
}

impl Token {
    pub fn new(
        token_type: TokenType,
        lexeme: String,
        literal: Option<LiteralValue>,
        line: usize,
    ) -> Self {
        Self {
            token_type,
            lexeme,
            literal,
            line,
        }
    }
}
impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let literal_str = match &self.literal {
            Some(l) => format!("{}", l),
            None => "nil".to_string(),
        };

        if self.lexeme.is_empty() {
            write!(f, "{} {}", self.token_type, literal_str)
        } else {
            write!(f, "{} {} {}", self.token_type, self.lexeme, literal_str)
        }
    }
}
