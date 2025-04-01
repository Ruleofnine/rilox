use crate::expr::{Expr, LiteralValue};
use log::debug;
use std::fmt;
pub fn print_ast(expr: &Expr) -> String {
    debug!("PRINTING AST");
    match expr {
        Expr::Literal(v) => match v {
            LiteralValue::Number(n) => n.to_string(),
            LiteralValue::Bool(b) => b.to_string(),
            LiteralValue::Nil => "nil".to_string(),
            LiteralValue::String(s) => format!("\"{}\"", s),
        },
        Expr::Grouping(inner) => format!("(group {})", print_ast(inner)),
        Expr::Unary(op, right) => format!("({} {})", op.lexeme, print_ast(right)),
        Expr::Binary(left, op, right) => {
            format!("({} {} {})", print_ast(left), op.lexeme, print_ast(right))
        }
    }
}
impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", print_ast(self))
    }
}
