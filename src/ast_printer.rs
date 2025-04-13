use crate::expr::{Expr, LiteralValue};
use std::fmt;
pub fn print_ast(expr: &Expr) -> String {
    match expr {
        Expr::Literal(v) => match v {
            LiteralValue::Number(n) => n.to_string(),
            LiteralValue::Bool(b) => b.to_string(),
            LiteralValue::Nil => "nil".to_string(),
            LiteralValue::String(s) => s.to_string(),
        },
        Expr::Grouping(inner) => format!("(group {})", print_ast(inner)),
        Expr::Unary(op, right) => format!("({} {})", op.lexeme, print_ast(right)),
        Expr::Binary {
            left,
            operator,
            right,
        } => {
            format!(
                "({} {} {})",
                print_ast(left),
                operator.lexeme,
                print_ast(right)
            )
        }
        Expr::Comma(left, right) => {
            format!("({}, {})", print_ast(left), print_ast(right))
        }
        Expr::Variable(name) => name.lexeme.clone(),
        Expr::Assign(name, value) => format!("(assign {} = {})", name.lexeme, print_ast(value)),
        Expr::Logical {
            left,
            operator,
            right,
        } => {
            format!(
                "left: {}, operator: {}, right, {}",
                print_ast(left),
                operator,
                print_ast(right)
            )
        }
        Expr::Ternary {
            condition,
            true_branch,
            false_branch,
        } => {
            format!(
                "({} ? {} : {})",
                print_ast(condition),
                print_ast(true_branch),
                print_ast(false_branch)
            )
        }
        Expr::Mutation {
            name,
            operator,
            value,
        } => {
            format!("(Mutation: {}  {} {})", name, operator, print_ast(value))
        }
        Expr::Error => "<error>".to_string(),
    }
}
impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", print_ast(self))
    }
}
