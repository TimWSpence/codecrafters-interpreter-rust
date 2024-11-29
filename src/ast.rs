use core::fmt;

use super::scanner::{Token, TokenType};

pub enum Stmt {
    Block {
        statements: Vec<Stmt>,
    },
    Class {
        name: Token,
        superclass: Variable,
        methods: Vec<Function>,
    },
    Expression {
        expr: Expr,
    },
    Function {
        value: Function,
    },
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Box<Stmt>,
    },
    Print {
        expr: Expr,
    },
    Return {
        keyword: Token,
        expr: Expr,
    },
    Var {
        value: Variable,
    },
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
}

pub struct Variable {
    name: Token,
    expr: Expr,
}
pub struct Function {
    name: Token,
    params: Vec<Token>,
    body: Vec<Stmt>,
}

pub enum Expr {
    Assign {
        name: Token,
        value: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        paren: Token,
        args: Vec<Expr>,
    },
    Get {
        object: Box<Expr>,
        name: Token,
    },
    Grouping {
        expr: Box<Expr>,
    },
    Literal {
        value: Literal,
    },
    Logical {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Set {
        object: Box<Expr>,
        name: Token,
        value: Box<Expr>,
    },
    Super {
        keyword: Token,
        method: Token,
    },
    This {
        keyword: Token,
    },
    Unary {
        op: Token,
        expr: Box<Expr>,
    },
    Variable {
        name: Token,
    },
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Assign { name, value } => write!(f, "({} = {})", name.lexeme, value),
            Expr::Binary { left, op, right } => write!(f, "({} {} {})", left, op.lexeme, right),
            Expr::Call {
                callee,
                paren: _,
                args,
            } => write!(
                f,
                "(call {} {})",
                callee,
                args.iter()
                    .map(|a| { a.to_string() })
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            Expr::Get { object, name } => write!(f, "(. {} {})", object, name.lexeme),
            Expr::Grouping { expr } => write!(f, "(group {})", expr),
            Expr::Literal { value } => write!(f, "{}", value),
            Expr::Logical { left, op, right } => write!(f, "({} {} {})", left, op.lexeme, right),
            Expr::Set {
                object,
                name,
                value,
            } => write!(f, "(= {} {} {})", object, name.lexeme, value),
            Expr::Super { keyword: _, method } => write!(f, "(super {})", method.lexeme),
            Expr::This { keyword: _ } => write!(f, "this"),
            Expr::Unary { op, expr } => write!(f, "({} {})", op.lexeme, expr),
            Expr::Variable { name } => write!(f, "{}", name.lexeme),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Str(String),
    Number(f64),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Str(s) => write!(f, "\"{}\"", s),
            Literal::Number(n) => {
                let mut s = n.to_string();
                if !s.contains('.') {
                    s.push_str(".0");
                }
                write!(f, "{}", s)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn num_literal(value: f64) -> Token {
        Token {
            token_type: TokenType::Number,
            lexeme: format!("{}", value),
            literal: Some(Literal::Number(value)),
            line: 0,
        }
    }

    fn token(name: &str) -> Token {
        Token {
            token_type: TokenType::Identifier,
            lexeme: name.to_string(),
            literal: None,
            line: 0,
        }
    }

    #[test]
    fn print_assign() {
        assert_eq!(
            format!(
                "{}",
                Expr::Assign {
                    name: token("foo"),
                    value: Box::new(Expr::Literal {
                        value: Literal::Str("bar".to_string())
                    })
                }
            ),
            "(foo = \"bar\")"
        )
    }

    #[test]
    fn print_binary() {
        assert_eq!(
            format!(
                "{}",
                Expr::Binary {
                    left: Box::new(Expr::Literal {
                        value: Literal::Number(1f64)
                    }),
                    op: Token {
                        token_type: TokenType::Less,
                        lexeme: "<".to_string(),
                        literal: None,
                        line: 0
                    },
                    right: Box::new(Expr::Literal {
                        value: Literal::Number(2f64)
                    })
                }
            ),
            "(1.0 < 2.0)"
        )
    }

    #[test]
    fn print_logical() {
        assert_eq!(
            format!(
                "{}",
                Expr::Logical {
                    left: Box::new(Expr::Literal {
                        value: Literal::Number(1f64)
                    }),
                    op: Token {
                        token_type: TokenType::Or,
                        lexeme: "or".to_string(),
                        literal: None,
                        line: 0
                    },
                    right: Box::new(Expr::Literal {
                        value: Literal::Number(2f64)
                    })
                }
            ),
            "(1.0 or 2.0)"
        )
    }

    #[test]
    fn print_get() {
        assert_eq!(
            format!(
                "{}",
                Expr::Get {
                    object: Box::new(Expr::Variable {
                        name: Token {
                            token_type: TokenType::Identifier,
                            lexeme: "foo".to_string(),
                            literal: None,
                            line: 0
                        }
                    }),
                    name: token("bar")
                }
            ),
            "(. foo bar)"
        )
    }

    #[test]
    fn print_this() {
        assert_eq!(
            format!(
                "{}",
                Expr::This {
                    keyword: token("this")
                }
            ),
            "this"
        )
    }

    #[test]
    fn print_group() {
        assert_eq!(
            format!(
                "{}",
                Expr::Grouping {
                    expr: Box::new(Expr::Literal {
                        value: Literal::Number(1f64)
                    })
                }
            ),
            "(group 1.0)"
        )
    }

    #[test]
    fn print_call() {
        assert_eq!(
            format!(
                "{}",
                Expr::Call {
                    callee: Box::new(Expr::Variable { name: token("foo") }),
                    paren: token("()"),
                    args: vec![
                        Expr::Literal {
                            value: Literal::Number(1f64)
                        },
                        Expr::Literal {
                            value: Literal::Str("bar".to_string())
                        }
                    ]
                }
            ),
            "(call foo 1.0 \"bar\")"
        )
    }

    #[test]
    fn print_super() {
        assert_eq!(
            format!(
                "{}",
                Expr::Super {
                    keyword: token("super"),
                    method: token("foo")
                }
            ),
            "(super foo)"
        )
    }

    #[test]
    fn print_set() {
        assert_eq!(
            format!(
                "{}",
                Expr::Set {
                    object: Box::new(Expr::Variable {
                        name: Token {
                            token_type: TokenType::Identifier,
                            lexeme: "foo".to_string(),
                            literal: None,
                            line: 0
                        }
                    }),
                    name: token("bar"),
                    value: Box::new(Expr::Literal {
                        value: Literal::Number(1f64)
                    })
                }
            ),
            "(= foo bar 1.0)"
        )
    }
}
