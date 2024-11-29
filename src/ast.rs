use core::fmt;

use super::scanner::{Token, TokenType};

pub enum Stmt {
    Block {
        statements: Vec<Stmt>,
    },
    Class {
        name: Token,
        superclass: Option<Token>,
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
        else_branch: Option<Box<Stmt>>,
    },
    Print {
        expr: Expr,
    },
    Return {
        keyword: Token,
        expr: Option<Expr>,
    },
    Var {
        name: Token,
        value: Option<Expr>,
    },
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Block { statements } => write!(
                f,
                "(block {})",
                statements
                    .iter()
                    .map(|s| { s.to_string() })
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            Stmt::Class {
                name,
                superclass,
                methods,
            } => {
                write!(f, "(class {}", name.lexeme)?;
                if let Some(sc) = superclass {
                    write!(f, " < {}", sc.lexeme)?;
                }
                for m in methods {
                    write!(f, " {}", m)?;
                }
                write!(f, ")")
            }
            Stmt::Expression { expr } => write!(f, "; {}", expr),
            Stmt::Function { value } => write!(f, "{}", value),
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => match else_branch {
                Some(eb) => write!(f, "(if-else {} {} {})", condition, then_branch, eb),
                None => write!(f, "(if {} {})", condition, then_branch),
            },
            Stmt::Print { expr } => write!(f, "(print {})", expr),
            Stmt::Return { keyword: _, expr } => match expr {
                Some(e) => write!(f, "(return {})", e),
                None => write!(f, "(return)"),
            },
            Stmt::Var { name, value } => match value {
                Some(v) => write!(f, "(var {} = {})", name.lexeme, v),
                None => write!(f, "(var {})", name.lexeme),
            },
            Stmt::While { condition, body } => write!(f, "(while {} {})", condition, body),
        }
    }
}

pub struct Function {
    name: Token,
    params: Vec<Token>,
    body: Vec<Stmt>,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "(fun {}({}) {})",
            self.name.lexeme,
            self.params
                .iter()
                .map(|p| { p.lexeme.clone() })
                .collect::<Vec<String>>()
                .join(" "),
            self.body
                .iter()
                .map(|s| { s.to_string() })
                .collect::<Vec<String>>()
                .join("")
        )
    }
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

    #[test]
    fn print_print() {
        assert_eq!(
            format!(
                "{}",
                Stmt::Print {
                    expr: Expr::Literal {
                        value: Literal::Number(1f64)
                    }
                }
            ),
            "(print 1.0)"
        )
    }

    #[test]
    fn print_return_nothing() {
        assert_eq!(
            format!(
                "{}",
                Stmt::Return {
                    keyword: token("return"),
                    expr: None
                }
            ),
            "(return)"
        )
    }

    #[test]
    fn print_return_something() {
        assert_eq!(
            format!(
                "{}",
                Stmt::Return {
                    keyword: token("return"),
                    expr: Some(Expr::Literal {
                        value: Literal::Number(1f64)
                    })
                }
            ),
            "(return 1.0)"
        )
    }

    #[test]
    fn print_function() {
        assert_eq!(
            format!(
                "{}",
                Stmt::Function {
                    value: Function {
                        name: token("foo"),
                        params: vec![token("bar")],
                        body: vec![Stmt::Print {
                            expr: Expr::Variable { name: token("bar") }
                        }]
                    }
                }
            ),
            "(fun foo(bar) (print bar))"
        )
    }

    #[test]
    fn print_expression_stmt() {
        assert_eq!(
            format!(
                "{}",
                Stmt::Expression {
                    expr: Expr::Variable { name: token("foo") }
                }
            ),
            "; foo"
        )
    }

    #[test]
    fn print_var_unassigned() {
        assert_eq!(
            format!(
                "{}",
                Stmt::Var {
                    name: token("foo"),
                    value: None
                }
            ),
            "(var foo)"
        )
    }

    #[test]
    fn print_var_assigned() {
        assert_eq!(
            format!(
                "{}",
                Stmt::Var {
                    name: token("foo"),
                    value: Some(Expr::Literal {
                        value: Literal::Number(1f64)
                    })
                }
            ),
            "(var foo = 1.0)"
        )
    }

    #[test]
    fn print_class_super() {
        assert_eq!(
            format!(
                "{}",
                Stmt::Class {
                    name: token("foo"),
                    superclass: Some(token("bar")),
                    methods: vec![Function {
                        name: token("baz"),
                        params: vec![token("bar")],
                        body: vec![Stmt::Print {
                            expr: Expr::Variable { name: token("bar") }
                        }]
                    }]
                }
            ),
            "(class foo < bar (fun baz(bar) (print bar)))"
        )
    }

    #[test]
    fn print_class_no_super() {
        assert_eq!(
            format!(
                "{}",
                Stmt::Class {
                    name: token("foo"),
                    superclass: None,
                    methods: vec![Function {
                        name: token("baz"),
                        params: vec![token("bar")],
                        body: vec![Stmt::Print {
                            expr: Expr::Variable { name: token("bar") }
                        }]
                    }]
                }
            ),
            "(class foo (fun baz(bar) (print bar)))"
        )
    }

    #[test]
    fn print_while() {
        assert_eq!(
            format!(
                "{}",
                Stmt::While {
                    condition: Expr::Binary {
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
                    },
                    body: Box::new(Stmt::Print {
                        expr: Expr::Literal {
                            value: Literal::Str("Hello world".to_string())
                        }
                    })
                }
            ),
            "(while (1.0 < 2.0) (print \"Hello world\"))"
        )
    }

    #[test]
    fn print_block() {
        assert_eq!(
            format!(
                "{}",
                Stmt::Block {
                    statements: vec![Stmt::Var {
                        name: token("foo"),
                        value: None
                    }]
                }
            ),
            "(block (var foo))"
        )
    }

    #[test]
    fn print_if() {
        assert_eq!(
            format!(
                "{}",
                Stmt::If {
                    condition: Expr::Binary {
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
                    },
                    then_branch: Box::new(Stmt::Print {
                        expr: Expr::Literal {
                            value: Literal::Str("Hello world".to_string())
                        }
                    }),
                    else_branch: None
                }
            ),
            "(if (1.0 < 2.0) (print \"Hello world\"))"
        )
    }

    #[test]
    fn print_if_else() {
        assert_eq!(
            format!(
                "{}",
                Stmt::If {
                    condition: Expr::Binary {
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
                    },
                    then_branch: Box::new(Stmt::Print {
                        expr: Expr::Literal {
                            value: Literal::Str("Hello world".to_string())
                        }
                    }),
                    else_branch: Some(Box::new(Stmt::Print {
                        expr: Expr::Literal {
                            value: Literal::Str("Hi world".to_string())
                        }
                    }))
                }
            ),
            "(if-else (1.0 < 2.0) (print \"Hello world\") (print \"Hi world\"))"
        )
    }
}
