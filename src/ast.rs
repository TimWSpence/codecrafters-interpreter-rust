use super::scanner::Token;

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

pub enum Literal {
    Number(f64),
    Str(String),
}
