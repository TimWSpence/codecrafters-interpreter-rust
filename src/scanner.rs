use anyhow::{anyhow, Result};
use std::fmt;

#[derive(Debug, PartialEq, Eq)]
pub enum TokenType {
    // Single character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    SemiColon,
    Slash,
    Star,

    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number,

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    EOF,
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Str(String),
    Number(f64),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Str(s) => write!(f, "{}", s),
            Literal::Number(n) => write!(f, "{}", n),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub literal: Option<Literal>,
    pub line: u32,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?} {} {}",
            self.token_type,
            self.lexeme,
            match &self.literal {
                Some(Literal::Str(s)) => s.to_string(),
                Some(Literal::Number(n)) => n.to_string(),
                None => "null".to_string(),
            }
        )
    }
}

pub fn scan(input: &str) -> Result<Vec<Token>> {
    let mut line: u32 = 0;
    let mut res = vec![];

    let mut add_token =
        |token_type: TokenType, lexeme: &str, literal: Option<Literal>, line: u32| {
            res.push(Token {
                token_type,
                lexeme: lexeme.to_string(),
                literal,
                line,
            });
        };

    let mut chars = input.chars();
    while let Some(c) = chars.next() {
        match c {
            '{' => add_token(TokenType::LeftParen, "{", None, line),
            '}' => add_token(TokenType::RightParen, "}", None, line),
            '\n' => line = line + 1,
            _ => {}
        }
    }

    res.push(Token {
        token_type: TokenType::EOF,
        lexeme: "".to_string(),
        literal: None,
        line: 0,
    });
    Ok(res)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn token(token_type: TokenType, lexeme: &str, literal: Option<Literal>) -> Token {
        Token {
            token_type,
            lexeme: lexeme.to_string(),
            literal,
            line: 0,
        }
    }

    fn eof() -> Token {
        Token {
            token_type: TokenType::EOF,
            lexeme: "".to_string(),
            literal: None,
            line: 0,
        }
    }

    #[test]
    fn empty_string() {
        assert_eq!(scan("").unwrap(), vec![eof()])
    }

    #[test]
    fn left_paren() {
        assert_eq!(
            scan("{").unwrap(),
            vec![token(TokenType::LeftParen, "{", None), eof()]
        )
    }

    #[test]
    fn right_paren() {
        assert_eq!(
            scan("}").unwrap(),
            vec![token(TokenType::RightParen, "}", None), eof()]
        )
    }
}
