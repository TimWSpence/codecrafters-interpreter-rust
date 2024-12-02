use super::ast::*;
use super::scanner::{Token, TokenType};
use anyhow::{anyhow, Result};

pub struct Parser {
    tokens: Vec<Token>,
    idx: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, idx: 0 }
    }

    pub fn parse(&self) -> Result<Expr> {
        self.primary()
    }

    fn current(&self) -> Option<&Token> {
        self.tokens.get(self.idx)
    }

    fn next(&self) -> Option<&Token> {
        self.tokens.get(self.idx + 1)
    }

    fn advance(&mut self) {
        self.idx += 1
    }

    fn primary(&self) -> Result<Expr> {
        let c = self.current();
        match c {
            Some(c) => match c.token_type {
                TokenType::True => Ok(Expr::Literal {
                    value: Literal::Boolean(true),
                }),
                TokenType::False => Ok(Expr::Literal {
                    value: Literal::Boolean(false),
                }),
                TokenType::Nil => Ok(Expr::Literal {
                    value: Literal::Nil,
                }),
                t => Err(anyhow!("Couldn't parse primary - found {}", t)),
            },
            None => Err(anyhow!("Couldn't parse primary - reached EOF")),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(tokens: Vec<Token>) -> Result<Expr> {
        let parser = Parser::new(tokens);
        parser.parse()
    }

    fn token(token_type: TokenType, lexeme: &str, literal: Option<Literal>) -> Token {
        Token {
            token_type,
            lexeme: lexeme.to_string(),
            literal,
            line: 1,
        }
    }

    #[test]
    fn parse_true() {
        assert_eq!(
            parse(vec![token(TokenType::True, "true", None)]).unwrap(),
            Expr::Literal {
                value: Literal::Boolean(true)
            }
        )
    }

    #[test]
    fn parse_false() {
        assert_eq!(
            parse(vec![token(TokenType::False, "false", None)]).unwrap(),
            Expr::Literal {
                value: Literal::Boolean(false)
            }
        )
    }

    #[test]
    fn parse_nil() {
        assert_eq!(
            parse(vec![token(TokenType::Nil, "nil", None)]).unwrap(),
            Expr::Literal {
                value: Literal::Nil
            }
        )
    }
}
