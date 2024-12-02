use std::any;

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

    pub fn parse(&mut self) -> Result<Expr> {
        self.expression()
    }

    fn expression(&mut self) -> Result<Expr> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr> {
        self.comparison()
    }

    fn comparison(&mut self) -> Result<Expr> {
        self.term()
    }

    fn term(&mut self) -> Result<Expr> {
        self.factor()
    }

    fn factor(&mut self) -> Result<Expr> {
        self.unary()
    }

    fn unary(&mut self) -> Result<Expr> {
        self.primary()
    }

    fn primary(&mut self) -> Result<Expr> {
        let c = self.current();
        match c {
            Some(c) => {
                if let Some(l) = &c.literal {
                    Ok(Expr::Literal { value: l.clone() })
                } else {
                    match c.token_type {
                        TokenType::True => Ok(Expr::Literal {
                            value: Literal::Boolean(true),
                        }),
                        TokenType::False => Ok(Expr::Literal {
                            value: Literal::Boolean(false),
                        }),
                        TokenType::Nil => Ok(Expr::Literal {
                            value: Literal::Nil,
                        }),
                        TokenType::LeftParen => {
                            self.advance();
                            let expr = self.expression()?;
                            self.advance();
                            self.consume(
                                TokenType::RightParen,
                                "Couldn't find matching right paren",
                            )?;
                            Ok(Expr::Grouping {
                                expr: Box::new(expr),
                            })
                        }
                        _ => Err(anyhow!("Couldn't parse primary - unexpected token {}", c)),
                    }
                }
            }
            None => Err(anyhow!("Couldn't parse primary - reached EOF")),
        }
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

    fn consume(&mut self, expected: TokenType, msg: &'static str) -> Result<()> {
        match self.current() {
            Some(c) => {
                if c.token_type == expected {
                    Ok(())
                } else {
                    Err(anyhow!(msg))
                }
            }
            None => Err(anyhow!(msg)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(tokens: Vec<Token>) -> Result<Expr> {
        let mut parser = Parser::new(tokens);
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
            parse(vec![token(
                TokenType::True,
                "true",
                Some(Literal::Boolean(true))
            )])
            .unwrap(),
            Expr::Literal {
                value: Literal::Boolean(true)
            }
        )
    }

    #[test]
    fn parse_false() {
        assert_eq!(
            parse(vec![token(
                TokenType::False,
                "false",
                Some(Literal::Boolean(false))
            )])
            .unwrap(),
            Expr::Literal {
                value: Literal::Boolean(false)
            }
        )
    }

    #[test]
    fn parse_nil() {
        assert_eq!(
            parse(vec![token(TokenType::Nil, "nil", Some(Literal::Nil))]).unwrap(),
            Expr::Literal {
                value: Literal::Nil
            }
        )
    }

    #[test]
    fn parse_number() {
        assert_eq!(
            parse(vec![token(
                TokenType::Number,
                "123.45",
                Some(Literal::Number(123.45f64))
            )])
            .unwrap(),
            Expr::Literal {
                value: Literal::Number(123.45f64)
            }
        )
    }

    #[test]
    fn parse_string() {
        assert_eq!(
            parse(vec![token(
                TokenType::String,
                "foo",
                Some(Literal::Str("foo".to_string()))
            )])
            .unwrap(),
            Expr::Literal {
                value: Literal::Str("foo".to_string())
            }
        )
    }

    #[test]
    fn parse_group() {
        assert_eq!(
            parse(vec![
                token(TokenType::LeftParen, "(", None),
                token(TokenType::Number, "123", Some(Literal::Number(123f64))),
                token(TokenType::RightParen, ")", None)
            ])
            .unwrap(),
            Expr::Grouping {
                expr: Box::new(Expr::Literal {
                    value: Literal::Number(123f64)
                })
            }
        )
    }
}
