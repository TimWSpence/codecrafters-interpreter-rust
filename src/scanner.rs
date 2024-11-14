use anyhow::anyhow;
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

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenType::LeftParen => write!(f, "LEFT_PAREN"),
            TokenType::RightParen => write!(f, "RIGHT_PAREN"),
            TokenType::LeftBrace => write!(f, "LEFT_BRACE"),
            TokenType::RightBrace => write!(f, "RIGHT_BRACE"),
            TokenType::Comma => write!(f, "COMMA"),
            TokenType::Dot => write!(f, "DOT"),
            TokenType::Minus => write!(f, "MINUS"),
            TokenType::Plus => write!(f, "PLUS"),
            TokenType::SemiColon => write!(f, "SEMICOLON"),
            TokenType::Slash => write!(f, "SLASH"),
            TokenType::Star => write!(f, "STAR"),
            TokenType::Bang => write!(f, "BANG"),
            TokenType::BangEqual => write!(f, "BANG_EQUAL"),
            TokenType::Equal => write!(f, "EQUAL"),
            TokenType::EqualEqual => write!(f, "EQUAL_EQUAL"),
            TokenType::Greater => write!(f, "GREATER"),
            TokenType::GreaterEqual => write!(f, "GREATER_EQUAL"),
            TokenType::Less => write!(f, "LESS"),
            TokenType::LessEqual => write!(f, "LESS_EQUAL"),
            TokenType::Identifier => todo!(),
            TokenType::String => write!(f, "STRING"),
            TokenType::Number => write!(f, "NUMBER"),
            TokenType::And => todo!(),
            TokenType::Class => todo!(),
            TokenType::Else => todo!(),
            TokenType::False => todo!(),
            TokenType::Fun => todo!(),
            TokenType::For => todo!(),
            TokenType::If => todo!(),
            TokenType::Nil => todo!(),
            TokenType::Or => todo!(),
            TokenType::Print => todo!(),
            TokenType::Return => todo!(),
            TokenType::Super => todo!(),
            TokenType::This => todo!(),
            TokenType::True => todo!(),
            TokenType::Var => todo!(),
            TokenType::While => todo!(),
            TokenType::EOF => write!(f, "EOF"),
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
            "{} {} {}",
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

pub fn scan(input: &str) -> Result<Vec<Token>, Vec<Token>> {
    let mut line: u32 = 1;
    let mut res = vec![];
    let mut error = false;

    let mut add_token =
        |token_type: TokenType, lexeme: &str, literal: Option<Literal>, line: u32| {
            res.push(Token {
                token_type,
                lexeme: lexeme.to_string(),
                literal,
                line,
            });
        };

    let mut chars = input.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            '(' => add_token(TokenType::LeftParen, "(", None, line),
            ')' => add_token(TokenType::RightParen, ")", None, line),
            '{' => add_token(TokenType::LeftBrace, "{", None, line),
            '}' => add_token(TokenType::RightBrace, "}", None, line),
            ',' => add_token(TokenType::Comma, ",", None, line),
            '.' => add_token(TokenType::Dot, ".", None, line),
            '-' => add_token(TokenType::Minus, "-", None, line),
            '+' => add_token(TokenType::Plus, "+", None, line),
            ';' => add_token(TokenType::SemiColon, ";", None, line),
            '*' => add_token(TokenType::Star, "*", None, line),
            '!' => match chars.peek() {
                Some('=') => {
                    chars.next();
                    add_token(TokenType::BangEqual, "!=", None, line)
                }
                _ => add_token(TokenType::Bang, "!", None, line),
            },
            '=' => match chars.peek() {
                Some('=') => {
                    chars.next();
                    add_token(TokenType::EqualEqual, "==", None, line)
                }
                _ => add_token(TokenType::Equal, "=", None, line),
            },
            '>' => match chars.peek() {
                Some('=') => {
                    chars.next();
                    add_token(TokenType::GreaterEqual, ">=", None, line)
                }
                _ => add_token(TokenType::Greater, ">", None, line),
            },
            '<' => match chars.peek() {
                Some('=') => {
                    chars.next();
                    add_token(TokenType::LessEqual, "<=", None, line)
                }
                _ => add_token(TokenType::Less, "<", None, line),
            },
            '\n' => line += 1,
            c if c.is_whitespace() => {}
            c if c.is_numeric() => {
                let mut value = String::new();
                let mut found_decimal = false;
                let mut trailing_dot = false;
                value.push(c);
                while let Some(c) = chars.peek() {
                    if c.is_numeric() {
                        value.push(*c);
                        chars.next();
                    } else if !found_decimal && *c == '.' {
                        chars.next();
                        if let Some(c) = chars.peek() {
                            if c.is_numeric() {
                                found_decimal = true;
                                value.push('.');
                                value.push(*c);
                                chars.next();
                            } else {
                                trailing_dot = true;
                                break;
                            }
                        } else {
                            trailing_dot = true;
                            break;
                        }
                    } else {
                        break;
                    }
                }
                match value.parse::<f64>() {
                    Ok(f) => add_token(TokenType::Number, &value, Some(Literal::Number(f)), line),
                    Err(_) => {
                        error = true;
                        eprintln!("Could not parse number {}", value);
                    }
                }
                if trailing_dot {
                    add_token(TokenType::Dot, ".", None, line)
                }
            }
            '/' => match chars.peek() {
                Some('/') => {
                    for c in chars.by_ref() {
                        if c == '\n' {
                            line += 1;
                            break;
                        }
                    }
                }
                _ => add_token(TokenType::Slash, "/", None, line),
            },
            '"' => {
                let mut s = String::new();
                let mut matching = false;
                let mut lines = 0;
                for c in chars.by_ref() {
                    if c == '"' {
                        matching = true;
                        break;
                    }
                    s.push(c);
                    if c == '\n' {
                        lines += 1;
                    }
                }
                if matching {
                    add_token(
                        TokenType::String,
                        &format!("\"{}\"", s.clone()),
                        Some(Literal::Str(s)),
                        line,
                    );
                    line += lines;
                } else {
                    error = true;
                    eprintln!("[line {}] Error: Unterminated string.", line);
                }
            }
            c => {
                error = true;
                eprintln!("[line {}] Error: Unexpected character: {}", line, c);
            }
        }
    }

    res.push(Token {
        token_type: TokenType::EOF,
        lexeme: "".to_string(),
        literal: None,
        line,
    });
    if error {
        Err(res)
    } else {
        Ok(res)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn token(token_type: TokenType, lexeme: &str, literal: Option<Literal>) -> Token {
        Token {
            token_type,
            lexeme: lexeme.to_string(),
            literal,
            line: 1,
        }
    }

    fn eof() -> Token {
        Token {
            token_type: TokenType::EOF,
            lexeme: "".to_string(),
            literal: None,
            line: 1,
        }
    }

    fn eof_at_line(line: u32) -> Token {
        Token {
            token_type: TokenType::EOF,
            lexeme: "".to_string(),
            literal: None,
            line,
        }
    }

    #[test]
    fn empty_string() {
        assert_eq!(scan("").unwrap(), vec![eof()])
    }

    #[test]
    fn left_paren() {
        assert_eq!(
            scan("(").unwrap(),
            vec![token(TokenType::LeftParen, "(", None), eof()]
        )
    }

    #[test]
    fn right_paren() {
        assert_eq!(
            scan(")").unwrap(),
            vec![token(TokenType::RightParen, ")", None), eof()]
        )
    }

    #[test]
    fn left_brace() {
        assert_eq!(
            scan("{").unwrap(),
            vec![token(TokenType::LeftBrace, "{", None), eof()]
        )
    }

    #[test]
    fn right_brace() {
        assert_eq!(
            scan("}").unwrap(),
            vec![token(TokenType::RightBrace, "}", None), eof()]
        )
    }

    #[test]
    fn comma() {
        assert_eq!(
            scan(",").unwrap(),
            vec![token(TokenType::Comma, ",", None), eof()]
        )
    }

    #[test]
    fn dot() {
        assert_eq!(
            scan(".").unwrap(),
            vec![token(TokenType::Dot, ".", None), eof()]
        )
    }

    #[test]
    fn minus() {
        assert_eq!(
            scan("-").unwrap(),
            vec![token(TokenType::Minus, "-", None), eof()]
        )
    }

    #[test]
    fn plus() {
        assert_eq!(
            scan("+").unwrap(),
            vec![token(TokenType::Plus, "+", None), eof()]
        )
    }

    #[test]
    fn semicolon() {
        assert_eq!(
            scan(";").unwrap(),
            vec![token(TokenType::SemiColon, ";", None), eof()]
        )
    }

    #[test]
    fn star() {
        assert_eq!(
            scan("*").unwrap(),
            vec![token(TokenType::Star, "*", None), eof()]
        )
    }

    #[test]
    fn equal() {
        assert_eq!(
            scan("=").unwrap(),
            vec![token(TokenType::Equal, "=", None), eof()]
        )
    }

    #[test]
    fn equal_equal() {
        assert_eq!(
            scan("==").unwrap(),
            vec![token(TokenType::EqualEqual, "==", None), eof()]
        )
    }

    #[test]
    fn greater() {
        assert_eq!(
            scan(">").unwrap(),
            vec![token(TokenType::Greater, ">", None), eof()]
        )
    }

    #[test]
    fn greater_equal() {
        assert_eq!(
            scan(">=").unwrap(),
            vec![token(TokenType::GreaterEqual, ">=", None), eof()]
        )
    }

    #[test]
    fn less() {
        assert_eq!(
            scan("<").unwrap(),
            vec![token(TokenType::Less, "<", None), eof()]
        )
    }

    #[test]
    fn less_equal() {
        assert_eq!(
            scan("<=").unwrap(),
            vec![token(TokenType::LessEqual, "<=", None), eof()]
        )
    }

    #[test]
    fn bang() {
        assert_eq!(
            scan("!").unwrap(),
            vec![token(TokenType::Bang, "!", None), eof()]
        )
    }

    #[test]
    fn bang_equal() {
        assert_eq!(
            scan("!=").unwrap(),
            vec![token(TokenType::BangEqual, "!=", None), eof()]
        )
    }

    #[test]
    fn slash() {
        assert_eq!(
            scan("/").unwrap(),
            vec![token(TokenType::Slash, "/", None), eof()]
        )
    }

    #[test]
    fn comment() {
        assert_eq!(
            scan("//foo\n/").unwrap(),
            vec![
                Token {
                    token_type: TokenType::Slash,
                    lexeme: "/".to_string(),
                    literal: None,
                    line: 2
                },
                eof_at_line(2)
            ]
        )
    }

    #[test]
    fn comment_at_end() {
        assert_eq!(scan("//foo\n").unwrap(), vec![eof_at_line(2)])
    }

    #[test]
    fn whitespace() {
        assert_eq!(scan(" \t").unwrap(), vec![eof()])
    }

    #[test]
    fn string_literal() {
        assert_eq!(
            scan("\"foo bar\"").unwrap(),
            vec![
                token(
                    TokenType::String,
                    "\"foo bar\"",
                    Some(Literal::Str("foo bar".to_string()))
                ),
                eof()
            ]
        )
    }

    #[test]
    fn empty_string_literal() {
        assert_eq!(
            scan("\"\"").unwrap(),
            vec![
                token(
                    TokenType::String,
                    "\"\"",
                    Some(Literal::Str("".to_string()))
                ),
                eof()
            ]
        )
    }

    #[test]
    fn multiline_string_literal() {
        assert_eq!(
            scan("\"foo\nbar\"").unwrap(),
            vec![
                token(
                    TokenType::String,
                    "\"foo\nbar\"",
                    Some(Literal::Str("foo\nbar".to_string()))
                ),
                eof_at_line(2)
            ]
        )
    }

    #[test]
    fn integer_number() {
        assert_eq!(
            scan("123").unwrap(),
            vec![
                token(TokenType::Number, "123", Some(Literal::Number(123f64))),
                eof()
            ]
        )
    }

    #[test]
    fn float_number() {
        assert_eq!(
            scan("123.45").unwrap(),
            vec![
                token(
                    TokenType::Number,
                    "123.45",
                    Some(Literal::Number(123.45f64))
                ),
                eof()
            ]
        )
    }

    #[test]
    fn integer_then_dot() {
        assert_eq!(
            scan("123.").unwrap(),
            vec![
                token(TokenType::Number, "123", Some(Literal::Number(123f64))),
                token(TokenType::Dot, ".", None),
                eof()
            ]
        )
    }

    #[test]
    fn float_then_dot() {
        assert_eq!(
            scan("123.45.").unwrap(),
            vec![
                token(
                    TokenType::Number,
                    "123.45",
                    Some(Literal::Number(123.45f64))
                ),
                token(TokenType::Dot, ".", None),
                eof()
            ]
        )
    }

    #[test]
    fn dot_then_float() {
        assert_eq!(
            scan(".123.45").unwrap(),
            vec![
                token(TokenType::Dot, ".", None),
                token(
                    TokenType::Number,
                    "123.45",
                    Some(Literal::Number(123.45f64))
                ),
                eof()
            ]
        )
    }
}
