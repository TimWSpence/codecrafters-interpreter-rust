use anyhow::anyhow;
use lazy_static::lazy_static;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
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
            TokenType::Identifier => write!(f, "IDENTIFIER"),
            TokenType::String => write!(f, "STRING"),
            TokenType::Number => write!(f, "NUMBER"),
            TokenType::And => write!(f, "AND"),
            TokenType::Class => write!(f, "CLASS"),
            TokenType::Else => write!(f, "ELSE"),
            TokenType::False => write!(f, "FALSE"),
            TokenType::Fun => write!(f, "FUN"),
            TokenType::For => write!(f, "FOR"),
            TokenType::If => write!(f, "IF"),
            TokenType::Nil => write!(f, "NIL"),
            TokenType::Or => write!(f, "OR"),
            TokenType::Print => write!(f, "PRINT"),
            TokenType::Return => write!(f, "RETURN"),
            TokenType::Super => write!(f, "SUPER"),
            TokenType::This => write!(f, "THIS"),
            TokenType::True => write!(f, "TRUE"),
            TokenType::Var => write!(f, "VAR"),
            TokenType::While => write!(f, "WHILE"),
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
                Some(Literal::Number(n)) => {
                    let mut s = n.to_string();
                    if !s.contains('.') {
                        s.push_str(".0");
                    }
                    s
                }
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

    let mut current: Option<char> = None;
    let chars = RefCell::new(input.chars().peekable());

    let peek = || chars.borrow_mut().peek().cloned();

    let mut advance = || {
        current = chars.borrow_mut().next();
        current
    };

    while let Some(c) = advance() {
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
            '!' => match peek() {
                Some('=') => {
                    advance();
                    add_token(TokenType::BangEqual, "!=", None, line)
                }
                _ => add_token(TokenType::Bang, "!", None, line),
            },
            '=' => match peek() {
                Some('=') => {
                    advance();
                    add_token(TokenType::EqualEqual, "==", None, line)
                }
                _ => add_token(TokenType::Equal, "=", None, line),
            },
            '>' => match peek() {
                Some('=') => {
                    advance();
                    add_token(TokenType::GreaterEqual, ">=", None, line)
                }
                _ => add_token(TokenType::Greater, ">", None, line),
            },
            '<' => match peek() {
                Some('=') => {
                    advance();
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
                while let Some(c) = peek() {
                    if c.is_numeric() {
                        value.push(c);
                        advance();
                    } else if !found_decimal && c == '.' {
                        advance();
                        if let Some(c) = peek() {
                            if c.is_numeric() {
                                found_decimal = true;
                                value.push('.');
                                value.push(c);
                                advance();
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
            c if c.is_alphabetic() || c == '_' => {
                let mut value = String::new();
                value.push(c);
                while let Some(c) = peek() {
                    if c.is_alphanumeric() || c == '_' {
                        value.push(c);
                        advance();
                    } else {
                        break;
                    }
                }
                match KEYWORDS.get(&value) {
                    Some(t) => add_token(*t, &value, None, line),
                    _ => add_token(TokenType::Identifier, &value, None, line),
                }
            }
            '/' => match peek() {
                Some('/') => {
                    while let Some(c) = advance() {
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
                while let Some(c) = advance() {
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

lazy_static! {
    static ref KEYWORDS: HashMap<String, TokenType> = {
        let mut m = HashMap::new();
        m.insert("and".to_string(), TokenType::And);
        m.insert("class".to_string(), TokenType::Class);
        m.insert("else".to_string(), TokenType::Else);
        m.insert("false".to_string(), TokenType::False);
        m.insert("fun".to_string(), TokenType::Fun);
        m.insert("for".to_string(), TokenType::For);
        m.insert("if".to_string(), TokenType::If);
        m.insert("nil".to_string(), TokenType::Nil);
        m.insert("or".to_string(), TokenType::Or);
        m.insert("print".to_string(), TokenType::Print);
        m.insert("return".to_string(), TokenType::Return);
        m.insert("super".to_string(), TokenType::Super);
        m.insert("this".to_string(), TokenType::This);
        m.insert("true".to_string(), TokenType::True);
        m.insert("var".to_string(), TokenType::Var);
        m.insert("while".to_string(), TokenType::While);
        m
    };
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

    #[test]
    fn identifier() {
        assert_eq!(
            scan("foo_").unwrap(),
            vec![token(TokenType::Identifier, "foo_", None), eof()]
        )
    }

    #[test]
    fn keyword_and() {
        assert_eq!(
            scan("and").unwrap(),
            vec![token(TokenType::And, "and", None), eof()]
        )
    }

    #[test]
    fn keyword_class() {
        assert_eq!(
            scan("class").unwrap(),
            vec![token(TokenType::Class, "class", None), eof()]
        )
    }

    #[test]
    fn keyword_else() {
        assert_eq!(
            scan("else").unwrap(),
            vec![token(TokenType::Else, "else", None), eof()]
        )
    }

    #[test]
    fn keyword_false() {
        assert_eq!(
            scan("false").unwrap(),
            vec![token(TokenType::False, "false", None), eof()]
        )
    }

    #[test]
    fn keyword_fun() {
        assert_eq!(
            scan("fun").unwrap(),
            vec![token(TokenType::Fun, "fun", None), eof()]
        )
    }

    #[test]
    fn keyword_for() {
        assert_eq!(
            scan("for").unwrap(),
            vec![token(TokenType::For, "for", None), eof()]
        )
    }

    #[test]
    fn keyword_if() {
        assert_eq!(
            scan("if").unwrap(),
            vec![token(TokenType::If, "if", None), eof()]
        )
    }

    #[test]
    fn keyword_nil() {
        assert_eq!(
            scan("nil").unwrap(),
            vec![token(TokenType::Nil, "nil", None), eof()]
        )
    }

    #[test]
    fn keyword_or() {
        assert_eq!(
            scan("or").unwrap(),
            vec![token(TokenType::Or, "or", None), eof()]
        )
    }

    #[test]
    fn keyword_print() {
        assert_eq!(
            scan("print").unwrap(),
            vec![token(TokenType::Print, "print", None), eof()]
        )
    }

    #[test]
    fn keyword_return() {
        assert_eq!(
            scan("return").unwrap(),
            vec![token(TokenType::Return, "return", None), eof()]
        )
    }

    #[test]
    fn keyword_super() {
        assert_eq!(
            scan("super").unwrap(),
            vec![token(TokenType::Super, "super", None), eof()]
        )
    }

    #[test]
    fn keyword_this() {
        assert_eq!(
            scan("this").unwrap(),
            vec![token(TokenType::This, "this", None), eof()]
        )
    }

    #[test]
    fn keyword_true() {
        assert_eq!(
            scan("true").unwrap(),
            vec![token(TokenType::True, "true", None), eof()]
        )
    }

    #[test]
    fn keyword_var() {
        assert_eq!(
            scan("var").unwrap(),
            vec![token(TokenType::Var, "var", None), eof()]
        )
    }

    #[test]
    fn keyword_while() {
        assert_eq!(
            scan("while").unwrap(),
            vec![token(TokenType::While, "while", None), eof()]
        )
    }
}
