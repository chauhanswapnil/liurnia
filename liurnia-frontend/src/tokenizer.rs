use super::token::{Token, TokenType};
use crate::position::*;
use std::iter::Peekable;
use std::str;
use std::str::Chars;

struct Scanner<'a> {
    current_position: LineColumn,
    iter: Peekable<Chars<'a>>,
}

impl<'a> Scanner<'a> {
    fn new(buf: &str) -> Scanner {
        Scanner {
            current_position: LineColumn::default(),
            iter: buf.chars().peekable(),
        }
    }

    fn next(&mut self) -> Option<char> {
        let next = self.iter.next();
        if let Some(c) = next {
            self.current_position = self.current_position.shift(c);
        }
        next
    }

    fn peek(&mut self) -> Option<&char> {
        self.iter.peek()
    }

    // Consume next char if it matches
    fn consume_if<F>(&mut self, x: F) -> bool
    where
        F: Fn(char) -> bool,
    {
        if let Some(&ch) = self.peek() {
            if x(ch) {
                self.next().unwrap();
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    // Consume next char if the next one after matches (so .3 eats . if 3 is numeric, for example)
    fn consume_if_next<F>(&mut self, x: F) -> bool
    where
        F: Fn(char) -> bool,
    {
        let mut it = self.iter.clone();
        match it.next() {
            None => return false,
            _ => (),
        }

        if let Some(&ch) = it.peek() {
            if x(ch) {
                self.next().unwrap();
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    fn consume_while<F>(&mut self, x: F) -> Vec<char>
    where
        F: Fn(char) -> bool,
    {
        let mut chars: Vec<char> = Vec::new();
        while let Some(&ch) = self.peek() {
            if x(ch) {
                self.next().unwrap();
                chars.push(ch);
            } else {
                break;
            }
        }
        chars
    }
}

struct Lexer<'a> {
    scanner: Scanner<'a>,
}

impl<'a> Lexer<'a> {
    fn new(buf: &str) -> Lexer {
        Lexer {
            scanner: Scanner::new(buf),
        }
    }

    fn match_token(&mut self, ch: char) -> Option<TokenType> {
        match ch {
            '=' => Some(self.either('=', TokenType::EqualEqual, TokenType::Equal)),
            '!' => Some(self.either('=', TokenType::BangEqual, TokenType::Bang)),
            '<' => Some(self.either('=', TokenType::LessEqual, TokenType::Less)),
            '>' => Some(self.either('=', TokenType::GreaterEqual, TokenType::Greater)),
            ' ' => None,
            '/' => {
                if self.scanner.consume_if(|ch| ch == '/') {
                    self.scanner.consume_while(|ch| ch != '\n');
                    None
                } else {
                    Some(TokenType::Slash)
                }
            }
            '\n' => None,
            '\t' => None,
            '\r' => None,
            '"' => {
                let string: String = self
                    .scanner
                    .consume_while(|ch| ch != '"')
                    .into_iter()
                    .collect();
                // Skip last "
                match self.scanner.next() {
                    None => Some(TokenType::UnterminatedString),
                    _ => Some(TokenType::StringLiteral(string)),
                }
            }
            x if x.is_numeric() => self.number(x),
            x if x.is_ascii_alphabetic() || x == '_' => self.identifier(x),
            '.' => Some(TokenType::Dot),
            '(' => Some(TokenType::LeftParen),
            ')' => Some(TokenType::RightParen),
            '{' => Some(TokenType::LeftBrace),
            '}' => Some(TokenType::RightBrace),
            '[' => Some(TokenType::LeftBracket),
            ']' => Some(TokenType::RightBracket),
            ',' => Some(TokenType::Comma),
            '-' => Some(TokenType::Minus),
            '+' => Some(TokenType::Plus),
            ';' => Some(TokenType::Semicolon),
            '*' => Some(TokenType::Star),
            '%' => Some(TokenType::Percent),
            '|' => Some(TokenType::Pipe),
            c => Some(TokenType::Unknown(c)),
        }
    }

    fn either(&mut self, to_match: char, matched: TokenType, unmatched: TokenType) -> TokenType {
        if self.scanner.consume_if(|ch| ch == to_match) {
            matched
        } else {
            unmatched
        }
    }

    fn keyword(&self, identifier: &str) -> Option<TokenType> {
        use std::collections::HashMap;
        let mut keywords: HashMap<&str, TokenType> = HashMap::new();
        keywords.insert("and", TokenType::And);
        keywords.insert("or", TokenType::Or);
        keywords.insert("if", TokenType::If);
        keywords.insert("else", TokenType::Else);
        keywords.insert("for", TokenType::For);
        keywords.insert("while", TokenType::While);
        keywords.insert("print", TokenType::Print);
        keywords.insert("return", TokenType::Return);
        keywords.insert("var", TokenType::Var);
        keywords.insert("type", TokenType::Type);
        keywords.insert("number", TokenType::Number);
        keywords.insert("string", TokenType::String);
        keywords.insert("bool", TokenType::Bool);
        keywords.insert("true", TokenType::True);
        keywords.insert("false", TokenType::False);
        keywords.insert("use", TokenType::Use);
        keywords.insert("from", TokenType::From);
        keywords.insert("fun", TokenType::Fun);
        keywords.insert("struct", TokenType::Struct);
        keywords.insert("this", TokenType::This);
        keywords.insert("nil", TokenType::Nil);

        keywords.get(identifier).cloned()
    }

    fn identifier(&mut self, x: char) -> Option<TokenType> {
        let mut identifier = String::new();
        identifier.push(x);
        let rest: String = self
            .scanner
            .consume_while(|a| a.is_ascii_alphanumeric() || a == '_')
            .into_iter()
            .collect();
        identifier.push_str(rest.as_str());
        match self.keyword(&identifier) {
            None => Some(TokenType::IdentifierLiteral(identifier)),
            Some(token) => Some(token),
        }
    }

    fn number(&mut self, x: char) -> Option<TokenType> {
        let mut number = String::new();
        number.push(x);
        let num: String = self
            .scanner
            .consume_while(|a| a.is_numeric())
            .into_iter()
            .collect();
        number.push_str(num.as_str());
        if self.scanner.peek() == Some(&'.') && self.scanner.consume_if_next(|ch| ch.is_numeric()) {
            let num2: String = self
                .scanner
                .consume_while(|a| a.is_numeric())
                .into_iter()
                .collect();
            number.push('.');
            number.push_str(num2.as_str());
        }
        Some(TokenType::NumberLiteral(number.parse::<f64>().unwrap()))
    }

    fn tokenize_with_context(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();
        loop {
            let initial_position = self.scanner.current_position;
            let ch = match self.scanner.next() {
                None => break,
                Some(c) => c,
            };
            if let Some(token_type) = self.match_token(ch) {
                tokens.push(Token {
                    token_type,
                    span: Span {
                        start: initial_position,
                        end: self.scanner.current_position,
                    },
                });
            }
        }
        tokens
    }
}

pub fn tokenize_with_context(buf: &str) -> Vec<Token> {
    let mut t = Lexer::new(buf);
    t.tokenize_with_context()
}

#[cfg(test)]
mod tests {
    use super::{tokenize_with_context, Token, TokenType};
    use crate::position::{LineColumn, Span};

    fn tokenize(buf: &str) -> Vec<TokenType> {
        tokenize_with_context(buf)
            .iter()
            .map(|tc| tc.token_type.clone())
            .collect()
    }

    #[test]
    fn test_errors() {
        assert_eq!(
            tokenize("\"Unterminated string..."),
            vec![TokenType::UnterminatedString]
        );
        assert_eq!(tokenize("&"), vec![TokenType::Unknown('&')]);
        assert_eq!(
            tokenize("^^"),
            vec![TokenType::Unknown('^'), TokenType::Unknown('^')]
        );
        assert_eq!(
            tokenize("& \"test"),
            vec![TokenType::Unknown('&'), TokenType::UnterminatedString]
        );
    }

    #[test]
    fn test_tokens() {
        assert_eq!(tokenize(""), vec![]);
        assert_eq!(tokenize("="), vec![TokenType::Equal]);
        assert_eq!(tokenize("=="), vec![TokenType::EqualEqual]);
        assert_eq!(
            tokenize("== = =="),
            vec![
                TokenType::EqualEqual,
                TokenType::Equal,
                TokenType::EqualEqual
            ]
        );
        assert_eq!(tokenize("//test"), vec![]);
        assert_eq!(tokenize("=//test"), vec![TokenType::Equal]);
        assert_eq!(
            tokenize(
                "=//test
        ="
            ),
            vec![TokenType::Equal, TokenType::Equal]
        );
        assert_eq!(
            tokenize("\"test\""),
            vec![TokenType::StringLiteral("test".to_string())]
        );
        assert_eq!(tokenize("12.34"), vec![TokenType::NumberLiteral(12.34)]);
        assert_eq!(
            tokenize("99."),
            vec![TokenType::NumberLiteral(99.00), TokenType::Dot]
        );
        assert_eq!(
            tokenize("99.="),
            vec![
                TokenType::NumberLiteral(99.00),
                TokenType::Dot,
                TokenType::Equal
            ]
        );
        assert_eq!(tokenize("!"), vec![TokenType::Bang]);
        assert_eq!(tokenize("!="), vec![TokenType::BangEqual]);
        assert_eq!(
            tokenize("test"),
            vec![TokenType::IdentifierLiteral("test".to_string())]
        );
        assert_eq!(tokenize("struct"), vec![TokenType::Struct]);
        assert_eq!(tokenize("or"), vec![TokenType::Or]);
        assert_eq!(tokenize("use"), vec![TokenType::Use]);
        assert_eq!(tokenize("["), vec![TokenType::LeftBracket]);
        assert_eq!(tokenize("]"), vec![TokenType::RightBracket]);
    }

    #[test]
    fn test_with_span() {
        let tokens = tokenize_with_context(
            "fun main() {\
        var a = \"100\";
        return a;
        }",
        );
        assert_eq!(
            tokens,
            vec![
                Token {
                    token_type: TokenType::Fun,
                    span: Span {
                        start: LineColumn { line: 1, column: 0 },
                        end: LineColumn { line: 1, column: 3 }
                    }
                },
                Token {
                    token_type: TokenType::IdentifierLiteral("main".to_string()),
                    span: Span {
                        start: LineColumn { line: 1, column: 4 },
                        end: LineColumn { line: 1, column: 8 }
                    }
                },
                Token {
                    token_type: TokenType::LeftParen,
                    span: Span {
                        start: LineColumn { line: 1, column: 8 },
                        end: LineColumn { line: 1, column: 9 }
                    }
                },
                Token {
                    token_type: TokenType::RightParen,
                    span: Span {
                        start: LineColumn { line: 1, column: 9 },
                        end: LineColumn {
                            line: 1,
                            column: 10
                        }
                    }
                },
                Token {
                    token_type: TokenType::LeftBrace,
                    span: Span {
                        start: LineColumn {
                            line: 1,
                            column: 11
                        },
                        end: LineColumn {
                            line: 1,
                            column: 12
                        }
                    }
                },
                Token {
                    token_type: TokenType::Var,
                    span: Span {
                        start: LineColumn {
                            line: 1,
                            column: 12
                        },
                        end: LineColumn {
                            line: 1,
                            column: 15
                        }
                    }
                },
                Token {
                    token_type: TokenType::IdentifierLiteral("a".to_string()),
                    span: Span {
                        start: LineColumn {
                            line: 1,
                            column: 16
                        },
                        end: LineColumn {
                            line: 1,
                            column: 17
                        }
                    }
                },
                Token {
                    token_type: TokenType::Equal,
                    span: Span {
                        start: LineColumn {
                            line: 1,
                            column: 18
                        },
                        end: LineColumn {
                            line: 1,
                            column: 19
                        }
                    }
                },
                Token {
                    token_type: TokenType::StringLiteral("100".to_string()),
                    span: Span {
                        start: LineColumn {
                            line: 1,
                            column: 20
                        },
                        end: LineColumn {
                            line: 1,
                            column: 25
                        }
                    }
                },
                Token {
                    token_type: TokenType::Semicolon,
                    span: Span {
                        start: LineColumn {
                            line: 1,
                            column: 25
                        },
                        end: LineColumn {
                            line: 1,
                            column: 26
                        }
                    }
                },
                Token {
                    token_type: TokenType::Return,
                    span: Span {
                        start: LineColumn { line: 2, column: 8 },
                        end: LineColumn {
                            line: 2,
                            column: 14
                        }
                    }
                },
                Token {
                    token_type: TokenType::IdentifierLiteral("a".to_string()),
                    span: Span {
                        start: LineColumn {
                            line: 2,
                            column: 15
                        },
                        end: LineColumn {
                            line: 2,
                            column: 16
                        }
                    }
                },
                Token {
                    token_type: TokenType::Semicolon,
                    span: Span {
                        start: LineColumn {
                            line: 2,
                            column: 16
                        },
                        end: LineColumn {
                            line: 2,
                            column: 17
                        }
                    }
                },
                Token {
                    token_type: TokenType::RightBrace,
                    span: Span {
                        start: LineColumn { line: 3, column: 8 },
                        end: LineColumn { line: 3, column: 9 }
                    }
                }
            ]
        );
        let tokens = tokenize_with_context("{ var a = \"100");
        assert_eq!(
            tokens,
            vec![
                Token {
                    token_type: TokenType::LeftBrace,
                    span: Span {
                        start: LineColumn { line: 1, column: 0 },
                        end: LineColumn { line: 1, column: 1 }
                    }
                },
                Token {
                    token_type: TokenType::Var,
                    span: Span {
                        start: LineColumn { line: 1, column: 2 },
                        end: LineColumn { line: 1, column: 5 }
                    }
                },
                Token {
                    token_type: TokenType::IdentifierLiteral("a".to_string()),
                    span: Span {
                        start: LineColumn { line: 1, column: 6 },
                        end: LineColumn { line: 1, column: 7 }
                    }
                },
                Token {
                    token_type: TokenType::Equal,
                    span: Span {
                        start: LineColumn { line: 1, column: 8 },
                        end: LineColumn { line: 1, column: 9 }
                    }
                },
                Token {
                    token_type: TokenType::UnterminatedString,
                    span: Span {
                        start: LineColumn {
                            line: 1,
                            column: 10
                        },
                        end: LineColumn {
                            line: 1,
                            column: 14
                        }
                    }
                }
            ]
        );
    }
}
