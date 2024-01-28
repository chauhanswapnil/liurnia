use super::token::Token;
use crate::position::*;
use std::iter::Peekable;
use std::str;
use std::str::Chars;

struct Scanner<'a> {
    current_position: BytePos,
    it: Peekable<Chars<'a>>,
}

impl<'a> Scanner<'a> {
    fn new(buf: &str) -> Scanner {
        Scanner {
            current_position: BytePos::default(),
            it: buf.chars().peekable(),
        }
    }

    fn next(&mut self) -> Option<char> {
        let next = self.it.next();
        if let Some(c) = next {
            self.current_position = self.current_position.shift(c);
        }
        next
    }

    fn peek(&mut self) -> Option<&char> {
        self.it.peek()
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
        let mut it = self.it.clone();
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
    it: Scanner<'a>,
}

impl<'a> Lexer<'a> {
    fn new(buf: &str) -> Lexer {
        Lexer {
            it: Scanner::new(buf),
        }
    }

    fn match_token(&mut self, ch: char) -> Option<Token> {
        match ch {
            '=' => Some(self.either('=', Token::EqualEqual, Token::Equal)),
            '!' => Some(self.either('=', Token::BangEqual, Token::Bang)),
            '<' => Some(self.either('=', Token::LessEqual, Token::Less)),
            '>' => Some(self.either('=', Token::GreaterEqual, Token::Greater)),
            ' ' => None,
            '/' => {
                if self.it.consume_if(|ch| ch == '/') {
                    self.it.consume_while(|ch| ch != '\n');
                    None
                } else {
                    Some(Token::Slash)
                }
            }
            '\n' => None,
            '\t' => None,
            '\r' => None,
            '"' => {
                let string: String = self.it.consume_while(|ch| ch != '"').into_iter().collect();
                // Skip last "
                match self.it.next() {
                    None => Some(Token::UnterminatedString),
                    _ => Some(Token::StringLiteral(string)),
                }
            }
            x if x.is_numeric() => self.number(x),
            x if x.is_ascii_alphabetic() || x == '_' => self.identifier(x),
            '.' => Some(Token::Dot),
            '(' => Some(Token::LeftParen),
            ')' => Some(Token::RightParen),
            '{' => Some(Token::LeftBrace),
            '}' => Some(Token::RightBrace),
            '[' => Some(Token::LeftBracket),
            ']' => Some(Token::RightBracket),
            ',' => Some(Token::Comma),
            '-' => Some(Token::Minus),
            '+' => Some(Token::Plus),
            ';' => Some(Token::Semicolon),
            '*' => Some(Token::Star),
            '%' => Some(Token::Percent),
            '|' => Some(Token::Pipe),
            c => Some(Token::Unknown(c)),
        }
    }

    fn either(&mut self, to_match: char, matched: Token, unmatched: Token) -> Token {
        if self.it.consume_if(|ch| ch == to_match) {
            matched
        } else {
            unmatched
        }
    }

    fn keyword(&self, identifier: &str) -> Option<Token> {
        use std::collections::HashMap;
        let mut keywords: HashMap<&str, Token> = HashMap::new();
        keywords.insert("and", Token::And);
        keywords.insert("or", Token::Or);
        keywords.insert("if", Token::If);
        keywords.insert("else", Token::Else);
        keywords.insert("for", Token::For);
        keywords.insert("while", Token::While);
        keywords.insert("print", Token::Print);
        keywords.insert("var", Token::Var);
        keywords.insert("type", Token::Type);
        keywords.insert("number", Token::Number);
        keywords.insert("string", Token::String);
        keywords.insert("bool", Token::Bool);
        keywords.insert("true", Token::True);
        keywords.insert("false", Token::False);
        keywords.insert("use", Token::Use);
        keywords.insert("export", Token::Export);
        keywords.insert("from", Token::From);
        keywords.insert("fun", Token::Fun);
        keywords.insert("struct", Token::Struct);
        keywords.insert("nil", Token::Nil);

        match keywords.get(identifier) {
            None => None,
            Some(token) => Some(token.clone()),
        }
    }

    fn identifier(&mut self, x: char) -> Option<Token> {
        let mut identifier = String::new();
        identifier.push(x);
        let rest: String = self
            .it
            .consume_while(|a| a.is_ascii_alphanumeric() || a == '_')
            .into_iter()
            .collect();
        identifier.push_str(rest.as_str());
        match self.keyword(&identifier) {
            None => Some(Token::IdentifierLiteral(identifier)),
            Some(token) => Some(token),
        }
    }

    fn number(&mut self, x: char) -> Option<Token> {
        let mut number = String::new();
        number.push(x);
        let num: String = self
            .it
            .consume_while(|a| a.is_numeric())
            .into_iter()
            .collect();
        number.push_str(num.as_str());
        if self.it.peek() == Some(&'.') && self.it.consume_if_next(|ch| ch.is_numeric()) {
            let num2: String = self
                .it
                .consume_while(|a| a.is_numeric())
                .into_iter()
                .collect();
            number.push('.');
            number.push_str(num2.as_str());
        }
        Some(Token::NumberLiteral(number.parse::<f64>().unwrap()))
    }

    fn tokenize_with_context(&mut self) -> Vec<WithSpan<Token>> {
        let mut tokens: Vec<WithSpan<Token>> = Vec::new();
        loop {
            let initial_position = self.it.current_position;
            let ch = match self.it.next() {
                None => break,
                Some(c) => c,
            };
            if let Some(token) = self.match_token(ch) {
                tokens.push(WithSpan::new(
                    token,
                    Span {
                        start: initial_position,
                        end: self.it.current_position,
                    },
                ));
            }
        }
        tokens
    }
}

pub fn tokenize_with_context(buf: &str) -> Vec<WithSpan<Token>> {
    let mut t = Lexer::new(buf);
    t.tokenize_with_context()
}

#[cfg(test)]
mod tests {
    use super::Token;
    fn tokenize(buf: &str) -> Vec<Token> {
        use super::tokenize_with_context;
        tokenize_with_context(buf)
            .iter()
            .map(|tc| tc.value.clone())
            .collect()
    }

    #[test]
    fn test_errors() {
        assert_eq!(
            tokenize("\"Unterminated string..."),
            vec![Token::UnterminatedString]
        );
        assert_eq!(tokenize("&"), vec![Token::Unknown('&')]);
        assert_eq!(
            tokenize("^^"),
            vec![Token::Unknown('^'), Token::Unknown('^')]
        );
        assert_eq!(
            tokenize("& \"test"),
            vec![Token::Unknown('&'), Token::UnterminatedString]
        );
    }

    #[test]
    fn test() {
        assert_eq!(tokenize(""), vec![]);
        assert_eq!(tokenize("="), vec![Token::Equal]);
        assert_eq!(tokenize("=="), vec![Token::EqualEqual]);
        assert_eq!(
            tokenize("== = =="),
            vec![Token::EqualEqual, Token::Equal, Token::EqualEqual]
        );
        assert_eq!(tokenize("//test"), vec![]);
        assert_eq!(tokenize("=//test"), vec![Token::Equal]);
        assert_eq!(
            tokenize(
                "=//test
        ="
            ),
            vec![Token::Equal, Token::Equal]
        );
        assert_eq!(
            tokenize("\"test\""),
            vec![Token::StringLiteral("test".to_string())]
        );
        assert_eq!(tokenize("12.34"), vec![Token::NumberLiteral(12.34)]);
        assert_eq!(
            tokenize("99."),
            vec![Token::NumberLiteral(99.00), Token::Dot]
        );
        assert_eq!(
            tokenize("99.="),
            vec![Token::NumberLiteral(99.00), Token::Dot, Token::Equal]
        );
        assert_eq!(tokenize("!"), vec![Token::Bang]);
        assert_eq!(tokenize("!="), vec![Token::BangEqual]);
        assert_eq!(
            tokenize("test"),
            vec![Token::IdentifierLiteral("test".to_string())]
        );
        assert_eq!(tokenize("struct"), vec![Token::Struct]);
        assert_eq!(tokenize("or"), vec![Token::Or]);
        assert_eq!(tokenize("use"), vec![Token::Use]);
        assert_eq!(tokenize("["), vec![Token::LeftBracket]);
        assert_eq!(tokenize("]"), vec![Token::RightBracket]);
    }
}
