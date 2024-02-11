use crate::position::{Diagnostic, Span};
use crate::token::{Token, TokenKind, TokenType};

static EOF_TOKEN: Token = Token {
    token_type: TokenType::Eof,
    span: Span::empty(),
};

pub struct Parser<'a> {
    tokens: &'a [Token],
    cursor: usize,
    diagnostics: Vec<Diagnostic>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Parser {
            tokens,
            cursor: 0,
            diagnostics: Vec::new(),
        }
    }

    pub fn peek_token(&self) -> &'a Token {
        self.tokens.get(self.cursor).unwrap_or(&EOF_TOKEN)
    }

    pub fn peek(&self) -> TokenKind {
        (&self.peek_token().token_type).into()
    }

    pub fn check(&self, match_token: TokenKind) -> bool {
        let token = self.peek();
        token == match_token
    }

    pub fn advance(&mut self) -> &'a Token {
        let token = self.tokens.get(self.cursor);
        if let Some(token) = token {
            self.cursor += 1;
            token
        } else {
            &EOF_TOKEN
        }
    }

    pub fn expect(&mut self, expected: TokenKind) -> Result<&'a Token, ()> {
        let token = self.advance();
        if TokenKind::from(&token.token_type) == expected {
            Ok(token)
        } else {
            self.error(
                &format!("Expected {} got {}", expected, token.token_type),
                token.span,
            );
            Err(())
        }
    }

    pub fn optionally(&mut self, expected: TokenKind) -> Result<bool, ()> {
        let token = self.peek();
        if token == expected {
            self.expect(expected)?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub fn is_eof(&self) -> bool {
        self.check(TokenKind::Eof)
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    pub fn error(&mut self, message: &str, span: Span) {
        self.diagnostics.push(Diagnostic {
            message: message.to_string(),
            span,
        });
    }
}
