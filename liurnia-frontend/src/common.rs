use crate::ast::Identifier;
use crate::parser::Parser;
use crate::position::WithSpan;
use crate::token::{Token, TokenKind, TokenType};

pub fn expect_identifier(p: &mut Parser) -> Result<WithSpan<Identifier>, ()> {
    let token = p.advance();
    match &token.token_type {
        TokenType::IdentifierLiteral(ident) => Ok(WithSpan::new(ident.clone(), token.span)),
        _ => {
            p.error(
                &format!(
                    "Expected {} got {}",
                    TokenKind::IdentifierLiteral,
                    token.token_type
                ),
                token.span,
            );
            Err(())
        }
    }
}

pub fn expect_string(p: &mut Parser) -> Result<WithSpan<String>, ()> {
    let token = p.advance();
    match &token.token_type {
        TokenType::StringLiteral(ident) => Ok(WithSpan::new(ident.clone(), token.span)),
        _ => {
            p.error(
                &format!("Expected {} got {}", TokenKind::String, token.token_type),
                token.span,
            );
            Err(())
        }
    }
}
