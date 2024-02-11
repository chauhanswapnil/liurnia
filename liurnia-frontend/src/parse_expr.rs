use crate::ast::{BinaryOperator, Expr, LogicalOperator, UnaryOperator};
use crate::parser::Parser;
use crate::position::{Span, WithSpan};
use crate::token::{TokenKind, TokenType};

#[derive(PartialEq, PartialOrd, Copy, Clone)]
enum Precedence {
    None,
    Assign,     // =
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < <= > >=
    Term,       // + -
    Factor,     // * / %
    Unary,      // ! -
    Call,       // ()
    Primary,
}

impl<'a> From<TokenKind> for Precedence {
    fn from(token: TokenKind) -> Precedence {
        match token {
            TokenKind::Equal => Precedence::Assign,
            TokenKind::Or => Precedence::Or,
            TokenKind::And => Precedence::And,
            TokenKind::BangEqual | TokenKind::EqualEqual => Precedence::Equality,
            TokenKind::Less
            | TokenKind::LessEqual
            | TokenKind::Greater
            | TokenKind::GreaterEqual => Precedence::Comparison,
            TokenKind::Plus | TokenKind::Minus => Precedence::Term,
            TokenKind::Star | TokenKind::Slash | TokenKind::Percent => Precedence::Factor,
            TokenKind::Bang => Precedence::Unary,
            TokenKind::LeftParen => Precedence::Call,
            TokenKind::Dot => Precedence::Call,
            _ => Precedence::None,
        }
    }
}

pub fn parse(parser: &mut Parser) -> Result<WithSpan<Expr>, ()> {
    parse_expr(parser, Precedence::None)
}

fn parse_expr(parser: &mut Parser, precedence: Precedence) -> Result<WithSpan<Expr>, ()> {
    let mut expr = parse_prefix(parser)?;
    while !parser.is_eof() {
        let next_precedence = Precedence::from(parser.peek());
        if precedence >= next_precedence {
            break;
        }
        expr = parse_infix(parser, expr)?;
    }
    Ok(expr)
}

fn parse_prefix(parser: &mut Parser) -> Result<WithSpan<Expr>, ()> {
    match parser.peek() {
        TokenKind::NumberLiteral
        | TokenKind::Nil
        | TokenKind::This
        | TokenKind::True
        | TokenKind::False
        | TokenKind::IdentifierLiteral
        | TokenKind::String => parse_primary(parser),
        TokenKind::Bang | TokenKind::Minus => parse_unary(parser),
        TokenKind::LeftParen => parse_grouping(parser),
        _ => {
            parser.error(
                &format!("Unexpected {}", parser.peek_token().token_type),
                parser.peek_token().span,
            );
            Err(())
        }
    }
}

fn parse_infix(parser: &mut Parser, left: WithSpan<Expr>) -> Result<WithSpan<Expr>, ()> {
    match parser.peek() {
        TokenKind::BangEqual
        | TokenKind::EqualEqual
        | TokenKind::Less
        | TokenKind::LessEqual
        | TokenKind::Greater
        | TokenKind::GreaterEqual
        | TokenKind::Plus
        | TokenKind::Minus
        | TokenKind::Star
        | TokenKind::Percent
        | TokenKind::Slash => parse_binary(parser, left),
        TokenKind::Or | TokenKind::And => parse_logical(parser, left),
        TokenKind::Equal => parse_assign(parser, left),
        TokenKind::LeftParen => parse_call(parser, left),
        TokenKind::Dot => parse_get(parser, left),
        _ => {
            parser.error(
                &format!("Unexpected {}", parser.peek_token().token_type),
                parser.peek_token().span,
            );
            Err(())
        }
    }
}

fn parse_binary(parser: &mut Parser, left: WithSpan<Expr>) -> Result<WithSpan<Expr>, ()> {
    let precedence = Precedence::from(parser.peek());
    let operator = parse_binary_op(parser)?;
    let right = parse_expr(parser, precedence)?;
    let span = Span::union(&left, &right);
    Ok(WithSpan::new(
        Expr::Binary(Box::new(left), operator, Box::new(right)),
        span,
    ))
}

fn parse_binary_op(parser: &mut Parser) -> Result<WithSpan<BinaryOperator>, ()> {
    let token = parser.advance();
    let operator = match &token.token_type {
        &TokenType::BangEqual => BinaryOperator::BangEqual,
        &TokenType::EqualEqual => BinaryOperator::EqualEqual,
        &TokenType::Less => BinaryOperator::Less,
        &TokenType::LessEqual => BinaryOperator::LessEqual,
        &TokenType::Greater => BinaryOperator::Greater,
        &TokenType::GreaterEqual => BinaryOperator::GreaterEqual,
        &TokenType::Plus => BinaryOperator::Plus,
        &TokenType::Minus => BinaryOperator::Minus,
        &TokenType::Star => BinaryOperator::Star,
        &TokenType::Slash => BinaryOperator::Slash,
        &TokenType::Percent => BinaryOperator::Percent,
        _ => {
            parser.error(
                &format!("Expected binary operator got {}", token.token_type),
                token.span,
            );
            return Err(());
        }
    };

    Ok(WithSpan::new(operator, token.span))
}

fn parse_unary(parser: &mut Parser) -> Result<WithSpan<Expr>, ()> {
    let operator = parse_unary_op(parser)?;
    let right = parse_expr(parser, Precedence::Unary)?;
    let span = Span::union(&operator, &right);
    Ok(WithSpan::new(Expr::Unary(operator, Box::new(right)), span))
}

fn parse_unary_op(parser: &mut Parser) -> Result<WithSpan<UnaryOperator>, ()> {
    let token = parser.advance();
    match &token.token_type {
        &TokenType::Bang => Ok(WithSpan::new(UnaryOperator::Bang, token.span)),
        &TokenType::Minus => Ok(WithSpan::new(UnaryOperator::Minus, token.span)),
        _ => {
            parser.error(
                &format!("Expected unary operator got {}", token.token_type),
                token.span,
            );
            Err(())
        }
    }
}

fn parse_logical(parser: &mut Parser, left: WithSpan<Expr>) -> Result<WithSpan<Expr>, ()> {
    let precedence = Precedence::from(parser.peek());
    let operator = parse_logical_operator(parser)?;
    let right = parse_expr(parser, precedence)?;
    let span = Span::union(&left, &right);
    Ok(WithSpan::new(
        Expr::Logical(Box::new(left), operator, Box::new(right)),
        span,
    ))
}

fn parse_logical_operator(parser: &mut Parser) -> Result<WithSpan<LogicalOperator>, ()> {
    let token = parser.advance();
    let operator = match &token.token_type {
        &TokenType::And => LogicalOperator::And,
        &TokenType::Or => LogicalOperator::Or,
        _ => {
            parser.error(
                &format!("Expected logical operator got {}", token.token_type),
                token.span,
            );
            return Err(());
        }
    };

    Ok(WithSpan::new(operator, token.span))
}

fn parse_assign(parser: &mut Parser, left: WithSpan<Expr>) -> Result<WithSpan<Expr>, ()> {
    parser.expect(TokenKind::Equal)?;
    let right = parse_expr(parser, Precedence::None)?;
    let span = Span::union(&left, &right);
    match &left.value {
        Expr::Variable(i) => Ok(WithSpan::new(
            Expr::Assignment(i.clone(), Box::new(right)),
            span,
        )),
        Expr::Get(l, i) => Ok(WithSpan::new(
            Expr::Set(l.clone(), i.clone(), Box::new(right)),
            span,
        )),
        _ => {
            parser.error("Got Invalid left expression", left.span);
            Err(())
        }
    }
}

fn parse_call(parser: &mut Parser, left: WithSpan<Expr>) -> Result<WithSpan<Expr>, ()> {
    parser.expect(TokenKind::LeftParen)?;
    let args = parse_arguments(parser)?;
    let most_right = parser.expect(TokenKind::RightParen)?;
    let span = Span::union_span(left.span, most_right.span);
    Ok(WithSpan::new(Expr::Call(Box::new(left), args), span))
}

fn parse_arguments(parser: &mut Parser) -> Result<Vec<WithSpan<Expr>>, ()> {
    let mut args = Vec::new();
    if !parser.check(TokenKind::RightParen) {
        args.push(parse_expr(parser, Precedence::None)?);
        while parser.check(TokenKind::Comma) {
            parser.expect(TokenKind::Comma)?;
            args.push(parse_expr(parser, Precedence::None)?);
        }
    }
    Ok(args)
}

fn parse_get(parser: &mut Parser, left: WithSpan<Expr>) -> Result<WithSpan<Expr>, ()> {
    parser.expect(TokenKind::Dot)?;
    let token = parser.advance();
    match &token.token_type {
        TokenType::IdentifierLiteral(i) => {
            let span = Span::union_span(left.span, token.span);
            Ok(WithSpan::new(
                Expr::Get(Box::new(left), WithSpan::new(i.clone(), token.span)),
                span,
            ))
        }
        _ => {
            parser.error(
                &format!("Expected identifier got {}", token.token_type),
                token.span,
            );
            Err(())
        }
    }
}

fn parse_primary(parser: &mut Parser) -> Result<WithSpan<Expr>, ()> {
    let token = parser.advance();
    match &token.token_type {
        &TokenType::Nil => Ok(WithSpan::new(Expr::NilLiteral, token.span)),
        &TokenType::This => Ok(WithSpan::new(Expr::This, token.span)),
        &TokenType::NumberLiteral(n) => Ok(WithSpan::new(Expr::NumberLiteral(n), token.span)),
        &TokenType::True => Ok(WithSpan::new(Expr::BooleanLiteral(true), token.span)),
        &TokenType::False => Ok(WithSpan::new(Expr::BooleanLiteral(false), token.span)),
        TokenType::StringLiteral(s) => {
            Ok(WithSpan::new(Expr::StringLiteral(s.clone()), token.span))
        }
        TokenType::IdentifierLiteral(s) => Ok(WithSpan::new(
            Expr::Variable(WithSpan::new(s.clone(), token.span)),
            token.span,
        )),
        _ => {
            parser.error(
                &format!("Expected primary got {}", token.token_type),
                token.span,
            );
            Err(())
        }
    }
}

fn parse_grouping(parser: &mut Parser) -> Result<WithSpan<Expr>, ()> {
    let left_paren = parser.expect(TokenKind::LeftParen)?;
    let expr = parse_expr(parser, Precedence::None)?;
    let right_paren = parser.expect(TokenKind::RightParen)?;

    let span = Span::union_span(left_paren.span, right_paren.span);
    Ok(WithSpan::new(Expr::Grouping(Box::new(expr)), span))
}

#[cfg(test)]
mod tests {
    use crate::ast::{BinaryOperator, Expr};
    use crate::parse_expr::{parse_expr, Precedence};
    use crate::parser::Parser;
    use crate::position::{LineColumn, Span, WithSpan};
    use crate::tokenizer::tokenize_with_context;

    #[test]
    fn test_primary() {
        let tokens = tokenize_with_context("x + y");
        let mut parser = Parser::new(&tokens);
        let result = parse_expr(&mut parser, Precedence::None);
        assert_eq!(
            result,
            Ok(WithSpan {
                value: Expr::Binary(
                    Box::from(WithSpan {
                        value: Expr::Variable(WithSpan {
                            value: "x".to_string(),
                            span: Span {
                                start: LineColumn { line: 1, column: 0 },
                                end: LineColumn { line: 1, column: 1 }
                            }
                        }),
                        span: Span {
                            start: LineColumn { line: 1, column: 0 },
                            end: LineColumn { line: 1, column: 1 }
                        }
                    }),
                    WithSpan {
                        value: BinaryOperator::Plus,
                        span: Span {
                            start: LineColumn { line: 1, column: 2 },
                            end: LineColumn { line: 1, column: 3 }
                        }
                    },
                    Box::from(WithSpan {
                        value: Expr::Variable(WithSpan {
                            value: "y".to_string(),
                            span: Span {
                                start: LineColumn { line: 1, column: 4 },
                                end: LineColumn { line: 1, column: 5 }
                            }
                        }),
                        span: Span {
                            start: LineColumn { line: 1, column: 4 },
                            end: LineColumn { line: 1, column: 5 }
                        }
                    })
                ),
                span: Span {
                    start: LineColumn { line: 1, column: 0 },
                    end: LineColumn { line: 1, column: 5 }
                }
            })
        );
    }
}
