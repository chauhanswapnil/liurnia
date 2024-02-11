use crate::ast::{Expr, Program, Stmt, UseStatement};
use crate::common::{expect_identifier, expect_string};
use crate::parser::Parser;
use crate::position::{Span, WithSpan};
use crate::token::{TokenKind};

pub fn parse_program(parser: &mut Parser) -> Result<Program, ()> {
    let use_statements = parse_use_statements(parser);
    let _statements = parse_declarations(parser);
    Ok(Program::Module(use_statements?, Vec::new()))
    // parse_statements(parser);
}

fn parse_use_statements(parser: &mut Parser) -> Result<Vec<WithSpan<UseStatement>>, ()> {
    let mut use_statements = Vec::new();
    while parser.check(TokenKind::Use) || parser.is_eof() {
        let begin_token = parser.expect(TokenKind::Use)?;
        let use_identifier = expect_identifier(parser)?;
        let _from_token = parser.expect(TokenKind::From)?;
        let from_string = expect_string(parser)?;
        let end_token = parser.expect(TokenKind::Semicolon)?;
        let use_stmt = Ok(WithSpan {
            value: UseStatement::Use(use_identifier, from_string),
            span: Span::union_span(begin_token.span, end_token.span),
        });
        use_statements.push(use_stmt?);
    }
    Ok(use_statements)
}

fn parse_declarations(parser: &mut Parser) -> Result<Vec<WithSpan<Stmt>>, ()> {
    let mut statements = Vec::new();
    while !parser.is_eof() {
        statements.push(match parser.peek() {
            // TokenKind::Var => parse_var_declaration(parser)?,
            _ => parse_statement(parser)?,
        })
    }
    Ok(statements)
}

fn parse_expr(parser: &mut Parser) -> Result<WithSpan<Expr>, ()> {
    super::parse_expr::parse(parser)
}

fn parse_expr_statement(parser: &mut Parser) -> Result<WithSpan<Stmt>, ()> {
    let expr = parse_expr(parser)?;
    let end_token = parser.expect(TokenKind::Semicolon)?;

    let span = Span::union_span(expr.span, end_token.span);
    Ok(WithSpan::new(
        Stmt::ExpressionStatement(Box::new(expr)),
        span,
    ))
}

fn parse_statement(parser: &mut Parser) -> Result<WithSpan<Stmt>, ()> {
    match parser.peek() {
        TokenKind::Return => parse_return_statement(parser),
        // TokenKind::If => parse_if_statement(parser),
        // TokenKind::While => parse_while_statement(parser),
        // TokenKind::For => parse_for_statement(parser),
        // TokenKind::LeftBrace => parse_block_statement(parser),
        TokenKind::Use => {
            parser.error(
                "Unexpected `use statement`. Imports can only happen at the start of the file.",
                parser.peek_token().span,
            );
            Err(())
        }
        _ => {
            parser.error("Expressions not implemented yet", parser.peek_token().span);
            Err(())
        }
        _ => parse_expr_statement(parser),
    }
}

fn parse_return_statement(parser: &mut Parser) -> Result<WithSpan<Stmt>, ()> {
    let begin_token = parser.expect(TokenKind::Return)?;
    let mut expr = None;
    if !parser.check(TokenKind::Semicolon) {
        expr = Some(parse_expr(parser)?);
    }
    let end_token = parser.expect(TokenKind::Semicolon)?;
    Ok(WithSpan::new(
        Stmt::ReturnStatement(expr.map(Box::new)),
        Span::union_span(begin_token.span, end_token.span),
    ))
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expr, Stmt, UseStatement};
    use crate::parse_stmt::{parse_return_statement, parse_use_statements};
    use crate::parser::Parser;
    use crate::position::{Span, WithSpan};
    use crate::tokenizer::tokenize_with_context;
    

    #[test]
    fn test_use_stmt() {
        let tokens = tokenize_with_context("use add from \"add.li\";");
        let mut parser = Parser::new(&tokens);
        match parse_use_statements(&mut parser) {
            Ok(use_statements) => {
                assert_eq!(
                    use_statements,
                    vec![WithSpan {
                        value: UseStatement::Use(
                            WithSpan {
                                value: "add".to_string(),
                                span: Span::easy_span_create((1, 4), (1, 7))
                            },
                            WithSpan {
                                value: "add.li".to_string(),
                                span: Span::easy_span_create((1, 13), (1, 21))
                            }
                        ),
                        span: Span::easy_span_create((1, 0), (1, 22))
                    }]
                );
            }
            Err(_err) => {
                println!("{:?}", parser.diagnostics().to_vec())
            }
        }
    }
    #[test]
    fn test_multiple_use_stmts() {
        let tokens = tokenize_with_context(
            "use add from \"add.li\";\nuse subtract from \"subtract.li\";\nfun abc() {}",
        );
        let mut parser = Parser::new(&tokens);
        assert_eq!(
            parse_use_statements(&mut parser),
            Ok(vec![
                WithSpan {
                    value: UseStatement::Use(
                        WithSpan {
                            value: "add".to_string(),
                            span: Span::easy_span_create((1, 4), (1, 7))
                        },
                        WithSpan {
                            value: "add.li".to_string(),
                            span: Span::easy_span_create((1, 13), (1, 21))
                        }
                    ),
                    span: Span::easy_span_create((1, 0), (1, 22))
                },
                WithSpan {
                    value: UseStatement::Use(
                        WithSpan {
                            value: "subtract".to_string(),
                            span: Span::easy_span_create((2, 4), (2, 12))
                        },
                        WithSpan {
                            value: "subtract.li".to_string(),
                            span: Span::easy_span_create((2, 18), (2, 31))
                        }
                    ),
                    span: Span::easy_span_create((2, 0), (2, 32))
                }
            ])
        );
    }

    #[test]
    fn test_return_stmt() {
        let tokens = tokenize_with_context("return true;");
        let mut parser = Parser::new(&tokens);
        let result = parse_return_statement(&mut parser);
        // println!("{:?}", parser.diagnostics().to_vec());
        assert_eq!(
            result,
            Ok(WithSpan {
                value: Stmt::ReturnStatement(Option::from(Box::from(WithSpan {
                    value: Expr::BooleanLiteral(true),
                    span: Span::easy_span_create((1, 7), (1, 11))
                }))),
                span: Span::easy_span_create((1, 0), (1, 12)),
            })
        )
    }
}
