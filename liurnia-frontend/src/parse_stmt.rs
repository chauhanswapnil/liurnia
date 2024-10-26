use crate::ast::{Expr, Parameter, Program, Stmt, StructField, TypeAnnotation, UseStatement};
use crate::common::{expect_identifier, expect_string};
use crate::parser::Parser;
use crate::position::{Span, WithSpan};
use crate::token::TokenKind;

pub fn parse_program(parser: &mut Parser) -> Result<Program, ()> {
    let use_statements = parse_use_statements(parser)?;
    let statements = parse_declarations(parser)?;
    Ok(Program::Module(use_statements, statements))
}

fn parse_use_statements(parser: &mut Parser) -> Result<Vec<WithSpan<UseStatement>>, ()> {
    let mut use_statements = Vec::new();
    while parser.check(TokenKind::Use) {
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
        statements.push(parse_declaration(parser)?);
    }
    Ok(statements)
}

fn parse_declaration(parser: &mut Parser) -> Result<WithSpan<Stmt>, ()> {
    match parser.peek() {
        TokenKind::Var => parse_var_declaration(parser),
        TokenKind::Fun => parse_fun_declaration(parser),
        TokenKind::Struct => parse_struct_declaration(parser),
        _ => parse_statement(parser),
    }
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

fn parse_var_declaration(parser: &mut Parser) -> Result<WithSpan<Stmt>, ()> {
    let begin_token = parser.expect(TokenKind::Var)?;
    let var_identifier = expect_identifier(parser)?;
    let mut type_annotation = None;
    if parser.optionally(TokenKind::Colon)? {
        type_annotation = Some(parse_type_annotation(parser)?);
    }
    let mut expr = None;
    if parser.optionally(TokenKind::Equal)? {
        expr = Some(parse_expr(parser)?);
    }
    let end_token = parser.expect(TokenKind::Semicolon)?;
    Ok(WithSpan {
        value: Stmt::Variable(var_identifier, type_annotation, expr.map(Box::new)),
        span: Span::union_span(begin_token.span, end_token.span),
    })
}

fn parse_struct_declaration(parser: &mut Parser) -> Result<WithSpan<Stmt>, ()> {
    let begin_token = parser.expect(TokenKind::Struct)?;
    let struct_name = expect_identifier(parser)?;
    parser.expect(TokenKind::LeftBrace)?;
    let mut var_declarations: Vec<WithSpan<StructField>> = Vec::new();
    while !parser.check(TokenKind::RightBrace) {
        let var_identifier = expect_identifier(parser)?;
        let mut type_annotation = None;
        if parser.optionally(TokenKind::Colon)? {
            type_annotation = Some(parse_type_annotation(parser)?);
        }
        let mut expr = None;
        if parser.optionally(TokenKind::Equal)? {
            expr = Some(parse_expr(parser)?);
        }
        let end_token = parser.expect(TokenKind::Semicolon)?;

        var_declarations.push(WithSpan {
            value: StructField {
                name: var_identifier.clone(),
                annotation: type_annotation,
                value: expr.map(Box::new),
            },
            span: Span::union_span(var_identifier.span, end_token.span),
        });
    }
    let end_token = parser.expect(TokenKind::RightBrace)?;
    Ok(WithSpan {
        value: Stmt::Struct(struct_name, var_declarations),
        span: Span::union_span(begin_token.span, end_token.span),
    })
}

fn parse_fun_declaration(parser: &mut Parser) -> Result<WithSpan<Stmt>, ()> {
    let fun_keyword = parser.expect(TokenKind::Fun)?;
    let fun_name = expect_identifier(parser)?;
    let parameters = parse_parameters(parser)?;
    let mut return_type = None;
    if parser.optionally(TokenKind::Minus)? {
        let _ = parser.expect(TokenKind::Greater)?;
        return_type = Some(parse_type_annotation(parser)?);
    }
    let fun_body_begin = parser.expect(TokenKind::LeftBrace)?;
    let mut function_body: Vec<WithSpan<Stmt>> = Vec::new();
    while !parser.check(TokenKind::RightBrace) {
        function_body.push(parse_declaration(parser)?);
    }
    let fun_body_end = parser.expect(TokenKind::RightBrace)?;
    Ok(WithSpan::new(
        Stmt::Function(
            fun_name.clone(),
            parameters,
            return_type,
            WithSpan {
                value: function_body,
                span: Span::union_span(fun_body_begin.span, fun_body_end.span),
            },
        ),
        Span::union_span(fun_keyword.span, fun_body_end.span),
    ))
}

fn parse_parameters(parser: &mut Parser) -> Result<WithSpan<Vec<Parameter>>, ()> {
    let mut parameters = Vec::new();
    let begin_token = parser.expect(TokenKind::LeftParen)?;
    while !parser.check(TokenKind::RightParen) {
        let ident = expect_identifier(parser)?;
        let type_annotation = if parser.optionally(TokenKind::Colon)? {
            Some(parse_type_annotation(parser)?)
        } else {
            None
        };
        let value = if parser.optionally(TokenKind::Equal)? {
            Some(parse_expr(parser)?)
        } else {
            None
        };
        parameters.push(Parameter {
            name: ident,
            annotation: type_annotation,
            value: value.map(Box::new),
        });
        if !parser.optionally(TokenKind::Comma)? {
            break;
        }
    }
    let end_token = parser.expect(TokenKind::RightParen)?;
    Ok(WithSpan {
        value: parameters,
        span: Span::union_span(begin_token.span, end_token.span),
    })
}

fn parse_type_annotation(parser: &mut Parser) -> Result<WithSpan<TypeAnnotation>, ()> {
    let type_annotation = match parser.peek() {
        TokenKind::Number => {
            let token = parser.advance();
            WithSpan {
                value: TypeAnnotation::Number,
                span: token.span,
            }
        }
        TokenKind::String => {
            let token = parser.advance();
            WithSpan {
                value: TypeAnnotation::String,
                span: token.span,
            }
        }
        TokenKind::Bool => {
            let token = parser.advance();
            WithSpan {
                value: TypeAnnotation::Bool,
                span: token.span,
            }
        }
        _ => {
            let ident = expect_identifier(parser)?;
            WithSpan {
                value: TypeAnnotation::Identifier(ident.value),
                span: ident.span,
            }
        }
    };
    Ok(type_annotation)
}

fn parse_statement(parser: &mut Parser) -> Result<WithSpan<Stmt>, ()> {
    match parser.peek() {
        TokenKind::Return => parse_return_statement(parser),
        TokenKind::If => parse_if_statement(parser),
        TokenKind::While => parse_while_statement(parser),
        TokenKind::LeftBrace => parse_block_statement(parser),
        TokenKind::Use => {
            parser.error(
                "Unexpected `use statement`. Imports can only happen at the start of the file.",
                parser.peek_token().span,
            );
            Err(())
        }
        _ => parse_expr_statement(parser),
    }
}

fn parse_while_statement(parser: &mut Parser) -> Result<WithSpan<Stmt>, ()> {
    let begin_token = parser.expect(TokenKind::While)?;
    parser.expect(TokenKind::LeftParen)?;
    let condition = parse_expr(parser)?;
    parser.expect(TokenKind::RightParen)?;
    let while_block = parse_block_statement(parser)?;
    let end_span = while_block.span;
    Ok(WithSpan::new(
        Stmt::WhileStatement(Box::new(condition), Box::new(while_block)),
        Span::union_span(begin_token.span, end_span),
    ))
}

fn parse_block_statement(parser: &mut Parser) -> Result<WithSpan<Stmt>, ()> {
    let begin_token = parser.expect(TokenKind::LeftBrace)?;
    let mut statements: Vec<WithSpan<Stmt>> = Vec::new();
    while !parser.check(TokenKind::RightBrace) {
        statements.push(parse_declaration(parser)?);
    }
    let end_token = parser.expect(TokenKind::RightBrace)?;
    Ok(WithSpan::new(
        Stmt::Block(statements),
        Span::union_span(begin_token.span, end_token.span),
    ))
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

fn parse_if_statement(parser: &mut Parser) -> Result<WithSpan<Stmt>, ()> {
    let begin_token = parser.expect(TokenKind::If)?;
    parser.expect(TokenKind::LeftParen)?;
    let condition = parse_expr(parser)?;
    parser.expect(TokenKind::RightParen)?;
    let if_body = parse_statement(parser)?;
    let mut end_span = if_body.span;
    let mut else_body: Option<WithSpan<Stmt>> = None;
    if parser.optionally(TokenKind::Else)? {
        let stmt = parse_statement(parser)?;
        end_span = stmt.span;
        else_body = Some(stmt);
    }
    Ok(WithSpan::new(
        Stmt::IfStatement(
            Box::new(condition),
            Box::new(if_body),
            else_body.map(Box::new),
        ),
        Span::union_span(begin_token.span, end_span),
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
