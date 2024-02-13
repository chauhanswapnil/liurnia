use crate::ast::Program;
use crate::parse_stmt::parse_program;
use crate::position::Diagnostic;

pub mod position;

mod token;

mod ast;
mod common;
mod parse_expr;
mod parse_stmt;
mod parser;
mod tokenizer;

pub fn parse(code: &str) -> Result<Program, Vec<Diagnostic>> {
    println!("Parsing: {}", code);
    use tokenizer::tokenize_with_context;
    let tokens = tokenize_with_context(code);
    println!("Tokens: {:#?}", tokens);

    let mut parser = parser::Parser::new(&tokens);
    let parsed_program = parse_program(&mut parser);
    println!("Parsed Program: {:#?}", parsed_program);
    println!("Diagnostics: {:#?}", parser.diagnostics().to_vec());
    match (parsed_program) {
        Ok(program) if parser.diagnostics().is_empty() => Ok(program),
        Ok(_) => Err(parser.diagnostics().to_vec()),
        Err(_) => Err(parser.diagnostics().to_vec()),
    }
}
