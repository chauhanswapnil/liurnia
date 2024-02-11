use crate::parse_stmt::parse_program;

pub mod position;

mod token;

mod ast;
mod common;
mod parse_expr;
mod parse_stmt;
mod parser;
mod tokenizer;

// Eventually Return an AST object with Diagnostics
pub fn parse(code: &str) {
    println!("Parsing: {}", code);
    use tokenizer::tokenize_with_context;
    let tokens = tokenize_with_context(code);
    println!("Tokens: {:#?}", tokens);

    let mut parser = parser::Parser::new(&tokens);
    parse_program(&mut parser);
}
