pub mod position;

mod token;

mod tokenizer;

// Eventually Return an AST object with Diagonostics
pub fn parse(code: &str) {
    println!("Parsing: {}", code);
    use tokenizer::tokenize_with_context;
    let tokens = tokenize_with_context(code);
    println!("Tokens: {:#?}", tokens);
}
