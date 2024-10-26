use crate::position::WithSpan;

pub type Identifier = String;

#[derive(Debug, PartialEq, Clone)]
pub enum Program {
    Module(Vec<WithSpan<UseStatement>>, Vec<WithSpan<Stmt>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum UseStatement {
    Use(WithSpan<String>, WithSpan<Identifier>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Variable(
        WithSpan<Identifier>,
        Option<WithSpan<TypeAnnotation>>,
        Option<Box<WithSpan<Expr>>>,
    ),
    Function(
        WithSpan<Identifier>,
        WithSpan<Vec<Parameter>>,
        Option<WithSpan<TypeAnnotation>>,
        WithSpan<Vec<WithSpan<Stmt>>>,
    ),
    Struct(WithSpan<Identifier>, Vec<WithSpan<StructField>>),
    ExpressionStatement(Box<WithSpan<Expr>>),
    IfStatement(
        Box<WithSpan<Expr>>,
        Box<WithSpan<Stmt>>,
        Option<Box<WithSpan<Stmt>>>,
    ),
    WhileStatement(Box<WithSpan<Expr>>, Box<WithSpan<Stmt>>),
    ReturnStatement(Option<Box<WithSpan<Expr>>>),
    Block(Vec<WithSpan<Stmt>>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructField {
    pub name: WithSpan<Identifier>,
    pub annotation: Option<WithSpan<TypeAnnotation>>,
    pub value: Option<Box<WithSpan<Expr>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Binary(
        Box<WithSpan<Expr>>,
        WithSpan<BinaryOperator>,
        Box<WithSpan<Expr>>,
    ),

    // Primary
    NumberLiteral(f64),
    StringLiteral(String),
    BooleanLiteral(bool),
    NilLiteral,
    This,
    Grouping(Box<WithSpan<Expr>>),

    Unary(WithSpan<UnaryOperator>, Box<WithSpan<Expr>>),
    Logical(
        Box<WithSpan<Expr>>,
        WithSpan<LogicalOperator>,
        Box<WithSpan<Expr>>,
    ),

    Variable(WithSpan<Identifier>),

    Assignment(WithSpan<Identifier>, Box<WithSpan<Expr>>),
    Call(Box<WithSpan<Expr>>, Vec<WithSpan<Expr>>),

    Get(Box<WithSpan<Expr>>, WithSpan<Identifier>),
    Set(
        Box<WithSpan<Expr>>,
        WithSpan<Identifier>,
        Box<WithSpan<Expr>>,
    ),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeAnnotation {
    Number,
    String,
    Bool,
    Identifier(Identifier),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Parameters {
    pub parameters: Vec<WithSpan<Parameter>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Parameter {
    pub name: WithSpan<Identifier>,
    pub annotation: Option<WithSpan<TypeAnnotation>>,
    pub value: Option<Box<WithSpan<Expr>>>,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum UnaryOperator {
    Bang,
    Minus,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Slash,
    Star,
    Percent,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    BangEqual,
    EqualEqual,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum LogicalOperator {
    And,
    Or,
}
