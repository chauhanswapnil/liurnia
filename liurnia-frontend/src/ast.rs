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
        WithSpan<TypeAnnotation>,
        Option<Box<WithSpan<Expr>>>,
    ),
    Function(
        WithSpan<Identifier>,
        WithSpan<Option<Parameters>>,
        Box<WithSpan<Stmt>>,
    ),
    Struct(
        WithSpan<Identifier>,
        Vec<WithSpan<StructField>>,
        Option<Box<WithSpan<Stmt>>>,
    ),

    ExpressionStatement(Box<WithSpan<Expr>>),

    IfStatement(
        Box<WithSpan<Expr>>,
        Box<WithSpan<Stmt>>,
        Option<Box<WithSpan<Stmt>>>,
    ),

    WhileStatement(Box<WithSpan<Expr>>, Box<WithSpan<Stmt>>),
    ForStatement(
        Option<WithSpan<Box<Stmt>>>,
        Option<WithSpan<Expr>>,
        Option<WithSpan<Expr>>,
        Box<WithSpan<Stmt>>,
    ),

    ReturnStatement(Option<Box<WithSpan<Expr>>>),

    Block(Vec<WithSpan<Stmt>>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructField {
    pub name: Identifier,
    pub annotation: TypeAnnotation,
    pub value: Option<WithSpan<Expr>>,
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
    NumberType,
    StringType,
    BoolType,
    AnyType,
    IdentifierType(Identifier),
    UnionType(Vec<TypeAnnotation>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Parameters {
    pub parameters: Vec<Parameter>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Parameter {
    NamedParameter(Identifier, TypeAnnotation),
    UnionType(Vec<TypeAnnotation>),
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
