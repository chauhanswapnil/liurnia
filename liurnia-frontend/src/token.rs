use crate::position::Span;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub span: Span,
}

#[derive(PartialEq, Debug, Clone)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,    // (
    RightParen,   // )
    LeftBrace,    // {
    RightBrace,   // }
    LeftBracket,  // [
    RightBracket, // ]
    Comma,        // ,
    Dot,          // .
    Minus,        // -
    Plus,         // +
    Semicolon,    // ;
    Slash,        // /
    Star,         // *
    Percent,      // %
    Pipe,         // |

    // One or two character tokens.
    Bang,         // !
    BangEqual,    // !=
    Equal,        // =
    EqualEqual,   // !=
    Greater,      // >
    GreaterEqual, // >=
    Less,         // <
    LessEqual,    // <=

    // Literals
    IdentifierLiteral(String),
    StringLiteral(String),
    NumberLiteral(f64),

    // Keywords
    And, // and
    Or,  // or

    If,   // if
    Else, // else

    For,   // for
    While, // while

    // Keeping this as a keyword for now,
    // will change it to function later
    Print,  // print
    Return, // return

    // Assignment and types
    Var,    // var
    Type,   // type
    Number, // number
    String, // string
    Bool,   // bool

    True,  // true
    False, // false

    // Module System
    Use,  // use
    From, // From

    Fun,    // fun
    Struct, // struct
    This,   // this

    Nil, // nil

    // Other
    Eof, // end of file
    UnterminatedString,
    Unknown(char),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TokenKind {
    // Single-character tokens.
    LeftParen,    // (
    RightParen,   // )
    LeftBrace,    // {
    RightBrace,   // }
    LeftBracket,  // [
    RightBracket, // ]
    Comma,        // ,
    Dot,          // .
    Minus,        // -
    Plus,         // +
    Semicolon,    // ;
    Slash,        // /
    Star,         // *
    Percent,      // %
    Pipe,         // |

    // One or two character tokens.
    Bang,         // !
    BangEqual,    // !=
    Equal,        // =
    EqualEqual,   // !=
    Greater,      // >
    GreaterEqual, // >=
    Less,         // <
    LessEqual,    // <=

    // Literals
    IdentifierLiteral,
    StringLiteral,
    NumberLiteral,

    // Keywords
    And, // and
    Or,  // or

    If,   // if
    Else, // else

    For,   // for
    While, // while

    // Keeping this as a keyword for now,
    // will change it to function later
    Print,  // print
    Return, // return

    // Assignment and types
    Var,    // var
    Type,   // type
    Number, // number
    String, // string
    Bool,   // bool

    True,  // true
    False, // false

    // Module System
    Use,  // use
    From, // From

    Fun,    // fun
    Struct, // struct
    This,   // this

    Nil, // nil

    // Other
    Eof, // end of file
    UnterminatedString,
    Unknown,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let kind: TokenKind = self.into();
        write!(f, "{}", kind)
    }
}

impl From<&TokenType> for TokenKind {
    fn from(token: &TokenType) -> Self {
        match token {
            TokenType::LeftParen => TokenKind::LeftParen,
            TokenType::RightParen => TokenKind::RightParen,
            TokenType::LeftBrace => TokenKind::LeftBrace,
            TokenType::RightBrace => TokenKind::RightBrace,
            TokenType::LeftBracket => TokenKind::LeftBracket,
            TokenType::RightBracket => TokenKind::RightBracket,
            TokenType::Comma => TokenKind::Comma,
            TokenType::Dot => TokenKind::Dot,
            TokenType::Minus => TokenKind::Minus,
            TokenType::Plus => TokenKind::Plus,
            TokenType::Semicolon => TokenKind::Semicolon,
            TokenType::Slash => TokenKind::Slash,
            TokenType::Star => TokenKind::Star,
            TokenType::Percent => TokenKind::Percent,
            TokenType::Pipe => TokenKind::Pipe,
            TokenType::Bang => TokenKind::Bang,
            TokenType::BangEqual => TokenKind::BangEqual,
            TokenType::Equal => TokenKind::Equal,
            TokenType::EqualEqual => TokenKind::EqualEqual,
            TokenType::Greater => TokenKind::Greater,
            TokenType::GreaterEqual => TokenKind::GreaterEqual,
            TokenType::Less => TokenKind::Less,
            TokenType::LessEqual => TokenKind::LessEqual,
            TokenType::IdentifierLiteral(_) => TokenKind::IdentifierLiteral,
            TokenType::StringLiteral(_) => TokenKind::StringLiteral,
            TokenType::NumberLiteral(_) => TokenKind::NumberLiteral,
            TokenType::And => TokenKind::And,
            TokenType::Or => TokenKind::Or,
            TokenType::If => TokenKind::If,
            TokenType::Else => TokenKind::Else,
            TokenType::For => TokenKind::For,
            TokenType::While => TokenKind::While,
            TokenType::Print => TokenKind::Print,
            TokenType::Return => TokenKind::Return,
            TokenType::Var => TokenKind::Var,
            TokenType::Type => TokenKind::Type,
            TokenType::Number => TokenKind::Number,
            TokenType::String => TokenKind::String,
            TokenType::Bool => TokenKind::Bool,
            TokenType::True => TokenKind::True,
            TokenType::False => TokenKind::False,
            TokenType::Use => TokenKind::Use,
            TokenType::From => TokenKind::From,
            TokenType::Fun => TokenKind::Fun,
            TokenType::Struct => TokenKind::Struct,
            TokenType::This => TokenKind::This,
            TokenType::Nil => TokenKind::Nil,
            TokenType::Eof => TokenKind::Eof,
            TokenType::UnterminatedString => TokenKind::UnterminatedString,
            TokenType::Unknown(_) => TokenKind::Unknown,
        }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TokenKind::LeftParen => "(",
                TokenKind::RightParen => ")",
                TokenKind::LeftBrace => "{",
                TokenKind::RightBrace => "}",
                TokenKind::LeftBracket => "[",
                TokenKind::RightBracket => "]",
                TokenKind::Comma => ",",
                TokenKind::Dot => ".",
                TokenKind::Minus => "-",
                TokenKind::Plus => "+",
                TokenKind::Semicolon => ";",
                TokenKind::Slash => "/",
                TokenKind::Star => "*",
                TokenKind::Percent => "%",
                TokenKind::Pipe => "|",
                TokenKind::Bang => "!",
                TokenKind::BangEqual => "!=",
                TokenKind::Equal => "=",
                TokenKind::EqualEqual => "==",
                TokenKind::Greater => ">",
                TokenKind::GreaterEqual => ">=",
                TokenKind::Less => "<",
                TokenKind::LessEqual => "<=",
                TokenKind::IdentifierLiteral => "identifier_literal",
                TokenKind::StringLiteral => "string_literal",
                TokenKind::NumberLiteral => "number_literal",
                TokenKind::And => "and",
                TokenKind::Or => "or",
                TokenKind::If => "if",
                TokenKind::Else => "else",
                TokenKind::For => "for",
                TokenKind::While => "while",
                TokenKind::Print => "print",
                TokenKind::Return => "return",
                TokenKind::Var => "var",
                TokenKind::Type => "type",
                TokenKind::Number => "number",
                TokenKind::String => "string",
                TokenKind::Bool => "bool",
                TokenKind::True => "true",
                TokenKind::False => "false",
                TokenKind::Use => "use",
                TokenKind::From => "from",
                TokenKind::Fun => "fun",
                TokenKind::Struct => "struct",
                TokenKind::This => "this",
                TokenKind::Nil => "nil",
                TokenKind::Eof => "<Eof>",
                TokenKind::UnterminatedString => "<UnterminatedString>",
                TokenKind::Unknown => "<Unknown>",
            }
        )
    }
}
