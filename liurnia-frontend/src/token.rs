use std::fmt::Display;

#[derive(PartialEq, Debug, Clone)]
pub enum Token {
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
    Print, // print

    // Assignment and types
    Var,    // var
    Type,   // type
    Number, // number
    String, // string
    Bool,   // bool

    True,  // true
    False, // false

    // Module System
    Use,    // use
    Export, // export
    From,   // From

    Fun,    // fun
    Struct, // struct

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
    Print, // print

    // Assignment and types
    Var,    // var
    Type,   // type
    Number, // number
    String, // string
    Bool,   // bool

    True,  // true
    False, // false

    // Module System
    Use,    // use
    Export, // export
    From,   // From

    Fun,    // fun
    Struct, // struct

    Nil, // nil

    // Other
    Eof, // end of file
    UnterminatedString,
    Unknown,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let kind: TokenKind = self.into();
        write!(f, "{}", kind)
    }
}

impl From<&Token> for TokenKind {
    fn from(token: &Token) -> Self {
        match token {
            Token::LeftParen => TokenKind::LeftParen,
            Token::RightParen => TokenKind::RightParen,
            Token::LeftBrace => TokenKind::LeftBrace,
            Token::RightBrace => TokenKind::RightBrace,
            Token::LeftBracket => TokenKind::LeftBracket,
            Token::RightBracket => TokenKind::RightBracket,
            Token::Comma => TokenKind::Comma,
            Token::Dot => TokenKind::Dot,
            Token::Minus => TokenKind::Minus,
            Token::Plus => TokenKind::Plus,
            Token::Semicolon => TokenKind::Semicolon,
            Token::Slash => TokenKind::Slash,
            Token::Star => TokenKind::Star,
            Token::Percent => TokenKind::Percent,
            Token::Pipe => TokenKind::Pipe,
            Token::Bang => TokenKind::Bang,
            Token::BangEqual => TokenKind::BangEqual,
            Token::Equal => TokenKind::Equal,
            Token::EqualEqual => TokenKind::EqualEqual,
            Token::Greater => TokenKind::Greater,
            Token::GreaterEqual => TokenKind::GreaterEqual,
            Token::Less => TokenKind::Less,
            Token::LessEqual => TokenKind::LessEqual,
            Token::IdentifierLiteral(_) => TokenKind::IdentifierLiteral,
            Token::StringLiteral(_) => TokenKind::StringLiteral,
            Token::NumberLiteral(_) => TokenKind::NumberLiteral,
            Token::And => TokenKind::And,
            Token::Or => TokenKind::Or,
            Token::If => TokenKind::If,
            Token::Else => TokenKind::Else,
            Token::For => TokenKind::For,
            Token::While => TokenKind::While,
            Token::Print => TokenKind::Print,
            Token::Var => TokenKind::Var,
            Token::Type => TokenKind::Type,
            Token::Number => TokenKind::Number,
            Token::String => TokenKind::String,
            Token::Bool => TokenKind::Bool,
            Token::True => TokenKind::True,
            Token::False => TokenKind::False,
            Token::Use => TokenKind::Use,
            Token::Export => TokenKind::Export,
            Token::From => TokenKind::From,
            Token::Fun => TokenKind::Fun,
            Token::Struct => TokenKind::Struct,
            Token::Nil => TokenKind::Nil,
            Token::Eof => TokenKind::Eof,
            Token::UnterminatedString => TokenKind::UnterminatedString,
            Token::Unknown(_) => TokenKind::Unknown,
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
                TokenKind::Var => "var",
                TokenKind::Type => "type",
                TokenKind::Number => "number",
                TokenKind::String => "string",
                TokenKind::Bool => "bool",
                TokenKind::True => "true",
                TokenKind::False => "false",
                TokenKind::Use => "use",
                TokenKind::Export => "export",
                TokenKind::From => "from",
                TokenKind::Fun => "fun",
                TokenKind::Struct => "struct",
                TokenKind::Nil => "nil",
                TokenKind::Eof => "<Eof>",
                TokenKind::UnterminatedString => "<UnterminatedString>",
                TokenKind::Unknown => "<Unknown>",
            }
        )
    }
}
