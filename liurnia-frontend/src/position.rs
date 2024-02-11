#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub struct LineColumn {
    pub line: u32,
    pub column: u32,
}

impl Default for LineColumn {
    fn default() -> Self {
        Self { line: 1, column: 0 }
    }
}

impl LineColumn {
    pub fn shift(&self, c: char) -> LineColumn {
        match c {
            '\n' => LineColumn {
                line: self.line + 1,
                column: 0,
            },
            _ => LineColumn {
                line: self.line,
                column: self.column + 1,
            },
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Span {
    pub start: LineColumn,
    pub end: LineColumn,
}

impl Span {
    pub unsafe fn new_unchecked(start: LineColumn, end: LineColumn) -> Self {
        Span { start, end }
    }

    pub const fn empty() -> Self {
        Span {
            start: LineColumn { line: 1, column: 0 },
            end: LineColumn { line: 1, column: 0 },
        }
    }

    pub fn union_span(a: Self, b: Self) -> Self {
        let start = LineColumn {
            line: std::cmp::min(a.start.line, b.start.line),
            column: std::cmp::min(a.start.column, b.start.column),
        };
        let end = LineColumn {
            line: std::cmp::max(a.end.line, b.end.line),
            column: std::cmp::max(a.end.column, b.end.column),
        };
        Span { start, end }
    }

    pub fn union<A, B>(a: &WithSpan<A>, b: &WithSpan<B>) -> Self {
        Self::union_span(a.into(), b.into())
    }

    pub fn easy_span_create(s: (u32, u32), e: (u32, u32)) -> Self {
        Span {
            start: LineColumn {
                line: s.0,
                column: s.1,
            },
            end: LineColumn {
                line: e.0,
                column: e.1,
            },
        }
    }
}

// We use this wrapper for every AST Node
#[derive(Debug, PartialEq, Clone)]
pub struct WithSpan<T> {
    pub value: T,
    pub span: Span,
}

impl<T> From<WithSpan<T>> for Span {
    fn from(with_span: WithSpan<T>) -> Span {
        with_span.span
    }
}

impl<T> From<&WithSpan<T>> for Span {
    fn from(with_span: &WithSpan<T>) -> Span {
        with_span.span
    }
}

impl<T> WithSpan<T> {
    pub const fn new(value: T, span: Span) -> Self {
        WithSpan { value, span }
    }

    pub const fn empty(value: T) -> Self {
        Self {
            value,
            span: Span {
                start: LineColumn { line: 1, column: 0 },
                end: LineColumn { line: 1, column: 0 },
            },
        }
    }

    pub const unsafe fn new_unchecked(value: T, _start: u32, _end: u32) -> Self {
        Self {
            value,
            span: Span {
                start: LineColumn { line: 1, column: 0 },
                end: LineColumn { line: 1, column: 0 },
            },
        }
    }

    pub const fn as_ref(&self) -> WithSpan<&T> {
        WithSpan {
            span: self.span,
            value: &self.value,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Diagnostic {
    pub span: Span,
    pub message: String,
}
