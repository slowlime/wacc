#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct Position {
    pub byte: usize,
    pub line: usize,
    pub col: usize,
}

impl Default for Position {
    fn default() -> Self {
        Self {
            byte: 0,
            line: 1,
            col: 1,
        }
    }
}

/// A span between two positions, inclusive on the both ends.
#[derive(Debug, Clone, Hash, Eq, PartialEq, Default)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq, Default)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}
