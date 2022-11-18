use std::borrow::Cow;
use std::fmt;
use std::num::NonZeroU32;
use std::path::Path;

use crate::source::{Source, SourceFile, SourceId};
use crate::util::{try_min, try_max};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PositionPath<'src> {
    /// src_id is non-empty, and src has a SourceFile with that id
    Path(&'src Path),

    /// src_id is non-empty, but does not correspond to a file stored in src
    Invalid,

    /// src_id is empty
    Unavailable,
}

impl<'src> PositionPath<'src> {
    pub fn new(src_id: Option<SourceId>, src: &'src Source) -> Self {
        match src_id {
            Some(id) => match src.get(id) {
                Some(src_file) => PositionPath::Path(src_file.path()),

                None => PositionPath::Invalid,
            },

            None => PositionPath::Unavailable,
        }
    }
}

impl fmt::Display for PositionPath<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Path(path) => path.display().fmt(f),
            Self::Invalid => write!(f, "<invalid file>"),
            Self::Unavailable => write!(f, "<anon>"),
        }
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct Position {
    pub src: Option<SourceId>,
    pub byte: usize,
    pub line: NonZeroU32,
    pub col: NonZeroU32,
}

impl Position {
    pub fn with_source_file(src_file: &SourceFile<'_>) -> Self {
        Self::with_source_id(src_file.id())
    }

    pub fn with_source_id(src: SourceId) -> Self {
        Self {
            src: Some(src),
            byte: 0,
            line: 1.try_into().unwrap(),
            col: 1.try_into().unwrap(),
        }
    }

    pub fn with_anon_source() -> Self {
        Self {
            src: None,
            byte: 0,
            line: 1.try_into().unwrap(),
            col: 1.try_into().unwrap(),
        }
    }

    pub fn display<'src>(&self, src: &'src Source<'_>) -> impl fmt::Display + Clone + 'src {
        #[derive(Debug, Clone)]
        struct PositionFormatter<'src> {
            path: PositionPath<'src>,
            pos: Position,
        }

        impl fmt::Display for PositionFormatter<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}:{}:{}", self.path, self.pos.line, self.pos.col)
            }
        }

        PositionFormatter {
            path: PositionPath::new(self.src, src),
            pos: *self,
        }
    }
}

impl PartialOrd for Position {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        (self.src == other.src).then(|| self.byte.cmp(&other.byte))
    }
}

impl fmt::Debug for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Position({}:{}, {} bytes into {:?})",
            self.line, self.col, self.byte, self.src
        )
    }
}

/// A span between two positions, inclusive on the both ends.
#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub fn convex_hull(&self, other: &Span) -> Span {
        Span {
            start: *try_min(&self.start, &other.start).unwrap_or(&self.start),
            end: *try_max(&self.end, &other.start).unwrap_or(&self.end),
        }
    }

    pub fn display<'src>(&self, src: &'src Source<'_>) -> impl fmt::Display + Clone + 'src {
        #[derive(Debug, Clone)]
        struct SpanFormatter<'src> {
            start_path: PositionPath<'src>,
            end_path: PositionPath<'src>,
            start: Position,
            end: Position,
        }

        impl fmt::Display for SpanFormatter<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                if self.start_path != self.end_path {
                    write!(
                        f,
                        "{}:{}:{} - {}:{}:{}",
                        self.start_path,
                        self.start.line,
                        self.start.col,
                        self.end_path,
                        self.end.line,
                        self.end.col,
                    )
                } else if self.start.line != self.end.line {
                    write!(
                        f,
                        "{}:{}:{}-{}:{}",
                        self.start_path,
                        self.start.line,
                        self.start.col,
                        self.end.line,
                        self.end.col,
                    )
                } else if self.start.col != self.end.col {
                    write!(
                        f,
                        "{}:{}:{}-{}",
                        self.start_path, self.start.line, self.start.col, self.end.col,
                    )
                } else {
                    write!(
                        f,
                        "{}:{}:{}",
                        self.start_path, self.start.line, self.start.col
                    )
                }
            }
        }

        SpanFormatter {
            start_path: PositionPath::new(self.start.src, src),
            end_path: PositionPath::new(self.end.src, src),
            start: self.start,
            end: self.end,
        }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.start.src != self.end.src {
            write!(f, "Span({:?} - {:?})", self.start, self.end)
        } else if self.start.line != self.end.line {
            write!(
                f,
                "Span({}:{}-{}:{}, bytes {}-{} in {:?})",
                self.start.line,
                self.start.col,
                self.end.line,
                self.end.col,
                self.start.byte,
                self.end.byte,
                self.start.src,
            )
        } else if self.start.col != self.end.col {
            write!(
                f,
                "Span({}:{}-{}, bytes {}-{} in {:?})",
                self.start.line,
                self.start.col,
                self.end.col,
                self.start.byte,
                self.end.byte,
                self.start.src,
            )
        } else if self.start.byte != self.end.byte {
            write!(
                f,
                "Span({}:{}, bytes {}-{} in {:?})",
                self.start.line, self.start.col, self.start.byte, self.end.byte, self.start.src,
            )
        } else {
            write!(
                f,
                "Span({}:{}, {} bytes into {:?})",
                self.start.line, self.start.col, self.start.byte, self.start.src,
            )
        }
    }
}

pub trait HasSpan {
    fn span(&self) -> Cow<'_, Span>;
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> HasSpan for Spanned<T> {
    fn span(&self) -> Cow<'_, Span> {
        Cow::Borrowed(&self.span)
    }
}
