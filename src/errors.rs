use std::error::Error;
use std::fmt::{self, Display};

use crate::position::{HasSpan, Span};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Level {
    Fatal,
    Error,
    Warn,
    Info,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiagnosticMessage {
    pub span: Option<Span>,
    pub message: String,
}

impl DiagnosticMessage {
    pub fn new(message: String) -> Self {
        Self {
            span: None,
            message,
        }
    }

    pub fn with_span(span: Span, message: String) -> Self {
        Self {
            span: Some(span),
            message,
        }
    }
}

impl Display for DiagnosticMessage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.message)
    }
}

#[derive(Debug)]
pub struct Diagnostic {
    pub level: Level,
    pub message: DiagnosticMessage,
    pub source: Option<Box<dyn Error + 'static>>,
}

impl Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.message)
    }
}

impl Error for Diagnostic {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        self.source.as_deref()
    }
}

#[must_use = "DiagnosticBuilder is useless unless emitted"]
pub struct DiagnosticBuilder<'a> {
    owner: &'a mut Diagnostics,
    level: Level,
    message: Option<DiagnosticMessage>,
    source: Option<Box<dyn Error + 'static>>,
}

pub trait SpannedError: Error + HasSpan {}

impl<T: Error + HasSpan> SpannedError for T {}

impl<'a> DiagnosticBuilder<'a> {
    fn new(owner: &'a mut Diagnostics, level: Level) -> Self {
        Self {
            owner,
            level,
            message: None,
            source: None,
        }
    }

    pub fn with_message(mut self, message: impl Into<DiagnosticMessage>) -> Self {
        let message = message.into();

        self.message = Some(message);

        self
    }

    /// Uses the `error` to fill in the following details of the diagnostic to be emitted:
    /// - the location (unless already set)
    /// - the span (unless already set)
    /// - the message (unless already set)
    /// - the source
    pub fn with_span_and_error(mut self, error: impl SpannedError + 'static) -> Self {
        self.message = self.message.or_else(|| Some(DiagnosticMessage {
            span: Some(error.span().into_owned()),
            message: format!("{}", error),
        }));

        self.source = Some(Box::new(error));

        self
    }

    /// Emits the diagnostic.
    ///
    /// Panics if the message is not set.
    pub fn emit(self) {
        let diagnostic = Diagnostic {
            level: self.level,
            message: self.message.expect("message must be set"),
            source: self.source,
        };

        self.owner.emit(diagnostic);
    }
}

pub struct Diagnostics {
    diagnostics: Vec<Diagnostic>,
}

impl Diagnostics {
    pub fn new() -> Self {
        Self {
            diagnostics: vec![],
        }
    }

    pub fn with_level(&mut self, level: Level) -> DiagnosticBuilder<'_> {
        DiagnosticBuilder::new(self, level)
    }

    pub fn fatal(&mut self) -> DiagnosticBuilder<'_> {
        self.with_level(Level::Fatal)
    }

    pub fn error(&mut self) -> DiagnosticBuilder<'_> {
        self.with_level(Level::Error)
    }

    pub fn warn(&mut self) -> DiagnosticBuilder<'_> {
        self.with_level(Level::Warn)
    }

    pub fn info(&mut self) -> DiagnosticBuilder<'_> {
        self.with_level(Level::Info)
    }

    fn emit(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }
}
