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

    pub fn new_with_span(span: Span, message: String) -> Self {
        Self::new(message).with_span(span)
    }

    pub fn with_span(self, span: Span) -> Self {
        Self {
            span: Some(span),
            ..self
        }
    }
}

impl Display for DiagnosticMessage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.message)
    }
}

impl From<String> for DiagnosticMessage {
    fn from(s: String) -> DiagnosticMessage {
        DiagnosticMessage::new(s)
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
pub struct DiagnosticBuilder<'a, 'e> {
    owner: &'a mut Diagnostics<'e>,
    level: Level,
    message: Option<DiagnosticMessage>,
    source: Option<Box<dyn Error + 'static>>,
}

pub trait SpannedError: Error + HasSpan {}

impl<T: Error + HasSpan> SpannedError for T {}

impl<'a, 'e> DiagnosticBuilder<'a, 'e> {
    fn new(owner: &'a mut Diagnostics<'e>, level: Level) -> Self {
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

    pub fn with_source(self, source: Box<dyn Error + 'static>) -> Self {
        Self {
            source: Some(source),
            ..self
        }
    }

    /// Uses the `error` to fill in the following details of the diagnostic to be emitted:
    /// - the location (unless already set)
    /// - the span (unless already set)
    /// - the message (unless already set)
    /// - the source
    pub fn with_span_and_error(mut self, error: impl SpannedError + 'static) -> Self {
        self.message = self.message.or_else(|| {
            Some(DiagnosticMessage {
                span: Some(error.span().into_owned()),
                message: format!("{}", error),
            })
        });

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

fn null_emitter(_: &Diagnostic) {}

pub struct Diagnostics<'e> {
    diagnostics: Vec<Diagnostic>,
    emitter: Box<dyn FnMut(&Diagnostic) + 'e>,
    has_errors: bool,
}

impl<'e> Diagnostics<'e> {
    pub fn new() -> Self {
        Self {
            diagnostics: vec![],
            emitter: Box::new(null_emitter),
            has_errors: false,
        }
    }

    pub fn set_emitter(&mut self, emitter: Box<dyn FnMut(&Diagnostic) + 'e>) {
        self.emitter = emitter;
    }

    pub fn has_errors(&self) -> bool {
        self.has_errors
    }

    pub fn with_level(&mut self, level: Level) -> DiagnosticBuilder<'_, 'e> {
        DiagnosticBuilder::new(self, level)
    }

    pub fn fatal(&mut self) -> DiagnosticBuilder<'_, 'e> {
        self.with_level(Level::Fatal)
    }

    pub fn error(&mut self) -> DiagnosticBuilder<'_, 'e> {
        self.with_level(Level::Error)
    }

    pub fn warn(&mut self) -> DiagnosticBuilder<'_, 'e> {
        self.with_level(Level::Warn)
    }

    pub fn info(&mut self) -> DiagnosticBuilder<'_, 'e> {
        self.with_level(Level::Info)
    }

    fn emit(&mut self, diagnostic: Diagnostic) {
        self.has_errors = self.has_errors || diagnostic.level <= Level::Error;
        (self.emitter)(&diagnostic);
        self.diagnostics.push(diagnostic);
    }

    pub fn into_vec(self) -> Vec<Diagnostic> {
        self.diagnostics
    }
}

impl Default for Diagnostics<'_> {
    fn default() -> Self {
        Self::new()
    }
}
