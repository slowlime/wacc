use owo_colors::{OwoColorize, Stream};

use crate::errors::{Diagnostic, DiagnosticMessage, Level};
use crate::position::Span;
use crate::source::Source;

fn format_level(level: Level) -> String {
    match level {
        Level::Fatal => format!(
            "{}",
            "FATAL".if_supports_color(Stream::Stderr, |text| text.red())
        ),

        Level::Error => format!(
            "{}",
            "ERROR".if_supports_color(Stream::Stderr, |text| text.bright_red())
        ),

        Level::Warn => format!(
            "{}",
            "WARN ".if_supports_color(Stream::Stderr, |text| text.yellow())
        ),

        Level::Info => format!(
            "{}",
            "INFO ".if_supports_color(Stream::Stderr, |text| text.bright_cyan())
        ),
    }
}

fn format_span(src: &Source<'_>, span: Option<&Span>) -> String {
    match span {
        None => "".to_owned(),
        Some(span) => format!("{} ", span.display(src)),
    }
}

pub fn print_diagnostic(src: &Source<'_>, diagnostic: &Diagnostic) {
    // TODO: prettier output
    let Diagnostic {
        level,
        message: DiagnosticMessage { span, message },
        source: _,
    } = diagnostic;

    let level = format_level(*level);
    let span = format_span(src, span.as_ref());
    eprintln!("{} {}{}", level, span, message);
}
