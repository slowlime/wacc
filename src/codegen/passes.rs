use crate::ast::Class;

use super::ctx::StringTable;
use super::string_collector::StringCollector;

pub use super::ctx::passes::*;

pub fn collect_strings<'buf>(classes: &[Class<'buf>]) -> StringTable<'buf> {
    StringCollector::new(classes).collect()
}
