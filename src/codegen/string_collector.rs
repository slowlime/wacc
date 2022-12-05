use crate::ast;
use crate::ast::Class;
use crate::ast::DefaultVisitor as AstDefaultVisitor;

use super::ctx::StringTable;

pub struct StringCollector<'buf, 'cls> {
    classes: &'cls [Class<'buf>],
    string_table: StringTable<'buf>,
}

impl<'buf, 'cls> StringCollector<'buf, 'cls> {
    pub fn new(classes: &'cls [Class<'buf>]) -> Self {
        Self {
            classes,
            string_table: StringTable::new(),
        }
    }

    pub fn collect(mut self) -> StringTable<'buf> {
        for class in self.classes {
            self.visit_class(class);
        }

        self.string_table
    }
}

impl<'buf> AstDefaultVisitor<'buf> for StringCollector<'buf, '_> {
    fn visit_string_lit(&mut self, expr: &ast::StringLit<'buf>) {
        self.string_table.insert(expr.0.value.clone());
    }
}
