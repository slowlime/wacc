use std::fmt::Debug;

use crate::ast::ty::{HasTy, TyExt};
use crate::ast::{self, AstRecurse, Class, Visitor as AstVisitor};
use crate::position::{HasSpan, Span};
use crate::source::Source;

trait AssertResolved {
    fn assert_resolved(&self, source: &Source<'_>, span: &Span);
}

impl<'a, T> AssertResolved for T
where
    T: TyExt<'a> + Debug,
{
    fn assert_resolved(&self, source: &Source<'_>, span: &Span) {
        if !self.is_resolved() {
            panic!(
                "Type {:?} (spanning {}) has not been resolved",
                self,
                span.display(source)
            );
        }
    }
}

struct Validator<'a, 'buf> {
    source: &'a Source<'buf>,
    classes: &'a [Class<'buf>],
}

impl<'buf> Validator<'_, 'buf> {
    fn validate(mut self) {
        for class in self.classes {
            self.visit_class(class);
        }
    }

    fn assert_resolved(&self, span: &Span, ty: &impl AssertResolved) {
        ty.assert_resolved(self.source, span);
    }
}

impl<'buf> ast::Visitor<'buf> for Validator<'_, 'buf> {
    type Output = ();

    fn visit_program(&mut self, program: &ast::Program<'buf>) -> Self::Output {
        program.recurse(self);
    }

    fn visit_class(&mut self, class: &Class<'buf>) -> Self::Output {
        self.assert_resolved(&class.name.span(), &class.ty);
        class.recurse(self);
    }

    fn visit_feature(&mut self, feature: &ast::Feature<'buf>) -> Self::Output {
        feature.recurse(self);
    }

    fn visit_method(&mut self, method: &ast::Method<'buf>) -> Self::Output {
        self.assert_resolved(&method.name.span(), &method.ty);
        method.recurse(self);
    }

    fn visit_field(&mut self, field: &ast::Field<'buf>) -> Self::Output {
        field.recurse(self);
    }

    fn visit_expr(&mut self, expr: &ast::Expr<'buf>) -> Self::Output {
        self.assert_resolved(&expr.span(), &expr.ty().as_deref());
        expr.recurse(self);
    }

    fn visit_assignment(&mut self, expr: &ast::Assignment<'buf>) -> Self::Output {
        self.assert_resolved(&expr.span(), &expr.ty().as_deref());
        expr.recurse(self);
    }

    fn visit_call(&mut self, expr: &ast::Call<'buf>) -> Self::Output {
        self.assert_resolved(&expr.span(), &expr.ty().as_deref());
        expr.recurse(self);
    }

    fn visit_if(&mut self, expr: &ast::If<'buf>) -> Self::Output {
        self.assert_resolved(&expr.span(), &expr.ty().as_deref());
        expr.recurse(self);
    }

    fn visit_while(&mut self, expr: &ast::While<'buf>) -> Self::Output {
        self.assert_resolved(&expr.span(), &expr.ty().as_deref());
        expr.recurse(self);
    }

    fn visit_block(&mut self, expr: &ast::Block<'buf>) -> Self::Output {
        self.assert_resolved(&expr.span(), &expr.ty().as_deref());
        expr.recurse(self);
    }

    fn visit_let(&mut self, expr: &ast::Let<'buf>) -> Self::Output {
        self.assert_resolved(&expr.span(), &expr.ty().as_deref());
        expr.recurse(self);
    }

    fn visit_case(&mut self, expr: &ast::Case<'buf>) -> Self::Output {
        self.assert_resolved(&expr.span(), &expr.ty().as_deref());
        expr.recurse(self);
    }

    fn visit_new(&mut self, expr: &ast::New<'buf>) -> Self::Output {
        self.assert_resolved(&expr.span(), &expr.ty().as_deref());
        expr.recurse(self);
    }

    fn visit_bin_op(&mut self, expr: &ast::BinOpExpr<'buf>) -> Self::Output {
        self.assert_resolved(&expr.span(), &expr.ty().as_deref());
        expr.recurse(self);
    }

    fn visit_un_op(&mut self, expr: &ast::UnOpExpr<'buf>) -> Self::Output {
        self.assert_resolved(&expr.span(), &expr.ty().as_deref());
        expr.recurse(self);
    }

    fn visit_name_expr(&mut self, expr: &ast::NameExpr<'buf>) -> Self::Output {
        self.assert_resolved(&expr.span(), &expr.ty().as_deref());
        expr.recurse(self);
    }

    fn visit_formal(&mut self, formal: &ast::Formal<'buf>) -> Self::Output {
        self.assert_resolved(&formal.span(), &formal.ty);
    }

    fn visit_receiver(&mut self, recv: &ast::Receiver<'buf>) -> Self::Output {
        self.assert_resolved(&recv.span(), &recv.ty().as_deref());
        recv.recurse(self);
    }

    fn visit_case_arm(&mut self, arm: &ast::CaseArm<'buf>) -> Self::Output {
        self.assert_resolved(&arm.binding_ty_name.span(), &arm.binding_ty);
        arm.recurse(self);
    }

    fn visit_ty_name(&mut self, _ty_name: &ast::TyName<'buf>) -> Self::Output {}

    fn visit_binding(&mut self, binding: &ast::Binding<'buf>) -> Self::Output {
        self.assert_resolved(&binding.span, &binding.ty);
        binding.recurse(self);
    }

    fn visit_name(&mut self, _name: &ast::Name<'buf>) -> Self::Output {}

    fn visit_int_lit(&mut self, expr: &ast::IntLit) -> Self::Output {
        self.assert_resolved(&expr.span(), &expr.ty().as_deref());
    }

    fn visit_string_lit(&mut self, expr: &ast::StringLit<'buf>) -> Self::Output {
        self.assert_resolved(&expr.span(), &expr.ty().as_deref());
    }

    fn visit_bool_lit(&mut self, expr: &ast::BoolLit) -> Self::Output {
        self.assert_resolved(&expr.span(), &expr.ty().as_deref());
    }
}

pub fn validate_classes<'buf>(source: &Source<'buf>, classes: &[Class<'buf>]) {
    let validator = Validator { classes, source };
    validator.validate();
}
