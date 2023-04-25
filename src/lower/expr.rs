use crate::ast::{Method, Visitor, Expr};
use crate::ir::value::Value;

use super::LoweringCtx;

pub fn lower_expr<'a, 'buf>(ctx: &mut LoweringCtx<'a, 'buf>, expr: &Expr<'buf>) -> Value {
    let mut visitor = ExprVisitor { ctx };
    visitor.visit_expr(expr)
}

pub struct ExprVisitor<'a, 'b, 'buf> {
    ctx: &'b mut LoweringCtx<'a, 'buf>,
}

impl<'buf> Visitor<'buf> for ExprVisitor<'_, '_, 'buf> {
    type Output = Value;

    fn visit_program(&mut self, _program: &crate::ast::Program<'buf>) -> Self::Output {
        unimplemented!()
    }

    fn visit_class(&mut self, _class: &crate::ast::Class<'buf>) -> Self::Output {
        unimplemented!()
    }

    fn visit_feature(&mut self, _feature: &crate::ast::Feature<'buf>) -> Self::Output {
        unimplemented!()
    }

    fn visit_method(&mut self, method: &Method<'buf>) -> Self::Output {
        unimplemented!()
    }

    fn visit_field(&mut self, field: &crate::ast::Field<'buf>) -> Self::Output {
        unimplemented!()
    }

    fn visit_expr(&mut self, expr: &crate::ast::Expr<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_assignment(&mut self, expr: &crate::ast::Assignment<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_call(&mut self, expr: &crate::ast::Call<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_if(&mut self, expr: &crate::ast::If<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_while(&mut self, expr: &crate::ast::While<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_block(&mut self, expr: &crate::ast::Block<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_let(&mut self, expr: &crate::ast::Let<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_case(&mut self, expr: &crate::ast::Case<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_new(&mut self, expr: &crate::ast::New<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_bin_op(&mut self, expr: &crate::ast::BinOpExpr<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_un_op(&mut self, expr: &crate::ast::UnOpExpr<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_name_expr(&mut self, expr: &crate::ast::NameExpr<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_formal(&mut self, formal: &crate::ast::Formal<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_receiver(&mut self, recv: &crate::ast::Receiver<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_case_arm(&mut self, arm: &crate::ast::CaseArm<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_ty_name(&mut self, ty_name: &crate::ast::TyName<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_binding(&mut self, binding: &crate::ast::Binding<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_name(&mut self, name: &crate::ast::Name<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_int_lit(&mut self, expr: &crate::ast::IntLit) -> Self::Output {
        todo!()
    }

    fn visit_string_lit(&mut self, expr: &crate::ast::StringLit<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_bool_lit(&mut self, expr: &crate::ast::BoolLit) -> Self::Output {
        todo!()
    }
}
