use std::borrow::Borrow;
use std::borrow::Cow;
use std::collections::HashMap;

use wast::core::FunctionType;
use wast::core::HeapType;
use wast::core::Instruction;
use wast::core::TypeDef;
use wast::token::Index as WasmIndex;

use crate::analysis::ClassName;
use crate::analysis::TypeCtx;
use crate::ast;
use crate::ast::ty::ResolvedTy;
use crate::ast::ty::UnwrapResolvedTy;
use crate::ast::Class;
use crate::ast::Visitor as AstVisitor;
use crate::position::HasSpan;
use crate::try_match;
use crate::util::slice_formatter;

use super::ctx::ty::WasmTy;
use super::ctx::CompleteWasmTy;
use super::ctx::LocalCtx;
use super::ctx::LocalId;
use super::ctx::MethodDefinition;
use super::ctx::MethodIndex;
use super::ctx::MethodTable;
use super::ctx::MethodTableId;
use super::ctx::StringTable;
use super::ctx::TyIndex;
use super::ctx::Vtable;
use super::ctx::{MethodId, TyId};

trait PositionOffset {
    fn pos(&self) -> usize;
}

impl<T: HasSpan> PositionOffset for T {
    fn pos(&self) -> usize {
        self.span().start.byte
    }
}

pub struct CodegenOutput {
    pub module: wast::core::Module<'static>,
}

pub struct Codegen<'a, 'buf> {
    ty_index: TyIndex<'buf, CompleteWasmTy<'buf>>,
    method_index: MethodIndex<'buf>,
    method_table: MethodTable,
    vtable: Vtable,
    string_table: StringTable<'buf>,
    classes: &'a [Class<'buf>],
    funcs: HashMap<MethodId, wast::core::Func<'static>>,
}

impl<'a, 'buf> Codegen<'a, 'buf> {
    pub fn new(
        ty_index: TyIndex<'buf, CompleteWasmTy<'buf>>,
        method_index: MethodIndex<'buf>,
        method_table: MethodTable,
        vtable: Vtable,
        string_table: StringTable<'buf>,
        classes: &'a [Class<'buf>],
    ) -> Self {
        Self {
            ty_index,
            method_index,
            method_table,
            vtable,
            string_table,
            classes,
            funcs: HashMap::new(),
        }
    }

    pub fn lower(mut self) -> CodegenOutput {
        for class in self.classes {
            self.lower_class(class);
        }

        todo!()
    }

    fn lower_class(&mut self, class: &Class<'buf>) {
        let name: ClassName = class.name.borrow().into();
        let wasm_ty = name.clone().into();
        let Some(ty_id) = self.ty_index.get_by_wasm_ty(&wasm_ty) else {
            panic!("The class {} was not found in the type index", &class.name);
        };

        for feature in &class.features {
            let Some(method) = try_match!(feature, ast::Feature::Method(method) => method) else { continue };
            self.lower_method(ty_id, &name, method);
        }
    }

    fn lower_method(
        &mut self,
        ty_id: TyId,
        class_name: &ClassName<'buf>,
        method: &ast::Method<'buf>,
    ) -> wast::core::Func<'static> {
        let Some(method_id) = self.method_index.get_by_name(ty_id, method.name.as_slice()) else {
            panic!("Method {}::{} was not found in the method index", class_name, &method.name);
        };

        let mut locals = Default::default();
        let mut visitor = CodegenVisitor::new(self, method_id, &mut locals, &method.params);
        let instrs = visitor.visit_expr(&method.body);

        todo!()
    }
}

struct CodegenVisitor<'a, 'cg, 'buf> {
    cg: &'cg Codegen<'a, 'buf>,
    method_id: MethodId,
    locals: &'cg LocalCtx<'buf>,
    params: Vec<LocalId<'cg, 'buf>>,
}

impl<'a, 'cg, 'buf> CodegenVisitor<'a, 'cg, 'buf> {
    pub fn new(
        cg: &'a Codegen<'a, 'buf>,
        method_id: MethodId,
        locals: &'cg mut LocalCtx<'buf>,
        params: &[ast::Formal<'buf>],
    ) -> Self {
        let mut this = Self {
            cg,
            method_id,
            locals,
            params: vec![],
        };
        this.params = this.bind_params(params);

        this
    }
}

impl<'cg, 'buf> CodegenVisitor<'_, 'cg, 'buf> {
    fn def(&self) -> &'cg MethodDefinition {
        let Some((_, def)) = self.cg.method_index.get_by_id(self.method_id) else { unreachable!() };

        def
    }

    fn method_ty_id(&self) -> TyId {
        self.def().method_ty_id
    }

    fn method_complete_ty(&self) -> &'cg CompleteWasmTy<'buf> {
        self.get_ty(self.method_ty_id())
    }

    fn get_ty(&self, ty_id: TyId) -> &'cg CompleteWasmTy<'buf> {
        let Some(ty) = self.cg.ty_index.get_by_id(ty_id) else {
            panic!("The type id {:?} was not found in the type index", ty_id);
        };

        ty
    }

    fn ty_id_for_wasm_ty(&self, wasm_ty: &WasmTy<'buf>) -> TyId {
        match self.cg.ty_index.get_by_wasm_ty(&wasm_ty) {
            Some(ty_id) => ty_id,
            _ => panic!("The type {:?} was not found in the type index", wasm_ty),
        }
    }

    fn ty_id_for_class_name(&self, class_name: ClassName<'buf>) -> TyId {
        self.ty_id_for_wasm_ty(&class_name.into())
    }

    fn ty_id_for_resolved_ty(&self, resolved_ty: ResolvedTy<'buf>) -> TyId {
        self.ty_id_for_wasm_ty(&resolved_ty.into())
    }

    fn method_id_by_name(&self, ty_id: TyId, method_name: &[u8]) -> MethodId {
        match self.cg.method_index.get_by_name(ty_id, method_name) {
            Some(method_id) => method_id,
            None => panic!(
                "The method {} (ty_id = {:?}) was not found in the method index",
                slice_formatter(method_name),
                ty_id
            ),
        }
    }

    fn method_by_id(&self, method_id: MethodId) -> (&'cg [u8], &'cg MethodDefinition) {
        match self.cg.method_index.get_by_id(method_id) {
            Some(res) => res,
            None => panic!(
                "The method id {:?} was not found in the method index",
                method_id,
            ),
        }
    }

    fn method_table_id(&self, method_id: MethodId) -> MethodTableId {
        let (name, def) = self.method_by_id(method_id);
        let method_id = def.last_def_id;

        match self
            .cg
            .method_table
            .get_by_method_id(def.method_ty_id, method_id)
        {
            Some(method_table_id) => method_table_id,
            None => panic!(
                "The last override of the method {} (id {:?}) was not found in the method table",
                slice_formatter(name),
                method_id,
            ),
        }
    }

    fn bind_params(&self, params: &[ast::Formal<'buf>]) -> Vec<LocalId<'cg, 'buf>> {
        let mut result = vec![];
        let wasm_params = match &self.method_complete_ty().wasm_ty {
            WasmTy::Func { params, .. } => params,
            _ => unreachable!(),
        };

        for (i, param_class_name) in wasm_params.into_iter().enumerate() {
            let param_ty_id = self.ty_id_for_class_name(param_class_name.clone());

            let name = match i {
                0 => Cow::Borrowed(b"self".as_slice()),
                _ => Cow::Owned(params[i - 1].name.as_slice().to_owned()),
            };

            result.push(self.locals.bind(Some(name), param_ty_id));
        }

        result
    }

    fn self_ty_id(&self) -> TyId {
        self.method_id.ty_id()
    }

    fn make_method_ref(&self, method_id: MethodId, pos: usize) -> HeapType<'static> {
        HeapType::Index(
            self.method_by_id(method_id)
                .1
                .method_ty_id
                .to_wasm_index(pos),
        )
    }

    fn reify_self_ty(
        &self,
        ty: &ResolvedTy<'buf>,
        pos: usize,
    ) -> Option<Instruction<'static>> {
        match ty {
            ResolvedTy::SelfType { .. } => {
                Some(Instruction::RefCast(self.self_ty_id().to_wasm_index(pos)))
            }
            _ => None,
        }
    }

    fn visit_static_call(&mut self, expr: &ast::Call<'buf>) -> Vec<Instruction<'static>> {
        use wast::core::*;

        let ast::Receiver::Static { object, ty, .. } = &expr.receiver else { unreachable!() };
        let mut result = vec![];

        result.extend(self.visit_expr(&object));

        for arg in &expr.args {
            result.extend(self.visit_expr(arg));
        }

        let ty_id = self.ty_id_for_resolved_ty(ty.unwrap_res_ty().into_owned());
        let method_id = self.method_id_by_name(ty_id, expr.method.as_slice());
        let method_table_id = self.method_table_id(method_id);

        result.push(method_table_id.to_i32_const());
        result.push(Instruction::TableGet(
            method_table_id.to_table_arg(expr.method.pos()),
        ));
        result.push(Instruction::CallRef(
            self.make_method_ref(method_id, expr.method.pos()),
        ));
        result.extend(self.reify_self_ty(&expr.unwrap_res_ty(), expr.method.pos()));

        result
    }

    fn visit_dynamic_call(&mut self, expr: &ast::Call<'buf>) -> Vec<Instruction<'static>> {
        todo!()
    }
}

impl<'buf> AstVisitor<'buf> for CodegenVisitor<'_, '_, 'buf> {
    type Output = Vec<Instruction<'static>>;

    fn visit_program(&mut self, _program: &ast::Program<'buf>) -> Self::Output {
        unreachable!()
    }

    fn visit_class(&mut self, _class: &Class<'buf>) -> Self::Output {
        unreachable!()
    }

    fn visit_feature(&mut self, _feature: &ast::Feature<'buf>) -> Self::Output {
        unreachable!()
    }

    fn visit_method(&mut self, _method: &ast::Method<'buf>) -> Self::Output {
        unreachable!()
    }

    fn visit_field(&mut self, _field: &ast::Field<'buf>) -> Self::Output {
        unreachable!()
    }

    fn visit_expr(&mut self, expr: &ast::Expr<'buf>) -> Self::Output {
        match expr {
            ast::Expr::Assignment(expr) => self.visit_assignment(expr),
            ast::Expr::Call(expr) => self.visit_call(expr),
            ast::Expr::If(expr) => self.visit_if(expr),
            ast::Expr::While(expr) => self.visit_while(expr),
            ast::Expr::Block(expr) => self.visit_block(expr),
            ast::Expr::Let(expr) => self.visit_let(expr),
            ast::Expr::Case(expr) => self.visit_case(expr),
            ast::Expr::New(expr) => self.visit_new(expr),
            ast::Expr::BinOp(expr) => self.visit_bin_op(expr),
            ast::Expr::UnOp(expr) => self.visit_un_op(expr),
            ast::Expr::Name(expr) => self.visit_name_expr(expr),
            ast::Expr::Int(expr) => self.visit_int_lit(expr),
            ast::Expr::String(expr) => self.visit_string_lit(expr),
            ast::Expr::Bool(expr) => self.visit_bool_lit(expr),
        }
    }

    fn visit_assignment(&mut self, expr: &ast::Assignment<'buf>) -> Self::Output {
        // TODO: determine if expr.name is a field
        let value = self.visit_expr(&expr.expr);
        let Some(local_id) = self.locals.get(expr.name.as_slice()) else {
            panic!("Local `{}` was not bound in method {:?}", &expr.name, self.method_id);
        };

        let mut result = value;
        result.push(Instruction::LocalTee(local_id.to_wasm_index(expr.pos())));

        result
    }

    fn visit_call(&mut self, expr: &ast::Call<'buf>) -> Self::Output {
        match expr.receiver {
            ast::Receiver::Static { .. } => self.visit_static_call(expr),
            _ => self.visit_dynamic_call(expr),
        }
    }

    fn visit_if(&mut self, expr: &ast::If<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_while(&mut self, expr: &ast::While<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_block(&mut self, expr: &ast::Block<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_let(&mut self, expr: &ast::Let<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_case(&mut self, expr: &ast::Case<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_new(&mut self, expr: &ast::New<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_bin_op(&mut self, expr: &ast::BinOpExpr<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_un_op(&mut self, expr: &ast::UnOpExpr<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_name_expr(&mut self, expr: &ast::NameExpr<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_formal(&mut self, formal: &ast::Formal<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_receiver(&mut self, recv: &ast::Receiver<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_case_arm(&mut self, arm: &ast::CaseArm<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_ty_name(&mut self, ty_name: &ast::TyName<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_binding(&mut self, binding: &ast::Binding<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_name(&mut self, name: &ast::Name<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_int_lit(&mut self, expr: &ast::IntLit) -> Self::Output {
        todo!()
    }

    fn visit_string_lit(&mut self, expr: &ast::StringLit<'buf>) -> Self::Output {
        todo!()
    }

    fn visit_bool_lit(&mut self, expr: &ast::BoolLit) -> Self::Output {
        todo!()
    }
}
