use std::borrow::Cow;

use wast::core::HeapType;
use wast::core::Instruction;
use wast::token::Index as WasmIndex;
use wast::token::Span as WasmSpan;

use crate::analysis::ClassName;
use crate::ast;
use crate::ast::ty::BuiltinClass;
use crate::ast::ty::ResolvedTy;
use crate::ast::ty::UnwrapResolvedTy;
use crate::ast::Class;
use crate::ast::Visitor as AstVisitor;
use crate::codegen::PositionOffset;
use crate::util::slice_formatter;

use super::Codegen;
use super::builtin::BUILTIN_FUNCS;
use super::ctx::ty::constructor_ty;
use super::ctx::ty::WasmTy;
use super::ctx::ty::CONSTRUCTOR_NAME;
use super::ctx::CompleteWasmTy;
use super::ctx::FieldId;
use super::ctx::LocalCtx;
use super::ctx::LocalId;
use super::ctx::MethodDefinition;
use super::ctx::MethodTableId;
use super::ctx::TableId;
use super::ctx::TyKind;
use super::ctx::VtableId;
use super::ctx::{MethodId, TyId};

enum NameKind<'ctx, 'buf> {
    Local(LocalId<'ctx, 'buf>),
    Field(FieldId),
}

pub struct CodegenVisitor<'a, 'cg, 'buf> {
    cg: &'cg Codegen<'a, 'buf>,
    method_id: MethodId,
    locals: &'cg LocalCtx<'buf>,
    params: Vec<LocalId<'cg, 'buf>>,
}

impl<'a, 'cg, 'buf> CodegenVisitor<'a, 'cg, 'buf> {
    pub fn new(
        cg: &'a Codegen<'a, 'buf>,
        method_id: MethodId,
        locals: &'cg LocalCtx<'buf>,
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

    fn table_id_for_method_ty(&self, method_ty_id: TyId) -> TableId {
        match self.cg.method_table.get_table_id(method_ty_id) {
            Some(table_id) => table_id,
            None => panic!(
                "The method type {:?} has no associated method table",
                method_ty_id
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
                _ => params[i - 1].name.0.value.clone(),
            };

            result.push(self.locals.bind(Some(name), param_ty_id));
        }

        result
    }

    fn get_field_id(&self, ty_id: TyId, field_name: &[u8]) -> FieldId {
        match self.cg.field_table.get_by_name(ty_id, field_name) {
            Some(field_id) => field_id,

            None => panic!(
                "The field {} (ty id {:?}) was not found in the field table",
                slice_formatter(field_name),
                ty_id,
            ),
        }
    }

    fn get_name(&self, name: &[u8]) -> NameKind<'cg, 'buf> {
        if let Some(local_id) = self.locals.get(name) {
            NameKind::Local(local_id)
        } else {
            NameKind::Field(self.get_field_id(self.self_ty_id(), name))
        }
    }

    fn self_ty_id(&self) -> TyId {
        self.method_id.ty_id()
    }

    fn builtin_ty_id(&self, builtin: BuiltinClass) -> TyId {
        self.ty_id_for_wasm_ty(&WasmTy::Class(ClassName::Builtin(builtin)))
    }

    fn object_ty_id(&self) -> TyId {
        self.builtin_ty_id(BuiltinClass::Object)
    }

    fn vtable_base_field(&self, pos: usize) -> WasmIndex<'static> {
        WasmIndex::Num(0, WasmSpan::from_offset(pos))
    }

    fn make_method_ref(&self, method_id: MethodId, pos: usize) -> HeapType<'static> {
        HeapType::Index(
            self.method_by_id(method_id)
                .1
                .method_ty_id
                .to_wasm_index(pos),
        )
    }

    fn make_ty_ref(&self, ty_id: TyId, pos: usize) -> HeapType<'static> {
        HeapType::Index(ty_id.to_wasm_index(pos))
    }

    fn reify_self_ty(&self, ty: &ResolvedTy<'buf>, pos: usize) -> Option<Instruction<'static>> {
        match ty {
            ResolvedTy::SelfType { .. } => {
                Some(Instruction::RefCast(self.self_ty_id().to_wasm_index(pos)))
            }
            _ => None,
        }
    }

    fn unbox(&self, ty: &ResolvedTy<'buf>, pos: usize) -> Option<Instruction<'static>> {
        use wast::core::*;

        let ty_id = match ty {
            &ResolvedTy::Builtin(
                builtin @ (BuiltinClass::Int | BuiltinClass::Bool | BuiltinClass::String),
            ) => self.builtin_ty_id(builtin),

            _ => return None,
        };

        Some(Instruction::StructGet(StructAccess {
            r#struct: ty_id.to_wasm_index(pos),
            field: WasmIndex::Num(1, WasmSpan::from_offset(pos)),
        }))
    }

    fn r#box(&self, ty: &ResolvedTy<'buf>, pos: usize) -> Vec<Instruction<'static>> {
        use wast::core::*;

        let mut result = vec![];

        let (ty_id, ty_kind) = match ty {
            &ResolvedTy::Builtin(builtin @ (BuiltinClass::Int | BuiltinClass::Bool)) => {
                (self.builtin_ty_id(builtin), TyKind::I32)
            }

            &ResolvedTy::Builtin(builtin @ BuiltinClass::String) => (
                self.builtin_ty_id(builtin),
                self.ty_id_for_wasm_ty(&WasmTy::ByteArray).into(),
            ),

            _ => return vec![],
        };

        let value_temp = self.locals.bind(None, ty_kind);
        result.push(value_temp.wasm_set(pos));
        result.extend(self.construct_new(ty_id, pos));
        result.push(value_temp.wasm_get(pos));
        result.push(Instruction::StructSet(StructAccess {
            r#struct: ty_id.to_wasm_index(pos),
            field: WasmIndex::Num(1, WasmSpan::from_offset(pos)),
        }));

        result
    }

    fn constructor_ty_id(&self) -> TyId {
        self.ty_id_for_wasm_ty(&constructor_ty())
    }

    fn constructor_table_id(&self) -> TableId {
        self.table_id_for_method_ty(self.constructor_ty_id())
    }

    fn constructor_method_id(&self) -> MethodId {
        self.method_id_by_name(self.self_ty_id(), CONSTRUCTOR_NAME)
    }

    fn construct_self(&self, pos: usize) -> Vec<Instruction<'static>> {
        use wast::core::*;

        // `new SELF_TYPE` is basically the same as `self.{new}()`
        let mut result = vec![];

        result.extend(self.push_self(pos));
        result.push(Instruction::StructGet(StructAccess {
            r#struct: self.self_ty_id().to_wasm_index(pos),
            field: self.vtable_base_field(pos),
        }));
        result.push(Instruction::TableGet(self.vtable_table_arg(pos)));
        result.push(Instruction::I31GetU);
        let constructor_table_id = self.constructor_table_id();
        result.push(Instruction::TableGet(
            constructor_table_id.to_table_arg(pos),
        ));
        result.push(Instruction::CallRef(
            self.make_method_ref(self.constructor_method_id(), pos),
        ));
        result.push(Instruction::RefCast(self.self_ty_id().to_wasm_index(pos)));

        result
    }

    fn base_offset(&self, ty_id: TyId) -> VtableId {
        match self.cg.vtable.base_offset(ty_id) {
            Some(vtable_id) => vtable_id,

            None => panic!("The type id {:?} was not found in the vtable", ty_id,),
        }
    }

    fn construct_new(&self, ty_id: TyId, pos: usize) -> Vec<Instruction<'static>> {
        use wast::core::*;

        let mut result = vec![];

        let vtable_id = self.base_offset(ty_id);
        let method_table_id = self.cg.vtable.get(vtable_id).unwrap();

        result.push(method_table_id.to_i32_const());
        result.push(Instruction::TableGet(
            method_table_id.table_id().to_table_arg(pos),
        ));
        result.push(Instruction::CallRef(
            self.make_method_ref(self.constructor_method_id(), pos),
        ));

        result
    }

    fn call_string_eq(&self, pos: usize) -> Instruction<'static> {
        let Some(func_id) = self.cg.func_registry.get_by_name(&BUILTIN_FUNCS.string_eq) else {
            panic!(
                "The function {} was not defined in the function registry",
                &BUILTIN_FUNCS.string_eq
            );
        };

        Instruction::Call(func_id.to_wasm_index(pos))
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
            method_table_id.table_id().to_table_arg(expr.method.pos()),
        ));
        result.push(Instruction::CallRef(
            self.make_method_ref(method_id, expr.method.pos()),
        ));
        result.extend(self.reify_self_ty(&expr.unwrap_res_ty(), expr.method.pos()));

        result
    }

    fn push_self(&self, pos: usize) -> Vec<Instruction<'static>> {
        vec![
            self.params[0].wasm_get(pos),
            Instruction::RefCast(self.self_ty_id().to_wasm_index(pos)),
        ]
    }

    fn vtable_table_arg(&self, pos: usize) -> wast::core::TableArg<'static> {
        self.cg.vtable_table_id.to_table_arg(pos)
    }

    fn string_table_arg(&self, pos: usize) -> wast::core::TableArg<'static> {
        self.cg.string_table_id.to_table_arg(pos)
    }

    fn visit_dynamic_call(&mut self, expr: &ast::Call<'buf>) -> Vec<Instruction<'static>> {
        use wast::core::*;

        let mut result = vec![];

        let recv_ty_id = match expr.receiver {
            ast::Receiver::SelfType {
                ref method_name_span,
                ..
            } => {
                result.extend(self.push_self(method_name_span.pos()));

                self.self_ty_id()
            }

            ast::Receiver::Dynamic(ref expr) => {
                result.extend(self.visit_expr(expr));

                self.ty_id_for_resolved_ty(expr.unwrap_res_ty().into_owned())
            }

            ast::Receiver::Static { .. } => unreachable!(),
        };

        let receiver_temp = self.locals.bind(None, recv_ty_id);
        result.push(receiver_temp.wasm_tee(expr.receiver.pos()));

        for arg in &expr.args {
            result.extend(self.visit_expr(arg));
        }

        result.push(receiver_temp.wasm_get(expr.receiver.pos()));
        // stack: <receiver> <args...> <receiver>
        // local receiver_temp = <receiver>

        result.push(Instruction::RefCast(
            self.object_ty_id().to_wasm_index(expr.method.pos()),
        ));
        result.push(Instruction::StructGet(StructAccess {
            r#struct: self.object_ty_id().to_wasm_index(expr.method.pos()),
            field: self.vtable_base_field(expr.method.pos()),
        }));
        // stack: <receiver> <args...> <receiver.vtable_base>

        let method_id = self.method_id_by_name(recv_ty_id, expr.method.as_slice());
        let (_, def) = self.method_by_id(method_id);
        let method_ty_id = def.method_ty_id;
        let method_table_id = self.table_id_for_method_ty(method_ty_id);
        let offset = method_id.index().try_into().unwrap();
        result.push(Instruction::I32Const(offset));
        result.push(Instruction::I32Add);
        // stack: <receiver> <args...> <receiver.vtable_base + offset>
        result.push(Instruction::TableGet(
            self.vtable_table_arg(expr.method.pos()),
        ));
        result.push(Instruction::I31GetU);
        // stack: <receiver> <args...> <method_table_method_idx>
        result.push(Instruction::TableGet(
            method_table_id.to_table_arg(expr.method.pos()),
        ));
        // stack: <receiver> <args...> <method_ref>
        result.push(Instruction::CallRef(
            self.make_method_ref(method_id, expr.method.pos()),
        ));
        result.extend(self.reify_self_ty(&expr.unwrap_res_ty(), expr.method.pos()));

        result
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
        let mut result = vec![];

        let name_kind = self.get_name(expr.name.as_slice());

        match name_kind {
            NameKind::Local(local_id) => {
                result.extend(self.visit_expr(&expr.expr));
                result.push(local_id.wasm_tee(expr.pos()));
            }

            NameKind::Field(field_id) => {
                result.extend(self.push_self(expr.pos()));
                result.extend(self.visit_expr(&expr.expr));
                result.push(field_id.wasm_set(expr.pos()));
                result.push(field_id.wasm_get(expr.pos()));
            }
        }

        result
    }

    fn visit_call(&mut self, expr: &ast::Call<'buf>) -> Self::Output {
        match expr.receiver {
            ast::Receiver::Static { .. } => self.visit_static_call(expr),
            _ => self.visit_dynamic_call(expr),
        }
    }

    fn visit_if(&mut self, expr: &ast::If<'buf>) -> Self::Output {
        use wast::core::*;

        let mut result = vec![];
        let ty_id = self.ty_id_for_resolved_ty(expr.unwrap_res_ty().into_owned());

        result.extend(self.visit_expr(&expr.antecedent));
        result.extend(self.unbox(&BuiltinClass::Bool.into(), expr.antecedent.pos()));
        // stack: <antecedent: i32>

        result.push(Instruction::If(BlockType {
            label: None,
            label_name: None,
            ty: TypeUse {
                index: None,
                inline: Some(FunctionType {
                    params: Box::new([]),
                    results: Box::new([ValType::Ref(RefType {
                        nullable: true,
                        heap: self.make_ty_ref(ty_id, expr.antecedent.pos()),
                    })]),
                }),
            },
        }));

        result.extend(self.visit_expr(&expr.consequent));
        result.push(Instruction::Else(None));
        result.extend(self.visit_expr(&expr.alternative));
        result.push(Instruction::End(None));

        result
    }

    fn visit_while(&mut self, expr: &ast::While<'buf>) -> Self::Output {
        use wast::core::*;

        let mut result = vec![];

        result.push(Instruction::Block(BlockType {
            label: None,
            label_name: None,
            ty: TypeUse {
                index: None,
                inline: None,
            },
        }));

        result.push(Instruction::Loop(BlockType {
            label: None,
            label_name: None,
            ty: TypeUse {
                index: None,
                inline: None,
            },
        }));

        result.extend(self.visit_expr(&expr.condition));
        result.extend(self.unbox(&BuiltinClass::Bool.into(), expr.condition.pos()));
        result.push(Instruction::I32Eqz);
        // stack: [block:1] [loop:0] <!antecedent: i32>
        result.push(Instruction::BrIf(WasmIndex::Num(
            1,
            WasmSpan::from_offset(expr.condition.pos()),
        )));
        result.extend(self.visit_expr(&expr.body));
        result.push(Instruction::Drop);
        // stack: [block:1] [loop:0]
        result.push(Instruction::Br(WasmIndex::Num(
            0,
            WasmSpan::from_offset(expr.pos()),
        )));
        result.push(Instruction::End(None));
        result.push(Instruction::End(None));

        // push a dummy Object
        result.push(Instruction::RefNull(
            self.make_ty_ref(self.object_ty_id(), expr.pos()),
        ));

        result
    }

    fn visit_block(&mut self, expr: &ast::Block<'buf>) -> Self::Output {
        let mut result = vec![];

        for (idx, inner_expr) in expr.body.iter().enumerate() {
            result.extend(self.visit_expr(inner_expr));

            if idx + 1 < expr.body.len() {
                result.push(Instruction::Drop);
            }
        }

        result
    }

    fn visit_let(&mut self, expr: &ast::Let<'buf>) -> Self::Output {
        let mut result = vec![];
        let local_ty_id = self.ty_id_for_resolved_ty(expr.binding.unwrap_res_ty().into_owned());

        if let Some(init) = &expr.binding.init {
            result.extend(self.visit_expr(init));
        } else {
            result.push(Instruction::RefNull(
                self.make_ty_ref(local_ty_id, expr.binding.pos()),
            ));
        }

        let local_id = self
            .locals
            .bind(Some(expr.binding.name.0.value.clone()), local_ty_id);
        result.push(local_id.wasm_set(expr.binding.pos()));
        result.extend(self.visit_expr(&expr.expr));

        result
    }

    fn visit_case(&mut self, expr: &ast::Case<'buf>) -> Self::Output {
        use wast::core::*;

        let mut result = vec![];
        let ty_id = self.ty_id_for_resolved_ty(expr.unwrap_res_ty().into_owned());

        result.push(Instruction::Block(BlockType {
            label: None,
            label_name: None,
            ty: TypeUse {
                index: None,
                inline: Some(FunctionType {
                    params: Box::new([]),
                    results: Box::new([ValType::Ref(RefType {
                        nullable: true,
                        heap: self.make_ty_ref(ty_id, expr.pos()),
                    })]),
                }),
            },
        }));
        result.extend(self.visit_expr(&expr.scrutinee));
        // stack: [block:0 -> ty_id] <scrutinee>

        for (idx, arm) in expr.arms.iter().enumerate() {
            let arm_ty_id = self.ty_id_for_resolved_ty(arm.binding_ty.unwrap_res_ty().into_owned());

            if idx + 1 < expr.arms.len() {
                result.push(Instruction::Block(BlockType {
                    label: None,
                    label_name: None,
                    ty: TypeUse {
                        index: None,
                        inline: Some(FunctionType {
                            params: Box::new([]),
                            results: Box::new([ValType::Ref(RefType {
                                nullable: true,
                                heap: self.make_ty_ref(ty_id, arm.pos()),
                            })]),
                        }),
                    },
                }));

                // stack: [block:1 -> ty_id] <scrutinee> [block:0 scrutinee_ty_id -> ty_id]
                result.push(Instruction::BrOnCastFail(BrOnCast {
                    label: WasmIndex::Num(0, WasmSpan::from_offset(arm.pos())),
                    r#type: arm_ty_id.to_wasm_index(arm.binding_ty_name.pos()),
                }));
                // stack: [block:1 -> ty_id] [block:0 scrutinee_ty_id -> ty_id] <scrutinee: arm_ty_id>
            } else {
                result.push(Instruction::RefCast(
                    arm_ty_id.to_wasm_index(arm.binding_ty_name.pos()),
                ));
            }

            let arm_local_id = self.locals.bind(Some(arm.name.0.value.clone()), arm_ty_id);
            result.extend(self.visit_expr(&arm.expr));
            drop(arm_local_id);
            result.push(Instruction::RefCast(ty_id.to_wasm_index(arm.expr.pos())));

            if idx + 1 < expr.arms.len() {
                result.push(Instruction::Br(WasmIndex::Num(
                    1,
                    WasmSpan::from_offset(arm.pos()),
                )));
                result.push(Instruction::End(None));
            }
        }

        result.push(Instruction::End(None));

        result
    }

    fn visit_new(&mut self, expr: &ast::New<'buf>) -> Self::Output {
        let ty = expr.unwrap_res_ty();

        match *ty {
            ResolvedTy::SelfType { .. } => self.construct_self(expr.pos()),
            _ => self.construct_new(self.ty_id_for_resolved_ty(ty.into_owned()), expr.pos()),
        }
    }

    fn visit_bin_op(&mut self, expr: &ast::BinOpExpr<'buf>) -> Self::Output {
        use ast::BinOpKind;

        let mut result = vec![];

        result.extend(self.visit_expr(&expr.lhs));
        result.extend(self.unbox(&expr.lhs.unwrap_res_ty(), expr.lhs.pos()));
        result.extend(self.visit_expr(&expr.rhs));
        result.extend(self.unbox(&expr.rhs.unwrap_res_ty(), expr.rhs.pos()));

        match expr.op {
            BinOpKind::Add => result.push(Instruction::I32Add),
            BinOpKind::Subtract => result.push(Instruction::I32Sub),
            BinOpKind::Multiply => result.push(Instruction::I32Mul),
            BinOpKind::Divide => result.push(Instruction::I32DivS),
            BinOpKind::LessThan => result.push(Instruction::I32LtS),
            BinOpKind::LessEquals => result.push(Instruction::I32LeS),

            BinOpKind::Equals => match *expr.lhs.unwrap_res_ty() {
                ResolvedTy::Builtin(BuiltinClass::Int) => {
                    result.push(Instruction::I32Eq);
                }

                ResolvedTy::Builtin(BuiltinClass::String) => {
                    result.push(self.call_string_eq(expr.pos()));
                }

                ResolvedTy::Builtin(BuiltinClass::Bool) => {
                    result.push(Instruction::I32Eq);
                }

                _ => {
                    result.push(Instruction::RefEq);
                }
            },
        }

        result.extend(self.r#box(&expr.unwrap_res_ty(), expr.pos()));

        result
    }

    fn visit_un_op(&mut self, expr: &ast::UnOpExpr<'buf>) -> Self::Output {
        use ast::UnOpKind;

        let mut result = vec![];
        result.extend(self.visit_expr(&expr.expr));

        match expr.op {
            UnOpKind::IsVoid => {
                result.push(Instruction::RefIsNull);
            }

            UnOpKind::Complement => {
                result.extend(self.unbox(&BuiltinClass::Int.into(), expr.expr.pos()));
                result.push(Instruction::I32Const(-1i32));
                result.push(Instruction::I32Xor);
            }

            UnOpKind::Not => {
                result.extend(self.unbox(&BuiltinClass::Bool.into(), expr.expr.pos()));
                result.push(Instruction::I32Eqz);
            }
        }

        result.extend(self.r#box(&expr.unwrap_res_ty(), expr.pos()));

        result
    }

    fn visit_name_expr(&mut self, expr: &ast::NameExpr<'buf>) -> Self::Output {
        let name_kind = self.get_name(expr.name.as_slice());
        let mut result = vec![];

        match name_kind {
            NameKind::Local(local_id) => {
                result.push(local_id.wasm_get(expr.pos()));
            }

            NameKind::Field(field_id) => {
                result.extend(self.push_self(expr.pos()));
                result.push(field_id.wasm_get(expr.pos()));
            }
        }

        result.extend(self.reify_self_ty(&expr.unwrap_res_ty(), expr.pos()));

        result
    }

    fn visit_formal(&mut self, _formal: &ast::Formal<'buf>) -> Self::Output {
        unreachable!()
    }

    fn visit_receiver(&mut self, _recv: &ast::Receiver<'buf>) -> Self::Output {
        unreachable!()
    }

    fn visit_case_arm(&mut self, _arm: &ast::CaseArm<'buf>) -> Self::Output {
        unreachable!()
    }

    fn visit_ty_name(&mut self, _ty_name: &ast::TyName<'buf>) -> Self::Output {
        unreachable!()
    }

    fn visit_binding(&mut self, _binding: &ast::Binding<'buf>) -> Self::Output {
        unreachable!()
    }

    fn visit_name(&mut self, _name: &ast::Name<'buf>) -> Self::Output {
        unreachable!()
    }

    fn visit_int_lit(&mut self, expr: &ast::IntLit) -> Self::Output {
        let mut result = vec![];
        result.push(Instruction::I32Const(expr.0.value));
        result.extend(self.r#box(&expr.unwrap_res_ty(), expr.pos()));

        result
    }

    fn visit_string_lit(&mut self, expr: &ast::StringLit<'buf>) -> Self::Output {
        let mut result = vec![];
        let string_id = self
            .cg
            .string_table
            .borrow_mut()
            .insert(expr.0.value.clone());
        result.push(string_id.to_i32_const());
        result.push(Instruction::TableGet(self.string_table_arg(expr.pos())));
        result.extend(self.r#box(&BuiltinClass::String.into(), expr.pos()));

        result
    }

    fn visit_bool_lit(&mut self, expr: &ast::BoolLit) -> Self::Output {
        let mut result = vec![];
        result.push(Instruction::I32Const(expr.0.value as i32));
        result.extend(self.r#box(&BuiltinClass::Bool.into(), expr.pos()));

        result
    }
}
