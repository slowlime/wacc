pub mod ctx;
mod funcs;
pub mod passes;
mod string_collector;
mod string_ops;
mod visitor;
mod wat;

use std::borrow::Borrow;
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::mem;

use wast::core::Instruction;
use wast::token::Span as WasmSpan;

use crate::analysis::ClassName;
use crate::analysis::TypeCtx;
use crate::ast;
use crate::ast::ty::BuiltinClass;
use crate::ast::Class;
use crate::ast::Visitor as AstVisitor;
use crate::codegen::ctx::ty::RegularTy;
use crate::codegen::ctx::VTABLE_FIELD_NAME;
use crate::codegen::funcs::ImportedFuncKey;
use crate::codegen::funcs::BUILTIN_FUNCS;
use crate::codegen::funcs::IMPORTED_FUNCS;
use crate::position::HasSpan;
use crate::try_match;

use ctx::ty::WasmTy;
use ctx::CompleteWasmTy;
use ctx::FieldTable;
use ctx::FuncDef;
use ctx::FuncDefKind;
use ctx::FuncName;
use ctx::FuncRegistry;
use ctx::LocalCtx;
use ctx::MethodIndex;
use ctx::MethodTable;
use ctx::StringTable;
use ctx::TableId;
use ctx::TyIndex;
use ctx::TyIndexEntry;
use ctx::TyKind;
use ctx::Vtable;
use ctx::{MethodId, TyId};
use visitor::CodegenVisitor;

use self::funcs::specials;
use self::funcs::BuiltinFuncKey;
use self::funcs::SpecialFuncKey;
use self::funcs::SpecialMethodKey;
use self::funcs::SPECIAL_METHODS;

fn process_locals<'buf>(locals: LocalCtx<'buf>, pos: usize) -> Vec<wast::core::Local<'static>> {
    use wast::core::*;

    locals
        .into_iter()
        .map(|ty_kind| Local {
            id: None,
            name: None,
            ty: ty_kind.to_val_type(pos, true),
        })
        .collect()
}

trait PositionOffset {
    fn pos(&self) -> usize;
}

impl<T: HasSpan> PositionOffset for T {
    fn pos(&self) -> usize {
        self.span().start.byte
    }
}

impl PositionOffset for crate::position::Span {
    fn pos(&self) -> usize {
        self.start.byte
    }
}

#[derive(Debug, Clone, Copy)]
enum BuiltinMethodGeneratorSource<'a, 'buf> {
    Class(&'a Class<'buf>),
    BuiltinClass(BuiltinClass),
}

impl PositionOffset for BuiltinMethodGeneratorSource<'_, '_> {
    fn pos(&self) -> usize {
        0
    }
}

impl<'a, 'buf> From<&'a Class<'buf>> for BuiltinMethodGeneratorSource<'a, 'buf> {
    fn from(class: &'a Class<'buf>) -> Self {
        Self::Class(class)
    }
}

impl From<BuiltinClass> for BuiltinMethodGeneratorSource<'_, '_> {
    fn from(builtin: BuiltinClass) -> Self {
        Self::BuiltinClass(builtin)
    }
}

#[derive(Debug)]
pub struct CodegenOutput {
    pub module: wast::core::Module<'static>,
}

#[derive(Debug)]
pub struct Codegen<'a, 'buf, T: TyIndexEntry<'buf> = CompleteWasmTy<'buf>> {
    ty_ctx: TypeCtx<'buf>,
    ty_index: TyIndex<'buf, T>,
    method_index: MethodIndex<'buf>,
    method_table: MethodTable,
    vtable: Vtable,
    vtable_table_id: TableId,
    string_table: RefCell<StringTable<'buf>>,
    string_table_id: TableId,
    field_table: FieldTable<'buf>,
    func_registry: FuncRegistry<'buf>,
    classes: &'a [Class<'buf>],
    funcs: HashMap<MethodId, wast::core::Func<'static>>,
}

impl<'a, 'buf> Codegen<'a, 'buf, CompleteWasmTy<'buf>> {
    pub fn new(
        ty_ctx: TypeCtx<'buf>,
        ty_index: TyIndex<'buf, CompleteWasmTy<'buf>>,
        method_index: MethodIndex<'buf>,
        method_table: MethodTable,
        vtable: Vtable,
        string_table: StringTable<'buf>,
        field_table: FieldTable<'buf>,
        classes: &'a [Class<'buf>],
    ) -> Self {
        let vtable_table_id = Self::compute_vtable_id(&method_table);
        let string_table_id = TableId::new(vtable_table_id.index() + 1);

        let mut this = Self {
            ty_ctx,
            ty_index,
            method_index,
            method_table,
            vtable,
            vtable_table_id,
            string_table: RefCell::new(string_table),
            string_table_id,
            field_table,
            func_registry: FuncRegistry::new(),
            classes,
            funcs: HashMap::new(),
        };

        this.register_special_funcs();
        this.register_methods();

        this
    }

    fn register_special_funcs(&mut self) {
        for (key, func) in specials() {
            self.func_registry.insert(
                func.name.clone(),
                FuncDef {
                    kind: match key {
                        SpecialFuncKey::Builtin(key) => FuncDefKind::Builtin(key),
                        SpecialFuncKey::Imported(key) => FuncDefKind::Imported(key),
                    },
                    method_ty_id: self.ty_index.get_by_wasm_ty(&func.ty).unwrap(),
                },
            );
        }
    }

    fn register_methods(&mut self) {
        for (_, method_ty_id, methods) in self.method_table.iter() {
            for method_id in methods {
                let (method_name, _) = self.method_index.get_by_id(method_id).unwrap();
                let ty_id = method_id.ty_id();
                let wasm_ty = &self.ty_index.get_by_id(ty_id).unwrap().wasm_ty;
                let class_name = wasm_ty.class_name().unwrap().clone();
                let func_def = FuncDef {
                    kind: FuncDefKind::ClassMethod(method_id),
                    method_ty_id,
                };

                self.func_registry.insert(
                    FuncName::Method {
                        class_name,
                        method_name: Cow::Owned(method_name.to_owned()),
                    },
                    func_def,
                );
            }
        }
    }

    fn compute_vtable_id(method_table: &MethodTable) -> TableId {
        TableId::new(
            method_table
                .iter()
                .map(|(table_id, _, _)| table_id)
                .max()
                .map(|table_id| table_id.index() + 1)
                .unwrap_or(0),
        )
    }

    pub fn lower(mut self) -> CodegenOutput {
        for class in self.classes {
            self.lower_class(class);
        }

        self.lower_object();
        self.lower_io();
        self.lower_int();
        self.lower_string();
        self.lower_bool();

        let module = self.generate_module();

        CodegenOutput { module }
    }

    fn generate_module(self) -> wast::core::Module<'static> {
        let (this, rec_decl) = self.generate_rec_decl();
        this.generate_module(rec_decl)
    }

    fn generate_rec_decl(self) -> (Codegen<'a, 'buf, WasmTy<'buf>>, wast::core::Rec<'static>) {
        use wast::core::*;

        let (types, ty_index) = self.ty_index.extract_completed_tys();

        (
            Codegen {
                ty_ctx: self.ty_ctx,
                ty_index,
                method_index: self.method_index,
                method_table: self.method_table,
                vtable: self.vtable,
                vtable_table_id: self.vtable_table_id,
                string_table: self.string_table,
                string_table_id: self.string_table_id,
                field_table: self.field_table,
                func_registry: self.func_registry,
                classes: self.classes,
                funcs: self.funcs,
            },
            Rec {
                span: WasmSpan::from_offset(0),
                types: types.map(|(_, ty)| ty).collect(),
            },
        )
    }

    fn lower_class(&mut self, class: &Class<'buf>) {
        let class_name: ClassName = class.name.borrow().into();
        let wasm_ty = class_name.clone().into();
        let Some(ty_id) = self.ty_index.get_by_wasm_ty(&wasm_ty) else {
            panic!("The class {} was not found in the type index", &class.name);
        };

        for feature in &class.features {
            let Some(method) = try_match!(feature, ast::Feature::Method(method) => method) else { continue };
            let Some(method_id) = self.method_index.get_by_name(ty_id, method.name.as_slice()) else {
                panic!("Method {}::{} was not found in the method index", &class_name, &method.name);
            };

            let func = self.lower_method(ty_id, &class_name, method);
            self.funcs.insert(method_id, func);
        }

        self.generate_special_methods(ty_id, class.into(), class_name);
    }

    fn lower_builtin(
        &mut self,
        builtin: BuiltinClass,
        methods: impl IntoIterator<Item = (&'static [u8], wast::core::Func<'static>)>,
    ) {
        let ty_id = self.ty_index.get_by_wasm_ty(&builtin.into()).unwrap();

        for (name, func) in methods {
            let method_id = self.method_index.get_by_name(ty_id, name).unwrap();
            self.funcs.insert(method_id, func);
        }

        self.generate_special_methods(ty_id, builtin.into(), builtin.into());
    }

    fn lower_object(&mut self) {
        self.lower_builtin(
            BuiltinClass::Object,
            [
                (b"abort".as_slice(), self.generate_object_abort()),
                (b"type_name".as_slice(), self.generate_object_type_name()),
                (b"copy".as_slice(), self.generate_object_copy()),
            ],
        );
    }

    fn lower_io(&mut self) {
        self.lower_builtin(
            BuiltinClass::IO,
            [
                (b"out_string".as_slice(), self.generate_io_out_string()),
                (b"out_int".as_slice(), self.generate_io_out_int()),
                (b"in_string".as_slice(), self.generate_io_in_string()),
                (b"in_int".as_slice(), self.generate_io_in_int()),
            ],
        );
    }

    fn lower_int(&mut self) {
        self.lower_builtin(BuiltinClass::Int, []);
    }

    fn lower_string(&mut self) {
        self.lower_builtin(
            BuiltinClass::String,
            [
                (b"length".as_slice(), self.generate_string_length()),
                (b"concat".as_slice(), self.generate_string_concat()),
                (b"substr".as_slice(), self.generate_string_substr()),
            ],
        );
    }

    fn lower_bool(&mut self) {
        self.lower_builtin(BuiltinClass::Bool, [])
    }

    fn generate_object_abort(&self) -> wast::core::Func<'static> {
        use wast::core::*;

        let abort_func = IMPORTED_FUNCS.get(ImportedFuncKey::Abort);
        let mut instrs = vec![];
        let abort_func_id = self.func_registry.get_by_name(&abort_func.name).unwrap();
        let object_ty_id = self
            .ty_index
            .get_by_wasm_ty(&BuiltinClass::Object.into())
            .unwrap();
        let method_id = self
            .method_index
            .get_by_name(object_ty_id, b"abort")
            .unwrap();
        let method_ty_id = self
            .method_index
            .get_by_id(method_id)
            .unwrap()
            .1
            .method_ty_id;
        let locals = LocalCtx::new();

        {
            let _self_local_id = locals.bind(Some(Cow::Borrowed(b"self")), object_ty_id);
            instrs.push(Instruction::Call(abort_func_id.to_wasm_index(0)));
            instrs.push(Instruction::RefNull(HeapType::Index(
                object_ty_id.to_wasm_index(0),
            )));
        }

        Func {
            span: WasmSpan::from_offset(0),
            id: None,
            name: None,
            exports: InlineExport { names: vec![] },
            kind: FuncKind::Inline {
                locals: process_locals(locals, 0),
                expression: Expression {
                    instrs: instrs.into(),
                },
            },
            ty: TypeUse {
                index: Some(method_ty_id.to_wasm_index(0)),
                inline: None,
            },
        }
    }

    fn generate_object_type_name(&self) -> wast::core::Func<'static> {
        use wast::core::*;

        let special_type_name_method = SPECIAL_METHODS.get(SpecialMethodKey::TypeName);
        let object_ty_id = self
            .ty_index
            .get_by_wasm_ty(&BuiltinClass::Object.into())
            .unwrap();
        let type_name_method_id = self
            .method_index
            .get_by_name(object_ty_id, b"type_name")
            .unwrap();
        let method_ty_id = self
            .method_index
            .get_by_id(type_name_method_id)
            .unwrap()
            .1
            .method_ty_id;
        let special_type_name_method_id = self
            .method_index
            .get_by_name(object_ty_id, special_type_name_method.name)
            .unwrap();

        let mut instrs = vec![];
        let locals = LocalCtx::new();

        {
            let _self_local_id = locals.bind(Some(Cow::Borrowed(b"self")), object_ty_id);
            let visitor = CodegenVisitor::new(self, type_name_method_id, &locals, None);

            instrs.extend(self.virtual_dispatch(
                &locals,
                object_ty_id,
                special_type_name_method_id,
            ));
            // stack: <Self::TYPE_NAME: bytes>
            instrs.extend(visitor.r#box(&BuiltinClass::String.into(), 0));
            // stack: <String(Self::TYPE_NAME)>
        }

        Func {
            span: WasmSpan::from_offset(0),
            id: None,
            name: None,
            exports: InlineExport { names: vec![] },
            kind: FuncKind::Inline {
                locals: process_locals(locals, 0),
                expression: Expression {
                    instrs: instrs.into(),
                },
            },
            ty: TypeUse {
                index: Some(method_ty_id.to_wasm_index(0)),
                inline: None,
            },
        }
    }

    fn generate_object_copy(&self) -> wast::core::Func<'static> {
        use wast::core::*;

        let special_copy_method = SPECIAL_METHODS.get(SpecialMethodKey::Copy);
        let object_ty_id = self
            .ty_index
            .get_by_wasm_ty(&BuiltinClass::Object.into())
            .unwrap();
        let copy_method_id = self
            .method_index
            .get_by_name(object_ty_id, b"copy")
            .unwrap();
        let method_ty_id = self
            .method_index
            .get_by_id(copy_method_id)
            .unwrap()
            .1
            .method_ty_id;
        let special_copy_method_id = self
            .method_index
            .get_by_name(object_ty_id, special_copy_method.name)
            .unwrap();

        let mut instrs = vec![];
        let locals = LocalCtx::new();

        {
            let _self_local_id = locals.bind(Some(Cow::Borrowed(b"self")), object_ty_id);

            instrs.extend(self.virtual_dispatch(&locals, object_ty_id, special_copy_method_id));
            // stack: <self_copy: Object>
        }

        Func {
            span: WasmSpan::from_offset(0),
            id: None,
            name: None,
            exports: InlineExport { names: vec![] },
            kind: FuncKind::Inline {
                locals: process_locals(locals, 0),
                expression: Expression {
                    instrs: instrs.into(),
                },
            },
            ty: TypeUse {
                index: Some(method_ty_id.to_wasm_index(0)),
                inline: None,
            },
        }
    }

    fn generate_io_out_string(&self) -> wast::core::Func<'static> {
        use wast::core::*;

        let io_ty_id = self.builtin_ty_id(BuiltinClass::IO);
        let string_ty_id = self.builtin_ty_id(BuiltinClass::String);
        let method_id = self
            .method_index
            .get_by_name(io_ty_id, b"out_string")
            .unwrap();
        let method_ty_id = self
            .method_index
            .get_by_id(method_id)
            .unwrap()
            .1
            .method_ty_id;
        let print_bytes_func = IMPORTED_FUNCS.get(ImportedFuncKey::PrintBytes);
        let print_bytes_func_id = self
            .func_registry
            .get_by_name(&print_bytes_func.name)
            .unwrap();

        let mut instrs = vec![];
        let locals = LocalCtx::new();

        {
            let self_local_id = locals.bind(Some(Cow::Borrowed(b"self")), io_ty_id);
            let x_local_id = locals.bind(Some(Cow::Borrowed(b"x")), string_ty_id);
            let visitor = CodegenVisitor::new(self, method_id, &locals, None);

            instrs.push(x_local_id.wasm_get(0));
            instrs.extend(visitor.unbox(&BuiltinClass::String.into(), 0));
            // stack: <x: bytes>
            instrs.push(Instruction::Call(print_bytes_func_id.to_wasm_index(0)));
            instrs.push(self_local_id.wasm_get(0));
            // stack: <self>
        }

        self.make_func(instrs, locals, method_ty_id, 0)
    }

    fn generate_io_out_int(&self) -> wast::core::Func<'static> {
        let io_ty_id = self.builtin_ty_id(BuiltinClass::IO);
        let int_ty_id = self.builtin_ty_id(BuiltinClass::Int);
        let method_id = self.method_index.get_by_name(io_ty_id, b"out_int").unwrap();
        let method_ty_id = self
            .method_index
            .get_by_id(method_id)
            .unwrap()
            .1
            .method_ty_id;
        let print_int_func = IMPORTED_FUNCS.get(ImportedFuncKey::PrintInt);
        let print_int_func_id = self
            .func_registry
            .get_by_name(&print_int_func.name)
            .unwrap();

        let mut instrs = vec![];
        let locals = LocalCtx::new();

        {
            let self_local_id = locals.bind(Some(Cow::Borrowed(b"self")), io_ty_id);
            let x_local_id = locals.bind(Some(Cow::Borrowed(b"x")), int_ty_id);
            let visitor = CodegenVisitor::new(self, method_id, &locals, None);

            instrs.push(x_local_id.wasm_get(0));
            instrs.extend(visitor.unbox(&BuiltinClass::Int.into(), 0));
            // stack: <x: i32>
            instrs.push(Instruction::Call(print_int_func_id.to_wasm_index(0)));
            instrs.push(self_local_id.wasm_get(0));
            // stack: <self>
        }

        self.make_func(instrs, locals, method_ty_id, 0)
    }

    fn generate_io_in_string(&self) -> wast::core::Func<'static> {
        let io_ty_id = self.builtin_ty_id(BuiltinClass::IO);
        let method_id = self
            .method_index
            .get_by_name(io_ty_id, b"in_string")
            .unwrap();
        let method_ty_id = self
            .method_index
            .get_by_id(method_id)
            .unwrap()
            .1
            .method_ty_id;
        let read_line_func = IMPORTED_FUNCS.get(ImportedFuncKey::ReadLine);
        let read_line_func_id = self
            .func_registry
            .get_by_name(&read_line_func.name)
            .unwrap();

        let mut instrs = vec![];
        let locals = LocalCtx::new();

        {
            let _self_local_id = locals.bind(Some(Cow::Borrowed(b"self")), io_ty_id);
            let visitor = CodegenVisitor::new(self, method_id, &locals, None);

            instrs.push(Instruction::Call(read_line_func_id.to_wasm_index(0)));
            instrs.extend(visitor.r#box(&BuiltinClass::String.into(), 0));
            // stack: <string: String>
        }

        self.make_func(instrs, locals, method_ty_id, 0)
    }

    fn generate_io_in_int(&self) -> wast::core::Func<'static> {
        let io_ty_id = self.builtin_ty_id(BuiltinClass::IO);
        let method_id = self.method_index.get_by_name(io_ty_id, b"in_int").unwrap();
        let method_ty_id = self
            .method_index
            .get_by_id(method_id)
            .unwrap()
            .1
            .method_ty_id;
        let read_int_func = IMPORTED_FUNCS.get(ImportedFuncKey::ReadInt);
        let read_int_func_id = self.func_registry.get_by_name(&read_int_func.name).unwrap();

        let mut instrs = vec![];
        let locals = LocalCtx::new();

        {
            let _self_local_id = locals.bind(Some(Cow::Borrowed(b"self")), io_ty_id);
            let visitor = CodegenVisitor::new(self, method_id, &locals, None);

            instrs.push(Instruction::Call(read_int_func_id.to_wasm_index(0)));
            instrs.extend(visitor.r#box(&BuiltinClass::Int.into(), 0));
            // stack: <int: Int>
        }

        self.make_func(instrs, locals, method_ty_id, 0)
    }

    fn generate_string_length(&self) -> wast::core::Func<'static> {
        let string_ty_id = self.builtin_ty_id(BuiltinClass::String);
        let method_id = self
            .method_index
            .get_by_name(string_ty_id, b"length")
            .unwrap();
        let method_ty_id = self
            .method_index
            .get_by_id(method_id)
            .unwrap()
            .1
            .method_ty_id;

        let mut instrs = vec![];
        let locals = LocalCtx::new();

        {
            let self_local_id = locals.bind(Some(Cow::Borrowed(b"self")), string_ty_id);
            let visitor = CodegenVisitor::new(self, method_id, &locals, None);

            instrs.push(self_local_id.wasm_get(0));
            instrs.extend(visitor.unbox(&BuiltinClass::String.into(), 0));
            instrs.push(Instruction::ArrayLen);
            instrs.extend(visitor.r#box(&BuiltinClass::Int.into(), 0));
            // stack: <self.len: Int>
        }

        self.make_func(instrs, locals, method_ty_id, 0)
    }

    fn generate_string_concat(&self) -> wast::core::Func<'static> {
        let string_ty_id = self.builtin_ty_id(BuiltinClass::String);
        let method_id = self
            .method_index
            .get_by_name(string_ty_id, b"concat")
            .unwrap();
        let method_ty_id = self
            .method_index
            .get_by_id(method_id)
            .unwrap()
            .1
            .method_ty_id;
        let string_concat_func = BUILTIN_FUNCS.get(BuiltinFuncKey::StringConcat);
        let string_concat_func_id = self
            .func_registry
            .get_by_name(&string_concat_func.name)
            .unwrap();

        let mut instrs = vec![];
        let locals = LocalCtx::new();

        {
            let self_local_id = locals.bind(Some(Cow::Borrowed(b"self")), string_ty_id);
            let s_local_id = locals.bind(Some(Cow::Borrowed(b"s")), string_ty_id);
            let visitor = CodegenVisitor::new(self, method_id, &locals, None);

            instrs.push(self_local_id.wasm_get(0));
            instrs.extend(visitor.unbox(&BuiltinClass::String.into(), 0));
            instrs.push(s_local_id.wasm_get(0));
            instrs.extend(visitor.unbox(&BuiltinClass::String.into(), 0));
            // stack: <self: bytes> <other: bytes>
            instrs.push(Instruction::Call(string_concat_func_id.to_wasm_index(0)));
            // stack: <self + other: bytes>
            instrs.extend(visitor.r#box(&BuiltinClass::String.into(), 0));
            // stack: <self + other: String>
        }

        self.make_func(instrs, locals, method_ty_id, 0)
    }

    fn generate_string_substr(&self) -> wast::core::Func<'static> {
        let string_ty_id = self.builtin_ty_id(BuiltinClass::String);
        let int_ty_id = self.builtin_ty_id(BuiltinClass::Int);
        let method_id = self
            .method_index
            .get_by_name(string_ty_id, b"substr")
            .unwrap();
        let method_ty_id = self
            .method_index
            .get_by_id(method_id)
            .unwrap()
            .1
            .method_ty_id;
        let string_substr_func = BUILTIN_FUNCS.get(BuiltinFuncKey::StringSubstr);
        let string_substr_func_id = self
            .func_registry
            .get_by_name(&string_substr_func.name)
            .unwrap();

        let mut instrs = vec![];
        let locals = LocalCtx::new();

        {
            let self_local_id = locals.bind(Some(Cow::Borrowed(b"self")), string_ty_id);
            let i_local_id = locals.bind(Some(Cow::Borrowed(b"i")), int_ty_id);
            let l_local_id = locals.bind(Some(Cow::Borrowed(b"l")), int_ty_id);
            let visitor = CodegenVisitor::new(self, method_id, &locals, None);

            instrs.push(self_local_id.wasm_get(0));
            instrs.extend(visitor.unbox(&BuiltinClass::String.into(), 0));
            instrs.push(i_local_id.wasm_get(0));
            instrs.extend(visitor.unbox(&BuiltinClass::Int.into(), 0));
            instrs.push(l_local_id.wasm_get(0));
            instrs.extend(visitor.unbox(&BuiltinClass::Int.into(), 0));
            // stack: <self: bytes> <i: i32> <l: i32>
            instrs.push(Instruction::Call(string_substr_func_id.to_wasm_index(0)));
            // stack: <self[i..(i + l)]: bytes>
            instrs.extend(visitor.r#box(&BuiltinClass::String.into(), 0));
            // stack: <self[i..(i + l)]: String>
        }

        self.make_func(instrs, locals, method_ty_id, 0)
    }

    fn generate_special_methods(
        &mut self,
        ty_id: TyId,
        source: BuiltinMethodGeneratorSource<'_, 'buf>,
        class_name: ClassName<'buf>,
    ) {
        for (key, func) in SPECIAL_METHODS.iter() {
            let method_id = self.method_index.get_by_name(ty_id, func.name).unwrap();

            let func = match key {
                SpecialMethodKey::Constructor => self.generate_constructor(ty_id, source),
                SpecialMethodKey::Initializer => {
                    self.generate_initializer(ty_id, &class_name, source)
                }
                SpecialMethodKey::Copy => self.generate_copy(ty_id, source),
                SpecialMethodKey::TypeName => self.generate_type_name(ty_id, &class_name, source),
            };

            self.funcs.insert(method_id, func);
        }
    }

    fn generate_copy(
        &self,
        ty_id: TyId,
        source: BuiltinMethodGeneratorSource<'_, 'buf>,
    ) -> wast::core::Func<'static> {
        use wast::core::*;

        let func = SPECIAL_METHODS.get(SpecialMethodKey::Copy);
        let object_ty_id = self.builtin_ty_id(BuiltinClass::Object);

        let mut instrs = vec![];
        let locals = LocalCtx::new();

        {
            let object_self_local_id = locals.bind(Some(Cow::Borrowed(b"self")), object_ty_id);
            instrs.push(object_self_local_id.wasm_get(source.pos()));
            instrs.push(Instruction::RefCast(ty_id.to_wasm_index(source.pos())));

            let self_local_id = locals.bind(Some(Cow::Borrowed(b"self")), ty_id);
            instrs.push(self_local_id.wasm_set(source.pos()));

            for (field_id, _, _) in self.field_table.fields(ty_id).unwrap() {
                instrs.push(self_local_id.wasm_get(source.pos()));
                instrs.push(field_id.wasm_get(source.pos()));
            }

            instrs.push(Instruction::StructNew(ty_id.to_wasm_index(source.pos())));
        }

        self.make_func(
            instrs,
            locals,
            self.ty_index.get_by_wasm_ty(&func.ty).unwrap(),
            source.pos(),
        )
    }

    fn generate_type_name(
        &self,
        ty_id: TyId,
        name: &ClassName<'buf>,
        source: BuiltinMethodGeneratorSource<'_, 'buf>,
    ) -> wast::core::Func<'static> {
        use wast::core::*;

        let func = SPECIAL_METHODS.get(SpecialMethodKey::TypeName);
        let string_id = self
            .string_table
            .borrow_mut()
            .insert(Cow::Owned(name.to_string().into()));

        let mut instrs = vec![];
        let locals = LocalCtx::new();

        {
            let _self_local_id = locals.bind(Some(Cow::Borrowed(b"self")), ty_id);

            instrs.push(string_id.to_i32_const());
            instrs.push(Instruction::TableGet(
                self.string_table_id.to_table_arg(source.pos()),
            ));
        }

        self.make_func(
            instrs,
            locals,
            self.ty_index.get_by_wasm_ty(&func.ty).unwrap(),
            source.pos(),
        )
    }

    fn default_initializer_for(&self, ty_kind: TyKind, pos: usize) -> Instruction<'static> {
        use wast::core::*;

        match ty_kind {
            TyKind::I32 => Instruction::I32Const(0),
            TyKind::Id(ty_id) => match self.ty_index.get_by_id(ty_id).unwrap().wasm_ty {
                WasmTy::Regular(RegularTy::I32) => unreachable!(),
                WasmTy::Regular(RegularTy::Class(_)) => {
                    Instruction::RefNull(HeapType::Index(ty_id.to_wasm_index(pos)))
                }
                WasmTy::Regular(RegularTy::ByteArray) => {
                    Instruction::ArrayNewFixed(ArrayNewFixed {
                        array: ty_id.to_wasm_index(pos),
                        length: 0,
                    })
                }
                WasmTy::Func { .. } => {
                    panic!("no default initializer is available for function types")
                }
            },
        }
    }

    fn create_object(&self, ty_id: TyId, pos: usize) -> Vec<Instruction<'static>> {
        let mut result = vec![];

        let fields = self.field_table.fields(ty_id).unwrap();

        let vtable_base = self.vtable.base_offset(ty_id).unwrap();
        result.push(vtable_base.to_i32_const());

        for (_, _, field_ty_kind) in fields.skip(1) {
            result.push(self.default_initializer_for(field_ty_kind, pos));
        }

        result.push(Instruction::StructNew(ty_id.to_wasm_index(pos)));

        result
    }

    fn generate_constructor(
        &mut self,
        ty_id: TyId,
        source: BuiltinMethodGeneratorSource<'_, 'buf>,
    ) -> wast::core::Func<'static> {
        use wast::core::*;

        let mut instrs = vec![];
        let locals = LocalCtx::new();

        instrs.extend(self.create_object(ty_id, source.pos()));
        let func = SPECIAL_METHODS.get(SpecialMethodKey::Constructor);
        let constructor_ty_id = self.ty_index.get_by_wasm_ty(&func.ty).unwrap();
        let initializer_method_id = self
            .method_index
            .get_by_name(
                ty_id,
                SPECIAL_METHODS.get(SpecialMethodKey::Initializer).name,
            )
            .unwrap();
        let initializer_func_id = self
            .func_registry
            .get_by_method_id(initializer_method_id)
            .unwrap();
        let object_ty_id = self
            .ty_index
            .get_by_wasm_ty(&ClassName::from(BuiltinClass::Object).into())
            .unwrap();
        instrs.push(Instruction::RefCast(
            object_ty_id.to_wasm_index(source.pos()),
        ));
        instrs.push(Instruction::Call(
            initializer_func_id.to_wasm_index(source.pos()),
        ));

        self.make_func(instrs, locals, constructor_ty_id, source.pos())
    }

    fn generate_initializer(
        &mut self,
        ty_id: TyId,
        class_name: &ClassName<'buf>,
        source: BuiltinMethodGeneratorSource<'_, 'buf>,
    ) -> wast::core::Func<'static> {
        use wast::core::*;

        let mut instrs = vec![];
        let locals = LocalCtx::new();

        let func = SPECIAL_METHODS.get(SpecialMethodKey::Initializer);
        let initializer_ty_id = self.ty_index.get_by_wasm_ty(&func.ty).unwrap();

        {
            let object_ty_id = self
                .ty_index
                .get_by_wasm_ty(&ClassName::from(BuiltinClass::Object).into())
                .unwrap();
            let object_self_local_id =
                locals.bind(Some(Cow::Borrowed(b"self")), TyKind::Id(object_ty_id));
            instrs.push(object_self_local_id.wasm_get(source.pos()));

            let class_index = self.ty_ctx.get_class(class_name).unwrap();

            if let Some(parent) = class_index.parent() {
                let parent_wasm_ty = parent.clone().into();
                let parent_ty_id = self.ty_index.get_by_wasm_ty(&parent_wasm_ty).unwrap();
                let parent_initializer_method_id = self
                    .method_index
                    .get_by_name(parent_ty_id, func.name)
                    .unwrap();
                let parent_initializer_func_id = self
                    .func_registry
                    .get_by_method_id(parent_initializer_method_id)
                    .unwrap();
                instrs.push(Instruction::Call(
                    parent_initializer_func_id.to_wasm_index(source.pos()),
                ));
            }

            match source {
                BuiltinMethodGeneratorSource::Class(class) => {
                    instrs.push(Instruction::RefCast(ty_id.to_wasm_index(source.pos())));
                    let self_local_id =
                        locals.bind(Some(Cow::Borrowed(b"self")), TyKind::Id(ty_id));
                    self_local_id.wasm_set(source.pos());

                    let initializer_method_id =
                        self.method_index.get_by_name(ty_id, func.name).unwrap();
                    let mut visitor =
                        CodegenVisitor::new(self, initializer_method_id, &locals, None);

                    for feature in &class.features {
                        let ast::Feature::Field(field) = feature else { continue };
                        let Some(ref init) = field.0.init else { continue };
                        let field_id = self
                            .field_table
                            .get_by_name(ty_id, field.0.name.as_slice())
                            .unwrap();
                        instrs.push(self_local_id.wasm_get(field.pos()));
                        instrs.extend(visitor.visit_expr(init));
                        instrs.push(field_id.wasm_set(field.pos()));
                    }

                    drop(self_local_id);

                    instrs.push(object_self_local_id.wasm_get(source.pos()));
                }

                BuiltinMethodGeneratorSource::BuiltinClass(_) => {}
            }
        }

        self.make_func(instrs, locals, initializer_ty_id, source.pos())
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

        let method_ty_id = self
            .method_index
            .get_by_id(method_id)
            .unwrap()
            .1
            .method_ty_id;

        let locals = Default::default();
        let instrs =
            CodegenVisitor::new(self, method_id, &locals, Some(&method.params)).visit_expr(&method.body);

        self.make_func(instrs, locals, method_ty_id, method.pos())
    }

    fn builtin_ty_id(&self, builtin: BuiltinClass) -> TyId {
        self.ty_index.get_by_wasm_ty(&builtin.into()).unwrap()
    }

    fn virtual_dispatch(
        &self,
        locals: &LocalCtx<'buf>,
        self_ty_id: TyId,
        method_id: MethodId,
    ) -> Vec<Instruction<'static>> {
        use wast::core::*;

        let method_ty_id = self
            .method_index
            .get_by_id(method_id)
            .unwrap()
            .1
            .method_ty_id;
        let method_table_id = self
            .method_table
            .get_by_method_id(method_ty_id, method_id)
            .unwrap();
        let vtable_field_id = self
            .field_table
            .get_by_name(self_ty_id, VTABLE_FIELD_NAME)
            .unwrap();

        let mut instrs = vec![];
        let self_local_id = locals.get(b"self").unwrap();
        instrs.push(self_local_id.wasm_get(0));
        // stack: <self: Object>
        instrs.push(vtable_field_id.wasm_get(0));
        // stack: <vtable_base: i32>
        instrs.push(method_id.to_i32_const());
        instrs.push(Instruction::I32Add);
        // stack: <Self::<method>::idx: i32>
        instrs.push(Instruction::TableGet(self.vtable_table_id.to_table_arg(0)));
        instrs.push(Instruction::I31GetU);
        instrs.push(Instruction::TableGet(
            method_table_id.table_id().to_table_arg(0),
        ));
        instrs.push(Instruction::CallRef(HeapType::Index(
            method_ty_id.to_wasm_index(0),
        )));

        instrs
    }

    fn make_func(
        &self,
        instrs: Vec<Instruction<'static>>,
        locals: LocalCtx<'buf>,
        method_ty_id: TyId,
        pos: usize,
    ) -> wast::core::Func<'static> {
        use wast::core::*;

        Func {
            span: WasmSpan::from_offset(pos),
            id: None,
            name: None,
            exports: InlineExport { names: vec![] },
            kind: FuncKind::Inline {
                locals: process_locals(locals, pos),
                expression: Expression {
                    instrs: instrs.into(),
                },
            },
            ty: TypeUse {
                index: Some(method_ty_id.to_wasm_index(0)),
                inline: None,
            },
        }
    }
}

impl<'a, 'buf> Codegen<'a, 'buf, WasmTy<'buf>> {
    fn generate_module(
        mut self,
        rec_decl: wast::core::Rec<'static>,
    ) -> wast::core::Module<'static> {
        use wast::core::*;

        let mut module_fields = vec![];
        module_fields.push(ModuleField::Rec(rec_decl));
        module_fields.extend(self.generate_imports().into_iter().map(ModuleField::Import));
        module_fields.extend(self.generate_tables().into_iter().map(ModuleField::Table));
        module_fields.extend(self.generate_functions().into_iter().map(ModuleField::Func));

        Module {
            span: WasmSpan::from_offset(0),
            id: None,
            name: None,
            kind: ModuleKind::Text(module_fields),
        }
    }

    fn generate_imports(&self) -> Vec<wast::core::Import<'static>> {
        use wast::core::*;

        self.func_registry
            .iter()
            .filter_map(|(_, _, func_def)| {
                try_match!(func_def.kind, FuncDefKind::Imported(key) => {
                    (func_def.method_ty_id, key)
                })
            })
            .map(|(func_ty_id, key)| Import {
                span: WasmSpan::from_offset(0),
                module: "cool-runtime",
                field: key.as_str(),
                item: ItemSig {
                    span: WasmSpan::from_offset(0),
                    id: None,
                    name: None,
                    kind: ItemKind::Func(TypeUse {
                        index: Some(func_ty_id.to_wasm_index(0)),
                        inline: None,
                    }),
                },
            })
            .collect()
    }

    fn generate_tables(&self) -> Vec<wast::core::Table<'static>> {
        use wast::core::*;

        let mut tables = vec![];

        for (_table_id, method_ty_id, methods) in self.method_table.iter() {
            tables.push(Table {
                span: WasmSpan::from_offset(0),
                id: None,
                name: None,
                exports: InlineExport { names: vec![] },
                kind: TableKind::Inline {
                    elem: RefType {
                        nullable: false,
                        heap: HeapType::Index(method_ty_id.to_wasm_index(0)),
                    },
                    payload: ElemPayload::Indices(
                        methods
                            .map(|method_id| {
                                self.func_registry
                                    .get_by_method_id(method_id)
                                    .unwrap()
                                    .to_wasm_index(0)
                            })
                            .collect(),
                    ),
                },
            });
        }

        tables.push(Table {
            span: WasmSpan::from_offset(0),
            id: None,
            name: None,
            exports: InlineExport { names: vec![] },
            kind: TableKind::Inline {
                elem: RefType {
                    nullable: false,
                    heap: HeapType::I31,
                },
                payload: ElemPayload::Exprs {
                    ty: RefType {
                        nullable: false,
                        heap: HeapType::I31,
                    },
                    exprs: self
                        .vtable
                        .iter()
                        .map(|(_, method_table_id)| Expression {
                            instrs: Box::new([method_table_id.to_i32_const(), Instruction::I31New]),
                        })
                        .collect(),
                },
            },
        });

        // XXX: using a table turns out to be quite an overkill apparently
        // it might be worth looking into memories and data segments
        let byte_array_id = self
            .ty_index
            .get_by_ty(&RegularTy::ByteArray.into())
            .unwrap();
        tables.push(Table {
            span: WasmSpan::from_offset(0),
            id: None,
            name: None,
            exports: InlineExport { names: vec![] },
            kind: TableKind::Inline {
                elem: RefType {
                    nullable: false,
                    heap: HeapType::Index(byte_array_id.to_wasm_index(0)),
                },
                payload: ElemPayload::Exprs {
                    ty: RefType {
                        nullable: false,
                        heap: HeapType::Index(byte_array_id.to_wasm_index(0)),
                    },
                    exprs: self
                        .string_table
                        .borrow()
                        .iter()
                        .flat_map(|(_, s)| s)
                        .map(|&b| Expression {
                            instrs: Box::new([Instruction::I32Const(b as _)]),
                        })
                        .collect(),
                },
            },
        });

        tables
    }

    fn generate_functions(&mut self) -> Vec<wast::core::Func<'static>> {
        let mut funcs = mem::replace(&mut self.funcs, HashMap::new());

        self.func_registry
            .iter()
            .filter_map(|(_, _, def)| match def.kind {
                FuncDefKind::Builtin(key) => Some(self.generate_builtin(key)),
                FuncDefKind::Imported(_) => None,
                FuncDefKind::ClassMethod(method_id) => Some(funcs.remove(&method_id).unwrap()),
            })
            .collect()
    }

    fn generate_builtin(&self, key: BuiltinFuncKey) -> wast::core::Func<'static> {
        match key {
            BuiltinFuncKey::StringEq => self.generate_string_eq(),
            BuiltinFuncKey::StringConcat => self.generate_string_concat(),
            BuiltinFuncKey::StringSubstr => self.generate_string_substr(),
        }
    }
}
