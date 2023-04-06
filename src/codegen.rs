// the code with explicit push calls seems more readable to me
#![allow(clippy::vec_init_then_push)]

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
use std::mem;

use elsa::FrozenMap;
use indexmap::Equivalent;
use indexmap::IndexMap;
use tracing::{trace, trace_span};
use wast::core::Instruction;
use wast::token::Index as WasmIndex;
use wast::token::NameAnnotation;
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
use crate::util::slice_formatter;

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
use ctx::TyIndex;
use ctx::TyIndexEntry;
use ctx::TyKind;
use ctx::Vtable;
use ctx::{MethodId, TyId};
use visitor::CodegenVisitor;

use self::ctx::LocalId;
use self::ctx::StringId;
use self::funcs::specials;
use self::funcs::BuiltinFuncKey;
use self::funcs::SpecialFuncKey;
use self::funcs::SpecialMethodKey;
use self::funcs::SPECIAL_METHODS;

fn process_locals<'buf>(
    locals: LocalCtx<'buf>,
    param_count: usize,
    pos: usize,
) -> Vec<wast::core::Local<'static>> {
    use wast::core::*;

    locals
        .into_iter()
        .skip(param_count)
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

type FrozenSet<T> = FrozenMap<T, Box<()>>;

#[derive(Default)]
pub struct AuxiliaryStorage {
    strings: FrozenSet<String>,
}

impl AuxiliaryStorage {
    pub fn new() -> Self {
        Self::default()
    }

    fn get<'a>(&'a self, s: &str) -> &'a str {
        if self.strings.get_key_value(s).is_none() {
            self.strings.insert(s.to_owned(), Box::new(()));
        }

        self.strings.get_key_value(s).unwrap().0
    }
}

#[derive(Debug)]
pub struct CodegenOutput<'aux> {
    pub module: wast::core::Module<'aux>,
}

pub struct Codegen<'buf, 'aux, 'a, T: TyIndexEntry<'buf> = CompleteWasmTy<'buf>>
where
    WasmTy<'buf>: Equivalent<T>,
{
    storage: &'aux AuxiliaryStorage,
    ty_ctx: TypeCtx<'buf>,
    ty_index: TyIndex<'buf, T>,
    method_index: MethodIndex<'buf>,
    method_table: MethodTable,
    vtable: Vtable,
    string_table: RefCell<StringTable<'buf>>,
    field_table: FieldTable<'buf>,
    func_registry: FuncRegistry<'buf>,
    classes: &'a [Class<'buf>],
    funcs: IndexMap<MethodId, wast::core::Func<'aux>>,
}

impl<'buf, 'aux, 'a> Codegen<'buf, 'aux, 'a, CompleteWasmTy<'buf>> {
    // yeah clippy, I know
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        storage: &'aux mut AuxiliaryStorage,
        ty_ctx: TypeCtx<'buf>,
        ty_index: TyIndex<'buf, CompleteWasmTy<'buf>>,
        method_index: MethodIndex<'buf>,
        method_table: MethodTable,
        vtable: Vtable,
        string_table: StringTable<'buf>,
        field_table: FieldTable<'buf>,
        classes: &'a [Class<'buf>],
    ) -> Self {
        let mut this = Self {
            storage,
            ty_ctx,
            ty_index,
            method_index,
            method_table,
            vtable,
            string_table: RefCell::new(string_table),
            field_table,
            func_registry: FuncRegistry::new(),
            classes,
            funcs: IndexMap::new(),
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

    pub fn lower(mut self) -> CodegenOutput<'aux> {
        let span = trace_span!("lower");
        let _span = span.enter();

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

    fn generate_string(&self, string_id: StringId, pos: usize) -> Vec<Instruction<'static>> {
        use wast::core::*;

        let mut instrs = vec![];

        let bytes_ty_id = self
            .ty_index
            .get_by_wasm_ty(&RegularTy::ByteArray.into())
            .unwrap();
        let string_table = self.string_table.borrow_mut();
        let s = string_table.get_by_id(string_id).unwrap();
        instrs.extend(s.iter().map(|&c| Instruction::I32Const(c as i32)));
        instrs.push(Instruction::ArrayNewFixed(ArrayNewFixed {
            array: bytes_ty_id.to_wasm_index(pos),
            length: s.len().try_into().unwrap(),
        }));

        instrs
    }

    fn generate_module(self) -> wast::core::Module<'aux> {
        let (this, rec_decl) = self.generate_rec_decl();
        this.generate_module(rec_decl)
    }

    fn generate_rec_decl(self) -> (Codegen<'buf, 'aux, 'a, WasmTy<'buf>>, wast::core::Rec<'aux>) {
        use wast::core::*;

        let (types, ty_index) = self.ty_index.extract_completed_tys();

        (
            Codegen {
                storage: self.storage,
                ty_ctx: self.ty_ctx,
                ty_index,
                method_index: self.method_index,
                method_table: self.method_table,
                vtable: self.vtable,
                string_table: self.string_table,
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
        let span = trace_span!("lower_class", class.name = %&class.name);
        let _span = span.enter();

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
        let span = trace_span!("lower_object");
        let _span = span.enter();

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
        let span = trace_span!("lower_io");
        let _span = span.enter();

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
        let span = trace_span!("lower_int");
        let _span = span.enter();

        self.lower_builtin(BuiltinClass::Int, []);
    }

    fn lower_string(&mut self) {
        let span = trace_span!("lower_string");
        let _span = span.enter();

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
        let span = trace_span!("lower_bool");
        let _span = span.enter();

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
            instrs.push(Instruction::RefNull(object_ty_id.to_heap_type(0)));
        }

        self.make_func(instrs, locals, method_ty_id, 0, "Object::abort")
    }

    fn generate_object_type_name(&self) -> wast::core::Func<'static> {
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
            let self_local_id = locals.bind(Some(Cow::Borrowed(b"self")), object_ty_id);
            let visitor = CodegenVisitor::new(self, type_name_method_id, &locals, None);

            instrs.extend(self.virtual_dispatch(
                self_local_id,
                object_ty_id,
                special_type_name_method_id,
            ));
            // stack: <Self::TYPE_NAME: bytes>
            instrs.extend(visitor.r#box(&BuiltinClass::String.into(), 0));
            // stack: <String(Self::TYPE_NAME)>
        }

        self.make_func(instrs, locals, method_ty_id, 0, "Object::type_name")
    }

    fn generate_object_copy(&self) -> wast::core::Func<'static> {
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
            let self_local_id = locals.bind(Some(Cow::Borrowed(b"self")), object_ty_id);

            instrs.push(self_local_id.wasm_get(0));
            instrs.extend(self.virtual_dispatch(
                self_local_id,
                object_ty_id,
                special_copy_method_id,
            ));
            // stack: <self_copy: Object>
        }

        self.make_func(instrs, locals, method_ty_id, 0, "Object::copy")
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
            instrs.push(Instruction::ExternExternalize);
            instrs.push(Instruction::Call(print_bytes_func_id.to_wasm_index(0)));
            instrs.push(self_local_id.wasm_get(0));
            // stack: <self>
        }

        self.make_func(instrs, locals, method_ty_id, 0, "IO::out_string")
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

        self.make_func(instrs, locals, method_ty_id, 0, "IO::out_int")
    }

    fn generate_io_in_string(&self) -> wast::core::Func<'static> {
        use wast::core::*;

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
        let bytes_ty_id = self
            .ty_index
            .get_by_wasm_ty(&RegularTy::ByteArray.into())
            .unwrap();

        let mut instrs = vec![];
        let locals = LocalCtx::new();

        {
            let _self_local_id = locals.bind(Some(Cow::Borrowed(b"self")), io_ty_id);
            let visitor = CodegenVisitor::new(self, method_id, &locals, None);

            instrs.push(Instruction::Call(read_line_func_id.to_wasm_index(0)));
            instrs.push(Instruction::ExternInternalize);
            instrs.push(Instruction::RefCast(bytes_ty_id.to_cast_arg(0)));
            instrs.extend(visitor.r#box(&BuiltinClass::String.into(), 0));
            // stack: <string: String>
        }

        self.make_func(instrs, locals, method_ty_id, 0, "IO::in_string")
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

        self.make_func(instrs, locals, method_ty_id, 0, "IO::in_int")
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

        self.make_func(instrs, locals, method_ty_id, 0, "String::length")
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

        self.make_func(instrs, locals, method_ty_id, 0, "String::concat")
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

        self.make_func(instrs, locals, method_ty_id, 0, "String::substr")
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
                SpecialMethodKey::Constructor => {
                    self.generate_constructor(ty_id, &class_name, source)
                }
                SpecialMethodKey::Initializer => {
                    self.generate_initializer(ty_id, &class_name, source)
                }
                SpecialMethodKey::Copy => self.generate_copy(ty_id, &class_name, source),
                SpecialMethodKey::TypeName => self.generate_type_name(ty_id, &class_name, source),
            };

            self.funcs.insert(method_id, func);
        }
    }

    fn generate_copy(
        &self,
        ty_id: TyId,
        class_name: &ClassName<'buf>,
        source: BuiltinMethodGeneratorSource<'_, 'buf>,
    ) -> wast::core::Func<'aux> {
        use wast::core::*;

        let span = trace_span!("generate_copy", ?ty_id);
        let _span = span.enter();

        trace!("Generating {}", SpecialMethodKey::Copy.as_str());

        let func = SPECIAL_METHODS.get(SpecialMethodKey::Copy);
        let object_ty_id = self.builtin_ty_id(BuiltinClass::Object);

        let mut instrs = vec![];
        let locals = LocalCtx::new();

        {
            let object_self_local_id = locals.bind(Some(Cow::Borrowed(b"self")), object_ty_id);
            instrs.push(object_self_local_id.wasm_get(source.pos()));
            instrs.push(Instruction::RefCast(ty_id.to_cast_arg(source.pos())));

            let self_local_id = locals.bind(Some(Cow::Borrowed(b"self")), ty_id);
            instrs.push(self_local_id.wasm_set(source.pos()));

            for (field_id, field_name, _) in self.field_table.fields(ty_id).unwrap() {
                trace!(
                    ?field_id,
                    "Retrieving a field {}",
                    slice_formatter(field_name)
                );
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
            self.storage.get(&format!(
                "{}:{}",
                class_name,
                SpecialMethodKey::Copy.as_str()
            )),
        )
    }

    fn generate_type_name(
        &self,
        ty_id: TyId,
        class_name: &ClassName<'buf>,
        source: BuiltinMethodGeneratorSource<'_, 'buf>,
    ) -> wast::core::Func<'aux> {
        let span = trace_span!("generate_type_name", ?ty_id);
        let _span = span.enter();

        let func = SPECIAL_METHODS.get(SpecialMethodKey::TypeName);
        let string_id = self
            .string_table
            .borrow_mut()
            .insert(Cow::Owned(class_name.to_string().into()));

        trace!(%class_name, "Generating {}", SpecialMethodKey::TypeName.as_str());

        let mut instrs = vec![];
        let locals = LocalCtx::new();

        {
            let _self_local_id = locals.bind(Some(Cow::Borrowed(b"self")), ty_id);

            instrs.extend(self.generate_string(string_id, source.pos()));
        }

        self.make_func(
            instrs,
            locals,
            self.ty_index.get_by_wasm_ty(&func.ty).unwrap(),
            source.pos(),
            self.storage.get(&format!(
                "{}::{}",
                class_name,
                SpecialMethodKey::TypeName.as_str()
            )),
        )
    }

    fn default_initializer_for(&self, ty_kind: TyKind, pos: usize) -> Vec<Instruction<'static>> {
        use wast::core::*;

        match ty_kind {
            TyKind::I32 => vec![Instruction::I32Const(0)],

            TyKind::Extern => vec![Instruction::RefNull(HeapType::Extern)],

            TyKind::Id(ty_id) => match self.ty_index.get_by_id(ty_id).unwrap().wasm_ty {
                WasmTy::Regular(RegularTy::I32) => unreachable!(),

                WasmTy::Regular(RegularTy::Extern) => unreachable!(),

                WasmTy::Regular(RegularTy::Class(ClassName::Builtin(
                    BuiltinClass::Int | BuiltinClass::String | BuiltinClass::Bool,
                ))) => self.create_object(ty_id, pos),

                WasmTy::Regular(RegularTy::Class(_)) => {
                    vec![Instruction::RefNull(ty_id.to_heap_type(pos))]
                }

                WasmTy::Regular(RegularTy::ByteArray) => {
                    vec![Instruction::ArrayNewFixed(ArrayNewFixed {
                        array: ty_id.to_wasm_index(pos),
                        length: 0,
                    })]
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
            result.extend(self.default_initializer_for(field_ty_kind, pos));
        }

        result.push(Instruction::StructNew(ty_id.to_wasm_index(pos)));

        result
    }

    fn generate_constructor(
        &mut self,
        ty_id: TyId,
        class_name: &ClassName<'buf>,
        source: BuiltinMethodGeneratorSource<'_, 'buf>,
    ) -> wast::core::Func<'aux> {
        use wast::core::*;

        let span = trace_span!("generate_constructor", ?ty_id);
        let _span = span.enter();

        let mut instrs = vec![];
        let locals = LocalCtx::new();

        trace!("Generating {}", SpecialMethodKey::Constructor.as_str());

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
        instrs.push(Instruction::RefCast(object_ty_id.to_cast_arg(source.pos())));
        instrs.push(Instruction::Call(
            initializer_func_id.to_wasm_index(source.pos()),
        ));

        self.make_func(
            instrs,
            locals,
            constructor_ty_id,
            source.pos(),
            self.storage.get(&format!(
                "{}::{}",
                class_name,
                SpecialMethodKey::Constructor.as_str()
            )),
        )
    }

    fn generate_initializer(
        &mut self,
        ty_id: TyId,
        class_name: &ClassName<'buf>,
        source: BuiltinMethodGeneratorSource<'_, 'buf>,
    ) -> wast::core::Func<'aux> {
        use wast::core::*;

        let span = trace_span!("generate_initializer", ?ty_id);
        let _span = span.enter();

        let mut instrs = vec![];
        let locals = LocalCtx::new();

        trace!(name = %class_name, "Generating {}", SpecialMethodKey::Initializer.as_str());

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

                trace!(?parent, "Inserted a static dispatch to the parent class");
            }

            match source {
                BuiltinMethodGeneratorSource::Class(class) => {
                    trace!("Generating field initializers");

                    instrs.push(Instruction::RefCast(ty_id.to_cast_arg(source.pos())));
                    let self_local_id =
                        locals.bind(Some(Cow::Borrowed(b"self")), TyKind::Id(ty_id));
                    instrs.push(self_local_id.wasm_set(source.pos()));

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
                        trace!(
                            ?field_id,
                            "Field {} has an initializer, generating code",
                            &field.0.name
                        );
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

        self.make_func(
            instrs,
            locals,
            initializer_ty_id,
            source.pos(),
            self.storage.get(&format!(
                "{}::{}",
                class_name,
                SpecialMethodKey::Initializer.as_str()
            )),
        )
    }

    fn lower_method(
        &mut self,
        ty_id: TyId,
        class_name: &ClassName<'buf>,
        method: &ast::Method<'buf>,
    ) -> wast::core::Func<'aux> {
        let span = trace_span!("lower_method", ?ty_id, %class_name, method.name = %&method.name);
        let _span = span.enter();

        let Some(method_id) = self.method_index.get_by_name(ty_id, method.name.as_slice()) else {
            panic!("Method {}::{} was not found in the method index", class_name, &method.name);
        };

        trace!(?method_id, "Lowering a class method");

        let method_ty_id = self
            .method_index
            .get_by_id(method_id)
            .unwrap()
            .1
            .method_ty_id;

        let locals = Default::default();
        let instrs = CodegenVisitor::new(self, method_id, &locals, Some(&method.params))
            .visit_expr(&method.body);

        self.make_func(
            instrs,
            locals,
            method_ty_id,
            method.pos(),
            self.storage
                .get(&format!("{}::{}", class_name, &method.name)),
        )
    }
}

impl<'buf, 'aux> Codegen<'buf, 'aux, '_, WasmTy<'buf>> {
    fn generate_module(mut self, rec_decl: wast::core::Rec<'aux>) -> wast::core::Module<'aux> {
        use wast::core::*;

        let span = trace_span!("generate_module");
        let _span = span.enter();

        let mut module_fields = vec![];
        module_fields.push(ModuleField::Rec(rec_decl));
        module_fields.extend(self.generate_imports().into_iter().map(ModuleField::Import));
        module_fields.extend(self.generate_tables());
        module_fields.extend(self.generate_functions().into_iter().map(ModuleField::Func));
        module_fields.extend(self.generate_memory());
        module_fields.extend(self.generate_exports().into_iter().map(ModuleField::Export));

        Module {
            span: WasmSpan::from_offset(0),
            id: None,
            name: Some(NameAnnotation { name: "compiled-cool-program" }),
            kind: ModuleKind::Text(module_fields),
        }
    }

    fn generate_imports(&self) -> Vec<wast::core::Import<'aux>> {
        use wast::core::*;

        let span = trace_span!("generate_imports");
        let _span = span.enter();

        self.func_registry
            .iter()
            .filter_map(|(_, _, func_def)| {
                try_match!(func_def.kind, FuncDefKind::Imported(key) => {
                    (func_def.method_ty_id, key)
                })
            })
            .map(|(func_ty_id, key)| {
                trace!(
                    field = key.as_str(),
                    "Generating an import entry for {:?}",
                    key
                );

                Import {
                    span: WasmSpan::from_offset(0),
                    module: "cool-runtime",
                    field: key.as_str(),
                    item: ItemSig {
                        span: WasmSpan::from_offset(0),
                        id: None,
                        name: Some(NameAnnotation { name: "cool-runtime-support" }),
                        kind: ItemKind::Func(TypeUse {
                            index: Some(func_ty_id.to_wasm_index(0)),
                            inline: None,
                        }),
                    },
                }
            })
            .collect()
    }

    fn generate_exports(&self) -> Vec<wast::core::Export<'static>> {
        use wast::core::*;

        let span = trace_span!("generate_exports");
        let _span = span.enter();

        let exported_builtins = [
            BuiltinFuncKey::Run,
            BuiltinFuncKey::BytesNew,
            BuiltinFuncKey::BytesLen,
            BuiltinFuncKey::BytesGet,
            BuiltinFuncKey::BytesSet,
        ];

        exported_builtins
            .into_iter()
            .map(|key| {
                let func = BUILTIN_FUNCS.get(key);
                let func_id = self.func_registry.get_by_name(&func.name).unwrap();
                trace!(func.name = %&func.name, func.id = ?func_id, "Generating an export entry for {:?}", key);

                Export {
                    span: WasmSpan::from_offset(0),
                    name: key.as_str(),
                    kind: ExportKind::Func,
                    item: func_id.to_wasm_index(0),
                }
            })
            .collect()
    }

    fn generate_tables(&mut self) -> Vec<wast::core::ModuleField<'aux>> {
        use wast::core::*;

        let span = trace_span!("generate_tables");
        let _span = span.enter();

        let mut result = vec![];

        for (table_id, method_ty_id, methods) in self.method_table.iter() {
            let table_span = trace_span!("method_table", ?table_id, ?method_ty_id);
            let _table_span = table_span.enter();

            let mut func_ids = vec![];

            for (idx, method_id) in methods.enumerate() {
                let func_id = self.func_registry.get_by_method_id(method_id).unwrap();
                trace!(?method_id, ?func_id, idx, "Pushing an index to the table");
                func_ids.push(func_id);
            }

            result.push(ModuleField::Table(Table {
                span: WasmSpan::from_offset(0),
                id: None,
                name: Some(NameAnnotation {
                    name: self.storage.get(&format!("method-table-ty-{}", method_ty_id.index())),
                }),
                exports: InlineExport { names: vec![] },
                kind: TableKind::Normal {
                    ty: TableType {
                        limits: Limits {
                            min: func_ids.len().try_into().unwrap(),
                            max: Some(func_ids.len().try_into().unwrap()),
                        },
                        elem: RefType {
                            nullable: true,
                            heap: method_ty_id.to_heap_type(0),
                        },
                    },
                    init_expr: None,
                },
            }));

            for (idx, func_id) in func_ids.into_iter().enumerate() {
                let name = match self.func_registry.get_by_id(func_id) {
                    Some((name, _)) => format!("method-table-ty-{}-func-{}", method_ty_id.index(), name),
                    None => format!("method-table-ty-{}-idx-{}", method_ty_id.index(), func_id.index()),
                };

                result.push(ModuleField::Elem(Elem {
                    span: WasmSpan::from_offset(0),
                    id: None,
                    name: Some(NameAnnotation {
                        name: self.storage.get(&name),
                    }),
                    kind: ElemKind::Active {
                        table: table_id.to_wasm_index(0),
                        offset: Expression {
                            instrs: vec![Instruction::I32Const(idx.try_into().unwrap())].into(),
                        },
                    },
                    payload: ElemPayload::Exprs {
                        ty: RefType {
                            nullable: true,
                            heap: method_ty_id.to_heap_type(0),
                        },
                        exprs: vec![Expression {
                            instrs: vec![Instruction::RefFunc(func_id.to_wasm_index(0))].into(),
                        }],
                    },
                }));
            }
        }

        result
    }

    fn generate_memory(&self) -> Vec<wast::core::ModuleField<'static>> {
        use wast::core::*;

        const PAGE_SIZE: usize = 65536;

        let span = trace_span!("generate_memory");
        let _span = span.enter();

        let memory_size = 4 * self.vtable.iter().count();
        let memory_size_pages = (memory_size + (PAGE_SIZE - 1)) / PAGE_SIZE;
        let memory = Memory {
            span: WasmSpan::from_offset(0),
            id: None,
            name: Some(NameAnnotation {
                name: "cool-vtable",
            }),
            exports: InlineExport { names: vec![] },
            kind: MemoryKind::Normal(MemoryType::B32 {
                limits: Limits {
                    min: memory_size_pages as u32,
                    max: None,
                },
                shared: false,
            }),
        };

        trace!(
            "Defined a 32-bit memory of size {} pages ({} B)",
            memory_size_pages,
            PAGE_SIZE
        );

        let mut vtable_bytes = vec![];

        {
            let vtable_span = trace_span!("compile_vtable");
            let _vtable_span = vtable_span.enter();

            for (vtable_id, method_table_id) in self.vtable.iter() {
                trace!(
                    ?vtable_id,
                    ?method_table_id,
                    "Pushing an index to the table"
                );
                let idx: u32 = method_table_id.method_idx().try_into().unwrap();
                vtable_bytes.extend(idx.to_le_bytes());
            }
        }

        let data = Data {
            span: WasmSpan::from_offset(0),
            id: None,
            name: Some(NameAnnotation {
                name: "cool-vtable",
            }),
            kind: DataKind::Active {
                memory: WasmIndex::Num(0, WasmSpan::from_offset(0)),
                offset: Expression {
                    instrs: vec![Instruction::I32Const(0)].into(),
                },
            },
            data: vec![DataVal::Integral(vtable_bytes)],
        };

        vec![ModuleField::Memory(memory), ModuleField::Data(data)]
    }

    fn generate_functions(&mut self) -> Vec<wast::core::Func<'aux>> {
        let span = trace_span!("generate_functions");
        let _span = span.enter();

        let mut funcs = mem::replace(&mut self.funcs, IndexMap::new());
        let mut result = vec![];

        for (func_id, func_name, def) in self.func_registry.iter() {
            let span = trace_span!("iter", ?func_id);
            let _span = span.enter();
            trace!(%func_name, ?def);

            let func = match def.kind {
                FuncDefKind::Builtin(key) => self.generate_builtin(key),
                FuncDefKind::Imported(_) => {
                    trace!("Skipping the function because it's imported");

                    continue;
                }
                FuncDefKind::ClassMethod(method_id) => funcs.remove(&method_id).unwrap(),
            };

            result.push(func);
            trace!("Added to the function list");
        }

        result
    }

    fn generate_builtin(&self, key: BuiltinFuncKey) -> wast::core::Func<'static> {
        let span = trace_span!("generate_builtin", ?key);
        let _span = span.enter();

        trace!(?key, "Generating a built-in function");

        match key {
            BuiltinFuncKey::StringEq => self.generate_string_eq(),
            BuiltinFuncKey::StringConcat => self.generate_string_concat(),
            BuiltinFuncKey::StringSubstr => self.generate_string_substr(),
            BuiltinFuncKey::Run => self.generate_run_func(),
            BuiltinFuncKey::BytesNew => self.generate_bytes_new(),
            BuiltinFuncKey::BytesLen => self.generate_bytes_len(),
            BuiltinFuncKey::BytesGet => self.generate_bytes_get(),
            BuiltinFuncKey::BytesSet => self.generate_bytes_set(),
        }
    }

    fn generate_run_func(&self) -> wast::core::Func<'static> {
        use wast::core::*;

        let func = BUILTIN_FUNCS.get(BuiltinFuncKey::Run);
        let func_ty_id = self.ty_index.get_by_wasm_ty(&func.ty).unwrap();

        let main_ty_id = self
            .ty_index
            .get_by_wasm_ty(&ClassName::from(b"Main".as_slice()).into())
            .unwrap();
        let main_method_id = self.method_index.get_by_name(main_ty_id, b"main").unwrap();
        let main_new_method_id = self
            .method_index
            .get_by_name(
                main_ty_id,
                SPECIAL_METHODS.get(SpecialMethodKey::Constructor).name,
            )
            .unwrap();
        let main_new_func_id = self
            .func_registry
            .get_by_method_id(main_new_method_id)
            .unwrap();

        let mut instrs = vec![];
        let locals = LocalCtx::new();

        {
            let main_local_id = locals.bind(None, main_ty_id);

            instrs.push(Instruction::Call(main_new_func_id.to_wasm_index(0)));
            // stack: <main: Object>
            instrs.push(Instruction::RefCast(main_ty_id.to_cast_arg(0)));
            instrs.push(main_local_id.wasm_tee(0));
            // stack: <main: Main>
            instrs.extend(self.virtual_dispatch(main_local_id, main_ty_id, main_method_id));
            // stack: <null: Object>
            instrs.push(Instruction::Drop);
        }

        self.make_func(instrs, locals, func_ty_id, 0, "run")
    }
}

impl<'buf, 'aux, T: TyIndexEntry<'buf>> Codegen<'buf, 'aux, '_, T>
where
    WasmTy<'buf>: Equivalent<T>,
{
    fn vtable_mem_arg(&self, pos: usize) -> wast::core::MemArg<'static> {
        wast::core::MemArg {
            align: 4,
            offset: 0,
            memory: WasmIndex::Num(0, WasmSpan::from_offset(pos)),
        }
    }

    fn vtable_idx_to_offset(&self) -> Vec<wast::core::Instruction<'static>> {
        use wast::core::*;

        vec![Instruction::I32Const(4), Instruction::I32Mul]
    }

    fn virtual_dispatch<'a>(
        &self,
        self_local_id: LocalId<'a, 'buf>,
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
        instrs.push(self_local_id.wasm_get(0));
        // stack: <self: Object>
        instrs.push(vtable_field_id.wasm_get(0));
        // stack: <vtable_base: i32>
        instrs.push(method_id.to_i32_const());
        instrs.push(Instruction::I32Add);
        // stack: <Self::<method>::idx: i32>
        instrs.extend(self.vtable_idx_to_offset());
        instrs.push(Instruction::I32Load(self.vtable_mem_arg(0)));
        instrs.push(Instruction::TableGet(
            method_table_id.table_id().to_table_arg(0),
        ));
        instrs.push(Instruction::CallRef(method_ty_id.to_heap_type(0)));

        instrs
    }

    fn builtin_ty_id(&self, builtin: BuiltinClass) -> TyId {
        self.ty_index.get_by_wasm_ty(&builtin.into()).unwrap()
    }

    fn make_func<'a>(
        &self,
        instrs: Vec<Instruction<'a>>,
        locals: LocalCtx<'buf>,
        method_ty_id: TyId,
        pos: usize,
        name: &'a str,
    ) -> wast::core::Func<'a> {
        use wast::core::*;

        let WasmTy::Func { ref params, .. } = self.ty_index.get_by_id(method_ty_id).unwrap().wasm_ty() else {
            panic!("method_ty_id {:?} does not refer to a method type", method_ty_id);
        };

        Func {
            span: WasmSpan::from_offset(pos),
            id: None,
            name: Some(NameAnnotation { name }),
            exports: InlineExport { names: vec![] },
            kind: FuncKind::Inline {
                locals: process_locals(locals, params.len(), pos),
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
