pub mod ctx;
mod funcs;
pub mod passes;
mod string_collector;
mod visitor;

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
use crate::codegen::ctx::ty::initializer_ty;
use crate::codegen::ctx::ty::RegularTy;
use crate::position::HasSpan;
use crate::try_match;

use ctx::ty::constructor_ty;
use ctx::ty::WasmTy;
use ctx::ty::CONSTRUCTOR_NAME;
use ctx::ty::INITIALIZER_NAME;
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

pub struct CodegenOutput {
    pub module: wast::core::Module<'static>,
}

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
        let name: ClassName = class.name.borrow().into();
        let wasm_ty = name.clone().into();
        let Some(ty_id) = self.ty_index.get_by_wasm_ty(&wasm_ty) else {
            panic!("The class {} was not found in the type index", &class.name);
        };

        for feature in &class.features {
            let Some(method) = try_match!(feature, ast::Feature::Method(method) => method) else { continue };
            let Some(method_id) = self.method_index.get_by_name(ty_id, method.name.as_slice()) else {
                panic!("Method {}::{} was not found in the method index", &name, &method.name);
            };

            let func = self.lower_method(ty_id, &name, method);
            self.funcs.insert(method_id, func);
        }

        let constructor_method_id = self
            .method_index
            .get_by_name(ty_id, CONSTRUCTOR_NAME)
            .unwrap();
        let initializer_method_id = self
            .method_index
            .get_by_name(ty_id, INITIALIZER_NAME)
            .unwrap();
        let constructor = self.generate_constructor(ty_id, class);
        let initializer = self.generate_initializer(ty_id, &name, class);
        self.funcs.insert(constructor_method_id, constructor);
        self.funcs.insert(initializer_method_id, initializer);
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
        class: &Class<'buf>,
    ) -> wast::core::Func<'static> {
        use wast::core::*;

        let mut result = vec![];

        result.extend(self.create_object(ty_id, class.pos()));
        let constructor_ty_id = self.ty_index.get_by_wasm_ty(&constructor_ty()).unwrap();
        let initializer_method_id = self
            .method_index
            .get_by_name(ty_id, INITIALIZER_NAME)
            .unwrap();
        let initializer_func_id = self
            .func_registry
            .get_by_method_id(initializer_method_id)
            .unwrap();
        let object_ty_id = self
            .ty_index
            .get_by_wasm_ty(&ClassName::from(BuiltinClass::Object).into())
            .unwrap();
        result.push(Instruction::RefCast(
            object_ty_id.to_wasm_index(class.pos()),
        ));
        result.push(Instruction::Call(
            initializer_func_id.to_wasm_index(class.pos()),
        ));

        let expression = Expression {
            instrs: result.into(),
        };

        Func {
            span: WasmSpan::from_offset(class.pos()),
            id: None,
            name: None,
            exports: InlineExport { names: vec![] },
            kind: FuncKind::Inline {
                locals: vec![],
                expression,
            },
            ty: TypeUse {
                index: Some(constructor_ty_id.to_wasm_index(class.pos())),
                inline: None,
            },
        }
    }

    fn generate_initializer(
        &mut self,
        ty_id: TyId,
        class_name: &ClassName<'buf>,
        class: &Class<'buf>,
    ) -> wast::core::Func<'static> {
        use wast::core::*;

        let locals = LocalCtx::new();
        let mut instrs = vec![];

        let initializer_ty_id = self.ty_index.get_by_wasm_ty(&initializer_ty()).unwrap();
        let object_ty_id = self
            .ty_index
            .get_by_wasm_ty(&ClassName::from(BuiltinClass::Object).into())
            .unwrap();
        let object_self_local_id =
            locals.bind(Some(Cow::Borrowed(b"self")), TyKind::Id(object_ty_id));
        instrs.push(object_self_local_id.wasm_get(class.pos()));

        let class_index = self.ty_ctx.get_class(class_name).unwrap();

        if let Some(parent) = class_index.parent() {
            let parent_wasm_ty = parent.clone().into();
            let parent_ty_id = self.ty_index.get_by_wasm_ty(&parent_wasm_ty).unwrap();
            let parent_initializer_method_id = self
                .method_index
                .get_by_name(parent_ty_id, INITIALIZER_NAME)
                .unwrap();
            let parent_initializer_func_id = self
                .func_registry
                .get_by_method_id(parent_initializer_method_id)
                .unwrap();
            instrs.push(Instruction::Call(
                parent_initializer_func_id.to_wasm_index(class.pos()),
            ));
        }

        instrs.push(Instruction::RefCast(ty_id.to_wasm_index(class.pos())));
        let self_local_id = locals.bind(Some(Cow::Borrowed(b"self")), TyKind::Id(ty_id));
        self_local_id.wasm_set(class.pos());

        let initializer_method_id = self
            .method_index
            .get_by_name(ty_id, INITIALIZER_NAME)
            .unwrap();
        let mut visitor = CodegenVisitor::new(self, initializer_method_id, &locals, &[]);

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
        instrs.push(object_self_local_id.wasm_get(class.pos()));

        let expression = Expression {
            instrs: instrs.into(),
        };

        Func {
            span: WasmSpan::from_offset(class.pos()),
            id: None,
            name: None,
            exports: InlineExport { names: vec![] },
            kind: FuncKind::Inline {
                locals: vec![Local {
                    id: None,
                    name: None,
                    ty: ValType::Ref(RefType {
                        nullable: true,
                        heap: HeapType::Index(object_ty_id.to_wasm_index(class.pos())),
                    }),
                }],
                expression,
            },
            ty: TypeUse {
                index: Some(initializer_ty_id.to_wasm_index(class.pos())),
                inline: None,
            },
        }
    }

    fn lower_method(
        &mut self,
        ty_id: TyId,
        class_name: &ClassName<'buf>,
        method: &ast::Method<'buf>,
    ) -> wast::core::Func<'static> {
        use wast::core::*;

        let Some(method_id) = self.method_index.get_by_name(ty_id, method.name.as_slice()) else {
            panic!("Method {}::{} was not found in the method index", class_name, &method.name);
        };

        let method_ty_id = self
            .method_index
            .get_by_id(method_id)
            .unwrap()
            .1
            .method_ty_id;

        let mut locals = Default::default();
        let mut visitor = CodegenVisitor::new(self, method_id, &mut locals, &method.params);
        let instrs = visitor.visit_expr(&method.body);
        let expression = Expression {
            instrs: instrs.into(),
        };
        drop(visitor);
        let locals = locals
            .into_iter()
            .map(|ty_kind| Local {
                id: None,
                name: None,
                ty: ty_kind.to_val_type(method.pos(), true),
            })
            .collect();

        Func {
            span: WasmSpan::from_offset(method.pos()),
            id: None,
            name: None,
            exports: InlineExport { names: vec![] },
            kind: FuncKind::Inline { locals, expression },
            ty: TypeUse {
                index: Some(method_ty_id.to_wasm_index(method.pos())),
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
            .map(|(func_ty_id, key)| {
                Import {
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
                }
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

    fn generate_builtin(&self, _key: BuiltinFuncKey) -> wast::core::Func<'static> {
        todo!()
    }
}
