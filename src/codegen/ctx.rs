mod layout;
mod locals;
pub mod passes;
pub mod ty;

use std::borrow::{Borrow, Cow};
use std::collections::HashMap;
use std::fmt::{self, Display};
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::Deref;

use indexmap::{Equivalent, IndexMap, IndexSet};
use tracing::{trace, trace_span, Level};

use crate::analysis::ClassName;
use crate::try_match;
use crate::util::slice_formatter;

use super::funcs::{BuiltinFuncKey, ImportedFuncKey};

use ty::WasmTy;

pub use layout::{VALUE_FIELD_NAME, VTABLE_FIELD_NAME};
pub use locals::{LocalCtx, LocalId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TyId(usize);

impl TyId {
    pub fn index(&self) -> usize {
        self.0
    }

    pub fn to_wasm_index(&self, pos: usize) -> wast::token::Index<'static> {
        wast::token::Index::Num(
            self.0.try_into().unwrap(),
            wast::token::Span::from_offset(pos),
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum TyKind {
    I32,
    Extern,
    Id(TyId),
}

impl TyKind {
    pub fn to_val_type(&self, pos: usize, nullable: bool) -> wast::core::ValType<'static> {
        use wast::core::*;

        match self {
            Self::I32 => ValType::I32,

            Self::Extern => ValType::Ref(RefType {
                nullable,
                heap: HeapType::Extern,
            }),

            Self::Id(ty_id) => ValType::Ref(RefType {
                nullable,
                heap: HeapType::Index(ty_id.to_wasm_index(pos)),
            }),
        }
    }
}

impl From<TyId> for TyKind {
    fn from(ty_id: TyId) -> Self {
        Self::Id(ty_id)
    }
}

pub trait TyIndexEntry<'buf>: private::Sealed + Hash + Eq + 'buf
where
    WasmTy<'buf>: Equivalent<Self>,
{
    fn wasm_ty(&self) -> &WasmTy<'buf>;
}

#[derive(Debug, Clone)]
pub struct TyIndex<'buf, T: TyIndexEntry<'buf>>
where
    WasmTy<'buf>: Equivalent<T>,
{
    types: IndexSet<T>,
    _marker: PhantomData<&'buf mut ()>,
}

impl private::Sealed for WasmTy<'_> {}

impl<'buf> TyIndexEntry<'buf> for WasmTy<'buf> {
    fn wasm_ty(&self) -> &WasmTy<'buf> {
        self
    }
}

#[derive(Debug)]
pub struct CompleteWasmTy<'buf> {
    pub complete_ty: wast::core::Type<'static>,
    pub wasm_ty: WasmTy<'buf>,
    _private: PhantomData<()>,
}

impl PartialEq for CompleteWasmTy<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.wasm_ty == other.wasm_ty
    }
}

impl Eq for CompleteWasmTy<'_> {}

impl Hash for CompleteWasmTy<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.wasm_ty.hash(state);
    }
}

impl<'buf> Equivalent<CompleteWasmTy<'buf>> for WasmTy<'buf> {
    fn equivalent(&self, key: &CompleteWasmTy<'buf>) -> bool {
        self == &key.wasm_ty
    }
}

impl private::Sealed for CompleteWasmTy<'_> {}

impl<'buf> TyIndexEntry<'buf> for CompleteWasmTy<'buf> {
    fn wasm_ty(&self) -> &WasmTy<'buf> {
        &self.wasm_ty
    }
}

impl<'buf> TyIndex<'buf, WasmTy<'buf>> {
    pub fn new() -> Self {
        Self {
            types: IndexSet::new(),
            _marker: Default::default(),
        }
    }

    pub fn insert(&mut self, ty: WasmTy<'buf>) -> TyId {
        assert!(ty.is_boxed());

        if let Some(idx) = self.types.get_index_of(&ty) {
            return TyId(idx);
        }

        let (idx, _) = self.types.insert_full(ty);

        TyId(idx)
    }

    pub fn iter(&self) -> impl Iterator<Item = (TyId, &WasmTy<'buf>)> {
        self.types
            .iter()
            .enumerate()
            .map(|(idx, ty)| (TyId(idx), ty))
    }
}

impl<'buf> TyIndex<'buf, CompleteWasmTy<'buf>> {
    pub fn new() -> Self {
        Self {
            types: IndexSet::new(),
            _marker: Default::default(),
        }
    }

    pub(super) fn insert(
        &mut self,
        complete_ty: wast::core::Type<'static>,
        wasm_ty: WasmTy<'buf>,
    ) -> TyId {
        assert!(wasm_ty.is_boxed());

        if let Some(idx) = self.types.get_index_of(&wasm_ty) {
            return TyId(idx);
        }

        let (idx, _) = self.types.insert_full(CompleteWasmTy {
            complete_ty,
            wasm_ty,
            _private: Default::default(),
        });

        TyId(idx)
    }

    pub fn extract_completed_tys(
        self,
    ) -> (
        impl Iterator<Item = (TyId, wast::core::Type<'static>)>,
        TyIndex<'buf, WasmTy<'buf>>,
    ) {
        let (complete_tys, wasm_tys): (Vec<_>, IndexSet<_>) = self
            .types
            .into_iter()
            .enumerate()
            .map(
                |(
                    idx,
                    CompleteWasmTy {
                        complete_ty,
                        wasm_ty,
                        ..
                    },
                )| ((TyId(idx), complete_ty), wasm_ty),
            )
            .unzip();

        (
            complete_tys.into_iter(),
            TyIndex {
                types: wasm_tys,
                _marker: Default::default(),
            },
        )
    }
}

impl<'buf, T: TyIndexEntry<'buf>> TyIndex<'buf, T>
where
    WasmTy<'buf>: Equivalent<T>,
{
    pub fn get_by_wasm_ty(&self, ty: &WasmTy<'buf>) -> Option<TyId> {
        self.types.get_index_of(ty).map(TyId)
    }

    pub fn get_by_id(&self, id: TyId) -> Option<&T> {
        self.types.get_index(id.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MethodId {
    /// The type id of the class defining the method.
    ty_id: TyId,
    idx: usize,
}

impl MethodId {
    pub fn ty_id(&self) -> TyId {
        self.ty_id
    }

    pub fn index(&self) -> usize {
        self.idx
    }

    pub fn to_i32_const(&self) -> wast::core::Instruction<'static> {
        wast::core::Instruction::I32Const(self.idx.try_into().unwrap())
    }
}

impl PartialOrd for MethodId {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        (self.ty_id == other.ty_id).then_some(self.idx.cmp(&other.idx))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MethodDefinition {
    /// The id of this method's type
    pub method_ty_id: TyId,
    /// The id of the first definition of the method in the inheritance chain.
    pub first_def_id: MethodId,
    /// The id of the last override of the method in the inheritance chain.
    pub last_def_id: MethodId,
}

#[derive(Debug, Clone)]
pub struct MethodIndex<'buf> {
    classes: IndexMap<TyId, IndexMap<Cow<'buf, [u8]>, MethodDefinition>>,
}

impl<'buf> MethodIndex<'buf> {
    pub fn new() -> Self {
        Self {
            classes: IndexMap::new(),
        }
    }

    pub fn insert(
        &mut self,
        ty_id: TyId,
        method_name: Cow<'buf, [u8]>,
        method_ty_id: TyId,
    ) -> MethodId {
        use indexmap::map::Entry;

        let class_map = self.classes.entry(ty_id).or_default();
        let entry = class_map.entry(method_name);
        let idx = entry.index();
        let id = MethodId { ty_id, idx };

        match entry {
            Entry::Occupied(mut entry) => {
                let &MethodDefinition {
                    method_ty_id: def_method_ty_id,
                    first_def_id,
                    last_def_id: _,
                } = entry.get();

                assert_eq!(def_method_ty_id, method_ty_id);

                entry.insert(MethodDefinition {
                    method_ty_id,
                    first_def_id,
                    last_def_id: id,
                });
            }

            Entry::Vacant(entry) => {
                entry.insert(MethodDefinition {
                    method_ty_id,
                    first_def_id: id,
                    last_def_id: id,
                });
            }
        }

        id
    }

    /// Copies methods from `super_ty_id` to `ty_id`.
    pub fn inherit(&mut self, super_ty_id: TyId, ty_id: TyId) {
        let span = trace_span!("MethodIndex::inherit", ?ty_id, ?super_ty_id);
        let _span = span.enter();

        let Some(super_class_map) = self.classes.get(&super_ty_id) else { return };
        let class_map = super_class_map.clone();

        trace!("Inheriting {:?} from {:?}", ty_id, super_ty_id);

        if tracing::enabled!(Level::TRACE) {
            for (idx, (name, def)) in class_map.iter().enumerate() {
                trace!(name = %slice_formatter(name), method_id = ?MethodId { ty_id, idx }, ?def);
            }
        }

        self.classes.insert(ty_id, class_map);
    }

    pub fn get_by_name(&self, ty_id: TyId, method_name: &[u8]) -> Option<MethodId> {
        self.classes
            .get(&ty_id)
            .and_then(|map| map.get_index_of(method_name))
            .map(|idx| MethodId { ty_id, idx })
    }

    pub fn get_by_id(&self, method_id: MethodId) -> Option<(&[u8], &MethodDefinition)> {
        let MethodId { ty_id, idx } = method_id;

        self.classes
            .get(&ty_id)
            .and_then(|map| map.get_index(idx))
            .map(|(name, def)| (name.deref(), def))
    }

    /// Returns an iterator over methods of the class with the given `ty_id`.
    ///
    /// The elements of the iterator are ordered by the method id.
    pub fn methods(&self, ty_id: TyId) -> Option<impl Iterator<Item = (&[u8], &MethodDefinition)>> {
        self.classes
            .get(&ty_id)
            .map(|map| map.iter().map(|(name, def)| (name.deref(), def)))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TableId(usize);

impl TableId {
    pub fn new(idx: usize) -> Self {
        Self(idx)
    }

    pub fn index(&self) -> usize {
        self.0
    }

    pub fn to_wasm_index(&self, pos: usize) -> wast::token::Index<'static> {
        wast::token::Index::Num(
            self.0.try_into().unwrap(),
            wast::token::Span::from_offset(pos),
        )
    }

    pub fn to_table_arg(&self, pos: usize) -> wast::core::TableArg<'static> {
        wast::core::TableArg {
            dst: wast::token::Index::Num(
                self.0.try_into().unwrap(),
                wast::token::Span::from_offset(pos),
            ),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct MethodTableId {
    table_id: TableId,
    method_idx: usize,
}

impl MethodTableId {
    pub fn table_id(&self) -> TableId {
        self.table_id
    }

    pub fn method_idx(&self) -> usize {
        self.method_idx
    }

    pub fn to_i32_const(&self) -> wast::core::Instruction<'static> {
        wast::core::Instruction::I32Const(self.method_idx.try_into().unwrap())
    }
}

#[derive(Debug, Clone)]
pub struct MethodTable {
    method_tys: IndexMap<TyId, IndexSet<MethodId>>,
}

impl MethodTable {
    pub fn new() -> Self {
        Self {
            method_tys: IndexMap::new(),
        }
    }

    pub fn insert(&mut self, method_ty_id: TyId, method_id: MethodId) -> MethodTableId {
        assert_ne!(method_ty_id, method_id.ty_id());
        let table_entry = self.method_tys.entry(method_ty_id);
        let table_idx = table_entry.index();
        let method_set = table_entry.or_default();
        let (method_idx, inserted) = method_set.insert_full(method_id);
        assert!(
            inserted,
            "Method id {:?} is already contained in the method table",
            method_id
        );

        MethodTableId {
            table_id: TableId(table_idx),
            method_idx,
        }
    }

    pub fn get_by_method_id(
        &self,
        method_ty_id: TyId,
        method_id: MethodId,
    ) -> Option<MethodTableId> {
        let (table_idx, _, method_set) = self.method_tys.get_full(&method_ty_id)?;
        let method_idx = method_set.get_index_of(&method_id)?;

        Some(MethodTableId {
            table_id: TableId(table_idx),
            method_idx,
        })
    }

    pub fn get_by_table_id(&self, table_id: MethodTableId) -> Option<(TyId, MethodId)> {
        self.method_tys
            .get_index(table_id.table_id.0)
            .and_then(|(&ty_id, method_set)| {
                method_set
                    .get_index(table_id.method_idx)
                    .map(|&method_id| (ty_id, method_id))
            })
    }

    pub fn get_table_id(&self, method_ty_id: TyId) -> Option<TableId> {
        self.method_tys.get_index_of(&method_ty_id).map(TableId)
    }

    pub fn iter(
        &self,
    ) -> impl Iterator<Item = (TableId, TyId, impl Iterator<Item = MethodId> + '_)> + '_ {
        self.method_tys.iter().map(|(&ty_id, methods)| {
            (
                TableId(self.method_tys.get_index_of(&ty_id).unwrap()),
                ty_id,
                methods.iter().copied(),
            )
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FieldId {
    ty_id: TyId,
    idx: usize,
}

impl FieldId {
    pub fn wasm_set(&self, pos: usize) -> wast::core::Instruction<'static> {
        use wast::core::*;
        use wast::token::Index as WasmIndex;
        use wast::token::Span as WasmSpan;

        Instruction::StructSet(StructAccess {
            r#struct: self.ty_id.to_wasm_index(pos),
            field: WasmIndex::Num(self.idx.try_into().unwrap(), WasmSpan::from_offset(pos)),
        })
    }

    pub fn wasm_get(&self, pos: usize) -> wast::core::Instruction<'static> {
        use wast::core::*;
        use wast::token::Index as WasmIndex;
        use wast::token::Span as WasmSpan;

        Instruction::StructGet(StructAccess {
            r#struct: self.ty_id.to_wasm_index(pos),
            field: WasmIndex::Num(self.idx.try_into().unwrap(), WasmSpan::from_offset(pos)),
        })
    }
}

#[derive(Debug, Clone)]
pub struct FieldTable<'buf> {
    classes: IndexMap<TyId, IndexMap<Cow<'buf, [u8]>, TyKind>>,
}

impl<'buf> FieldTable<'buf> {
    pub fn new() -> Self {
        Self {
            classes: IndexMap::new(),
        }
    }

    pub fn insert(
        &mut self,
        ty_id: TyId,
        field_name: Cow<'buf, [u8]>,
        field_ty_kind: impl Into<TyKind>,
    ) -> FieldId {
        use indexmap::map::Entry;

        let class_map = self.classes.entry(ty_id).or_default();
        let entry = class_map.entry(field_name);
        let idx = entry.index();

        match entry {
            Entry::Vacant(entry) => {
                entry.insert(field_ty_kind.into());
            }

            Entry::Occupied(entry) => panic!(
                "Field `{}` (ty id = {:?}) is defined twice",
                slice_formatter(entry.key()),
                ty_id
            ),
        }

        FieldId { ty_id, idx }
    }

    pub fn get_by_name(&self, ty_id: TyId, field_name: &[u8]) -> Option<FieldId> {
        let class_map = self.classes.get(&ty_id)?;
        let idx = class_map.get_index_of(field_name)?;

        Some(FieldId { ty_id, idx })
    }

    pub fn get_by_id(&self, field_id: FieldId) -> Option<(&[u8], TyKind)> {
        let class_map = self.classes.get(&field_id.ty_id)?;
        let (name, &ty_kind) = class_map.get_index(field_id.idx)?;

        Some((name, ty_kind))
    }

    pub fn fields<'a>(
        &'a self,
        ty_id: TyId,
    ) -> Option<impl Iterator<Item = (FieldId, &'a [u8], TyKind)>> {
        self.classes.get(&ty_id).map(move |class_map| {
            class_map.iter().map(move |(name, &ty_kind)| {
                (
                    FieldId {
                        ty_id,
                        idx: class_map.get_index_of(name).unwrap(),
                    },
                    name.borrow(),
                    ty_kind,
                )
            })
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct VtableId(usize);

impl VtableId {
    pub fn offset(&self, offset: usize) -> VtableId {
        VtableId(self.0 + offset)
    }

    pub fn to_i32_const(&self) -> wast::core::Instruction<'static> {
        wast::core::Instruction::I32Const(self.0.try_into().unwrap())
    }
}

/// The program vtable.
///
/// For each method `m` (of type `t`) having index `i` in the class `C`,
/// the vtable maps index `C.base + i` to `j` where `method_table[t][j] == m`.
///
/// `Vtable` also stores the base offsets `C.base`.
#[derive(Debug, Clone)]
pub struct Vtable {
    table: Vec<MethodTableId>,
    base_offsets: HashMap<TyId, usize>,
}

impl Vtable {
    pub fn new() -> Self {
        Self {
            table: Vec::new(),
            base_offsets: HashMap::new(),
        }
    }

    fn insert(&mut self, ty_id: TyId, methods: Vec<MethodTableId>) -> VtableId {
        use std::collections::hash_map::Entry;

        let base_offset = self.table.len();

        match self.base_offsets.entry(ty_id) {
            Entry::Occupied(_) => {
                panic!("The type {:?} was already added to the vtable", ty_id);
            }

            Entry::Vacant(entry) => {
                entry.insert(base_offset);
            }
        }

        self.table.extend_from_slice(&methods);

        VtableId(base_offset)
    }

    pub fn get(&self, id: VtableId) -> Option<MethodTableId> {
        self.table.get(id.0).copied()
    }

    pub fn base_offset(&self, ty_id: TyId) -> Option<VtableId> {
        self.base_offsets.get(&ty_id).copied().map(VtableId)
    }

    pub fn iter(&self) -> impl Iterator<Item = (VtableId, MethodTableId)> + '_ {
        self.table
            .iter()
            .enumerate()
            .map(|(idx, &method_table_id)| (VtableId(idx), method_table_id))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct StringId(usize);

impl StringId {
    pub fn index(&self) -> usize {
        self.0
    }

    pub fn to_i32_const(&self) -> wast::core::Instruction<'static> {
        wast::core::Instruction::I32Const(self.0.try_into().unwrap())
    }
}

#[derive(Debug, Clone)]
pub struct StringTable<'buf> {
    strings: IndexSet<Cow<'buf, [u8]>>,
}

impl<'buf> StringTable<'buf> {
    pub fn new() -> Self {
        Self {
            strings: IndexSet::new(),
        }
    }

    pub fn insert(&mut self, bytes: Cow<'buf, [u8]>) -> StringId {
        let (idx, _) = self.strings.insert_full(bytes);

        StringId(idx)
    }

    pub fn get_by_str(&self, bytes: &[u8]) -> Option<StringId> {
        self.strings.get_index_of(bytes).map(StringId)
    }

    pub fn get_by_id(&self, id: StringId) -> Option<&[u8]> {
        self.strings.get_index(id.0).map(Deref::deref)
    }

    pub fn iter<'a>(&'a self) -> impl Iterator<Item = (StringId, &'a [u8])> {
        self.strings
            .iter()
            .enumerate()
            .map(|(idx, s)| (StringId(idx), &**s))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FuncName<'buf> {
    Plain(Cow<'buf, str>),
    Method {
        class_name: ClassName<'buf>,
        method_name: Cow<'buf, [u8]>,
    },
}

impl FuncName<'_> {
    pub fn as_plain(&self) -> Option<&str> {
        try_match!(self, Self::Plain(name) => &**name)
    }
}

impl Display for FuncName<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Plain(name) => name.fmt(f),
            Self::Method {
                class_name,
                method_name,
            } => {
                write!(f, "{}::{}", class_name, slice_formatter(method_name))
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FuncDefKind {
    /// A method defined a class, possibly built-in.
    ClassMethod(MethodId),

    /// A built-in freestanding function generated by wacc.
    Builtin(BuiltinFuncKey),

    /// An external function provided by the runtime environment.
    Imported(ImportedFuncKey),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncDef {
    pub kind: FuncDefKind,
    pub method_ty_id: TyId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FuncId(usize);

impl FuncId {
    pub fn index(&self) -> usize {
        self.0
    }

    pub fn to_wasm_index(&self, pos: usize) -> wast::token::Index<'static> {
        wast::token::Index::Num(
            self.0.try_into().unwrap(),
            wast::token::Span::from_offset(pos),
        )
    }
}

#[derive(Debug, Clone)]
pub struct FuncRegistry<'buf> {
    funcs: IndexMap<FuncName<'buf>, FuncDef>,
    methods: HashMap<MethodId, FuncId>,
    imported_allowed: bool,
}

impl<'buf> FuncRegistry<'buf> {
    pub fn new() -> Self {
        Self {
            funcs: IndexMap::new(),
            methods: HashMap::new(),
            imported_allowed: true,
        }
    }

    pub fn insert(&mut self, func_name: FuncName<'buf>, def: FuncDef) -> FuncId {
        let kind = def.kind;

        match kind {
            FuncDefKind::Imported(_) if self.imported_allowed => {}
            FuncDefKind::Imported(_) => {
                panic!("Imported functions must be inserted before other kinds")
            }
            _ => {
                self.imported_allowed = false;
            }
        }

        let (idx, _) = self.funcs.insert_full(func_name, def);

        if let FuncDefKind::ClassMethod(method_id) = kind {
            self.methods.insert(method_id, FuncId(idx));
        }

        FuncId(idx)
    }

    pub fn get_by_name(&self, func_name: &FuncName<'buf>) -> Option<FuncId> {
        self.funcs.get_index_of(func_name).map(FuncId)
    }

    pub fn get_by_id(&self, id: FuncId) -> Option<(&FuncName<'buf>, &FuncDef)> {
        self.funcs.get_index(id.0)
    }

    pub fn get_by_method_id(&self, method_id: MethodId) -> Option<FuncId> {
        self.methods.get(&method_id).copied()
    }

    pub fn iter<'a>(&'a self) -> impl Iterator<Item = (FuncId, &'a FuncName<'buf>, &'a FuncDef)> {
        self.funcs
            .iter()
            .enumerate()
            .map(|(idx, (func_name, func_def))| (FuncId(idx), func_name, func_def))
    }
}

mod private {
    pub trait Sealed {}
}
