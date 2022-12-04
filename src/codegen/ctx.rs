pub mod passes;

use std::borrow::Cow;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::ops::Deref;

use crate::analysis::ClassName;
use crate::ast::ty::{FunctionTy, ResolvedTy};

use indexmap::{IndexMap, IndexSet};
pub use passes::collect_types;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum WasmTy<'buf> {
    Class(ClassName<'buf>),

    Func {
        params: Vec<WasmTy<'buf>>,
        ret: ClassName<'buf>,
    },
}

impl<'buf> From<FunctionTy<'buf>> for WasmTy<'buf> {
    fn from(FunctionTy { params, ret }: FunctionTy<'buf>) -> Self {
        Self::Func {
            params: params.into_iter().map(Into::into).collect(),
            ret: (*ret).try_into().unwrap(),
        }
    }
}

impl<'buf> From<ClassName<'buf>> for WasmTy<'buf> {
    fn from(class_name: ClassName<'buf>) -> Self {
        assert_ne!(class_name, ClassName::SelfType);

        Self::Class(class_name)
    }
}

impl<'buf> From<ResolvedTy<'buf>> for WasmTy<'buf> {
    fn from(ty: ResolvedTy<'buf>) -> WasmTy<'buf> {
        match ty {
            ResolvedTy::Builtin(builtin) => Self::Class(builtin.into()),
            ResolvedTy::SelfType { enclosed } => (*enclosed).into(),
            ResolvedTy::Class(name) => Self::Class(name.into()),
            ResolvedTy::Function(ty) => ty.into(),

            ResolvedTy::Bottom | ResolvedTy::Untyped => {
                unreachable!("the ast must have passed typeck");
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TyId(usize);

pub trait TyIndexEntry<'buf>: private::Sealed + 'buf {}

#[derive(Debug, Clone)]
pub struct TyIndex<'buf, T: TyIndexEntry<'buf>> {
    types: Vec<T>,
    indices: HashMap<T, usize>,
    _marker: PhantomData<&'buf mut ()>,
}

impl private::Sealed for WasmTy<'_> {}
impl<'buf> TyIndexEntry<'buf> for WasmTy<'buf> {}

impl<'buf> TyIndex<'buf, WasmTy<'buf>> {
    pub fn new() -> Self {
        Self {
            types: Vec::new(),
            indices: HashMap::new(),
            _marker: Default::default(),
        }
    }

    pub fn insert(&mut self, ty: WasmTy<'buf>) -> TyId {
        if let Some(&idx) = self.indices.get(&ty) {
            return TyId(idx);
        }

        let idx = self.types.len();
        self.types.push(ty.clone());
        self.indices.insert(ty, idx);

        TyId(idx)
    }

    pub fn get_by_ty(&self, ty: &WasmTy<'buf>) -> Option<TyId> {
        self.indices.get(ty).copied().map(TyId)
    }

    pub fn get_by_id(&self, id: TyId) -> Option<&WasmTy<'buf>> {
        self.types.get(id.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MethodId {
    /// The type id of the class defining the method.
    ty_id: TyId,
    idx: usize,
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
    classes: HashMap<TyId, IndexMap<Cow<'buf, [u8]>, MethodDefinition>>,
}

impl<'buf> MethodIndex<'buf> {
    pub fn new() -> Self {
        Self {
            classes: HashMap::new(),
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
        let Some(super_class_map) = self.classes.get(&super_ty_id) else { return };
        let class_map = super_class_map.clone();
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
pub struct MethodTableId {
    table_idx: usize,
    method_idx: usize,
}

impl MethodTableId {
    pub fn table_idx(&self) -> usize {
        self.table_idx
    }

    pub fn method_idx(&self) -> usize {
        self.method_idx
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
        let table_entry = self.method_tys.entry(method_ty_id);
        let table_idx = table_entry.index();
        let method_set = table_entry.or_default();
        let (method_idx, inserted) = method_set.insert_full(method_id);
        assert!(
            !inserted,
            "Method id {:?} is already contained in the method table",
            method_id
        );

        MethodTableId {
            table_idx,
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
            table_idx,
            method_idx,
        })
    }

    pub fn get_by_table_id(&self, table_id: MethodTableId) -> Option<(TyId, MethodId)> {
        self.method_tys
            .get_index(table_id.table_idx)
            .and_then(|(&ty_id, method_set)| {
                method_set
                    .get_index(table_id.method_idx)
                    .map(|&method_id| (ty_id, method_id))
            })
    }
}

pub struct VtableId(usize);

impl VtableId {
    pub fn offset(&self, offset: usize) -> VtableId {
        VtableId(self.0 + offset)
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
            Entry::Occupied(entry) => {
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
}

mod private {
    pub trait Sealed {}
}
