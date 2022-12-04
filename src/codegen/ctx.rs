pub mod passes;

use std::borrow::Cow;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::ops::Deref;

use crate::analysis::ClassName;
use crate::ast::ty::{FunctionTy, ResolvedTy};

use indexmap::IndexMap;
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
    /// The id of the (super-)class that first defines this method.
    pub base_ty_id: TyId,
    /// The id of the (super-)class that provides a concrete implementation of the method.
    pub head_ty_id: TyId,
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
        let class_map = self.classes.entry(ty_id).or_default();
        let def = match class_map.get(&method_name) {
            Some(&MethodDefinition {
                method_ty_id: def_method_ty_id,
                base_ty_id,
                head_ty_id,
            }) => {
                assert_eq!(def_method_ty_id, method_ty_id);

                MethodDefinition {
                    method_ty_id,
                    base_ty_id,
                    head_ty_id: ty_id,
                }
            }

            None => MethodDefinition {
                method_ty_id,
                base_ty_id: ty_id,
                head_ty_id: ty_id,
            },
        };
        let (idx, _) = class_map.insert_full(method_name, def);

        MethodId { ty_id, idx }
    }

    /// Copies methods from `super_ty_id` to `ty_id`.
    pub fn inherit(&mut self, super_ty_id: TyId, ty_id: TyId) {
        let Some(super_class_map) = self.classes.get(&super_ty_id) else { return };
        let class_map = super_class_map.clone();
        self.classes.insert(ty_id, class_map);
    }

    pub fn get_by_name(&self, ty_id: TyId, method_name: Cow<'buf, [u8]>) -> Option<MethodId> {
        self.classes
            .get(&ty_id)
            .and_then(|map| map.get_index_of(&method_name))
            .map(|idx| MethodId { ty_id, idx })
    }

    pub fn get_by_id(&self, method_id: MethodId) -> Option<(&[u8], &MethodDefinition)> {
        let MethodId { ty_id, idx } = method_id;

        self.classes
            .get(&ty_id)
            .and_then(|map| map.get_index(idx))
            .map(|(name, def)| (name.deref(), def))
    }
}

mod private {
    pub trait Sealed {}
}
