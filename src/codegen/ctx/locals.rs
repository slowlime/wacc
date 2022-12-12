use std::borrow::Cow;
use std::cell::RefCell;
use std::marker::PhantomData;

use indexmap::{IndexMap, IndexSet};

use super::TyKind;

#[derive(Debug, Clone)]
struct LocalDef {
    idx: usize,
    gen: usize,
    ty_kind: TyKind,
    ref_count: usize,
    // prevent inferring Send and Sync
    _marker: PhantomData<*mut ()>,
}

impl LocalDef {
    fn make_ref(&self) -> LocalRef {
        let &Self { idx, gen, .. } = self;

        LocalRef { gen, idx }
    }

    fn make_id<'ctx, 'buf>(
        &mut self,
        ctx: &'ctx LocalCtx<'buf>,
        binding_idx: Option<usize>,
    ) -> LocalId<'ctx, 'buf> {
        self.ref_count += 1;

        LocalId {
            ctx,
            idx: self.idx,
            binding_idx,
        }
    }

    fn is_ref_valid(&self, local_ref: &LocalRef) -> bool {
        assert!(local_ref.gen <= self.gen);

        local_ref.gen == self.gen && local_ref.idx == self.idx && self.ref_count > 0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct LocalRef {
    gen: usize,
    idx: usize,
}

#[derive(Debug, Clone, Default)]
pub struct LocalCtx<'buf> {
    locals: RefCell<Vec<LocalDef>>,
    bindings: RefCell<IndexMap<Cow<'buf, [u8]>, IndexSet<LocalRef>>>,
}

#[derive(Debug)]
pub struct LocalId<'ctx, 'buf> {
    ctx: &'ctx LocalCtx<'buf>,
    idx: usize,
    binding_idx: Option<usize>,
}

impl LocalId<'_, '_> {
    pub fn to_wasm_index(&self, pos: usize) -> wast::token::Index<'static> {
        wast::token::Index::Num(self.idx as _, wast::token::Span::from_offset(pos))
    }

    pub fn wasm_get(&self, pos: usize) -> wast::core::Instruction<'static> {
        wast::core::Instruction::LocalGet(self.to_wasm_index(pos))
    }

    pub fn wasm_set(&self, pos: usize) -> wast::core::Instruction<'static> {
        wast::core::Instruction::LocalSet(self.to_wasm_index(pos))
    }

    pub fn wasm_tee(&self, pos: usize) -> wast::core::Instruction<'static> {
        wast::core::Instruction::LocalTee(self.to_wasm_index(pos))
    }

    fn make_ref(&self) -> LocalRef {
        let locals = self.ctx.locals.borrow();

        locals[self.idx].make_ref()
    }
}

impl Drop for LocalId<'_, '_> {
    fn drop(&mut self) {
        let locals = &mut *self.ctx.locals.borrow_mut();
        let def = &mut locals[self.idx];

        def.ref_count -= 1;

        if def.ref_count == 0 {
            def.gen += 1;
        }

        if let Some(binding_idx) = self.binding_idx {
            let mut bindings = self.ctx.bindings.borrow_mut();
            let Some((_, bindings)) = bindings.get_index_mut(binding_idx) else { return };
            assert!(bindings.shift_remove(&def.make_ref()));
        }
    }
}

impl<'buf> LocalCtx<'buf> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn bind(
        &self,
        name: Option<Cow<'buf, [u8]>>,
        ty_kind: impl Into<TyKind>,
    ) -> LocalId<'_, 'buf> {
        let mut id = self.allocate(ty_kind.into());

        if let Some(name) = name {
            let mut bindings = self.bindings.borrow_mut();
            let entry = bindings.entry(name);
            id.binding_idx = Some(entry.index());

            entry.or_default().insert(id.make_ref());
        }

        id
    }

    pub fn get(&self, name: &[u8]) -> Option<LocalId<'_, 'buf>> {
        let (local_ref, binding_idx) = {
            let bindings = self.bindings.borrow();
            let (binding_idx, _, bindings) = bindings.get_full(name)?;

            (*bindings.last()?, binding_idx)
        };
        let mut locals = self.locals.borrow_mut();
        let def = locals.get_mut(local_ref.idx).unwrap();

        if def.is_ref_valid(&local_ref) {
            Some(def.make_id(self, Some(binding_idx)))
        } else {
            self.bindings.borrow_mut().remove(name);

            None
        }
    }

    fn allocate(&self, ty_kind: TyKind) -> LocalId<'_, 'buf> {
        let locals = &mut *self.locals.borrow_mut();
        let def = locals
            .iter_mut()
            .find(|local| local.ref_count == 0 && local.ty_kind == ty_kind);

        match def {
            Some(def) => def.make_id(self, None),

            None => {
                let idx = locals.len();

                locals.push(LocalDef {
                    idx,
                    gen: 0,
                    ty_kind,
                    ref_count: 0,
                    _marker: Default::default(),
                });

                locals[idx].make_id(self, None)
            }
        }
    }
}

impl IntoIterator for LocalCtx<'_> {
    type Item = TyKind;
    type IntoIter = LocalCtxIter;

    fn into_iter(self) -> Self::IntoIter {
        LocalCtxIter(self.locals.into_inner().into_iter())
    }
}

pub struct LocalCtxIter(<Vec<LocalDef> as IntoIterator>::IntoIter);

impl Iterator for LocalCtxIter {
    type Item = TyKind;

    fn next(&mut self) -> Option<TyKind> {
        self.0.next().map(|def| def.ty_kind)
    }
}
