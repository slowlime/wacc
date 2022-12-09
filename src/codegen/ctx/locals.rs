use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashMap;

use super::TyId;

#[derive(Debug, Clone)]
struct LocalDef {
    idx: usize,
    gen: usize,
    ty_id: TyId,
    allocated: bool,
}

impl LocalDef {
    fn make_ref(&self) -> LocalRef {
        let &Self { idx, gen, .. } = self;

        LocalRef { gen, idx }
    }

    fn is_ref_valid(&self, local_ref: &LocalRef) -> bool {
        assert!(local_ref.gen <= self.gen);

        local_ref.gen == self.gen && local_ref.idx == self.idx && self.allocated
    }
}

#[derive(Debug, Clone, Copy)]
struct LocalRef {
    gen: usize,
    idx: usize,
}

#[derive(Debug, Clone, Default)]
pub struct LocalCtx<'buf> {
    locals: RefCell<Vec<LocalDef>>,
    bindings: RefCell<HashMap<Cow<'buf, [u8]>, LocalRef>>,
}

#[derive(Debug)]
pub struct LocalId<'a, 'buf> {
    ctx: &'a LocalCtx<'buf>,
    idx: usize,
}

impl LocalId<'_, '_> {
    fn make_ref(&self) -> LocalRef {
        let locals = self.ctx.locals.borrow();

        locals[self.idx].make_ref()
    }
}

impl Drop for LocalId<'_, '_> {
    fn drop(&mut self) {
        let locals = &mut *self.ctx.locals.borrow_mut();
        let def = &mut locals[self.idx];
        def.allocated = false;
        def.gen += 1;
    }
}

impl<'buf> LocalCtx<'buf> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn bind(&self, name: Option<Cow<'buf, [u8]>>, ty_id: TyId) -> LocalId<'_, 'buf> {
        let id = self.allocate(ty_id);

        if let Some(name) = name {
            self.bindings.borrow_mut().insert(name, id.make_ref());
        }

        id
    }

    pub fn get(&self, name: &[u8]) -> Option<LocalId<'_, 'buf>> {
        let &local_ref = self.bindings.borrow().get(name)?;
        let locals = self.locals.borrow();
        let def = locals.get(local_ref.idx).unwrap();

        if def.is_ref_valid(&local_ref) {
            Some(LocalId {
                ctx: self,
                idx: def.idx,
            })
        } else {
            self.bindings.borrow_mut().remove(name);

            None
        }
    }

    fn allocate(&self, ty_id: TyId) -> LocalId<'_, 'buf> {
        let locals = &mut *self.locals.borrow_mut();
        let def = locals.iter_mut().find(|local| !local.allocated && local.ty_id == ty_id);

        match def {
            Some(def) => {
                def.allocated = true;

                LocalId {
                    ctx: self,
                    idx: def.idx,
                }
            }

            None => {
                let idx = locals.len();

                locals.push(LocalDef {
                    idx,
                    gen: 0,
                    ty_id,
                    allocated: true,
                });

                LocalId {
                    ctx: self,
                    idx,
                }
            }
        }
    }
}

impl IntoIterator for LocalCtx<'_> {
    type Item = TyId;
    type IntoIter = LocalCtxIter;

    fn into_iter(self) -> Self::IntoIter {
        LocalCtxIter(self.locals.into_inner().into_iter())
    }
}

pub struct LocalCtxIter(<Vec<LocalDef> as IntoIterator>::IntoIter);

impl Iterator for LocalCtxIter {
    type Item = TyId;

    fn next(&mut self) -> Option<TyId> {
        self.0.next().map(|def| def.ty_id)
    }
}
