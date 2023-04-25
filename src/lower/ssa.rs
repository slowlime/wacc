use std::borrow::Cow;
use std::collections::HashMap;

use slotmap::SecondaryMap;

use crate::ir::bb::Block;
use crate::ir::value::Value;

type Bindings<'buf> = HashMap<Cow<'buf, [u8]>, Value>;

#[derive(Debug, Clone, Default)]
pub struct Scope<'buf> {
    bbs: SecondaryMap<Block, Bindings<'buf>>,
}

impl<'buf> Scope<'buf> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_local_binding(&self, bb: Block, name: &[u8]) -> Option<Value> {
        self.bbs.get(bb)?.get(name).copied()
    }

    pub fn bind(&mut self, bb: Block, name: Cow<'buf, [u8]>, value: Value) {
        let bindings = self.bbs.entry(bb).unwrap().or_default();
        bindings.insert(name, value);
    }
}
