use std::borrow::Cow;
use std::collections::HashMap;
use std::mem;

use slotmap::SecondaryMap;

use crate::ir::bb::Block;
use crate::ir::func::Func;
use crate::ir::ty::{IrClass, IrTy};
use crate::ir::value::Value;

use super::GlobalCtx;

type Var<'buf> = Cow<'buf, [u8]>;
type Bindings<'buf> = HashMap<Var<'buf>, Value>;

#[derive(Debug, Clone)]
pub enum Sealed<'buf> {
    Yes,

    No { incomplete_vars: Vec<Var<'buf>> },
}

impl<'buf> Default for Sealed<'buf> {
    fn default() -> Self {
        Self::No {
            incomplete_vars: vec![],
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct SsaBlock<'buf> {
    pub bindings: Bindings<'buf>,
    pub sealed: Sealed<'buf>,
}

pub struct LoweringCtx<'a, 'buf> {
    pub gctx: &'a mut GlobalCtx<'a>,
    pub class: IrClass<'a>,
    pub func: Func<'a>,
    pub current_bb: Block,
    pub bbs: SecondaryMap<Block, SsaBlock<'buf>>,
}

fn get_block_mut<'a, 'buf>(
    bbs: &'a mut SecondaryMap<Block, SsaBlock<'buf>>,
    bb: Block,
) -> &'a mut SsaBlock<'buf> {
    bbs.entry(bb).unwrap().or_default()
}

impl<'a, 'buf> LoweringCtx<'a, 'buf> {
    pub fn new(
        gctx: &'a mut GlobalCtx<'a>,
        class: IrClass<'a>,
        func: Func<'a>,
        entry_bb: Block,
    ) -> Self {
        Self {
            gctx,
            class,
            func,
            current_bb: entry_bb,
            bbs: Default::default(),
        }
    }

    pub fn bind(&mut self, name: Var<'buf>, value: Value) {
        self.bind_in(self.current_bb, name, value);
    }

    pub fn bind_in(&mut self, bb: Block, name: Var<'buf>, value: Value) {
        let ssa_data = get_block_mut(&mut self.bbs, bb);
        ssa_data.bindings.insert(name, value);
    }

    pub fn lookup(&mut self, name: Var<'buf>, ty: IrTy<'a>) -> Value {
        self.lookup_in(self.current_bb, name, ty)
    }

    pub fn lookup_local(&self, bb: Block, name: &[u8]) -> Option<Value> {
        self.bbs.get(bb)?.bindings.get(name).copied()
    }

    pub fn lookup_in(&mut self, bb: Block, name: Var<'buf>, ty: IrTy<'a>) -> Value {
        if let Some(value) = self.lookup_local(bb, &name) {
            return value;
        }

        let value;

        if let Sealed::No {
            ref mut incomplete_vars,
        } = get_block_mut(&mut self.bbs, bb).sealed
        {
            // the block may have more predecessors than currently available — defer
            // TODO: use the marker algorithm
            incomplete_vars.push(name.clone());
            value = self.func.borrow_mut().append_param(bb, ty);
        } else {
            let preds = &self.func.borrow().bb_preds[bb];

            if self.func.borrow().bb_preds[bb].len() == 1 {
                // no control-flow-dependent def
                value = self.lookup_in(preds[0], name.clone(), ty);
            } else {
                // the def may be control-flow-dependent
                value = self.func.borrow_mut().append_param(bb, ty);
                self.bind(name.clone(), value);
                self.add_jump_params(bb, name.clone(), value);
            }
        }

        self.bind(name, value);

        value
    }

    fn add_jump_params(&mut self, bb: Block, name: Var<'buf>, param: Value) {
        let param_idx = self.func.borrow().param_def(param).unwrap().idx;

        for pred in bb.preds(self.func) {
            let value = self.lookup_in(
                pred,
                name.clone(),
                self.func.borrow().values[param].ty.clone(),
            );
            let terminator = self.func.borrow().bbs[pred].terminator;

            for jmp in self.func.borrow_mut().term_instrs[terminator].jumps_to_mut(bb) {
                jmp.args.insert(param_idx, value);
            }
        }

        self.optimize_block_param(bb, param);
    }

    fn optimize_block_param(&mut self, bb: Block, param: Value) {
        let mut same_value = None;
        let param_idx = self.func.borrow().param_def(param).unwrap().idx;
        let preds = bb.preds(self.func);

        for &pred in &preds {
            let func = self.func.borrow();

            for jmp in func.term_instrs[pred.terminator(self.func)].jumps_to(bb) {
                let arg = func.resolve_value(jmp.args[param_idx]);

                if same_value == Some(arg) || arg == param {
                    continue;
                }

                if same_value.is_some() {
                    return;
                }

                same_value = Some(arg);
            }
        }

        let Some(same_value) = same_value else {
            panic!("the block parameter is not supplied with any defining value");
        };

        self.func.borrow_mut().replace_with_id(param, same_value);
        // TODO: optimize in users
    }

    pub fn seal_block(&mut self, bb: Block) {
        let incomplete_vars =
            match mem::replace(&mut get_block_mut(&mut self.bbs, bb).sealed, Sealed::Yes) {
                Sealed::Yes => return,
                Sealed::No { incomplete_vars } => incomplete_vars,
            };

        for var in incomplete_vars {
            let param = self.lookup_local(bb, &var).unwrap();
            self.add_jump_params(bb, var, param);
        }
    }
}
