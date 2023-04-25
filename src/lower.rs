//! AST-to-IR lowering.

use crate::ir::bb::Block;
use crate::ir::func::Func;
use crate::ir::mem::ArenaRef;
use crate::ir::ty::{IrTyRegistry, IrClass};

use self::ssa::Scope;

pub mod expr;
pub mod func;
pub mod ssa;
pub mod ty;

pub struct GlobalCtx<'a> {
    pub arena: ArenaRef<'a>,
    pub ty_registry: IrTyRegistry<'a>,
}

pub struct LoweringCtx<'a, 'buf> {
    pub gctx: &'a mut GlobalCtx<'a>,
    pub class: IrClass<'a>,
    pub func: Func<'a>,
    pub current_bb: Block,
    pub scope: Scope<'buf>,
}
