//! AST-to-IR lowering.

pub mod expr;
pub mod func;
pub mod ssa;
pub mod ty;

use crate::ir::mem::ArenaRef;
use crate::ir::ty::IrTyRegistry;

pub use self::ssa::LoweringCtx;

pub struct GlobalCtx<'a> {
    pub arena: ArenaRef<'a>,
    pub ty_registry: IrTyRegistry<'a>,
}
