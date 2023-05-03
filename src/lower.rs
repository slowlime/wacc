//! AST-to-IR lowering.

pub mod bindings;
pub mod class;
pub mod expr;
pub mod func;
pub mod ssa;

use crate::ir::func::FuncRegistry;
use crate::ir::mem::ArenaRef;
use crate::ir::ty::IrTyRegistry;

use self::bindings::Bindings;

pub use self::ssa::LoweringCtx;

pub struct GlobalCtx<'a> {
    pub arena: ArenaRef<'a>,
    pub ty_registry: IrTyRegistry<'a>,
    pub func_registry: FuncRegistry<'a>,
    pub bindings: Bindings<'a>,
}
