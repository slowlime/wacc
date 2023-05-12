//! AST-to-IR lowering.

pub mod bindings;
pub mod class;
pub mod expr;
pub mod func;
mod method_gen;
pub mod ssa;

use crate::ast;
use crate::ir::func::FuncRegistry;
use crate::ir::mem::ArenaRef;
use crate::ir::ty::IrTyRegistry;

use self::bindings::Bindings;

use self::func::lower_method;
use self::method_gen::{generate_init, generate_copy, generate_new, generate_type_name};
pub use self::ssa::LoweringCtx;

pub struct GlobalCtx<'a> {
    pub arena: ArenaRef<'a>,
    pub ty_registry: IrTyRegistry<'a>,
    pub func_registry: FuncRegistry<'a>,
    pub bindings: Bindings<'a>,
}

pub fn lower_class<'a>(gctx: &mut GlobalCtx<'a>, class: &ast::Class<'_>) {
    let class_name = gctx.arena.alloc(class.name.0.as_slice());

    // lower user-defined methods
    for feature in &class.features {
        match feature {
            ast::Feature::Method(method) => {
                lower_method(gctx, class_name, method);
            }

            ast::Feature::Field(_) => {}
        }
    }

    // generate special methods
    generate_new(gctx, class);
    generate_init(gctx, class);
    generate_copy(gctx, class);
    generate_type_name(gctx, class);
}
