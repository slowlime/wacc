use crate::ast::Method;
use crate::ir::bb::BlockData;
use crate::ir::func::Func;
use crate::ir::instr::{MethodName, TermInstrKind};
use crate::ir::ty::IrClassName;
use crate::lower::expr::lower_expr;

use super::{GlobalCtx, LoweringCtx};

fn lower_params<'a>(
    ctx: &mut LoweringCtx<'a, '_>,
    method_name: MethodName<'a>,
    method: &Method<'_>,
) {
    let func_ty = ctx.gctx.ty_registry[ctx.class].methods[&method_name]
        .ty
        .args()
        .to_vec();

    for (i, arg_ty) in func_ty.into_iter().enumerate() {
        let binding_id = match i.checked_sub(1) {
            // `self`
            None => ctx.gctx.bindings.get_self_binding(ctx.class).id,

            Some(idx) => method.params[idx].binding_id.unwrap(),
        };

        let current_bb = ctx.current_bb;
        let param = ctx.func_mut().append_param(current_bb, arg_ty);
        ctx.bind(binding_id, param);
    }
}

pub fn lower_method<'a, 'gctx>(
    gctx: &'gctx mut GlobalCtx<'a>,
    class_name: IrClassName<'a>,
    method: &Method<'_>,
) -> &'gctx mut Func<'a> {
    let class = &gctx.ty_registry[class_name];
    let method_name = gctx.arena.alloc(method.name.as_slice());
    let func_name = class.name.func_name(method_name);
    let func = gctx.func_registry.add_local_func(func_name);
    let terminator = func.add_term_instr(TermInstrKind::Diverge);
    let entry_bb = func.add_bb(BlockData::new(terminator));
    let mut ctx = LoweringCtx::new(gctx, class_name, func_name, entry_bb);
    lower_params(&mut ctx, method_name, method);
    ctx.seal_block(ctx.current_bb);
    let ret = lower_expr(&mut ctx, &method.body);
    let ret_instr = ctx.func_mut().add_term_instr(TermInstrKind::Return(ret));

    let exit_bb = ctx.current_bb;
    ctx.func_mut().terminate_block(exit_bb, ret_instr);

    ctx.into_func_mut()
}
