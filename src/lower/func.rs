use crate::ast::ty::UnwrapResolvedTy;
use crate::ast::{Formal, Method};
use crate::ir::bb::BlockData;
use crate::ir::func::{Func, FuncData, FuncName};
use crate::ir::instr::TermInstrKind;
use crate::ir::ty::IrClass;
use crate::lower::expr::lower_expr;
use crate::lower::ssa::Scope;

use super::{GlobalCtx, LoweringCtx};

fn lower_params<'a, 'buf>(ctx: &mut LoweringCtx<'a, 'buf>, method: &Method<'buf>) {
    for Formal { name, ty, .. } in &method.params {
        let param = ctx.func.borrow_mut().append_param(ctx.current_bb, ctx.gctx.lower_ty(&ty.unwrap_res_ty()));
        ctx.scope.bind(ctx.current_bb, name.0.value.clone(), param);
    }
}

pub fn lower_method<'a, 'buf>(
    gctx: &'a mut GlobalCtx<'a>,
    class: IrClass<'a>,
    method: &Method<'buf>,
) -> Func<'a> {
    let name = FuncName::new(gctx.arena, method.name.as_slice());
    let func = gctx.arena.alloc_func(FuncData::new(name));
    let terminator = func.borrow_mut().add_term_instr(TermInstrKind::Diverge);
    let entry_bb = func.borrow_mut().add_bb(BlockData::new(terminator));
    let mut ctx = LoweringCtx {
        gctx,
        class,
        func,
        current_bb: entry_bb,
        scope: Scope::new(),
    };
    lower_params(&mut ctx, method);
    let ret = lower_expr(&mut ctx, &method.body);

    todo!("terminate ctx.current_bb with Return(Some(ret))")
}
