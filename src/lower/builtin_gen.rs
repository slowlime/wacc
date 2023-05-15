//! Built-in method generation.

use crate::ir::instr::{
    BlockJump, Branch, BytesGet, BytesSet, Call, CallRef, InstrKind, TermInstrKind, VTableLookup,
};
use crate::ir::ty::{object_ty, DynTy, IrClassName, IrTy};

use super::method_gen::{
    create_func, generate_copy, generate_new, generate_type_name, CreateFuncResult,
};
use super::GlobalCtx;

pub fn lower_object<'a>(gctx: &mut GlobalCtx<'a>) {
    let class_name = gctx.ty_registry.object_class;
    generate_new(gctx, class_name);
    generate_unit_init(gctx, class_name);
    generate_copy(gctx, class_name);
    generate_type_name(gctx, class_name);
    generate_object_abort(gctx);
    generate_object_type_name(gctx);
    generate_object_copy(gctx);
}

pub fn lower_int<'a>(gctx: &mut GlobalCtx<'a>) {
    let class_name = gctx.ty_registry.int_class;
    generate_int_new(gctx);
    generate_unit_init(gctx, class_name);
    generate_int_copy(gctx);
    generate_type_name(gctx, class_name);
}

pub fn lower_string<'a>(gctx: &mut GlobalCtx<'a>) {
    let class_name = gctx.ty_registry.string_class;
    generate_string_new(gctx);
    generate_unit_init(gctx, class_name);
    generate_string_copy(gctx);
    generate_type_name(gctx, class_name);
    generate_string_length(gctx);
    generate_string_concat(gctx);
    generate_string_substr(gctx);
}

pub fn lower_bool<'a>(gctx: &mut GlobalCtx<'a>) {
    let class_name = gctx.ty_registry.bool_class;
    generate_bool_new(gctx);
    generate_unit_init(gctx, class_name);
    generate_bool_copy(gctx);
    generate_type_name(gctx, class_name);
}

pub fn lower_io<'a>(gctx: &mut GlobalCtx<'a>) {
    let class_name = gctx.ty_registry.io_class;
    generate_new(gctx, class_name);
    generate_unit_init(gctx, class_name);
    generate_copy(gctx, class_name);
    generate_type_name(gctx, class_name);
    generate_io_out_string(gctx);
    generate_io_out_int(gctx);
    generate_io_in_string(gctx);
    generate_io_in_int(gctx);
}

fn generate_unit_init<'a>(gctx: &mut GlobalCtx<'a>, class_name: IrClassName<'a>) {
    let method_name = gctx.ty_registry.init_method;
    let CreateFuncResult {
        entry_bb, mut ctx, ..
    } = create_func(gctx, class_name, method_name);
    let object_class = ctx.gctx.ty_registry.object_class;
    ctx.func_mut()
        .append_param(entry_bb, object_ty!((object_class)[? <: class_name]));

    let unit = ctx.emit(InstrKind::Unit, IrTy::Unit);
    ctx.terminate(TermInstrKind::Return(unit));
}

fn generate_int_new<'a>(gctx: &mut GlobalCtx<'a>) {
    let class_name = gctx.ty_registry.int_class;
    let method_name = gctx.ty_registry.new_method;
    let CreateFuncResult { mut ctx, .. } = create_func(gctx, class_name, method_name);

    let ret = ctx.emit(InstrKind::I32(0), IrTy::I32);
    let ret = ctx.coerce(
        ret,
        IrTy::Object(ctx.gctx.ty_registry.object_class, DynTy::Known(class_name)),
    );
    ctx.terminate(TermInstrKind::Return(ret));
}

fn generate_int_copy<'a>(gctx: &mut GlobalCtx<'a>) {
    let class_name = gctx.ty_registry.int_class;
    let method_name = gctx.ty_registry.copy_method;
    let CreateFuncResult {
        entry_bb, mut ctx, ..
    } = create_func(gctx, class_name, method_name);
    let object_class = ctx.gctx.ty_registry.object_class;
    let self_param = ctx
        .func_mut()
        .append_param(entry_bb, object_ty!((object_class)[? <: class_name]));

    // This means copied ints have the same address
    // (observable via match-casting to `Object` and using `=` on that).
    // The spec is unclear on whether this is allowed.
    ctx.terminate(TermInstrKind::Return(self_param));
}

fn generate_string_new<'a>(gctx: &mut GlobalCtx<'a>) {
    let class_name = gctx.ty_registry.string_class;
    let method_name = gctx.ty_registry.new_method;
    let CreateFuncResult { mut ctx, .. } = create_func(gctx, class_name, method_name);

    let ret = ctx.emit(InstrKind::Bytes(ctx.gctx.arena.alloc(b"")), IrTy::Bytes);
    let ret = ctx.coerce(
        ret,
        IrTy::Object(ctx.gctx.ty_registry.object_class, DynTy::Known(class_name)),
    );
    ctx.terminate(TermInstrKind::Return(ret));
}

fn generate_string_copy<'a>(gctx: &mut GlobalCtx<'a>) {
    let class_name = gctx.ty_registry.string_class;
    let method_name = gctx.ty_registry.copy_method;
    let CreateFuncResult {
        entry_bb, mut ctx, ..
    } = create_func(gctx, class_name, method_name);
    let object_class = ctx.gctx.ty_registry.object_class;
    let self_param = ctx
        .func_mut()
        .append_param(entry_bb, object_ty!((object_class)[? <: class_name]));

    // see the comment in generate_int_copy
    ctx.terminate(TermInstrKind::Return(self_param));
}

fn generate_bool_new<'a>(gctx: &mut GlobalCtx<'a>) {
    let class_name = gctx.ty_registry.bool_class;
    let method_name = gctx.ty_registry.new_method;
    let CreateFuncResult { mut ctx, .. } = create_func(gctx, class_name, method_name);

    let ret = ctx.emit(InstrKind::Bool(false), IrTy::Bool);
    let ret = ctx.coerce(
        ret,
        IrTy::Object(ctx.gctx.ty_registry.object_class, DynTy::Known(class_name)),
    );
    ctx.terminate(TermInstrKind::Return(ret));
}

fn generate_bool_copy<'a>(gctx: &mut GlobalCtx<'a>) {
    let class_name = gctx.ty_registry.bool_class;
    let method_name = gctx.ty_registry.copy_method;
    let CreateFuncResult {
        entry_bb, mut ctx, ..
    } = create_func(gctx, class_name, method_name);
    let object_class = ctx.gctx.ty_registry.object_class;
    let self_param = ctx
        .func_mut()
        .append_param(entry_bb, object_ty!((object_class)[? <: class_name]));

    // see the comment in generate_int_copy
    ctx.terminate(TermInstrKind::Return(self_param));
}

fn generate_object_abort<'a>(gctx: &mut GlobalCtx<'a>) {
    let class_name = gctx.ty_registry.object_class;
    let method_name = gctx.arena.alloc(b"abort");
    let CreateFuncResult {
        entry_bb, mut ctx, ..
    } = create_func(gctx, class_name, method_name);
    let object_class = ctx.gctx.ty_registry.object_class;
    ctx.func_mut()
        .append_param(entry_bb, object_ty!((object_class)[? <: class_name]));

    ctx.emit(
        Call {
            func: ctx.gctx.func_registry.abort_func,
            args: vec![],
        }
        .into(),
        IrTy::Unit,
    );
    ctx.terminate(TermInstrKind::Diverge);
}

fn generate_object_type_name<'a>(gctx: &mut GlobalCtx<'a>) {
    let class_name = gctx.ty_registry.object_class;
    let method_name = gctx.arena.alloc(b"type_name");
    let CreateFuncResult {
        entry_bb, mut ctx, ..
    } = create_func(gctx, class_name, method_name);

    let object_class = ctx.gctx.ty_registry.object_class;
    ctx.func_mut()
        .append_param(entry_bb, object_ty!((object_class)[? <: class_name]));

    let type_name_method_ty = ctx.gctx.ty_registry.type_name_method_ty().bind(None);
    let func_ref = ctx.emit(
        VTableLookup {
            obj: ctx.self_param(),
            class: class_name,
            method_name: ctx.gctx.ty_registry.type_name_method,
        }
        .into(),
        type_name_method_ty.into(),
    );
    let type_name = ctx.emit(CallRef::new(func_ref, &[]).into(), IrTy::Bytes);
    let ret = ctx.coerce(
        type_name,
        IrTy::new_known(ctx.gctx.ty_registry.string_class),
    );

    ctx.terminate(TermInstrKind::Return(ret));
}

fn generate_object_copy<'a>(gctx: &mut GlobalCtx<'a>) {
    let class_name = gctx.ty_registry.object_class;
    let method_name = gctx.arena.alloc(b"copy");
    let CreateFuncResult {
        entry_bb, mut ctx, ..
    } = create_func(gctx, class_name, method_name);
    let object_class = ctx.gctx.ty_registry.object_class;
    ctx.func_mut()
        .append_param(entry_bb, object_ty!((object_class)[? <: class_name]));

    let copy_method_ty = ctx
        .gctx
        .ty_registry
        .copy_method_ty(class_name)
        .bind(Some((ctx.self_param(), ctx.self_param_dyn_ty())));
    let func_ref = ctx.emit(
        VTableLookup {
            obj: ctx.self_param(),
            class: class_name,
            method_name: ctx.gctx.ty_registry.copy_method,
        }
        .into(),
        copy_method_ty.into(),
    );
    let clone = ctx.emit(
        CallRef::new(func_ref, &[ctx.self_param()]).into(),
        IrTy::Object(
            class_name,
            DynTy::Unknown.bind(ctx.self_param(), ctx.self_param_dyn_ty()),
        ),
    );

    ctx.terminate(TermInstrKind::Return(clone));
}

fn generate_string_length<'a>(gctx: &mut GlobalCtx<'a>) {
    let class_name = gctx.ty_registry.string_class;
    let method_name = gctx.arena.alloc(b"length");
    let CreateFuncResult {
        entry_bb, mut ctx, ..
    } = create_func(gctx, class_name, method_name);
    let self_param = ctx
        .func_mut()
        .append_param(entry_bb, object_ty!(class_name));

    let self_bytes = ctx.coerce(self_param, IrTy::Bytes);
    let len = ctx.emit(InstrKind::BytesLen(self_bytes), IrTy::I32);
    let ret = ctx.coerce(len, IrTy::new_known(ctx.gctx.ty_registry.int_class));
    ctx.terminate(TermInstrKind::Return(ret));
}

fn generate_string_concat<'a>(gctx: &mut GlobalCtx<'a>) {
    let class_name = gctx.ty_registry.string_class;
    let method_name = gctx.arena.alloc(b"concat");
    let CreateFuncResult {
        entry_bb, mut ctx, ..
    } = create_func(gctx, class_name, method_name);
    let self_param = ctx
        .func_mut()
        .append_param(entry_bb, object_ty!(class_name));
    let other_param = ctx
        .func_mut()
        .append_param(entry_bb, object_ty!(class_name));

    let self_bytes = ctx.coerce(self_param, IrTy::Bytes);
    let other_bytes = ctx.coerce(other_param, IrTy::Bytes);

    let self_len = ctx.emit(InstrKind::BytesLen(self_bytes), IrTy::I32);
    let other_len = ctx.emit(InstrKind::BytesLen(other_bytes), IrTy::I32);
    let result_len = ctx.emit(InstrKind::Add(self_len, other_len), IrTy::I32);
    let result_bytes = ctx.emit(InstrKind::BytesNew(result_len), IrTy::Bytes);
    let zero = ctx.emit(InstrKind::I32(0), IrTy::I32);

    let copy_self_bb = ctx.func_mut().add_diverging_bb();
    let self_idx = ctx.func_mut().append_param(copy_self_bb, IrTy::I32);
    ctx.terminate(
        BlockJump {
            bb: copy_self_bb,
            args: vec![zero],
        }
        .into(),
    );

    ctx.select_bb(copy_self_bb);
    let self_idx_cond = ctx.emit(InstrKind::Ge(self_idx, self_len), IrTy::Bool);

    let copy_other_bb = ctx.func_mut().add_diverging_bb();
    let other_idx = ctx.func_mut().append_param(copy_other_bb, IrTy::I32);
    let copy_self_body_bb = ctx.func_mut().add_diverging_bb();
    ctx.terminate(
        Branch::new(
            self_idx_cond,
            BlockJump {
                bb: copy_other_bb,
                args: vec![self_idx],
            },
            BlockJump {
                bb: copy_self_body_bb,
                args: vec![],
            },
        )
        .into(),
    );

    ctx.seal_block(copy_self_body_bb);
    ctx.select_bb(copy_self_body_bb);
    let byte = ctx.emit(BytesGet::new(self_bytes, self_idx).into(), IrTy::I32);
    ctx.emit(
        BytesSet::new(result_bytes, self_idx, byte).into(),
        IrTy::Unit,
    );
    let one = ctx.emit(InstrKind::I32(1), IrTy::I32);
    let self_new_idx = ctx.emit(InstrKind::Add(self_idx, one), IrTy::I32);
    ctx.terminate(
        BlockJump {
            bb: copy_self_bb,
            args: vec![self_new_idx],
        }
        .into(),
    );
    ctx.seal_block(copy_self_bb);

    ctx.select_bb(copy_other_bb);
    let other_idx_cond = ctx.emit(InstrKind::Ge(other_idx, result_len), IrTy::Bool);

    let copy_other_body_bb = ctx.func_mut().add_diverging_bb();
    let out_bb = ctx.func_mut().add_diverging_bb();
    ctx.terminate(
        Branch::new(
            other_idx_cond,
            BlockJump {
                bb: out_bb,
                args: vec![],
            },
            BlockJump {
                bb: copy_other_body_bb,
                args: vec![],
            },
        )
        .into(),
    );

    ctx.seal_block(copy_other_body_bb);
    ctx.seal_block(out_bb);

    ctx.select_bb(copy_other_body_bb);
    let other_byte_idx = ctx.emit(InstrKind::Sub(other_idx, self_len), IrTy::I32);
    let byte = ctx.emit(BytesGet::new(other_bytes, other_byte_idx).into(), IrTy::I32);
    ctx.emit(
        BytesSet::new(result_bytes, other_idx, byte).into(),
        IrTy::Unit,
    );
    let one = ctx.emit(InstrKind::I32(1), IrTy::I32);
    let other_new_idx = ctx.emit(InstrKind::Add(other_idx, one), IrTy::I32);
    ctx.terminate(
        BlockJump {
            bb: copy_other_bb,
            args: vec![other_new_idx],
        }
        .into(),
    );

    ctx.select_bb(out_bb);
    let ret = ctx.coerce(result_bytes, IrTy::new_known(class_name));
    ctx.terminate(TermInstrKind::Return(ret).into());
}

fn generate_string_substr<'a>(gctx: &mut GlobalCtx<'a>) {
    let class_name = gctx.ty_registry.string_class;
    let method_name = gctx.arena.alloc(b"substr");
    let CreateFuncResult {
        entry_bb, mut ctx, ..
    } = create_func(gctx, class_name, method_name);
    let self_param = ctx
        .func_mut()
        .append_param(entry_bb, IrTy::new_known(class_name));
    let int_class = ctx.gctx.ty_registry.int_class;
    let start_idx_param = ctx
        .func_mut()
        .append_param(entry_bb, IrTy::new_known(int_class));
    let len_param = ctx
        .func_mut()
        .append_param(entry_bb, IrTy::new_known(int_class));

    let self_bytes = ctx.coerce(self_param, IrTy::Bytes);
    let start_idx = ctx.coerce(start_idx_param, IrTy::I32);
    let len = ctx.coerce(len_param, IrTy::I32);
    let zero = ctx.emit(InstrKind::I32(0), IrTy::I32);
    let start_lt_zero = ctx.emit(InstrKind::Lt(start_idx, zero), IrTy::Bool);

    let out_of_range_bb = ctx.func_mut().add_diverging_bb();
    let check_length_bb = ctx.func_mut().add_diverging_bb();

    ctx.terminate(
        Branch::new(
            start_lt_zero,
            BlockJump {
                bb: out_of_range_bb,
                args: vec![],
            },
            BlockJump {
                bb: check_length_bb,
                args: vec![],
            },
        )
        .into(),
    );

    ctx.seal_block(check_length_bb);
    ctx.select_bb(check_length_bb);

    let zero = ctx.emit(InstrKind::I32(0), IrTy::I32);
    let len_lt_zero = ctx.emit(InstrKind::Lt(len, zero), IrTy::Bool);

    let check_end_bb = ctx.func_mut().add_diverging_bb();

    ctx.terminate(
        Branch::new(
            len_lt_zero,
            BlockJump {
                bb: out_of_range_bb,
                args: vec![],
            },
            BlockJump {
                bb: check_end_bb,
                args: vec![],
            },
        )
        .into(),
    );

    ctx.seal_block(check_end_bb);
    ctx.select_bb(check_end_bb);

    let end_idx = ctx.emit(InstrKind::Add(start_idx, len), IrTy::I32);
    let self_len = ctx.emit(InstrKind::BytesLen(self_bytes), IrTy::I32);
    let end_gt_len = ctx.emit(InstrKind::Gt(end_idx, self_len), IrTy::Bool);

    let substr_bb = ctx.func_mut().add_diverging_bb();

    ctx.terminate(
        Branch::new(
            end_gt_len,
            BlockJump {
                bb: out_of_range_bb,
                args: vec![],
            },
            BlockJump {
                bb: substr_bb,
                args: vec![],
            },
        )
        .into(),
    );

    ctx.seal_block(substr_bb);
    ctx.select_bb(substr_bb);

    let result_bytes = ctx.emit(InstrKind::BytesNew(len), IrTy::Bytes);
    let zero = ctx.emit(InstrKind::I32(0), IrTy::I32);

    let copy_bb = ctx.func_mut().add_diverging_bb();
    let idx = ctx.func_mut().append_param(copy_bb, IrTy::I32);

    ctx.terminate(
        BlockJump {
            bb: copy_bb,
            args: vec![zero],
        }
        .into(),
    );

    ctx.select_bb(copy_bb);

    let idx_ge_len = ctx.emit(InstrKind::Ge(idx, len), IrTy::Bool);

    let out_bb = ctx.func_mut().add_diverging_bb();
    let copy_body_bb = ctx.func_mut().add_diverging_bb();

    ctx.terminate(
        Branch::new(
            idx_ge_len,
            BlockJump {
                bb: out_bb,
                args: vec![],
            },
            BlockJump {
                bb: copy_body_bb,
                args: vec![],
            },
        )
        .into(),
    );

    ctx.seal_block(copy_body_bb);
    ctx.seal_block(out_bb);

    ctx.select_bb(copy_body_bb);

    let self_idx = ctx.emit(InstrKind::Add(start_idx, idx), IrTy::I32);
    let byte = ctx.emit(BytesGet::new(self_bytes, self_idx).into(), IrTy::I32);
    ctx.emit(BytesSet::new(result_bytes, idx, byte).into(), IrTy::Unit);
    let one = ctx.emit(InstrKind::I32(1), IrTy::I32);
    let new_idx = ctx.emit(InstrKind::Add(idx, one), IrTy::I32);

    ctx.terminate(
        BlockJump {
            bb: copy_bb,
            args: vec![new_idx],
        }
        .into(),
    );

    ctx.seal_block(copy_bb);
    ctx.select_bb(out_bb);

    let ret = ctx.coerce(result_bytes, IrTy::new_known(class_name));
    ctx.terminate(TermInstrKind::Return(ret));
}

fn generate_io_out_string<'a>(gctx: &mut GlobalCtx<'a>) {
    let class_name = gctx.ty_registry.io_class;
    let method_name = gctx.arena.alloc(b"out_string");
    let CreateFuncResult {
        entry_bb, mut ctx, ..
    } = create_func(gctx, class_name, method_name);
    let self_param = ctx
        .func_mut()
        .append_param(entry_bb, object_ty!((class_name)[? <: class_name]));
    let string_class = ctx.gctx.ty_registry.string_class;
    let string_param = ctx
        .func_mut()
        .append_param(entry_bb, object_ty!(string_class));

    let bytes = ctx.coerce(string_param, IrTy::Bytes);
    ctx.emit(
        Call {
            func: ctx.gctx.func_registry.print_bytes_func,
            args: vec![bytes],
        }
        .into(),
        IrTy::Unit,
    );

    ctx.terminate(TermInstrKind::Return(self_param));
}

fn generate_io_out_int<'a>(gctx: &mut GlobalCtx<'a>) {
    let class_name = gctx.ty_registry.io_class;
    let method_name = gctx.arena.alloc(b"out_int");
    let CreateFuncResult {
        entry_bb, mut ctx, ..
    } = create_func(gctx, class_name, method_name);
    let self_param = ctx
        .func_mut()
        .append_param(entry_bb, object_ty!((class_name)[? <: class_name]));
    let int_class = ctx.gctx.ty_registry.int_class;
    let int_param = ctx.func_mut().append_param(entry_bb, object_ty!(int_class));

    let int = ctx.coerce(int_param, IrTy::I32);
    ctx.emit(
        Call {
            func: ctx.gctx.func_registry.print_int_func,
            args: vec![int],
        }
        .into(),
        IrTy::Unit,
    );

    ctx.terminate(TermInstrKind::Return(self_param));
}

fn generate_io_in_string<'a>(gctx: &mut GlobalCtx<'a>) {
    let class_name = gctx.ty_registry.io_class;
    let method_name = gctx.arena.alloc(b"in_string");
    let CreateFuncResult {
        entry_bb, mut ctx, ..
    } = create_func(gctx, class_name, method_name);
    ctx.func_mut()
        .append_param(entry_bb, object_ty!((class_name)[? <: class_name]));

    let bytes = ctx.emit(
        Call {
            func: ctx.gctx.func_registry.read_line_func,
            args: vec![],
        }
        .into(),
        IrTy::Bytes,
    );
    let ret = ctx.coerce(bytes, IrTy::new_known(ctx.gctx.ty_registry.string_class));

    ctx.terminate(TermInstrKind::Return(ret));
}

fn generate_io_in_int<'a>(gctx: &mut GlobalCtx<'a>) {
    let class_name = gctx.ty_registry.io_class;
    let method_name = gctx.arena.alloc(b"in_int");
    let CreateFuncResult {
        entry_bb, mut ctx, ..
    } = create_func(gctx, class_name, method_name);
    ctx.func_mut()
        .append_param(entry_bb, object_ty!((class_name)[? <: class_name]));

    let int = ctx.emit(
        Call {
            func: ctx.gctx.func_registry.read_int_func,
            args: vec![],
        }
        .into(),
        IrTy::I32,
    );
    let ret = ctx.coerce(int, IrTy::new_known(ctx.gctx.ty_registry.int_class));

    ctx.terminate(TermInstrKind::Return(ret));
}
