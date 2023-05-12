use crate::ast;
use crate::ir::instr::{CallRef, FieldGet, FieldSet, InstrKind, MethodLookup, TermInstrKind};
use crate::ir::ty::{object_ty, DynTy, IrTy};

use super::expr::lower_expr;
use super::{GlobalCtx, LoweringCtx};

pub fn generate_new<'a>(gctx: &mut GlobalCtx<'a>, class: &ast::Class<'_>) {
    let class_name = gctx.arena.alloc(class.name.0.as_slice());
    let method_name = gctx.ty_registry.new_method;
    let func_name = gctx.ty_registry[class_name].name.func_name(method_name);
    let func = gctx.func_registry.add_local_func(func_name);
    let entry_bb = func.add_diverging_bb();
    let mut ctx = LoweringCtx::new(gctx, class_name, func_name, entry_bb);

    ctx.seal_block(entry_bb);
    let ret = ctx.emit(InstrKind::New(class_name), IrTy::new_known(class_name));
    let ret = ctx.coerce(ret, IrTy::new_known(class_name));
    ctx.terminate(TermInstrKind::Return(ret));
}

pub fn generate_init<'a>(gctx: &mut GlobalCtx<'a>, class: &ast::Class<'_>) {
    let class_name = gctx.arena.alloc(class.name.0.as_slice());
    let method_name = gctx.ty_registry.init_method;
    let func_name = gctx.ty_registry[class_name].name.func_name(method_name);
    let func = gctx.func_registry.add_local_func(func_name);
    let entry_bb = func.add_diverging_bb();
    func.append_param(
        entry_bb,
        object_ty!((gctx.ty_registry.object_class)[? <: class_name]),
    );
    let mut ctx = LoweringCtx::new(gctx, class_name, func_name, entry_bb);

    ctx.seal_block(entry_bb);

    if let Some(parent_class) = ctx.gctx.ty_registry[class_name].parent {
        // call parent's (init)
        let func_ref_ty = ctx.gctx.ty_registry[class_name].methods[&method_name]
            .ty
            .clone()
            .bind(Some((ctx.self_param(), ctx.self_param_dyn_ty())));
        let parent_init_ref = ctx.emit(
            MethodLookup {
                class: parent_class,
                method: method_name,
            }
            .into(),
            func_ref_ty.into(),
        );
        ctx.emit(
            CallRef::new(parent_init_ref, &[ctx.self_param()]).into(),
            IrTy::Unit,
        );
    }

    // initialize fields defined in this class
    for feature in &class.features {
        let ast::Feature::Field(ast::Field(binding)) = feature else { continue };
        let field_ty = ctx
            .get_binding(binding.binding_id.unwrap())
            .ty
            .clone()
            .bind(ctx.self_param(), ctx.self_param_dyn_ty());
        let field_class = field_ty.get_class().unwrap();
        let field_name = ctx.gctx.arena.alloc(binding.name.as_slice());

        let init = match binding.init {
            Some(ref expr) => {
                let value = lower_expr(&mut ctx, expr);
                ctx.coerce(value, field_ty)
            }

            None => ctx.default_init(field_class),
        };

        ctx.emit(
            FieldSet::new(ctx.self_param(), field_name, init).into(),
            IrTy::Unit,
        );
    }

    let ret = ctx.emit(InstrKind::Unit, IrTy::Unit);
    ctx.terminate(TermInstrKind::Return(ret));
}

pub fn generate_copy<'a>(gctx: &mut GlobalCtx<'a>, class: &ast::Class<'_>) {
    let class_name = gctx.arena.alloc(class.name.0.as_slice());
    let method_name = gctx.ty_registry.copy_method;
    let func_name = gctx.ty_registry[class_name].name.func_name(method_name);
    let func = gctx.func_registry.add_local_func(func_name);
    let entry_bb = func.add_diverging_bb();
    func.append_param(
        entry_bb,
        object_ty!((gctx.ty_registry.object_class)[? <: class_name]),
    );
    let mut ctx = LoweringCtx::new(gctx, class_name, func_name, entry_bb);

    ctx.seal_block(entry_bb);

    let clone = ctx.emit(InstrKind::New(class_name), IrTy::new_known(class_name));
    let fields = ctx.gctx.ty_registry[class_name]
        .fields
        .keys()
        .copied()
        .collect::<Vec<_>>();

    for field_name in fields {
        let field_ty = ctx.gctx.ty_registry[class_name].fields[&field_name]
            .ty
            .clone()
            .bind(ctx.self_param(), ctx.self_param_dyn_ty());
        let field_value = ctx.emit(
            FieldGet {
                obj: ctx.self_param(),
                field: field_name,
            }
            .into(),
            field_ty,
        );
        ctx.emit(
            FieldSet::new(clone, field_name, field_value).into(),
            IrTy::Unit,
        );
    }

    let clone = ctx.coerce(
        clone,
        IrTy::Object(ctx.gctx.ty_registry.object_class, DynTy::Unknown),
    );
    ctx.terminate(TermInstrKind::Return(clone));
}

pub fn generate_type_name<'a>(gctx: &mut GlobalCtx<'a>, class: &ast::Class<'_>) {
    let class_name = gctx.arena.alloc(class.name.0.as_slice());
    let method_name = gctx.ty_registry.type_name_method;
    let func_name = gctx.ty_registry[class_name].name.func_name(method_name);
    let func = gctx.func_registry.add_local_func(func_name);
    let entry_bb = func.add_diverging_bb();
    let mut ctx = LoweringCtx::new(gctx, class_name, func_name, entry_bb);

    ctx.seal_block(entry_bb);
    let name = ctx.emit(InstrKind::Bytes(ctx.gctx.arena.alloc(class_name.as_slice())), IrTy::Bytes);
    ctx.terminate(TermInstrKind::Return(name));
}
