use crate::analysis::{ClassName, TypeckResult};
use crate::ast;
use crate::ast::ty::ResolvedTy;
use crate::ir::func::FuncRegistry;
use crate::ir::mem::ArenaRef;
use crate::ir::ty::{DynTy, IrClassBuilder, IrClassName, IrFuncRef, IrTy, IrTyRegistry, MaybeSelfTy};

use super::bindings::Bindings;
use super::GlobalCtx;

pub(super) fn lower_object_ty<'a, 'b, 'buf>(
    arena: ArenaRef<'a>,
    ty_registry: &'b IrTyRegistry<'a>,
    class: IrClassName<'a>,
    ty: &'b ResolvedTy<'buf>,
) -> MaybeSelfTy<'a> {
    match ty {
        ResolvedTy::Builtin(builtin) => {
            IrTy::Object(ty_registry.get_builtin_class(*builtin).name, DynTy::Unknown).into()
        }
        ResolvedTy::SelfType { .. } => MaybeSelfTy::SelfTy(class),
        ResolvedTy::Class(name) => {
            IrTy::Object(ty_registry[arena.alloc(name)].name, DynTy::Unknown).into()
        }
        ResolvedTy::Function(_) => unreachable!(),
        ResolvedTy::Bottom => unreachable!(),
        ResolvedTy::Untyped => unreachable!(),
    }
}

fn lower_method_ty<'a, 'b, 'buf>(
    arena: ArenaRef<'a>,
    ty_registry: &IrTyRegistry<'a>,
    class: IrClassName<'a>,
    func_ty: &'b ast::ty::FunctionTy<'buf>,
) -> IrFuncRef<'a> {
    let mut args = vec![IrTy::Object(class, DynTy::SubtypeOf(class))];

    for arg in &func_ty.params {
        args.push(match lower_object_ty(arena, ty_registry, class, arg) {
            MaybeSelfTy::Other(ty) => ty,
            _ => unreachable!(),
        });
    }

    let ret = Box::new(lower_object_ty(arena, ty_registry, class, &func_ty.ret));

    IrFuncRef::new(args, ret)
}

fn make_global_ctx<'a>(arena: ArenaRef<'a>, typeck_result: TypeckResult<'_>) -> GlobalCtx<'a> {
    let mut ty_registry = IrTyRegistry::new(arena);

    for class_name in typeck_result.sorted {
        let class = match class_name {
            ClassName::Builtin(_) => continue,
            ClassName::Named(ref name) => arena.alloc(name),
            ClassName::SelfType => unreachable!(),
        };
        let class_index = typeck_result.ctx.get_class(&class_name).unwrap();
        let parent = class_index
            .parent()
            .map(|name| ty_registry.get_class_by_class_name(name).unwrap());

        let mut builder = IrClassBuilder::new(class, parent, false);

        for (name, _location, func_ty) in class_index.methods() {
            let name = arena.alloc(name);
            let ty = match builder.get_method(name) {
                Some(method) => method.ty.clone(),
                None => lower_method_ty(arena, &ty_registry, class, func_ty),
            };

            builder.add_method(name, ty);
        }

        for (name, _location, ty) in class_index.fields() {
            let name = arena.alloc(name);
            let ty = lower_object_ty(arena, &ty_registry, class, ty);
            builder.add_field(name, ty);
        }

        builder.register(&mut ty_registry);
    }

    let func_registry = FuncRegistry::new();
    let bindings = Bindings::new(arena, &mut ty_registry, typeck_result.bindings);

    GlobalCtx {
        arena,
        ty_registry,
        func_registry,
        bindings,
    }
}
