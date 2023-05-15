use std::collections::HashMap;
use std::mem;

use slotmap::SecondaryMap;

use crate::analysis::BindingId;
use crate::ast::ty::ResolvedTy;
use crate::ir::bb::Block;
use crate::ir::func::{FullFuncName, Func};
use crate::ir::instr::{Cast, InstrKind, TermInstrKind, TermInstr};
use crate::ir::ty::{DynTy, IrClass, IrClassName, IrTy};
use crate::ir::value::Value;

use super::bindings::Binding;
use super::GlobalCtx;

type Var = BindingId;
type Bindings = HashMap<Var, Value>;

#[derive(Debug, Clone)]
pub enum Sealed {
    Yes,

    No { incomplete_vars: Vec<Var> },
}

impl Default for Sealed {
    fn default() -> Self {
        Self::No {
            incomplete_vars: vec![],
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct SsaBlock {
    pub bindings: Bindings,
    pub sealed: Sealed,
}

pub struct LoweringCtx<'a, 'gctx> {
    pub gctx: &'gctx mut GlobalCtx<'a>,
    pub class: IrClassName<'a>,
    pub func_name: FullFuncName<'a>,
    pub bbs: SecondaryMap<Block, SsaBlock>,
    current_bb: Option<Block>,
}

fn get_block_mut<'a>(bbs: &'a mut SecondaryMap<Block, SsaBlock>, bb: Block) -> &'a mut SsaBlock {
    bbs.entry(bb).unwrap().or_default()
}

impl<'a, 'gctx> LoweringCtx<'a, 'gctx> {
    pub fn new(
        gctx: &'gctx mut GlobalCtx<'a>,
        class: IrClassName<'a>,
        func_name: FullFuncName<'a>,
        entry_bb: Block,
    ) -> Self {
        let mut this = Self {
            gctx,
            class,
            func_name,
            current_bb: Some(entry_bb),
            bbs: Default::default(),
        };

        this.func_mut().set_entry_bb(entry_bb);

        this
    }

    pub fn self_param(&self) -> Value {
        self.func().unwrap_entry_bb().param(self.func(), 0).unwrap()
    }

    pub fn self_param_dyn_ty(&self) -> DynTy<'a> {
        self.func().values[self.self_param()]
            .ty
            .get_dyn_ty()
            .unwrap()
    }

    pub fn func(&self) -> &Func<'a> {
        self.gctx.func_registry[self.func_name].local().unwrap()
    }

    pub fn func_mut(&mut self) -> &mut Func<'a> {
        self.gctx.func_registry[self.func_name].local_mut().unwrap()
    }

    pub fn into_func_mut(self) -> &'gctx mut Func<'a> {
        self.gctx.func_registry[self.func_name].local_mut().unwrap()
    }

    pub fn class(&self) -> &IrClass<'a> {
        &self.gctx.ty_registry[self.class]
    }

    pub fn current_bb(&self) -> Block {
        self.current_bb.unwrap()
    }

    pub fn bind(&mut self, var: Var, value: Value) {
        self.bind_in(self.current_bb(), var, value);
    }

    pub fn bind_in(&mut self, bb: Block, var: Var, value: Value) {
        let ssa_data = get_block_mut(&mut self.bbs, bb);
        ssa_data.bindings.insert(var, value);
    }

    pub fn lookup(&mut self, var: Var, ty: IrTy<'a>) -> Value {
        self.lookup_in(self.current_bb(), var, ty)
    }

    pub fn lookup_local(&self, bb: Block, var: Var) -> Option<Value> {
        self.bbs.get(bb)?.bindings.get(&var).copied()
    }

    pub fn lookup_in(&mut self, bb: Block, var: Var, ty: IrTy<'a>) -> Value {
        if let Some(value) = self.lookup_local(bb, var) {
            return value;
        }

        let value;

        if let Sealed::No {
            ref mut incomplete_vars,
        } = get_block_mut(&mut self.bbs, bb).sealed
        {
            // the block may have more predecessors than currently available — defer
            // TODO: use the marker algorithm
            incomplete_vars.push(var);
            value = self.func_mut().append_param(bb, ty);
        } else {
            let preds = &self.func().bb_preds[bb];

            if self.func().bb_preds[bb].len() == 1 {
                // no control-flow-dependent def
                value = self.lookup_in(*preds.iter().next().unwrap(), var, ty);
            } else {
                // the def may be control-flow-dependent
                value = self.func_mut().append_param(bb, ty);
                self.bind(var, value);
                self.add_jump_params(bb, var, value);
            }
        }

        self.bind(var, value);

        value
    }

    fn add_jump_params(&mut self, bb: Block, var: Var, param: Value) {
        let param_idx = self.func().param_def(param).unwrap().idx;

        for pred in bb.preds(self.func()) {
            let value = self.lookup_in(pred, var, self.func().values[param].ty.clone());
            let terminator = self.func().bbs[pred].terminator;

            for jmp in self.func_mut().term_instrs[terminator].jumps_to_mut(bb) {
                jmp.args.insert(param_idx, value);
            }
        }

        self.optimize_block_param(bb, param);
    }

    fn optimize_block_param(&mut self, bb: Block, param: Value) {
        let mut same_value = None;
        let param_idx = self.func().param_def(param).unwrap().idx;
        let preds = bb.preds(self.func());

        for &pred in &preds {
            for jmp in self.func().term_instrs[pred.terminator(self.func())].jumps_to(bb) {
                let arg = self.func().resolve_value(jmp.args[param_idx]);

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

        self.func_mut().replace_with_id(param, same_value);
        // TODO: optimize in users
    }

    pub fn seal_block(&mut self, bb: Block) {
        let incomplete_vars =
            match mem::replace(&mut get_block_mut(&mut self.bbs, bb).sealed, Sealed::Yes) {
                Sealed::Yes => return,
                Sealed::No { incomplete_vars } => incomplete_vars,
            };

        for var in incomplete_vars {
            let param = self.lookup_local(bb, var).unwrap();
            self.add_jump_params(bb, var, param);
        }
    }

    pub fn terminate_with(&mut self, term_instr: TermInstr) {
        let current_bb = self.current_bb.take().unwrap();
        self.seal_block(current_bb);
        self.func_mut().terminate_block(current_bb, term_instr);
    }

    pub fn terminate(&mut self, instr: TermInstrKind<'a>) {
        let term_instr = self.func_mut().add_term_instr(instr);
        self.terminate_with(term_instr);
    }

    pub fn select_bb(&mut self, bb: Block) {
        assert!(self.current_bb.replace(bb).is_none());
    }

    /// Appends an instruction to the current block.
    pub fn emit(&mut self, instr_kind: InstrKind<'a>, ty: IrTy<'a>) -> Value {
        let current_bb = self.current_bb();

        self.func_mut().append_instr(current_bb, instr_kind, ty)
    }

    /// Appends cast/box instructions to the current block as necessary.
    pub fn coerce(&mut self, value: Value, ty: IrTy<'a>) -> Value {
        let current_ty = &self.func().values[value].ty;

        if current_ty.base_ty_equals(&ty) {
            value
        } else if current_ty.is_boxed() && ty.is_primitive() {
            let boxed_ty = self.gctx.ty_registry.to_boxed_ty(&ty).unwrap();
            let unboxed_ty = self.gctx.ty_registry.to_unboxed_ty(&boxed_ty).unwrap();

            let value = if !boxed_ty.base_ty_equals(&current_ty) {
                self.coerce(value, boxed_ty)
            } else {
                value
            };

            self.emit(InstrKind::Unbox(value), unboxed_ty)
        } else if current_ty.is_primitive() && ty.is_boxed() {
            let boxed_ty = self.gctx.ty_registry.to_boxed_ty(current_ty).unwrap();
            let boxed_value = self.emit(InstrKind::Box(value), boxed_ty);

            self.coerce(boxed_value, ty)
        } else if current_ty.is_primitive() && ty.is_primitive() {
            panic!("cannot coerce a primitive type to another");
        } else if !current_ty.is_class_ty() || !ty.is_class_ty() {
            panic!("cannot cast between non-class types");
        } else {
            let IrTy::Object(to_class, dyn_ty) = ty else { unreachable!() };
            let ty = ty.bind(value, dyn_ty);

            self.emit(Cast { value, to_class }.into(), ty)
        }
    }

    pub fn get_binding(&self, binding_id: BindingId) -> &Binding<'a> {
        self.gctx.bindings.get(self.class, binding_id)
    }

    pub fn lower_object_ty_to_class_name(&self, res_ty: &ResolvedTy<'_>) -> IrClassName<'a> {
        match res_ty {
            ResolvedTy::Builtin(builtin) => self.gctx.ty_registry.get_builtin_class(*builtin).name,
            ResolvedTy::SelfType { .. } => self.class,
            ResolvedTy::Class(name) => self.gctx.ty_registry[self.gctx.arena.alloc(name)].name,
            ResolvedTy::Function(_) => unreachable!(),
            ResolvedTy::Bottom => unreachable!(),
            ResolvedTy::Untyped => unreachable!(),
        }
    }

    pub fn lower_object_ty(&self, res_ty: &ResolvedTy<'_>) -> IrTy<'a> {
        match res_ty {
            ResolvedTy::Builtin(builtin) => {
                let class = self.gctx.ty_registry.get_builtin_class(*builtin);

                if class.r#final {
                    IrTy::new_known(class.name)
                } else {
                    IrTy::Object(class.name, DynTy::Unknown)
                }
            }

            ResolvedTy::SelfType { .. } => IrTy::Object(self.class, DynTy::Unknown)
                .bind(self.self_param(), self.self_param_dyn_ty()),
            ResolvedTy::Class(name) => IrTy::Object(
                self.gctx.ty_registry[self.gctx.arena.alloc(name)].name,
                DynTy::Unknown,
            ),
            ResolvedTy::Function(_) => unreachable!(),
            ResolvedTy::Bottom => unreachable!(),
            ResolvedTy::Untyped => unreachable!(),
        }
    }

    pub fn default_init(&mut self, class: IrClassName<'a>) -> Value {
        match class {
            _ if class == self.gctx.ty_registry.int_class => {
                let unboxed = self.emit(InstrKind::I32(0), IrTy::I32);
                self.coerce(unboxed, IrTy::Object(class, DynTy::Known(class)))
            }

            _ if class == self.gctx.ty_registry.string_class => {
                let unboxed = self.emit(InstrKind::Bytes(self.gctx.arena.alloc(b"")), IrTy::Bytes);
                self.coerce(unboxed, IrTy::Object(class, DynTy::Known(class)))
            }

            _ if class == self.gctx.ty_registry.bool_class => {
                let unboxed = self.emit(InstrKind::Bool(false), IrTy::Bool);
                self.coerce(unboxed, IrTy::Object(class, DynTy::Known(class)))
            }

            _ => self.emit(
                InstrKind::Null(class),
                IrTy::Object(class, DynTy::Known(class)),
            ),
        }
    }
}
