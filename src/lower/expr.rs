use std::iter::once;

use crate::analysis::BindingKind;
use crate::ast::ty::{ResolvedTy, UnwrapResolvedTy};
use crate::ast::{self, Expr, Visitor};
use crate::ir::instr::{
    BlockJump, Branch, CallRef, Cast, CastBranch, FieldGet, FieldSet, InstrKind, MethodLookup,
    TermInstrKind, VTableLookup,
};
use crate::ir::ty::{DynTy, IrTy};
use crate::ir::value::Value;

use super::LoweringCtx;

pub fn lower_expr<'a>(ctx: &mut LoweringCtx<'a, '_>, expr: &Expr<'_>) -> Value {
    let mut visitor = ExprVisitor { ctx };
    visitor.visit_expr(expr)
}

pub struct ExprVisitor<'a, 'ctx, 'gctx> {
    ctx: &'ctx mut LoweringCtx<'a, 'gctx>,
}

impl<'buf> Visitor<'buf> for ExprVisitor<'_, '_, '_> {
    type Output = Value;

    fn visit_program(&mut self, _program: &ast::Program<'buf>) -> Self::Output {
        unimplemented!()
    }

    fn visit_class(&mut self, _class: &ast::Class<'buf>) -> Self::Output {
        unimplemented!()
    }

    fn visit_feature(&mut self, _feature: &ast::Feature<'buf>) -> Self::Output {
        unimplemented!()
    }

    fn visit_method(&mut self, _method: &ast::Method<'buf>) -> Self::Output {
        unimplemented!()
    }

    fn visit_field(&mut self, _field: &ast::Field<'buf>) -> Self::Output {
        unimplemented!()
    }

    fn visit_expr(&mut self, expr: &ast::Expr<'buf>) -> Self::Output {
        expr.recurse(self)
    }

    fn visit_assignment(&mut self, expr: &ast::Assignment<'buf>) -> Self::Output {
        let value = self.visit_expr(&expr.expr);

        let expected_ty = self
            .ctx
            .get_binding(expr.binding_id.unwrap())
            .ty
            .clone()
            .bind(self.ctx.self_param(), self.ctx.self_param_dyn_ty());
        let coerced_value = self.ctx.coerce(value, expected_ty);
        let binding = self.ctx.get_binding(expr.binding_id.unwrap());

        match binding.kind {
            BindingKind::Field { .. } => {
                let field_name = self.ctx.gctx.arena.alloc(expr.name.as_slice());

                self.ctx.emit(
                    FieldSet::new(self.ctx.self_param(), field_name, coerced_value).into(),
                    IrTy::Unit,
                );
            }

            _ => self.ctx.bind(expr.binding_id.unwrap(), coerced_value),
        }

        value
    }

    fn visit_call(&mut self, expr: &ast::Call<'buf>) -> Self::Output {
        let method_name = self.ctx.gctx.arena.alloc(expr.method.as_slice());

        let obj;
        let func_ref_ty;
        let func_ref = match &expr.receiver {
            ast::Receiver::SelfType { .. } => {
                obj = self.ctx.self_param();
                func_ref_ty = self.ctx.class().methods[&method_name]
                    .ty
                    .clone()
                    .bind(Some((obj, self.ctx.self_param_dyn_ty())));

                self.ctx.emit(
                    VTableLookup {
                        obj,
                        class: self.ctx.class,
                        method_name,
                    }
                    .into(),
                    func_ref_ty.clone().into(),
                )
            }

            ast::Receiver::Dynamic(recv) => {
                obj = self.visit_expr(recv);
                let recv_ty = self.ctx.func().values[obj].ty.clone();
                let IrTy::Object(class, recv_dyn_ty) = recv_ty else { unreachable!() };
                func_ref_ty = self.ctx.gctx.ty_registry[class].methods[&method_name]
                    .ty
                    .clone()
                    .bind(Some((obj, recv_dyn_ty)));

                self.ctx.emit(
                    VTableLookup {
                        obj,
                        class,
                        method_name,
                    }
                    .into(),
                    func_ref_ty.clone().into(),
                )
            }

            ast::Receiver::Static { object, ty, .. } => {
                obj = self.visit_expr(object);
                let recv_dyn_ty = self.ctx.func().values[obj].ty.get_dyn_ty().unwrap();
                let class = self.ctx.lower_object_ty_to_class_name(&ty.unwrap_res_ty());
                func_ref_ty = self.ctx.gctx.ty_registry[class].methods[&method_name]
                    .ty
                    .clone()
                    .bind(Some((obj, recv_dyn_ty)));

                self.ctx.emit(
                    MethodLookup {
                        class,
                        method: method_name,
                    }
                    .into(),
                    func_ref_ty.clone().into(),
                )
            }
        };

        let args = once(obj)
            .chain(expr.args.iter().map(|expr| self.visit_expr(expr)))
            // so that we can coerce later, which needs a &mut
            .collect::<Vec<_>>()
            .into_iter()
            .zip(func_ref_ty.args())
            .map(|(value, arg_ty)| self.ctx.coerce(value, arg_ty.clone()))
            .collect::<Vec<_>>();
        let expected_class = self
            .ctx
            .lower_object_ty_to_class_name(&expr.unwrap_res_ty());
        let ret = self.ctx.emit(
            CallRef::new(func_ref, &args).into(),
            func_ref_ty.ret().clone(),
        );

        self.ctx
            .coerce(ret, IrTy::Object(expected_class, DynTy::Unknown))
    }

    fn visit_if(&mut self, expr: &ast::If<'buf>) -> Self::Output {
        let cond = self.visit_expr(&expr.antecedent);
        let cond = self.ctx.coerce(cond, IrTy::Bool);

        let true_bb = self.ctx.func_mut().add_diverging_bb();
        let false_bb = self.ctx.func_mut().add_diverging_bb();
        let join_bb = self.ctx.func_mut().add_diverging_bb();

        let expected_ty = self.ctx.lower_object_ty(&expr.unwrap_res_ty());
        let join_param = self
            .ctx
            .func_mut()
            .append_param(join_bb, expected_ty.clone());
        self.ctx.terminate(
            Branch::new(
                cond,
                BlockJump {
                    bb: true_bb,
                    args: vec![],
                },
                BlockJump {
                    bb: false_bb,
                    args: vec![],
                },
            )
            .into(),
        );

        self.ctx.seal_block(true_bb);
        self.ctx.seal_block(false_bb);

        {
            self.ctx.select_bb(true_bb);
            let value = self.visit_expr(&expr.consequent);
            let coerced = self.ctx.coerce(value, expected_ty.clone());

            self.ctx.terminate(
                BlockJump {
                    bb: join_bb,
                    args: vec![coerced],
                }
                .into(),
            );
        }

        {
            self.ctx.select_bb(false_bb);
            let value = self.visit_expr(&expr.alternative);
            let coerced = self.ctx.coerce(value, expected_ty);

            self.ctx.terminate(
                BlockJump {
                    bb: join_bb,
                    args: vec![coerced],
                }
                .into(),
            );
        }

        self.ctx.select_bb(join_bb);
        self.ctx.seal_block(join_bb);

        join_param
    }

    fn visit_while(&mut self, expr: &ast::While<'buf>) -> Self::Output {
        let header_bb = self.ctx.func_mut().add_diverging_bb();
        self.ctx.terminate(
            BlockJump {
                bb: header_bb,
                args: vec![],
            }
            .into(),
        );

        self.ctx.select_bb(header_bb);
        let cond = self.visit_expr(&expr.condition);
        let cond = self.ctx.coerce(cond, IrTy::Bool);

        let body_bb = self.ctx.func_mut().add_diverging_bb();
        let join_bb = self.ctx.func_mut().add_diverging_bb();
        self.ctx.terminate(
            Branch::new(
                cond,
                BlockJump {
                    bb: body_bb,
                    args: vec![],
                },
                BlockJump {
                    bb: join_bb,
                    args: vec![],
                },
            )
            .into(),
        );
        self.ctx.seal_block(body_bb);
        self.ctx.seal_block(join_bb);

        self.ctx.select_bb(body_bb);
        self.visit_expr(&expr.body);
        self.ctx.terminate(
            BlockJump {
                bb: header_bb,
                args: vec![],
            }
            .into(),
        );
        self.ctx.seal_block(header_bb);

        self.ctx.select_bb(join_bb);

        let object_class = self.ctx.gctx.ty_registry.object_class;
        self.ctx
            .emit(InstrKind::Null(object_class), IrTy::new_known(object_class))
    }

    fn visit_block(&mut self, expr: &ast::Block<'buf>) -> Self::Output {
        for expr in &expr.body[0..expr.body.len() - 2] {
            self.visit_expr(expr);
        }

        self.visit_expr(&expr.body[expr.body.len() - 1])
    }

    fn visit_let(&mut self, expr: &ast::Let<'buf>) -> Self::Output {
        let binding_id = expr.binding.binding_id.unwrap();
        let ty = self
            .ctx
            .gctx
            .bindings
            .get(self.ctx.class, binding_id)
            .ty
            .clone()
            .bind(self.ctx.self_param(), self.ctx.self_param_dyn_ty());
        let init_value = match expr.binding.init {
            Some(ref init) => self.visit_expr(init),
            None => self.ctx.default_init(ty.get_class().unwrap()),
        };

        self.ctx.bind(expr.binding.binding_id.unwrap(), init_value);

        self.visit_expr(&expr.expr)
    }

    fn visit_case(&mut self, expr: &ast::Case<'buf>) -> Self::Output {
        let scrutinee = self.visit_expr(&expr.scrutinee);

        let join_ty = self.ctx.lower_object_ty(&expr.unwrap_res_ty());
        let join_bb = self.ctx.func_mut().add_diverging_bb();
        let join_param = self.ctx.func_mut().append_param(join_bb, join_ty.clone());

        for ast::CaseArm {
            binding_id, expr, ..
        } in &expr.arms
        {
            let success_bb = self.ctx.func_mut().add_diverging_bb();
            let fallthrough_bb = self.ctx.func_mut().add_diverging_bb();
            let ty = self
                .ctx
                .get_binding(binding_id.unwrap())
                .ty
                .clone()
                .unwrap_ty();
            let to_class = ty.get_class().unwrap();

            let term_instr = self.ctx.func_mut().add_term_instr(
                CastBranch::new(
                    Cast {
                        value: scrutinee,
                        to_class,
                    },
                    BlockJump {
                        bb: success_bb,
                        args: vec![],
                    },
                    BlockJump {
                        bb: fallthrough_bb,
                        args: vec![],
                    },
                )
                .into(),
            );
            let arg = self.ctx.func_mut().make_term_instr_value(term_instr, ty);

            match self.ctx.func_mut().term_instrs[term_instr] {
                TermInstrKind::CastBranch(ref mut br) => {
                    br.on_success_mut().args.push(arg);
                }

                _ => unreachable!(),
            }

            self.ctx.terminate_with(term_instr);
            self.ctx.seal_block(success_bb);
            self.ctx.seal_block(fallthrough_bb);

            self.ctx.select_bb(success_bb);
            let value = self.visit_expr(expr);
            let coerced = self.ctx.coerce(value, join_ty.clone());
            self.ctx.terminate(
                BlockJump {
                    bb: join_bb,
                    args: vec![coerced],
                }
                .into(),
            );

            self.ctx.select_bb(fallthrough_bb);
        }

        // we're in the fallthrough block, having exhausted all case arms, so emit a trap
        self.ctx.seal_block(join_bb);
        self.ctx.terminate(TermInstrKind::Diverge);

        self.ctx.select_bb(join_bb);

        join_param
    }

    fn visit_new(&mut self, expr: &ast::New<'buf>) -> Self::Output {
        let init_method_name = self.ctx.gctx.ty_registry.init_method;

        match &*expr.unwrap_res_ty() {
            ResolvedTy::SelfType { .. } => {
                let self_param = self.ctx.self_param();
                let self_param_dyn_ty = self.ctx.self_param_dyn_ty();
                let new_method_name = self.ctx.gctx.ty_registry.new_method;
                let new_func_ref_ty = self.ctx.class().methods[&new_method_name]
                    .ty
                    .clone()
                    .bind(Some((self_param, self_param_dyn_ty)));
                let new_func_ref = self.ctx.emit(
                    VTableLookup {
                        obj: self_param,
                        class: self.ctx.class,
                        method_name: new_method_name,
                    }
                    .into(),
                    new_func_ref_ty.into(),
                );
                let obj_dyn_ty = DynTy::Unknown.bind(self_param, self_param_dyn_ty);
                let obj = self.ctx.emit(
                    CallRef::new(new_func_ref, &[]).into(),
                    IrTy::Object(self.ctx.gctx.ty_registry.object_class, obj_dyn_ty),
                );

                let init_func_ref_ty = self.ctx.class().methods[&init_method_name]
                    .ty
                    .clone()
                    .bind(Some((obj, obj_dyn_ty)));
                let init_func_ref = self.ctx.emit(
                    VTableLookup {
                        obj,
                        class: self.ctx.class,
                        method_name: init_method_name,
                    }
                    .into(),
                    init_func_ref_ty.into(),
                );
                self.ctx
                    .emit(CallRef::new(init_func_ref, &[obj]).into(), IrTy::Unit);

                obj
            }

            ty => {
                let class = self.ctx.lower_object_ty_to_class_name(ty);
                let obj_dyn_ty = DynTy::Known(class);
                let obj = self
                    .ctx
                    .emit(InstrKind::New(class), IrTy::Object(class, obj_dyn_ty));

                let init_func_ref_ty = self.ctx.gctx.ty_registry[class].methods[&init_method_name]
                    .ty
                    .clone()
                    .bind(Some((obj, obj_dyn_ty)));
                let init_func_ref = self.ctx.emit(
                    MethodLookup {
                        class,
                        method: init_method_name,
                    }
                    .into(),
                    init_func_ref_ty.into(),
                );
                self.ctx
                    .emit(CallRef::new(init_func_ref, &[obj]).into(), IrTy::Unit);

                obj
            }
        }
    }

    fn visit_bin_op(&mut self, expr: &ast::BinOpExpr<'buf>) -> Self::Output {
        use crate::ast::BinOpKind;

        let mut lhs = self.visit_expr(&expr.lhs);
        let mut rhs = self.visit_expr(&expr.rhs);

        if let Some(unboxed_ty) = self
            .ctx
            .gctx
            .ty_registry
            .to_unboxed_ty(&self.ctx.func().values[lhs].ty)
        {
            if unboxed_ty != IrTy::Unit {
                lhs = self.ctx.coerce(lhs, unboxed_ty.clone());
                rhs = self.ctx.coerce(rhs, unboxed_ty);
            }
        }

        let unboxed = match expr.op {
            BinOpKind::Add => self.ctx.emit(InstrKind::Add(lhs, rhs), IrTy::I32),
            BinOpKind::Subtract => self.ctx.emit(InstrKind::Sub(lhs, rhs), IrTy::I32),
            BinOpKind::Multiply => self.ctx.emit(InstrKind::Mul(lhs, rhs), IrTy::I32),
            BinOpKind::LessThan => self.ctx.emit(InstrKind::Lt(lhs, rhs), IrTy::Bool),
            BinOpKind::LessEquals => self.ctx.emit(InstrKind::Le(lhs, rhs), IrTy::Bool),
            BinOpKind::Equals => self.ctx.emit(InstrKind::Eq(lhs, rhs), IrTy::Bool),

            // special-handling:
            // - division by zero, which should trap,
            // - i32::MAX / -1, which is undefined in wasm
            BinOpKind::Divide => {
                let join_bb = self.ctx.func_mut().add_diverging_bb();
                let trap_bb = self.ctx.func_mut().add_diverging_bb();
                let min_check_bb = self.ctx.func_mut().add_diverging_bb();

                let join_param = self.ctx.func_mut().append_param(join_bb, IrTy::I32);

                // if rhs == 0
                let zero_value = self.ctx.emit(InstrKind::I32(0), IrTy::I32);
                let div_zero_cond = self.ctx.emit(InstrKind::Eq(rhs, zero_value), IrTy::Bool);
                self.ctx.terminate(
                    Branch::new(
                        div_zero_cond,
                        BlockJump {
                            bb: trap_bb,
                            args: vec![],
                        },
                        BlockJump {
                            bb: min_check_bb,
                            args: vec![],
                        },
                    )
                    .into(),
                );

                self.ctx.seal_block(trap_bb);
                self.ctx.seal_block(min_check_bb);

                self.ctx.select_bb(trap_bb);
                self.ctx.terminate(TermInstrKind::Diverge);

                let neg_one_bb = self.ctx.func_mut().add_diverging_bb();
                let div_bb = self.ctx.func_mut().add_diverging_bb();

                // if lhs == i32::MIN
                self.ctx.select_bb(min_check_bb);
                let min_value = self.ctx.emit(InstrKind::I32(i32::MIN), IrTy::I32);
                let eq_min_cond = self.ctx.emit(InstrKind::Eq(lhs, min_value), IrTy::Bool);
                self.ctx.terminate(
                    Branch::new(
                        eq_min_cond,
                        BlockJump {
                            bb: neg_one_bb,
                            args: vec![],
                        },
                        BlockJump {
                            bb: div_bb,
                            args: vec![],
                        },
                    )
                    .into(),
                );

                // if rhs == -1
                self.ctx.seal_block(neg_one_bb);
                self.ctx.select_bb(neg_one_bb);
                let neg_one_value = self.ctx.emit(InstrKind::I32(-1), IrTy::I32);
                let neg_one_cond = self.ctx.emit(InstrKind::Eq(rhs, neg_one_value), IrTy::Bool);
                self.ctx.terminate(
                    Branch::new(
                        neg_one_cond,
                        BlockJump {
                            bb: join_bb,
                            args: vec![lhs],
                        },
                        BlockJump {
                            bb: div_bb,
                            args: vec![],
                        },
                    )
                    .into(),
                );

                // rhs != 0 && (lhs != i32::MIN || rhs != -1)
                self.ctx.seal_block(div_bb);
                self.ctx.select_bb(div_bb);
                let div_value = self.ctx.emit(InstrKind::Div(lhs, rhs), IrTy::I32);
                self.ctx.terminate(
                    BlockJump {
                        bb: join_bb,
                        args: vec![div_value],
                    }
                    .into(),
                );

                self.ctx.seal_block(join_bb);

                join_param
            }
        };

        let expected_ty = self
            .ctx
            .lower_object_ty_to_class_name(&expr.unwrap_res_ty());

        self.ctx
            .coerce(unboxed, IrTy::Object(expected_ty, DynTy::Unknown))
    }

    fn visit_un_op(&mut self, expr: &ast::UnOpExpr<'buf>) -> Self::Output {
        use crate::ast::UnOpKind;

        let operand = self.visit_expr(&expr.expr);

        let unboxed = match expr.op {
            UnOpKind::Not => {
                let value = self.ctx.coerce(operand, IrTy::Bool);
                self.ctx.emit(InstrKind::Not(value), IrTy::Bool)
            }

            UnOpKind::IsVoid => self.ctx.emit(InstrKind::IsNull(operand), IrTy::Bool),

            UnOpKind::Complement => {
                let value = self.ctx.coerce(operand, IrTy::I32);
                self.ctx.emit(InstrKind::Inv(value), IrTy::I32)
            }
        };

        let expected_ty = self
            .ctx
            .lower_object_ty_to_class_name(&expr.unwrap_res_ty());

        self.ctx
            .coerce(unboxed, IrTy::Object(expected_ty, DynTy::Unknown))
    }

    fn visit_name_expr(&mut self, expr: &ast::NameExpr<'buf>) -> Self::Output {
        let binding_id = expr.binding_id.unwrap();
        let ty = self
            .ctx
            .get_binding(binding_id)
            .ty
            .clone()
            .bind(self.ctx.self_param(), self.ctx.self_param_dyn_ty());

        let value = match self.ctx.get_binding(binding_id).kind {
            BindingKind::Field { .. } => {
                let field_name = self.ctx.gctx.arena.alloc(expr.name.as_slice());

                self.ctx.emit(
                    FieldGet {
                        obj: self.ctx.self_param(),
                        field: field_name,
                    }
                    .into(),
                    ty,
                )
            }

            _ => self.ctx.lookup(binding_id, ty),
        };
        let expected_class = self
            .ctx
            .lower_object_ty_to_class_name(&expr.unwrap_res_ty());

        self.ctx
            .coerce(value, IrTy::Object(expected_class, DynTy::Unknown))
    }

    fn visit_formal(&mut self, _formal: &ast::Formal<'buf>) -> Self::Output {
        unimplemented!()
    }

    fn visit_receiver(&mut self, _recv: &ast::Receiver<'buf>) -> Self::Output {
        unimplemented!()
    }

    fn visit_case_arm(&mut self, _arm: &ast::CaseArm<'buf>) -> Self::Output {
        unimplemented!()
    }

    fn visit_ty_name(&mut self, _ty_name: &ast::TyName<'buf>) -> Self::Output {
        unimplemented!()
    }

    fn visit_binding(&mut self, _binding: &ast::Binding<'buf>) -> Self::Output {
        unimplemented!()
    }

    fn visit_name(&mut self, _name: &ast::Name<'buf>) -> Self::Output {
        unimplemented!()
    }

    fn visit_int_lit(&mut self, expr: &ast::IntLit) -> Self::Output {
        let unboxed = self.ctx.emit(InstrKind::I32(expr.0.value), IrTy::I32);
        self.ctx.coerce(
            unboxed,
            self.ctx.gctx.ty_registry.to_boxed_ty(&IrTy::I32).unwrap(),
        )
    }

    fn visit_string_lit(&mut self, expr: &ast::StringLit<'buf>) -> Self::Output {
        let unboxed = self.ctx.emit(
            InstrKind::Bytes(self.ctx.gctx.arena.alloc(&expr.0.value)),
            IrTy::Bytes,
        );
        self.ctx.coerce(
            unboxed,
            self.ctx.gctx.ty_registry.to_boxed_ty(&IrTy::Bytes).unwrap(),
        )
    }

    fn visit_bool_lit(&mut self, expr: &ast::BoolLit) -> Self::Output {
        let unboxed = self.ctx.emit(InstrKind::Bool(expr.0.value), IrTy::Bool);
        self.ctx.coerce(
            unboxed,
            self.ctx.gctx.ty_registry.to_boxed_ty(&IrTy::Bool).unwrap(),
        )
    }
}
