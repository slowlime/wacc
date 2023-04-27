use std::borrow::Borrow;
use std::error::Error;
use std::fmt::{self, Debug, Display};

use slotmap::Key;

use crate::ast::ty::{BuiltinClass, FunctionTy, HasTy, TyExt};
use crate::ast::{self, AstRecurse, Class, Visitor as AstVisitor};
use crate::errors::{DiagnosticMessage, Diagnostics};
use crate::position::{HasSpan, Span};
use crate::source::Source;
use crate::util::CloneStatic;

use super::typectx::{ClassName, DefinitionLocation};
use super::{BindingId, TypeCtx};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NoEntryPointError {
    NoMainClass,

    NoMainMethod {
        class_location: DefinitionLocation,
    },

    MainMethodInherited {
        main_class_location: DefinitionLocation,
        def_class: ClassName<'static>,
        def_location: DefinitionLocation,
    },

    InvalidMainSignature {
        method_location: DefinitionLocation,
        method_ty: FunctionTy<'static>,
    },
}

impl NoEntryPointError {
    pub fn span(&self) -> Option<&Span> {
        match self {
            Self::NoMainClass => None,
            Self::NoMainMethod { class_location } => class_location.span(),
            Self::MainMethodInherited {
                main_class_location,
                ..
            } => main_class_location.span(),
            Self::InvalidMainSignature {
                method_location, ..
            } => method_location.span(),
        }
    }
}

impl Display for NoEntryPointError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NoMainClass => write!(f, "no `Main` class found"),

            Self::NoMainMethod { .. } => {
                write!(f, "the `Main` class must define a method named `main`")
            }

            Self::MainMethodInherited { def_class, .. } => {
                write!(
                    f,
                    "the `main` method is inherited from `{}`; must be defined in the `Main` class",
                    def_class
                )
            }

            Self::InvalidMainSignature { .. } => {
                write!(f, "the method `Main.main` has an invalid signature")
            }
        }
    }
}

impl Error for NoEntryPointError {}

impl From<NoEntryPointError> for DiagnosticMessage {
    fn from(err: NoEntryPointError) -> DiagnosticMessage {
        let message = DiagnosticMessage::new(err.to_string());

        match err.span() {
            Some(span) => message.with_span(span.clone()),
            None => message,
        }
    }
}

trait AssertResolved {
    fn assert_resolved(&self, source: &Source<'_>, span: &Span);
}

impl<'a, T> AssertResolved for T
where
    T: TyExt<'a> + Debug,
{
    fn assert_resolved(&self, source: &Source<'_>, span: &Span) {
        if !self.is_resolved() {
            panic!(
                "Type {:?} (spanning {}) has not been resolved",
                self,
                span.display(source)
            );
        }
    }
}

struct Validator<'a, 'buf> {
    source: &'a Source<'buf>,
    ty_ctx: &'a TypeCtx<'buf>,
    classes: &'a [Class<'buf>],
}

impl<'buf> Validator<'_, 'buf> {
    fn validate(mut self) {
        self.check_types_resolved();
        self.check_object_is_maximum();
    }

    fn check_types_resolved(&mut self) {
        for class in self.classes {
            self.visit_class(class);
        }
    }

    fn check_object_is_maximum(&self) {
        for class in self.classes {
            let name: ClassName<'_> = class.name.borrow().into();

            let Some(index) = self.ty_ctx.get_class(&name) else {
                panic!("Class {} is not present in the index", name);
            };

            match (&name, index.parent()) {
                (ClassName::Builtin(BuiltinClass::Object), Some(parent)) => {
                    panic!(
                        "The built-in class `Object` must not inherit from {}",
                        parent
                    );
                }

                (_, None) => {
                    panic!("The class `{}` must have a parent class", name);
                }

                _ => {}
            }
        }
    }

    fn assert_resolved(&self, span: &Span, ty: &impl AssertResolved) {
        ty.assert_resolved(self.source, span);
    }

    fn assert_binding_resolved(&self, span: &Span, binding_id: BindingId) {
        assert!(
            !binding_id.is_null(),
            "the binding at {} has not been resolved",
            span.display(self.source)
        );
    }
}

impl<'buf> ast::Visitor<'buf> for Validator<'_, 'buf> {
    type Output = ();

    fn visit_program(&mut self, program: &ast::Program<'buf>) -> Self::Output {
        program.recurse(self);
    }

    fn visit_class(&mut self, class: &Class<'buf>) -> Self::Output {
        self.assert_resolved(&class.name.span(), &class.ty);
        class.recurse(self);
    }

    fn visit_feature(&mut self, feature: &ast::Feature<'buf>) -> Self::Output {
        feature.recurse(self);
    }

    fn visit_method(&mut self, method: &ast::Method<'buf>) -> Self::Output {
        self.assert_resolved(&method.name.span(), &method.ty);
        method.recurse(self);
    }

    fn visit_field(&mut self, field: &ast::Field<'buf>) -> Self::Output {
        field.recurse(self);
    }

    fn visit_expr(&mut self, expr: &ast::Expr<'buf>) -> Self::Output {
        self.assert_resolved(&expr.span(), &expr.ty().as_deref());
        expr.recurse(self);
    }

    fn visit_assignment(&mut self, expr: &ast::Assignment<'buf>) -> Self::Output {
        self.assert_binding_resolved(&expr.span(), expr.binding_id);
        self.assert_resolved(&expr.span(), &expr.ty().as_deref());
        expr.recurse(self);
    }

    fn visit_call(&mut self, expr: &ast::Call<'buf>) -> Self::Output {
        self.assert_resolved(&expr.span(), &expr.ty().as_deref());
        expr.recurse(self);
    }

    fn visit_if(&mut self, expr: &ast::If<'buf>) -> Self::Output {
        self.assert_resolved(&expr.span(), &expr.ty().as_deref());
        expr.recurse(self);
    }

    fn visit_while(&mut self, expr: &ast::While<'buf>) -> Self::Output {
        self.assert_resolved(&expr.span(), &expr.ty().as_deref());
        expr.recurse(self);
    }

    fn visit_block(&mut self, expr: &ast::Block<'buf>) -> Self::Output {
        self.assert_resolved(&expr.span(), &expr.ty().as_deref());
        expr.recurse(self);
    }

    fn visit_let(&mut self, expr: &ast::Let<'buf>) -> Self::Output {
        self.assert_resolved(&expr.span(), &expr.ty().as_deref());
        expr.recurse(self);
    }

    fn visit_case(&mut self, expr: &ast::Case<'buf>) -> Self::Output {
        self.assert_resolved(&expr.span(), &expr.ty().as_deref());
        expr.recurse(self);
    }

    fn visit_new(&mut self, expr: &ast::New<'buf>) -> Self::Output {
        self.assert_resolved(&expr.span(), &expr.ty().as_deref());
        expr.recurse(self);
    }

    fn visit_bin_op(&mut self, expr: &ast::BinOpExpr<'buf>) -> Self::Output {
        self.assert_resolved(&expr.span(), &expr.ty().as_deref());
        expr.recurse(self);
    }

    fn visit_un_op(&mut self, expr: &ast::UnOpExpr<'buf>) -> Self::Output {
        self.assert_resolved(&expr.span(), &expr.ty().as_deref());
        expr.recurse(self);
    }

    fn visit_name_expr(&mut self, expr: &ast::NameExpr<'buf>) -> Self::Output {
        self.assert_binding_resolved(&expr.span(), expr.binding_id);
        self.assert_resolved(&expr.span(), &expr.ty().as_deref());
        expr.recurse(self);
    }

    fn visit_formal(&mut self, formal: &ast::Formal<'buf>) -> Self::Output {
        self.assert_binding_resolved(&formal.span(), formal.binding_id);
        self.assert_resolved(&formal.span(), &formal.ty);
    }

    fn visit_receiver(&mut self, recv: &ast::Receiver<'buf>) -> Self::Output {
        if let ast::Receiver::SelfType { binding_id, method_name_span, .. } = recv {
            self.assert_binding_resolved(method_name_span, *binding_id);
        }

        self.assert_resolved(&recv.span(), &recv.ty().as_deref());
        recv.recurse(self);
    }

    fn visit_case_arm(&mut self, arm: &ast::CaseArm<'buf>) -> Self::Output {
        self.assert_resolved(&arm.binding_ty_name.span(), &arm.binding_ty);
        arm.recurse(self);
    }

    fn visit_ty_name(&mut self, _ty_name: &ast::TyName<'buf>) -> Self::Output {}

    fn visit_binding(&mut self, binding: &ast::Binding<'buf>) -> Self::Output {
        self.assert_binding_resolved(&binding.span, binding.binding_id);
        self.assert_resolved(&binding.span, &binding.ty);
        binding.recurse(self);
    }

    fn visit_name(&mut self, _name: &ast::Name<'buf>) -> Self::Output {}

    fn visit_int_lit(&mut self, expr: &ast::IntLit) -> Self::Output {
        self.assert_resolved(&expr.span(), &expr.ty().as_deref());
    }

    fn visit_string_lit(&mut self, expr: &ast::StringLit<'buf>) -> Self::Output {
        self.assert_resolved(&expr.span(), &expr.ty().as_deref());
    }

    fn visit_bool_lit(&mut self, expr: &ast::BoolLit) -> Self::Output {
        self.assert_resolved(&expr.span(), &expr.ty().as_deref());
    }
}

pub fn validate_classes<'buf>(
    source: &Source<'buf>,
    ty_ctx: &TypeCtx<'buf>,
    classes: &[Class<'buf>],
) {
    let validator = Validator {
        classes,
        ty_ctx,
        source,
    };
    validator.validate();
}

pub fn check_has_main_class<'buf>(diagnostics: &mut Diagnostics<'_>, ty_ctx: &TypeCtx<'buf>) {
    let mut emit = |err: NoEntryPointError| {
        diagnostics
            .error()
            .with_message(err.clone())
            .with_source(Box::new(err))
            .emit();
    };

    let class_name = b"Main".as_slice().into();

    let Some(index) = ty_ctx.get_class(&class_name) else {
        emit(NoEntryPointError::NoMainClass);

        return;
    };

    let Some((main_def_class, method_location, method_ty)) = ty_ctx.get_method_ty(&class_name, b"main") else {
        emit(NoEntryPointError::NoMainMethod {
            class_location: index.location().clone(),
        });

        return;
    };

    if main_def_class != &class_name {
        emit(NoEntryPointError::MainMethodInherited {
            main_class_location: index.location().clone(),
            def_class: main_def_class.clone_static(),
            def_location: method_location.clone(),
        });

        return;
    }

    if !method_ty.params.is_empty() {
        emit(NoEntryPointError::InvalidMainSignature {
            method_location: method_location.clone(),
            method_ty: method_ty.clone_static(),
        });

        // keep it for consistency
        #[allow(clippy::needless_return)]
        return;
    }
}
