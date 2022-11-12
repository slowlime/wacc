use std::borrow::Cow;
use std::collections::HashSet;
use std::error::Error;
use std::fmt::{self, Display};

use crate::analysis::typectx::{BindingMap, ClassIndex, TypeCtx};
use crate::ast::ty::{BuiltinClass, FunctionTy, ResolvedTy};
use crate::ast::{self, Class, TyName, Name};
use crate::errors::{DiagnosticMessage, Diagnostics};
use crate::position::HasSpan;
use crate::try_match;

use super::typectx::ClassName;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IllegalSelfTypePosition {
    ClassName,
}

impl Display for IllegalSelfTypePosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // NOTE: the output should make sense in the following context: `in the {} position`
        write!(
            f,
            "{}",
            match self {
                Self::ClassName => "class name",
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeckError {
    UnrecognizedTy(TyName<'static>),
    IllegalSelfType {
        ty_name: TyName<'static>,
        position: IllegalSelfTypePosition,
    },
}

impl Display for TypeckError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnrecognizedTy(ty_name) => {
                write!(f, "the type name `{}` is not recognized", ty_name)
            }

            Self::IllegalSelfType { position, .. } => {
                write!(f, "`SELF_TYPE` cannot occur in the {} position", position)
            }
        }
    }
}

impl Error for TypeckError {}

impl HasSpan for TypeckError {
    fn span(&self) -> Cow<'_, crate::position::Span> {
        match self {
            Self::UnrecognizedTy(ty_name) => ty_name.span(),
            Self::IllegalSelfType { ty_name, .. } => ty_name.span(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Variance {
    Contravariant,
    Covariant,
}

fn builtin_ctx() -> TypeCtx<'static> {
    let mut ctx = TypeCtx::empty();

    ctx.add_class(
        b"Object"[..].into(),
        ClassIndex::with_methods([
            (
                b"abort"[..].into(),
                FunctionTy {
                    params: vec![],
                    ret: Box::new(BuiltinClass::Object.into()),
                },
            ),
            (
                b"type_name"[..].into(),
                FunctionTy {
                    params: vec![],
                    ret: Box::new(BuiltinClass::String.into()),
                },
            ),
            (
                b"copy"[..].into(),
                FunctionTy {
                    params: vec![],
                    ret: Box::new(ResolvedTy::SelfType {
                        enclosed: Box::new(BuiltinClass::Object.into()),
                    }),
                },
            ),
        ]),
    );

    ctx.add_class(
        b"IO"[..].into(),
        ClassIndex::with_methods([
            (
                b"out_string"[..].into(),
                FunctionTy {
                    params: vec![BuiltinClass::String.into()],
                    ret: Box::new(ResolvedTy::SelfType {
                        enclosed: Box::new(BuiltinClass::IO.into()),
                    }),
                },
            ),
            (
                b"out_int"[..].into(),
                FunctionTy {
                    params: vec![BuiltinClass::Int.into()],
                    ret: Box::new(ResolvedTy::SelfType {
                        enclosed: Box::new(BuiltinClass::IO.into()),
                    }),
                },
            ),
            (
                b"in_string"[..].into(),
                FunctionTy {
                    params: vec![],
                    ret: Box::new(BuiltinClass::String.into()),
                },
            ),
            (
                b"in_int"[..].into(),
                FunctionTy {
                    params: vec![],
                    ret: Box::new(BuiltinClass::Int.into()),
                },
            ),
        ]),
    );

    ctx.add_class(b"Int"[..].into(), ClassIndex::new());

    ctx.add_class(
        b"String"[..].into(),
        ClassIndex::with_methods([
            (
                b"length"[..].into(),
                FunctionTy {
                    params: vec![],
                    ret: Box::new(BuiltinClass::Int.into()),
                },
            ),
            (
                b"concat"[..].into(),
                FunctionTy {
                    params: vec![BuiltinClass::String.into()],
                    ret: Box::new(BuiltinClass::String.into()),
                },
            ),
            (
                b"substr"[..].into(),
                FunctionTy {
                    params: vec![BuiltinClass::Int.into(); 2],
                    ret: Box::new(BuiltinClass::String.into()),
                },
            ),
        ]),
    );

    ctx.add_class(b"Bool"[..].into(), ClassIndex::new());

    ctx
}

/// The context associated with an occurrence of a `TyName`.
struct TyNameCtx<'a, 'buf> {
    pub variance: Variance,
    pub enclosed_class: &'a ResolvedTy<'buf>,
}

impl<'a, 'buf> TyNameCtx<'a, 'buf> {
    pub fn contravariant_in(enclosed_class: &'a ResolvedTy<'buf>) -> Self {
        Self {
            variance: Variance::Contravariant,
            enclosed_class,
        }
    }

    pub fn covariant_in(enclosed_class: &'a ResolvedTy<'buf>) -> Self {
        Self {
            variance: Variance::Covariant,
            enclosed_class,
        }
    }
}

struct MethodResolverResult<'buf> {
    pub unrecognized_tys: Vec<TyName<'buf>>,
    pub ctx: TypeCtx<'buf>,
}

struct MethodResolver<'dia, 'buf, 'cls> {
    diagnostics: &'dia mut Diagnostics,
    unrecognized_tys: Vec<TyName<'buf>>,
    classes: &'cls [Class<'buf>],
    class_names: HashSet<ClassName<'buf>>,
    ctx: TypeCtx<'buf>,
}

impl<'dia, 'buf, 'cls> MethodResolver<'dia, 'buf, 'cls> {
    pub fn new(diagnostics: &'dia mut Diagnostics, classes: &'cls [Class<'buf>]) -> Self {
        let ctx = builtin_ctx();
        let class_names = classes
            .iter()
            .map(|class| ClassName::from(class.name.clone()))
            .chain(ctx.iter().map(|(k, _)| k.clone()))
            .collect::<HashSet<_>>();

        Self {
            diagnostics,
            unrecognized_tys: vec![],
            classes,
            class_names,
            ctx,
        }
    }

    pub fn resolve(mut self) -> MethodResolverResult<'buf> {
        for class in self.classes {
            self.resolve_methods(class);
        }

        MethodResolverResult {
            unrecognized_tys: self.unrecognized_tys,
            ctx: self.ctx,
        }
    }

    /// Resolves a type name using the list of available classes.
    ///
    /// If the name is not found, adds it to the list of unrecognized types and returns:
    /// - `ResolvedTy::Bottom` if the type occurs in a covariant position
    /// - `ResolvedTy::Builtin(BuiltinClass::Object)` if the type occurs in a contravariant position
    fn resolve_ty_name<'a>(
        &mut self,
        ty_name: &TyName<'buf>,
        ty_name_ctx: TyNameCtx<'a, 'buf>,
    ) -> ResolvedTy<'buf> {
        let class_name: ClassName<'buf> = ty_name.clone().into();

        match class_name {
            ClassName::Builtin(builtin) => builtin.into(),
            ClassName::SelfType => ResolvedTy::SelfType {
                enclosed: Box::new(ty_name_ctx.enclosed_class.clone()),
            },
            ClassName::Named(name) if self.class_names.contains(&class_name) => {
                ResolvedTy::Class(name)
            }

            ClassName::Named(_) => {
                self.unrecognized_tys.push(ty_name.clone());

                match ty_name_ctx.variance {
                    Variance::Covariant => ResolvedTy::Bottom,
                    Variance::Contravariant => BuiltinClass::Object.into(),
                }
            }
        }
    }

    fn resolve_methods(&mut self, class: &Class<'buf>) {
        let class_name: ClassName<'buf> = class.name.clone().into();
        let Ok(class_ty) = class_name.try_into() else {
            self.diagnostics.error()
                .with_span_and_error(TypeckError::IllegalSelfType {
                    ty_name: class.name.clone_static(),
                    position: IllegalSelfTypePosition::ClassName
                })
                .emit();

            return;
        };

        let methods = class
            .features
            .iter()
            .filter_map(|feature| try_match!(feature, ast::Feature::Method(method) => method))
            .map(
                |ast::Method {
                     name,
                     params,
                     return_ty,
                     ..
                 }| {
                    let params = params
                        .iter()
                        .map(|ast::Formal { ty_name, .. }| {
                            self.resolve_ty_name(ty_name, TyNameCtx::contravariant_in(&class_ty))
                        })
                        .collect();
                    let ret = Box::new(
                        self.resolve_ty_name(return_ty, TyNameCtx::covariant_in(&class_ty)),
                    );

                    (name.0.value.clone(), FunctionTy { params, ret }.into())
                },
            );
        let index = ClassIndex::with_methods(methods);

        self.ctx.add_class(class.name.0.0.value.clone(), index);
    }
}

pub struct TypeChecker<'dia, 'buf> {
    diagnostics: &'dia mut Diagnostics,
    classes: Vec<Class<'buf>>,
    ctx: TypeCtx<'buf>,
    bindings: BindingMap<'buf>,
    unrecognized_tys: Vec<TyName<'buf>>,
    unrecognized_names: Vec<Name<'buf>>,
}

impl<'dia, 'buf> TypeChecker<'dia, 'buf> {
    pub fn new(diagnostics: &'dia mut Diagnostics, classes: Vec<Class<'buf>>) -> Self {
        let method_resolver = MethodResolver::new(diagnostics, &classes);
        let MethodResolverResult { unrecognized_tys, ctx } = method_resolver.resolve();
        let bindings = BindingMap::new();

        Self {
            diagnostics,
            classes,
            ctx,
            bindings,
            unrecognized_tys,
            unrecognized_names: vec![],
        }
    }
}
