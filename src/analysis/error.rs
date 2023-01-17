use std::borrow::Cow;
use std::error::Error;
use std::fmt::{self, Debug, Display};
use std::ops::Deref;

use crate::analysis::typectx::{ClassName, DefinitionLocation};
use crate::ast::ty::{BuiltinClass, ResolvedTy};
use crate::ast::{Name, TyName};
use crate::position::{HasSpan, Span};
use crate::util::{CloneStatic, slice_formatter};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IllegalSelfTypePosition {
    ClassName,
    Inherits,
    Parameter,
    StaticDispatch,
    CaseArm,
}

impl Display for IllegalSelfTypePosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // NOTE: should make sense in the following context: `occured in {}`
        write!(
            f,
            "{}{}",
            match self {
                _ if !f.alternate() => "",
                Self::ClassName => "a ",
                Self::Inherits => "an ",
                Self::Parameter => "a ",
                Self::StaticDispatch => "a ",
                Self::CaseArm => "a ",
            },
            match self {
                Self::ClassName => "class name position",
                Self::Inherits => "inherits clause",
                Self::Parameter => "method parameter",
                Self::StaticDispatch => "static dispatch type specifier",
                Self::CaseArm => "case arm",
            }
        )
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum UnrecognizedNamePosition<'buf> {
    Method(Cow<'buf, [u8]>),
    Field(Cow<'buf, [u8]>),
}

impl Debug for UnrecognizedNamePosition<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Method(name) => f.debug_tuple("Method").field(&slice_formatter(name)).finish(),
            Self::Field(name) => f.debug_tuple("Field").field(&slice_formatter(name)).finish(),
        }
    }
}

impl CloneStatic<UnrecognizedNamePosition<'static>> for UnrecognizedNamePosition<'_> {
    fn clone_static(&self) -> UnrecognizedNamePosition<'static> {
        match self {
            Self::Method(name) => UnrecognizedNamePosition::Method(name.clone_static()),
            Self::Field(name) => UnrecognizedNamePosition::Field(name.clone_static()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnrecognizedName<'buf> {
    pub name: Box<Name<'buf>>,
    pub class_name: ClassName<'buf>,
    pub position: UnrecognizedNamePosition<'buf>,
}

impl CloneStatic<UnrecognizedName<'static>> for UnrecognizedName<'_> {
    fn clone_static(&self) -> UnrecognizedName<'static> {
        UnrecognizedName {
            name: Box::new(self.name.clone_static()),
            class_name: self.class_name.clone_static(),
            position: self.position.clone_static(),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum UnrecognizedTyPosition<'buf> {
    Inherits,
    Method(Cow<'buf, [u8]>),
    Field(Cow<'buf, [u8]>),
}

impl Debug for UnrecognizedTyPosition<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Inherits => f.write_str("Inherits"),
            Self::Method(name) => f.debug_tuple("Method").field(&slice_formatter(name)).finish(),
            Self::Field(name) => f.debug_tuple("Field").field(&slice_formatter(name)).finish(),
        }
    }
}

impl CloneStatic<UnrecognizedTyPosition<'static>> for UnrecognizedTyPosition<'_> {
    fn clone_static(&self) -> UnrecognizedTyPosition<'static> {
        match self {
            Self::Inherits => UnrecognizedTyPosition::Inherits,
            Self::Method(name) => UnrecognizedTyPosition::Method(name.clone_static()),
            Self::Field(name) => UnrecognizedTyPosition::Field(name.clone_static()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnrecognizedTy<'buf> {
    pub ty_name: TyName<'buf>,
    pub class_name: ClassName<'buf>,
    pub position: UnrecognizedTyPosition<'buf>,
}

impl CloneStatic<UnrecognizedTy<'static>> for UnrecognizedTy<'_> {
    fn clone_static(&self) -> UnrecognizedTy<'static> {
        UnrecognizedTy {
            ty_name: self.ty_name.clone_static(),
            class_name: self.class_name.clone_static(),
            position: self.position.clone_static(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MultipleDefinitionKind {
    Field { inherited: bool },

    Method { inherited: bool },

    Parameter,
}

impl Display for MultipleDefinitionKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Field { .. } => "field",
                Self::Method { .. } => "method",
                Self::Parameter => "parameter",
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnrelatedTypesInCase<'buf> {
    pub scrutinee_span: Span,
    pub scrutinee_ty: ResolvedTy<'buf>,
    pub arm_span: Span,
    pub arm_ty: ResolvedTy<'buf>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CaseArmSubsumed<'buf> {
    pub subsuming_arm_span: Span,
    pub subsuming_arm_ty: ResolvedTy<'buf>,
    pub subsumed_arm_span: Span,
    pub subsumed_arm_ty: ResolvedTy<'buf>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MismatchedTypes<'buf> {
    pub span: Span,
    pub expected_ty: ResolvedTy<'buf>,
    pub actual_ty: ResolvedTy<'buf>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeckError {
    UnrecognizedTy(Box<UnrecognizedTy<'static>>),

    IllegalSelfType {
        ty_name: Box<TyName<'static>>,
        position: IllegalSelfTypePosition,
    },

    BuiltinRedefined {
        ty_name: Box<TyName<'static>>,
        builtin: BuiltinClass,
    },

    ForbiddenInheritance {
        ty_name: Box<TyName<'static>>,
        builtin: BuiltinClass,
    },

    InheritanceCycle {
        ty_name: Box<TyName<'static>>,
        cycle: Vec<ClassName<'static>>,
    },

    MismatchedTypes(Box<MismatchedTypes<'static>>),

    UnrecognizedName(Box<UnrecognizedName<'static>>),

    UnknownMethod {
        class: Box<ClassName<'static>>,
        method: Box<Name<'static>>,
    },

    InvalidNumberOfArguments {
        call_span: Span,
        expected_count: usize,
        supplied_count: usize,
    },

    UnrelatedTypesInCase(Box<UnrelatedTypesInCase<'static>>),

    CaseArmSubsumed(Box<CaseArmSubsumed<'static>>),

    MultipleDefinition {
        kind: MultipleDefinitionKind,
        name: Box<Name<'static>>,
        previous: DefinitionLocation,
    },

    MultipleClassDefinition {
        ty_name: Box<TyName<'static>>,
        previous: DefinitionLocation,
    },

    IllegalSelf(Box<Name<'static>>),
}

impl Display for TypeckError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnrecognizedTy(err) => {
                let UnrecognizedTy { ty_name, .. } = err.deref();

                write!(f, "the type name `{}` is not recognized", ty_name)
            }

            Self::IllegalSelfType { position, .. } => {
                write!(f, "`SELF_TYPE` cannot occur in {:#}", position)
            }

            Self::BuiltinRedefined { builtin, .. } => {
                write!(f, "redefinition of the built-in class `{}`", builtin)
            }

            Self::ForbiddenInheritance {
                ty_name: class,
                builtin,
            } => {
                write!(
                    f,
                    "class `{}` inherits from the built-in class `{}`, which is forbidden",
                    class, builtin
                )
            }

            Self::InheritanceCycle { ty_name, .. } => {
                write!(
                    f,
                    "an inheritance cycle containing `{}` has been detected",
                    ty_name
                )
            }

            Self::MismatchedTypes(err) => {
                let MismatchedTypes {
                    expected_ty,
                    actual_ty,
                    ..
                } = err.deref();

                write!(
                    f,
                    "mismatched types: expected `{}`, found `{}`",
                    expected_ty, actual_ty,
                )
            }

            Self::UnrecognizedName(err) => {
                let UnrecognizedName { name, .. } = err.deref();

                write!(f, "unrecognized name `{}`", name)
            }

            Self::UnknownMethod { class, method } => {
                write!(
                    f,
                    "class `{}` does not have a method named `{}`",
                    class, method,
                )
            }

            Self::InvalidNumberOfArguments {
                expected_count,
                supplied_count,
                ..
            } => {
                write!(
                    f,
                    "this function takes {} argument{} but {} argument{} {} supplied",
                    expected_count,
                    if *expected_count != 1usize { "s" } else { "" },
                    supplied_count,
                    if *supplied_count != 1usize { "s" } else { "" },
                    if *supplied_count != 1usize {
                        "were"
                    } else {
                        "was"
                    },
                )
            }

            Self::UnrelatedTypesInCase(err) => {
                let UnrelatedTypesInCase {
                    scrutinee_ty,
                    arm_ty,
                    ..
                } = err.deref();

                write!(
                    f,
                    "unrelated types in a case expression: `{}` and `{}`",
                    scrutinee_ty, arm_ty,
                )
            }

            Self::CaseArmSubsumed(err) => {
                let CaseArmSubsumed {
                    subsuming_arm_ty,
                    subsumed_arm_ty,
                    ..
                } = err.deref();

                write!(
                    f,
                    "the case arm (matching type `{}`) is subsumed by a more general arm above matching `{}`",
                    subsumed_arm_ty,
                    subsuming_arm_ty,
                )
            }

            Self::MultipleDefinition { kind, name, .. } => {
                write!(
                    f,
                    "detected multiple definition: {} `{}` is already defined{}",
                    kind,
                    name,
                    match *kind {
                        MultipleDefinitionKind::Field { inherited: true }
                        | MultipleDefinitionKind::Method { inherited: true } => " in a superclass",

                        _ => "",
                    },
                )
            }

            Self::MultipleClassDefinition { ty_name, .. } => {
                write!(
                    f,
                    "detected multiple definition: class `{}` is already defined",
                    ty_name,
                )
            }

            Self::IllegalSelf(_) => write!(f, "`self` cannot be used here"),
        }
    }
}

impl Error for TypeckError {}

impl HasSpan for TypeckError {
    fn span(&self) -> Cow<'_, crate::position::Span> {
        match self {
            Self::UnrecognizedTy(err) => err.ty_name.span(),
            Self::IllegalSelfType { ty_name, .. } => ty_name.span(),
            Self::BuiltinRedefined { ty_name, .. } => ty_name.span(),
            Self::ForbiddenInheritance { ty_name, .. } => ty_name.span(),
            Self::InheritanceCycle { ty_name, .. } => ty_name.span(),
            Self::MismatchedTypes(err) => Cow::Borrowed(&err.span),
            Self::UnrecognizedName(err) => err.name.span(),
            Self::UnknownMethod { method, .. } => method.span(),
            Self::InvalidNumberOfArguments { call_span, .. } => Cow::Borrowed(call_span),
            Self::UnrelatedTypesInCase(err) => Cow::Borrowed(&err.arm_span),
            Self::CaseArmSubsumed(err) => Cow::Borrowed(&err.subsumed_arm_span),
            Self::MultipleDefinition { name, .. } => name.span(),
            Self::MultipleClassDefinition { ty_name, .. } => ty_name.span(),
            Self::IllegalSelf(name) => name.span(),
        }
    }
}
