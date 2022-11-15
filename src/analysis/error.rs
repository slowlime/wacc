use std::borrow::Cow;
use std::error::Error;
use std::fmt::{self, Display};

use crate::analysis::typectx::{ClassName, DefinitionLocation};
use crate::ast::ty::{BuiltinClass, ResolvedTy};
use crate::ast::{Name, TyName};
use crate::position::{HasSpan, Span};
use crate::util::CloneStatic;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IllegalSelfName {
    Field,
    Param,
    Local,
}

impl Display for IllegalSelfName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Field => "field",
                Self::Param => "parameter",
                Self::Local => "local",
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnrecognizedNamePosition<'buf> {
    Method(Cow<'buf, [u8]>),
    Field(Cow<'buf, [u8]>),
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
    pub name: Name<'buf>,
    pub class_name: ClassName<'buf>,
    pub position: UnrecognizedNamePosition<'buf>,
}

impl CloneStatic<UnrecognizedName<'static>> for UnrecognizedName<'_> {
    fn clone_static(&self) -> UnrecognizedName<'static> {
        UnrecognizedName {
            name: self.name.clone_static(),
            class_name: self.class_name.clone_static(),
            position: self.position.clone_static(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnrecognizedTyPosition<'buf> {
    Inherits,
    Method(Cow<'buf, [u8]>),
    Field(Cow<'buf, [u8]>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnrecognizedTy<'buf> {
    pub ty_name: TyName<'buf>,
    pub class_name: ClassName<'buf>,
    pub position: UnrecognizedTyPosition<'buf>,
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
pub enum TypeckError {
    UnrecognizedTy(UnrecognizedTy<'static>),

    IllegalSelfType {
        ty_name: TyName<'static>,
        position: IllegalSelfTypePosition,
    },

    BuiltinRedefined {
        ty_name: TyName<'static>,
        builtin: BuiltinClass,
    },

    InheritanceCycle {
        ty_name: TyName<'static>,
        cycle: Vec<ClassName<'static>>,
    },

    MismatchedTypes {
        span: Span,
        expected_ty: ResolvedTy<'static>,
        actual_ty: ResolvedTy<'static>,
    },

    UnrecognizedName(UnrecognizedName<'static>),

    InvalidNumberOfArguments {
        call_span: Span,
        expected_count: usize,
        supplied_count: usize,
    },

    UnrelatedTypesInCase {
        scrutinee_span: Span,
        scrutinee_ty: ResolvedTy<'static>,
        arm_span: Span,
        arm_ty: ResolvedTy<'static>,
    },

    CaseArmSubsumed {
        subsuming_arm_span: Span,
        subsuming_arm_ty: ResolvedTy<'static>,
        subsumed_arm_span: Span,
        subsumed_arm_ty: ResolvedTy<'static>,
    },

    MultipleDefinition {
        kind: MultipleDefinitionKind,
        name: Name<'static>,
        previous: DefinitionLocation,
    },

    IllegalSelf(Name<'static>),
}

impl Display for TypeckError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnrecognizedTy(UnrecognizedTy { ty_name, .. }) => {
                write!(f, "the type name `{}` is not recognized", ty_name)
            }

            Self::IllegalSelfType { position, .. } => {
                write!(f, "`SELF_TYPE` cannot occur in {:#}", position)
            }

            Self::BuiltinRedefined { builtin, .. } => {
                write!(f, "redefinition of the built-in class `{}`", builtin)
            }

            Self::InheritanceCycle { ty_name, .. } => {
                write!(
                    f,
                    "an inheritance cycle containing `{}` has been detected",
                    ty_name
                )
            }

            Self::MismatchedTypes {
                expected_ty,
                actual_ty,
                ..
            } => {
                write!(
                    f,
                    "mismatched types: expected `{}`, found `{}`",
                    expected_ty, actual_ty,
                )
            }

            Self::UnrecognizedName(UnrecognizedName { name, .. }) => {
                write!(f, "unrecognized name `{}`", name)
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

            Self::UnrelatedTypesInCase {
                scrutinee_ty,
                arm_ty,
                ..
            } => {
                write!(
                    f,
                    "unrelated types in a case expression: `{}` and `{}`",
                    scrutinee_ty, arm_ty,
                )
            }

            Self::CaseArmSubsumed {
                subsuming_arm_ty,
                subsumed_arm_ty,
                ..
            } => {
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

            Self::IllegalSelf(_) => write!(f, "`self` cannot be used here"),
        }
    }
}

impl Error for TypeckError {}

impl HasSpan for TypeckError {
    fn span(&self) -> Cow<'_, crate::position::Span> {
        match self {
            Self::UnrecognizedTy(UnrecognizedTy { ty_name, .. }) => ty_name.span(),
            Self::IllegalSelfType { ty_name, .. } => ty_name.span(),
            Self::BuiltinRedefined { ty_name, .. } => ty_name.span(),
            Self::InheritanceCycle { ty_name, .. } => ty_name.span(),
            Self::MismatchedTypes { span, .. } => Cow::Borrowed(span),
            Self::UnrecognizedName(UnrecognizedName { name, .. }) => name.span(),
            Self::InvalidNumberOfArguments { call_span, .. } => Cow::Borrowed(call_span),
            Self::UnrelatedTypesInCase { arm_span, .. } => Cow::Borrowed(arm_span),
            Self::CaseArmSubsumed {
                subsumed_arm_span, ..
            } => Cow::Borrowed(subsumed_arm_span),
            Self::MultipleDefinition { name, .. } => name.span(),
            Self::IllegalSelf(name) => name.span(),
        }
    }
}
