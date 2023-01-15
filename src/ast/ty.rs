use std::borrow::Cow;
use std::fmt::{self, Display};

use serde::Serialize;

use crate::try_match;
use crate::util::{slice_formatter, CloneStatic};

use super::TyName;

fn serialize_bytes_as_string<S>(bytes: &[u8], ser: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    String::from_utf8_lossy(bytes).serialize(ser)
}

/// A potentially-unresolved type.
#[derive(Serialize, Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ty<'buf> {
    Unresolved(UnresolvedTy<'buf>),
    Resolved(ResolvedTy<'buf>),
}

impl CloneStatic<Ty<'static>> for Ty<'_> {
    fn clone_static(&self) -> Ty<'static> {
        match self {
            Self::Unresolved(ty) => Ty::Unresolved(ty.clone_static()),
            Self::Resolved(ty) => Ty::Resolved(ty.clone_static()),
        }
    }
}

impl<'buf> From<UnresolvedTy<'buf>> for Ty<'buf> {
    fn from(ty: UnresolvedTy<'buf>) -> Self {
        Self::Unresolved(ty)
    }
}

impl<'buf> From<ResolvedTy<'buf>> for Ty<'buf> {
    fn from(ty: ResolvedTy<'buf>) -> Self {
        Self::Resolved(ty)
    }
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnresolvedTy<'buf> {
    Named(TyName<'buf>),

    Function {
        args: Vec<Ty<'buf>>,
        ret: Box<Ty<'buf>>,
    },

    SelfType,
}

impl CloneStatic<UnresolvedTy<'static>> for UnresolvedTy<'_> {
    fn clone_static(&self) -> UnresolvedTy<'static> {
        match self {
            Self::Named(name) => UnresolvedTy::Named(name.clone_static()),
            Self::Function { args, ret } => UnresolvedTy::Function {
                args: args.clone_static(),
                ret: Box::new(ret.clone_static()),
            },
            Self::SelfType => UnresolvedTy::SelfType,
        }
    }
}

pub trait TyExt<'buf>: private::Sealed {
    /// Returns:
    /// - `None` if the type is resolved
    /// - `Some(None)` if the type is unresolved and has to be inferred
    /// - `Some(Some(name))` if the type is unresolved and explicitly named
    fn raw(&self) -> Option<Option<&UnresolvedTy<'buf>>>;

    /// If the type is resolved as `t`, returns `Some(t)`.
    /// Returns `None` otherwise.
    fn resolved(&self) -> Option<&ResolvedTy<'buf>>;

    fn is_resolved(&self) -> bool;
}

impl<'buf> private::Sealed for Option<Ty<'buf>> {}

impl<'buf> TyExt<'buf> for Option<Ty<'buf>> {
    fn raw(&self) -> Option<Option<&UnresolvedTy<'buf>>> {
        match self {
            None => Some(None),
            Some(Ty::Unresolved(ty)) => Some(Some(ty)),
            Some(Ty::Resolved(_)) => None,
        }
    }

    fn resolved(&self) -> Option<&ResolvedTy<'buf>> {
        self.as_ref().and_then(Ty::resolved)
    }

    fn is_resolved(&self) -> bool {
        self.as_ref().is_resolved()
    }
}

impl<'buf> private::Sealed for Option<&Ty<'buf>> {}

impl<'buf> TyExt<'buf> for Option<&Ty<'buf>> {
    fn raw(&self) -> Option<Option<&UnresolvedTy<'buf>>> {
        match self {
            None => Some(None),
            Some(Ty::Unresolved(ty)) => Some(Some(ty)),
            Some(Ty::Resolved(_)) => None,
        }
    }

    fn resolved(&self) -> Option<&ResolvedTy<'buf>> {
        self.and_then(Ty::resolved)
    }

    fn is_resolved(&self) -> bool {
        self.map(Ty::is_resolved).unwrap_or(false)
    }
}

impl<'buf> private::Sealed for Ty<'buf> {}

impl<'buf> TyExt<'buf> for Ty<'buf> {
    fn raw(&self) -> Option<Option<&UnresolvedTy<'buf>>> {
        match self {
            Ty::Unresolved(ty) => Some(Some(ty)),
            Ty::Resolved(_) => None,
        }
    }

    fn resolved(&self) -> Option<&ResolvedTy<'buf>> {
        try_match!(self, Self::Resolved(ty) => ty)
    }

    fn is_resolved(&self) -> bool {
        matches!(self, Self::Resolved(_))
    }
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolvedTy<'buf> {
    Builtin(BuiltinClass),

    /// A `SELF_TYPE` occuring in the body of `enclosed`.
    SelfType {
        enclosed: Box<ResolvedTy<'buf>>,
    },

    /// A user-defined class.
    Class(
        /// The name of the class.
        #[serde(serialize_with = "serialize_bytes_as_string")]
        Cow<'buf, [u8]>,
    ),

    Function(FunctionTy<'buf>),

    /// The bottom type, a subclass of every type.
    ///
    /// A valid Cool program has no expression of this type;
    /// it only occurs as a result of a typing error.
    Bottom,

    /// An expression that cannot have a valid type in Col.
    ///
    /// How this is different from...
    /// - ...the top type: Untyped allows use in any covariant context
    /// - ...the bottom type: Untyped alllows use in any contravariant context
    ///
    /// An untyped expression can also be used in an invariant position.
    ///
    /// A valid Cool program has not expression of this type;
    /// it only occurs as a result of a typing error.
    Untyped,
}

impl CloneStatic<ResolvedTy<'static>> for ResolvedTy<'_> {
    fn clone_static(&self) -> ResolvedTy<'static> {
        match self {
            Self::Builtin(builtin) => ResolvedTy::Builtin(*builtin),
            Self::SelfType { enclosed } => ResolvedTy::SelfType {
                enclosed: Box::new(enclosed.clone_static()),
            },
            Self::Class(name) => ResolvedTy::Class(Cow::Owned(name.clone().into_owned())),
            Self::Function(ty) => ty.clone_static().into(),
            Self::Bottom => ResolvedTy::Bottom,
            Self::Untyped => ResolvedTy::Untyped,
        }
    }
}

impl Display for ResolvedTy<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Builtin(builtin) => builtin.fmt(f),
            Self::SelfType { enclosed } => write!(f, "SELF_TYPE[{}]", enclosed),
            Self::Class(name) => write!(f, "{}", slice_formatter(name)),
            Self::Function(ty) => ty.fmt(f),
            Self::Bottom => write!(f, "⊥"),
            Self::Untyped => write!(f, "{{invalid}}"),
        }
    }
}

/// A built-in class type.
#[derive(Serialize, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinClass {
    Int,
    String,
    Bool,

    /// The top type, a superclass of every type.
    Object,

    IO,
}

impl Display for BuiltinClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Int => "Int",
                Self::String => "String",
                Self::Bool => "Bool",
                Self::Object => "Object",
                Self::IO => "IO",
            }
        )
    }
}

impl<'buf> From<BuiltinClass> for ResolvedTy<'buf> {
    fn from(builtin: BuiltinClass) -> ResolvedTy<'buf> {
        ResolvedTy::Builtin(builtin)
    }
}

/// A function type, used to represent method types.
#[derive(Serialize, Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionTy<'buf> {
    pub params: Vec<ResolvedTy<'buf>>,
    pub ret: Box<ResolvedTy<'buf>>,
}

impl CloneStatic<FunctionTy<'static>> for FunctionTy<'_> {
    fn clone_static(&self) -> FunctionTy<'static> {
        FunctionTy {
            params: self.params.iter().map(ResolvedTy::clone_static).collect(),
            ret: Box::new(self.ret.clone_static()),
        }
    }
}

impl Display for FunctionTy<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, param_ty) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }

            write!(f, "{}", param_ty)?;
        }

        write!(f, " → {}", &self.ret)
    }
}

impl<'buf> From<FunctionTy<'buf>> for ResolvedTy<'buf> {
    fn from(ty: FunctionTy<'buf>) -> ResolvedTy<'buf> {
        ResolvedTy::Function(ty)
    }
}

impl<'buf> From<FunctionTy<'buf>> for Ty<'buf> {
    fn from(ty: FunctionTy<'buf>) -> Ty<'buf> {
        ResolvedTy::from(ty).into()
    }
}

pub trait HasTy<'buf> {
    fn ty(&self) -> Option<Cow<'_, Ty<'buf>>>;
}

pub trait HasExplicitTy<'buf> {
    fn explicit_ty(&self) -> Cow<'_, Ty<'buf>>;
}

impl<'buf, T: HasExplicitTy<'buf>> HasTy<'buf> for T {
    fn ty(&self) -> Option<Cow<'_, Ty<'buf>>> {
        Some(self.explicit_ty())
    }
}

impl<'buf> HasExplicitTy<'buf> for Ty<'buf> {
    fn explicit_ty(&self) -> Cow<'_, Ty<'buf>> {
        Cow::Borrowed(self)
    }
}

pub trait UnwrapResolvedTy<'buf>: HasTy<'buf> {
    fn unwrap_res_ty(&self) -> Cow<'_, ResolvedTy<'buf>>;
}

impl<'buf, T: HasTy<'buf>> UnwrapResolvedTy<'buf> for T {
    fn unwrap_res_ty(&self) -> Cow<'_, ResolvedTy<'buf>> {
        let resolved = match self
            .ty()
            .expect("unwrap_res_ty called on an uninferred type")
        {
            Cow::Borrowed(borrowed) => borrowed.resolved().map(Cow::Borrowed),
            Cow::Owned(owned) => match owned {
                Ty::Resolved(resolved) => Some(Cow::Owned(resolved)),
                Ty::Unresolved(_) => None,
            },
        };

        resolved.expect("unwrap_res_ty called on an unresolved type")
    }
}

mod private {
    pub trait Sealed {}
}
