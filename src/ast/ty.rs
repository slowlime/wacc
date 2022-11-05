use std::borrow::Cow;

use crate::try_match;

use super::TyName;

/// A potentially-unresolved type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ty<'buf> {
    Unresolved(UnresolvedTy<'buf>),
    Resolved(ResolvedTy<'buf>),
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnresolvedTy<'buf> {
    Named(TyName<'buf>),

    Function {
        args: Vec<Ty<'buf>>,
        ret: Box<Ty<'buf>>,
    },

    SelfType,
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
        self.as_ref().map(Ty::is_resolved).unwrap_or(false)
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolvedTy<'buf> {
    Builtin(BuiltinClass),

    /// A `SELF_TYPE` occuring in the body of `enclosed`.
    SelfType {
        enclosed: Box<ResolvedTy<'buf>>,
    },

    /// A user-defined class.
    Class(
        /// The name of the class.
        &'buf [u8],
    ),

    Function(FunctionTy<'buf>),

    /// The bottom type, a subclass of every type.
    ///
    /// A valid Cool program has no expression of this type;
    /// it only occurs as a result of a typing error.
    Bottom,
}

/// A built-in class type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BuiltinClass {
    Int,
    String,
    Bool,

    /// The top type, a superclass of every type.
    Object,
}

/// A function type, used to represent method types.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionTy<'buf> {
    args: Vec<ResolvedTy<'buf>>,
    ret: Box<ResolvedTy<'buf>>,
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

mod private {
    pub trait Sealed {}
}
