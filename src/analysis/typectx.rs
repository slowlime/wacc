use std::borrow::Cow;
use std::collections::{hash_map, HashMap};
use std::fmt::{self, Display};
use std::iter::successors;

use crate::ast::ty::{BuiltinClass, FunctionTy, ResolvedTy, Ty, UnresolvedTy};
use crate::ast::TyName;
use crate::util::slice_formatter;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ClassName<'buf> {
    Builtin(BuiltinClass),
    Named(Cow<'buf, [u8]>),
    SelfType,
}

impl ClassName<'_> {
    pub fn clone_static(&self) -> ClassName<'static> {
        match self {
            Self::Builtin(builtin) => ClassName::Builtin(*builtin),
            Self::Named(name) => ClassName::Named(Cow::Owned(name.clone().into_owned())),
            Self::SelfType => ClassName::SelfType,
        }
    }
}

impl Display for ClassName<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Builtin(builtin) => write!(f, "{}", builtin),
            Self::Named(name) => write!(f, "{}", slice_formatter(name)),
            Self::SelfType => write!(f, "SELF_TYPE"),
        }
    }
}

impl<'buf> From<Cow<'buf, [u8]>> for ClassName<'buf> {
    fn from(name: Cow<'buf, [u8]>) -> Self {
        match &*name {
            b"Int" => BuiltinClass::Int.into(),
            b"String" => BuiltinClass::String.into(),
            b"Bool" => BuiltinClass::Bool.into(),
            b"Object" => BuiltinClass::Object.into(),
            b"IO" => BuiltinClass::IO.into(),
            b"SELF_TYPE" => Self::SelfType,
            _ => Self::Named(name.into()),
        }
    }
}

impl<'buf> From<&'buf [u8]> for ClassName<'buf> {
    fn from(name: &'buf [u8]) -> Self {
        Cow::Borrowed(name).into()
    }
}

impl<'buf> From<BuiltinClass> for ClassName<'buf> {
    fn from(builtin: BuiltinClass) -> Self {
        Self::Builtin(builtin)
    }
}

impl<'buf> From<TyName<'buf>> for ClassName<'buf> {
    fn from(ty_name: TyName<'buf>) -> Self {
        ty_name.0 .0.value.into()
    }
}

impl<'buf> From<&TyName<'buf>> for ClassName<'buf> {
    fn from(ty_name: &TyName<'buf>) -> Self {
        ty_name.0 .0.value.clone().into()
    }
}

impl<'buf> TryFrom<ResolvedTy<'buf>> for ClassName<'buf> {
    type Error = ();

    fn try_from(ty: ResolvedTy<'buf>) -> Result<Self, ()> {
        match ty {
            ResolvedTy::Builtin(builtin) => Ok(builtin.into()),
            ResolvedTy::Class(name) => Ok(Self::Named(name)),
            ResolvedTy::SelfType { .. } => Ok(Self::SelfType),
            _ => Err(()),
        }
    }
}

impl<'buf> TryFrom<Ty<'buf>> for ClassName<'buf> {
    type Error = ();

    fn try_from(ty: Ty<'buf>) -> Result<Self, ()> {
        match ty {
            Ty::Unresolved(UnresolvedTy::Named(name)) => Ok(name.into()),
            Ty::Resolved(resolved_ty) => resolved_ty.try_into(),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ClassNameIsSelfType;

impl<'buf> TryFrom<ClassName<'buf>> for ResolvedTy<'buf> {
    type Error = ClassNameIsSelfType;

    fn try_from(name: ClassName<'buf>) -> Result<Self, ClassNameIsSelfType> {
        Ok(match name {
            ClassName::Builtin(builtin) => builtin.into(),
            ClassName::Named(name) => ResolvedTy::Class(name),
            ClassName::SelfType => return Err(ClassNameIsSelfType),
        })
    }
}

#[derive(Debug, Clone)]
pub struct ClassIndex<'buf> {
    parent: Option<ClassName<'buf>>,
    methods: HashMap<Cow<'buf, [u8]>, FunctionTy<'buf>>,
}

impl<'buf> ClassIndex<'buf> {
    pub fn new(parent: Option<ClassName<'buf>>) -> Self {
        Self {
            parent,
            methods: HashMap::new(),
        }
    }

    pub fn with_methods(
        mut self,
        iter: impl IntoIterator<Item = (Cow<'buf, [u8]>, FunctionTy<'buf>)>,
    ) -> Self {
        self.add_methods(iter);

        self
    }

    pub fn add_method(&mut self, name: Cow<'buf, [u8]>, ty: FunctionTy<'buf>) {
        match self.methods.entry(name) {
            hash_map::Entry::Occupied(entry) => {
                panic!(
                    "the method {} has already been added",
                    slice_formatter(entry.key()),
                );
            }

            hash_map::Entry::Vacant(entry) => {
                entry.insert(ty);
            }
        }
    }

    pub fn add_methods(
        &mut self,
        iter: impl IntoIterator<Item = (Cow<'buf, [u8]>, FunctionTy<'buf>)>,
    ) {
        for (name, ty) in iter {
            self.add_method(name, ty);
        }
    }

    pub fn get_method_ty(&self, name: &[u8]) -> Option<&FunctionTy<'buf>> {
        self.methods.get(name)
    }

    pub fn parent(&self) -> Option<&ClassName<'buf>> {
        self.parent.as_ref()
    }
}

#[derive(Debug, Clone)]
pub struct TypeCtx<'buf> {
    types: HashMap<ClassName<'buf>, ClassIndex<'buf>>,
}

impl<'buf> TypeCtx<'buf> {
    pub fn empty() -> Self {
        Self {
            types: HashMap::new(),
        }
    }

    pub fn add_class(&mut self, name: impl Into<ClassName<'buf>>, index: ClassIndex<'buf>) {
        let class_name = name.into();

        match self.types.entry(class_name) {
            hash_map::Entry::Occupied(entry) => {
                panic!("the class {} has already been added", entry.key());
            }

            hash_map::Entry::Vacant(entry) => {
                let class_name = entry.into_key();

                let parent_added = index
                    .parent
                    .as_ref()
                    .map(|parent| self.types.contains_key(parent))
                    .unwrap_or(true);

                if parent_added {
                    self.types.insert(class_name, index);
                } else {
                    panic!(
                        "the parent class {} of the class {} is not available in the type context",
                        index.parent.unwrap(),
                        class_name
                    );
                }
            }
        }
    }

    pub fn get_class<'a>(
        &'a self,
        name: impl Into<&'a ClassName<'buf>>,
    ) -> Option<&ClassIndex<'buf>> {
        let name = name.into();

        self.types.get(&name)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&ClassName<'buf>, &ClassIndex<'buf>)> {
        self.types.iter()
    }

    pub fn inheritance_chain<'a>(
        &'a self,
        class_name: impl Into<&'a ClassName<'buf>>,
    ) -> impl Iterator<Item = (&'a ClassName<'buf>, &'a ClassIndex<'buf>)> {
        const PRESENT_MESSAGE: &'static str =
            "the iterator only returns entries present in the typectx";

        let iter = successors(Some(class_name.into()), |name| {
            self.types.get(name).expect(PRESENT_MESSAGE).parent()
        });

        iter.map(|name| (name, self.get_class(name).expect(PRESENT_MESSAGE)))
    }

    pub fn get_method_ty<'a>(
        &'a self,
        class_name: impl Into<&'a ClassName<'buf>>,
        method: &[u8],
    ) -> Option<(&'a ClassName<'buf>, &'a FunctionTy<'buf>)> {
        self.inheritance_chain(class_name)
            .find_map(|(name, index)| index.get_method_ty(method).map(|ty| (name, ty)))
    }
}

#[derive(Debug, Clone)]
pub struct BindingMap<'buf> {
    scopes: Vec<BindingScope<'buf>>,
}

impl<'buf> BindingMap<'buf> {
    pub fn new() -> Self {
        Self {
            scopes: vec![BindingScope::new()],
        }
    }

    pub fn with_scope<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        self.scopes.push(BindingScope::new());
        let result = f(self);
        self.scopes.pop();

        result
    }

    pub fn resolve(&self, name: &[u8]) -> Option<&ResolvedTy<'buf>> {
        self.scopes
            .iter()
            .rev()
            .flat_map(|scope| scope.get(name))
            .next()
    }

    pub fn innermost(&self) -> &BindingScope<'buf> {
        self.scopes.last().unwrap()
    }

    pub fn innermost_mut(&mut self) -> &mut BindingScope<'buf> {
        self.scopes.last_mut().unwrap()
    }
}

#[derive(Debug, Clone)]
pub struct BindingScope<'buf> {
    // TODO: track spans
    local_bindings: HashMap<&'buf [u8], ResolvedTy<'buf>>,
}

impl<'buf> BindingScope<'buf> {
    pub fn new() -> Self {
        Self {
            local_bindings: HashMap::new(),
        }
    }

    pub fn get(&self, name: &[u8]) -> Option<&ResolvedTy<'buf>> {
        self.local_bindings.get(name)
    }

    pub fn bind(&mut self, name: &'buf [u8], ty: ResolvedTy<'buf>) -> Option<ResolvedTy<'buf>> {
        self.local_bindings.insert(name, ty)
    }
}
