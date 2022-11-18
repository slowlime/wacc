use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::{hash_map, HashMap};
use std::fmt::{self, Display};
use std::iter::successors;

use indexmap::IndexMap;
use itertools::Itertools;

use crate::ast::ty::{BuiltinClass, FunctionTy, ResolvedTy, Ty, UnresolvedTy};
use crate::ast::TyName;
use crate::position::Span;
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

impl<'buf> TryFrom<&ResolvedTy<'buf>> for ClassName<'buf> {
    type Error = ();

    fn try_from(ty: &ResolvedTy<'buf>) -> Result<Self, ()> {
        match ty {
            ResolvedTy::Builtin(builtin) => Ok((*builtin).into()),
            ResolvedTy::Class(name) => Ok(Self::Named(name.clone())),
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
    location: DefinitionLocation,
    methods: HashMap<Cow<'buf, [u8]>, (DefinitionLocation, FunctionTy<'buf>)>,
    fields: IndexMap<Cow<'buf, [u8]>, (DefinitionLocation, ResolvedTy<'buf>)>,
}

impl<'buf> ClassIndex<'buf> {
    pub fn new(location: DefinitionLocation, parent: Option<ClassName<'buf>>) -> Self {
        Self {
            parent,
            location,
            methods: HashMap::new(),
            fields: IndexMap::new(),
        }
    }

    pub fn with_methods(
        mut self,
        iter: impl IntoIterator<Item = (Cow<'buf, [u8]>, DefinitionLocation, FunctionTy<'buf>)>,
    ) -> Self {
        self.add_methods(iter);

        self
    }

    pub fn with_fields(
        mut self,
        iter: impl IntoIterator<Item = (Cow<'buf, [u8]>, DefinitionLocation, ResolvedTy<'buf>)>,
    ) -> Self {
        self.add_fields(iter);

        self
    }

    pub fn add_method(
        &mut self,
        name: Cow<'buf, [u8]>,
        location: DefinitionLocation,
        ty: FunctionTy<'buf>,
    ) {
        match self.methods.entry(name) {
            hash_map::Entry::Occupied(entry) => {
                panic!(
                    "the method {} has already been added",
                    slice_formatter(entry.key()),
                );
            }

            hash_map::Entry::Vacant(entry) => {
                entry.insert((location, ty));
            }
        }
    }

    pub fn add_methods(
        &mut self,
        iter: impl IntoIterator<Item = (Cow<'buf, [u8]>, DefinitionLocation, FunctionTy<'buf>)>,
    ) {
        for (name, location, ty) in iter {
            self.add_method(name, location, ty);
        }
    }

    pub fn add_field(
        &mut self,
        name: Cow<'buf, [u8]>,
        location: DefinitionLocation,
        ty: ResolvedTy<'buf>,
    ) {
        assert_ne!(&*name, &b"self"[..]);

        match self.fields.entry(name) {
            indexmap::map::Entry::Occupied(entry) => {
                panic!(
                    "the field {} has already been added",
                    slice_formatter(entry.key()),
                );
            }

            indexmap::map::Entry::Vacant(entry) => {
                entry.insert((location, ty));
            }
        }
    }

    pub fn add_fields(
        &mut self,
        iter: impl IntoIterator<Item = (Cow<'buf, [u8]>, DefinitionLocation, ResolvedTy<'buf>)>,
    ) {
        for (name, location, ty) in iter {
            self.add_field(name, location, ty);
        }
    }

    pub fn get_method_ty(&self, name: &[u8]) -> Option<(&DefinitionLocation, &FunctionTy<'buf>)> {
        self.methods.get(name).map(|(location, ty)| (location, ty))
    }

    pub fn get_field_ty(&self, name: &[u8]) -> Option<(&DefinitionLocation, &ResolvedTy<'buf>)> {
        self.fields.get(name).map(|(location, ty)| (location, ty))
    }

    /// Returns whether the definition of `lhs` precedes `rhs`.
    ///
    /// Panics if either of the names are not present in the index.
    pub fn field_def_precedes(&self, lhs: &[u8], rhs: &[u8]) -> bool {
        let (index_lhs, _, _) = self
            .fields
            .get_full(lhs)
            .expect("lhs is present in the index");
        let (index_rhs, _, _) = self
            .fields
            .get_full(rhs)
            .expect("rhs is present in the index");

        index_lhs < index_rhs
    }

    pub fn location(&self) -> &DefinitionLocation {
        &self.location
    }

    pub fn parent(&self) -> Option<&ClassName<'buf>> {
        self.parent.as_ref()
    }

    pub fn methods(&self) -> impl Iterator<Item = (&Cow<'buf, [u8]>, &DefinitionLocation, &FunctionTy<'buf>)> {
        self.methods.iter().map(|(name, (location, ty))| (name, location, ty))
    }

    pub fn fields(&self) -> impl Iterator<Item = (&Cow<'buf, [u8]>, &DefinitionLocation, &ResolvedTy<'buf>)> {
        self.fields.iter().map(|(name, (location, ty))| (name, location, ty))
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
    ) -> Option<(
        &'a ClassName<'buf>,
        &'a DefinitionLocation,
        &'a FunctionTy<'buf>,
    )> {
        self.inheritance_chain(class_name)
            .find_map(|(name, index)| {
                index
                    .get_method_ty(method)
                    .map(|(location, ty)| (name, location, ty))
            })
    }

    pub fn get_field_ty<'a>(
        &'a self,
        class_name: impl Into<&'a ClassName<'buf>>,
        field: &[u8],
    ) -> Option<(
        &'a ClassName<'buf>,
        &'a DefinitionLocation,
        &'a ResolvedTy<'buf>,
    )> {
        self.inheritance_chain(class_name)
            .find_map(|(name, index)| {
                index
                    .get_field_ty(field)
                    .map(|(location, ty)| (name, location, ty))
            })
    }

    pub fn is_subtype<'a>(
        &'a self,
        lhs: impl Into<&'a ClassName<'buf>>,
        rhs: impl Into<&'a ClassName<'buf>>,
    ) -> bool {
        self.inheritance_chain(lhs)
            .map(|(name, _)| name)
            .contains(rhs.into())
    }

    pub fn subtype_order<'a>(
        &'a self,
        lhs: impl Into<&'a ClassName<'buf>>,
        rhs: impl Into<&'a ClassName<'buf>>,
    ) -> Option<Ordering> {
        let lhs = lhs.into();
        let rhs = rhs.into();

        Some(if lhs == rhs {
            Ordering::Equal
        } else if self.is_subtype(lhs, rhs) {
            Ordering::Less
        } else if self.is_subtype(rhs, lhs) {
            Ordering::Greater
        } else {
            return None;
        })
    }

    pub fn field_def_order(
        &self,
        lhs: (&ClassName<'buf>, &[u8]),
        rhs: (&ClassName<'buf>, &[u8]),
    ) -> Option<Ordering> {
        let (lhs_class, lhs_field) = lhs;
        let (rhs_class, rhs_field) = rhs;
        let lhs_index = self
            .get_class(lhs_class)
            .expect("the class name of lhs is present in the index");
        self.get_class(rhs_class)
            .expect("the class name of rhs is present in the index");

        if lhs_class == rhs_class {
            Some(if lhs_field == rhs_field {
                Ordering::Equal
            } else if lhs_index.field_def_precedes(lhs_field, rhs_field) {
                Ordering::Less
            } else {
                Ordering::Greater
            })
        } else {
            self.subtype_order(lhs_class, rhs_class)
        }
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

    pub fn resolve(&self, name: &[u8]) -> Option<&Binding<'buf>> {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BindErrorKind<'a, 'buf> {
    DoubleDefinition { previous: &'a Binding<'buf> },

    BindingToSelf,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BindError<'a, 'buf> {
    pub kind: BindErrorKind<'a, 'buf>,
    pub name: Cow<'buf, [u8]>,
}

impl Display for BindError<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            BindErrorKind::DoubleDefinition { .. } => {
                write!(
                    f,
                    "the name `{}` is already bound",
                    slice_formatter(&self.name)
                )
            }

            BindErrorKind::BindingToSelf => {
                write!(f, "cannot bind to `self`")
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindingKind {
    Field { inherited: bool },

    Parameter,
    Local,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DefinitionLocation {
    UserCode(Span),
    Synthetic(BuiltinClass),
}

impl From<Span> for DefinitionLocation {
    fn from(span: Span) -> Self {
        Self::UserCode(span)
    }
}

impl From<BuiltinClass> for DefinitionLocation {
    fn from(builtin: BuiltinClass) -> Self {
        Self::Synthetic(builtin)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binding<'buf> {
    pub kind: BindingKind,
    pub ty: ResolvedTy<'buf>,
    pub location: DefinitionLocation,
}

#[derive(Debug, Clone)]
pub struct BindingScope<'buf> {
    local_bindings: HashMap<Cow<'buf, [u8]>, Binding<'buf>>,
}

impl<'buf> BindingScope<'buf> {
    pub fn new() -> Self {
        Self {
            local_bindings: HashMap::new(),
        }
    }

    pub fn get(&self, name: &[u8]) -> Option<&Binding<'buf>> {
        self.local_bindings.get(name)
    }

    pub fn bind(
        &mut self,
        name: Cow<'buf, [u8]>,
        binding: Binding<'buf>,
    ) -> Result<Option<Binding<'buf>>, BindError<'_, 'buf>> {
        if &*name == b"self" {
            Err(BindError {
                kind: BindErrorKind::BindingToSelf,
                name,
            })
        } else {
            Ok(self.local_bindings.insert(name, binding))
        }
    }

    pub fn bind_if_empty(
        &mut self,
        name: Cow<'buf, [u8]>,
        binding: Binding<'buf>,
    ) -> Result<(), BindError<'_, 'buf>> {
        if self.local_bindings.contains_key(&name) {
            let previous = self.local_bindings.get(&name).unwrap();

            Err(BindError {
                kind: BindErrorKind::DoubleDefinition { previous },
                name,
            })
        } else {
            self.bind(name, binding).map(drop)
        }
    }
}
