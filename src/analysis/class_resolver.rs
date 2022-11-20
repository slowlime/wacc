use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::iter::successors;

use itertools::{Either, Itertools};

use crate::analysis::error::{
    IllegalSelfTypePosition, MultipleDefinitionKind, TypeckError, UnrecognizedTy,
    UnrecognizedTyPosition,
};
use crate::analysis::typeck::{SelfTypeAllowed, Variance};
use crate::analysis::typectx::{ClassIndex, ClassName, DefinitionLocation, TypeCtx};
use crate::ast::ty::{BuiltinClass, FunctionTy, ResolvedTy};
use crate::ast::{self, Class, Name, TyName};
use crate::errors::Diagnostics;
use crate::position::HasSpan;
use crate::util::CloneStatic;

fn builtin_ctx() -> TypeCtx<'static> {
    let mut ctx = TypeCtx::empty();

    ctx.add_class(
        &b"Object"[..],
        ClassIndex::new(BuiltinClass::Object.into(), None).with_methods([
            (
                b"abort"[..].into(),
                BuiltinClass::Object.into(),
                FunctionTy {
                    params: vec![],
                    ret: Box::new(BuiltinClass::Object.into()),
                },
            ),
            (
                b"type_name"[..].into(),
                BuiltinClass::Object.into(),
                FunctionTy {
                    params: vec![],
                    ret: Box::new(BuiltinClass::String.into()),
                },
            ),
            (
                b"copy"[..].into(),
                BuiltinClass::Object.into(),
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
        &b"IO"[..],
        ClassIndex::new(BuiltinClass::IO.into(), Some(BuiltinClass::Object.into())).with_methods([
            (
                b"out_string"[..].into(),
                BuiltinClass::IO.into(),
                FunctionTy {
                    params: vec![BuiltinClass::String.into()],
                    ret: Box::new(ResolvedTy::SelfType {
                        enclosed: Box::new(BuiltinClass::IO.into()),
                    }),
                },
            ),
            (
                b"out_int"[..].into(),
                BuiltinClass::IO.into(),
                FunctionTy {
                    params: vec![BuiltinClass::Int.into()],
                    ret: Box::new(ResolvedTy::SelfType {
                        enclosed: Box::new(BuiltinClass::IO.into()),
                    }),
                },
            ),
            (
                b"in_string"[..].into(),
                BuiltinClass::IO.into(),
                FunctionTy {
                    params: vec![],
                    ret: Box::new(BuiltinClass::String.into()),
                },
            ),
            (
                b"in_int"[..].into(),
                BuiltinClass::IO.into(),
                FunctionTy {
                    params: vec![],
                    ret: Box::new(BuiltinClass::Int.into()),
                },
            ),
        ]),
    );

    ctx.add_class(
        &b"Int"[..],
        ClassIndex::new(BuiltinClass::Int.into(), Some(BuiltinClass::Object.into())),
    );

    ctx.add_class(
        &b"String"[..],
        ClassIndex::new(
            BuiltinClass::String.into(),
            Some(BuiltinClass::Object.into()),
        )
        .with_methods([
            (
                b"length"[..].into(),
                BuiltinClass::String.into(),
                FunctionTy {
                    params: vec![],
                    ret: Box::new(BuiltinClass::Int.into()),
                },
            ),
            (
                b"concat"[..].into(),
                BuiltinClass::String.into(),
                FunctionTy {
                    params: vec![BuiltinClass::String.into()],
                    ret: Box::new(BuiltinClass::String.into()),
                },
            ),
            (
                b"substr"[..].into(),
                BuiltinClass::String.into(),
                FunctionTy {
                    params: vec![BuiltinClass::Int.into(); 2],
                    ret: Box::new(BuiltinClass::String.into()),
                },
            ),
        ]),
    );

    ctx.add_class(
        &b"Bool"[..],
        ClassIndex::new(BuiltinClass::Bool.into(), Some(BuiltinClass::Object.into())),
    );

    ctx
}

#[must_use]
struct TyNameCtxBuilder<'a, 'buf> {
    variance: Option<Variance>,
    allow_self_ty: Option<SelfTypeAllowed>,
    position: Option<UnrecognizedTyPosition<'buf>>,
    class_ty: Option<&'a ResolvedTy<'buf>>,
}

impl<'a, 'buf> TyNameCtxBuilder<'a, 'buf> {
    pub fn with_variance(self, variance: Variance) -> Self {
        Self {
            variance: Some(variance),
            ..self
        }
    }

    pub fn with_allow_self_ty(self, allow_self_ty: impl Into<SelfTypeAllowed>) -> Self {
        Self {
            allow_self_ty: Some(allow_self_ty.into()),
            ..self
        }
    }

    pub fn with_position(self, position: UnrecognizedTyPosition<'buf>) -> Self {
        Self {
            position: Some(position),
            ..self
        }
    }

    pub fn with_class_ty(self, class_ty: &'a ResolvedTy<'buf>) -> Self {
        Self {
            class_ty: Some(class_ty),
            ..self
        }
    }

    pub fn build(self) -> TyNameCtx<'a, 'buf> {
        TyNameCtx {
            variance: self.variance.expect("variance must be set"),
            allow_self_ty: self.allow_self_ty.expect("allow_self_ty must be set"),
            position: self.position.expect("position must be set"),
            class_ty: self.class_ty.expect("class_ty must be set"),
        }
    }
}

/// The context associated with an occurrence of a `TyName`.
struct TyNameCtx<'a, 'buf> {
    pub variance: Variance,
    pub allow_self_ty: SelfTypeAllowed,
    pub position: UnrecognizedTyPosition<'buf>,
    pub class_ty: &'a ResolvedTy<'buf>,
}

impl<'a, 'buf> TyNameCtx<'a, 'buf> {
    pub fn builder() -> TyNameCtxBuilder<'a, 'buf> {
        TyNameCtxBuilder {
            variance: None,
            allow_self_ty: None,
            position: None,
            class_ty: None,
        }
    }

    pub fn covariant() -> TyNameCtxBuilder<'a, 'buf> {
        Self::builder().with_variance(Variance::Covariant)
    }

    pub fn invariant() -> TyNameCtxBuilder<'a, 'buf> {
        Self::builder().with_variance(Variance::Invariant)
    }
}

pub(super) struct ClassResolverResult<'buf, 'cls> {
    pub unrecognized_tys: Vec<UnrecognizedTy<'buf>>,
    pub ctx: TypeCtx<'buf>,
    pub excluded: HashSet<&'cls TyName<'buf>>,
}

pub(super) struct ClassResolver<'dia, 'emt, 'buf, 'cls> {
    diagnostics: RefCell<&'dia mut Diagnostics<'emt>>,
    unrecognized_tys: Vec<UnrecognizedTy<'buf>>,
    classes: &'cls [Class<'buf>],
    class_names: HashSet<ClassName<'buf>>,
    ctx: TypeCtx<'buf>,
}

impl<'dia, 'emt, 'buf, 'cls> ClassResolver<'dia, 'emt, 'buf, 'cls> {
    pub fn new(diagnostics: &'dia mut Diagnostics<'emt>, classes: &'cls [Class<'buf>]) -> Self {
        let ctx = builtin_ctx();
        let class_names = classes
            .iter()
            .map(|class| ClassName::from(class.name.clone()))
            .chain(ctx.iter().map(|(k, _)| k.clone()))
            .collect::<HashSet<_>>();

        Self {
            diagnostics: RefCell::new(diagnostics),
            unrecognized_tys: vec![],
            classes,
            class_names,
            ctx,
        }
    }

    pub fn resolve(mut self) -> ClassResolverResult<'buf, 'cls> {
        let (indexes, mut excluded): (Vec<_>, HashSet<_>) = self
            .classes
            .iter()
            .map(|class| self.resolve_class(class).ok_or(&class.name))
            .partition_result();

        // excluded must include span info
        let indexes = self
            .check_duplicate_classes(indexes, &mut excluded)
            .collect();
        self.add_to_ctx(indexes, &mut excluded);

        ClassResolverResult {
            unrecognized_tys: self.unrecognized_tys,
            ctx: self.ctx,
            excluded,
        }
    }

    fn check_duplicate_classes(
        &self,
        indexes: impl IntoIterator<Item = (&'cls TyName<'buf>, ClassIndex<'buf>)>,
        excluded: &mut HashSet<&'cls TyName<'buf>>,
    ) -> impl Iterator<Item = (&'cls TyName<'buf>, ClassIndex<'buf>)> {
        use std::collections::hash_map::Entry;

        let mut map = HashMap::new();

        for (ty_name, index) in indexes {
            match map.entry(ty_name.0.as_slice()) {
                Entry::Vacant(entry) => {
                    entry.insert((ty_name, index));
                }

                Entry::Occupied(entry) => {
                    self.diagnostics
                        .borrow_mut()
                        .error()
                        .with_span_and_error(TypeckError::MultipleClassDefinition {
                            ty_name: Box::new(ty_name.clone_static()),
                            previous: entry.get().1.location().clone(),
                        })
                        .emit();

                    excluded.insert(ty_name);
                }
            }
        }

        map.into_values()
    }

    /// Returns names of classes excluded from the context due to inheritance cycles.
    fn add_to_ctx(
        &mut self,
        ty_indexes: HashMap<&'cls TyName<'buf>, ClassIndex<'buf>>,
        ignored: &mut HashSet<&'cls TyName<'buf>>,
    ) {
        // perform a toposort on the indexes;
        // if a cycle is detected, remove the offending nodes and repeat

        struct Indexes<'cls, 'buf> {
            ty_indexes: HashMap<&'cls TyName<'buf>, ClassIndex<'buf>>,
            name_map: HashMap<ClassName<'buf>, &'cls TyName<'buf>>,
        }

        let name_map = ty_indexes
            .iter()
            .map(|(ty_name, _)| ty_name)
            .chain(ignored.iter())
            .map(|&ty_name| (ty_name.into(), ty_name))
            .collect::<HashMap<_, _>>();

        let mut indexes = Indexes {
            ty_indexes,
            name_map,
        };

        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        enum State {
            Unvisited,
            Discovered,
            Visited,
            Ignored,
        }

        use State::*;

        fn get_parent<'a, 'buf>(
            this: &'a ClassResolver<'_, '_, 'buf, '_>,
            indexes: &'a Indexes<'_, 'buf>,
            name: &'a ClassName<'buf>,
        ) -> Option<&'a ClassName<'buf>> {
            let index = this.ctx.get_class(name).or_else(|| {
                let ty_name = indexes
                    .name_map
                    .get(name)
                    .expect("all names have been resolved");

                indexes.ty_indexes.get(ty_name)
            });

            index.expect("all names have been resolved").parent()
        }

        fn handle_cycle<'a, 'buf, 'cls>(
            this: &ClassResolver<'_, '_, 'buf, 'cls>,
            offending: &'a ClassName<'buf>,
            ignored: &mut HashSet<&'cls TyName<'buf>>,
            indexes: &'a Indexes<'cls, 'buf>,
        ) {
            let cycle = successors(Some(offending), |node| {
                let parent = get_parent(this, indexes, node)
                    .expect("a class participating in the cycle must have a parent");

                Some(parent).filter(|&parent| parent != offending)
            })
            .collect::<Vec<_>>();

            let ty_name = this
                .classes
                .iter()
                .find_map(|class| {
                    (&ClassName::from(&class.name) == offending).then_some(&class.name)
                })
                .expect("built-in class do not participate in the inheritance cycle");

            for &name in &cycle {
                let ty_name = indexes
                    .name_map
                    .get(name)
                    .expect("all known type names are registered in the name map");
                ignored.insert(ty_name);
            }

            let cycle = cycle.into_iter().map(ClassName::clone_static).collect();

            this.diagnostics
                .borrow_mut()
                .error()
                .with_span_and_error(TypeckError::InheritanceCycle {
                    ty_name: Box::new(ty_name.clone_static()),
                    cycle,
                })
                .emit();
        }

        let sorted: Vec<ClassName<'buf>> = 'outer: loop {
            let mut visited = self
                .class_names
                .iter()
                .map(|name| {
                    if self.ctx.get_class(name).is_some() {
                        return (name.clone(), Unvisited);
                    }

                    let ty_name = indexes
                        .name_map
                        .get(name)
                        .expect("all known type names are registered in the name map");

                    (
                        name.clone(),
                        if ignored.contains(ty_name) {
                            Ignored
                        } else {
                            Unvisited
                        },
                    )
                })
                .collect::<HashMap<_, _>>();

            let mut sorted = vec![];

            for &ty_name in indexes.ty_indexes.keys() {
                let name = ty_name.into();
                let mut chain = vec![];

                match visited[&name] {
                    Ignored | Visited => continue,

                    Discovered => {
                        unreachable!("Each DFS must traverse all nodes or reset the outer loop")
                    }

                    Unvisited => {
                        let mut stack = vec![];

                        fn discover<'buf>(
                            stack: &mut Vec<ClassName<'buf>>,
                            visited: &mut HashMap<ClassName<'buf>, State>,
                            name: ClassName<'buf>,
                        ) {
                            stack.push(name.clone());
                            visited.insert(name, Discovered);
                        }

                        discover(&mut stack, &mut visited, name);

                        while let Some(name) = stack.pop() {
                            match get_parent(self, &indexes, &name)
                                .map(|parent| (parent, visited[parent]))
                            {
                                None | Some((_, Ignored | Visited)) => {}
                                Some((parent, Unvisited)) => {
                                    discover(&mut stack, &mut visited, parent.clone())
                                }

                                Some((parent, Discovered)) => {
                                    handle_cycle(self, parent, ignored, &indexes);

                                    continue 'outer;
                                }
                            }

                            *visited.get_mut(&name).unwrap() = Visited;

                            if self.ctx.get_class(&name).is_none() {
                                chain.push(name.clone());
                            }
                        }
                    }
                }

                sorted.extend(chain.into_iter().rev());
            }

            break sorted;
        };

        for name in sorted {
            let ty_name = indexes
                .name_map
                .get(&name)
                .expect("toposort does not add new items");
            let (name, index) = indexes
                .ty_indexes
                .remove_entry(ty_name)
                .expect("toposort does not add new names");
            self.ctx.add_class(name, index);
        }
    }

    /// Resolves a type name using the list of available classes.
    ///
    /// If the name is not found, adds it to the list of unrecognized types and returns:
    /// - `ResolvedTy::Bottom` if the type occurs in a covariant position
    /// - `ResolvedTy::Builtin(BuiltinClass::Object)` if the type occurs in a contravariant position
    /// - `ResolvedTy::Untyped` if the type occurs in an invariant position
    fn resolve_ty_name<'a>(
        &mut self,
        ty_name: &TyName<'buf>,
        ty_name_ctx: TyNameCtx<'a, 'buf>,
    ) -> ResolvedTy<'buf> {
        let class_name: ClassName<'buf> = ty_name.clone().into();

        match class_name {
            ClassName::Builtin(builtin) => return builtin.into(),

            ClassName::SelfType => match ty_name_ctx.allow_self_ty {
                SelfTypeAllowed::Yes => {
                    return ResolvedTy::SelfType {
                        enclosed: Box::new(ty_name_ctx.class_ty.clone()),
                    };
                }

                SelfTypeAllowed::No(position) => {
                    self.diagnostics
                        .borrow_mut()
                        .error()
                        .with_span_and_error(TypeckError::IllegalSelfType {
                            ty_name: Box::new(ty_name.clone_static()),
                            position,
                        })
                        .emit();
                }
            },

            ClassName::Named(name) if self.class_names.contains(&class_name) => {
                return ResolvedTy::Class(name)
            }

            ClassName::Named(_) => {
                self.unrecognized_tys.push(UnrecognizedTy {
                    ty_name: ty_name.clone_static(),
                    class_name: ty_name_ctx
                        .class_ty
                        .try_into()
                        .expect("class_ty is a class type"),
                    position: ty_name_ctx.position,
                });
            }
        };

        // the match above has emitted an error
        match ty_name_ctx.variance {
            Variance::Covariant => ResolvedTy::Bottom,
            Variance::Contravariant => BuiltinClass::Object.into(),
            Variance::Invariant => ResolvedTy::Untyped,
        }
    }

    fn check_duplicate_definitions<'a, T>(
        &mut self,
        items: impl IntoIterator<Item = (&'a Name<'buf>, DefinitionLocation, T)>,
        kind: MultipleDefinitionKind,
    ) -> impl Iterator<Item = (Cow<'buf, [u8]>, DefinitionLocation, T)> + 'a
    where
        'buf: 'a,
        T: 'a,
    {
        use std::collections::hash_map::Entry;

        struct NameKey<'a, 'buf>(&'a Name<'buf>);

        impl PartialEq for NameKey<'_, '_> {
            fn eq(&self, other: &Self) -> bool {
                self.0.as_slice() == other.0.as_slice()
            }
        }

        impl Eq for NameKey<'_, '_> {}

        impl Hash for NameKey<'_, '_> {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                self.0.as_slice().hash(state);
            }
        }

        let mut map = HashMap::new();

        for (name, location, ty) in items {
            match map.entry(NameKey(name)) {
                Entry::Vacant(entry) => {
                    entry.insert((location, ty));
                }

                Entry::Occupied(entry) => {
                    self.diagnostics
                        .borrow_mut()
                        .error()
                        .with_span_and_error(TypeckError::MultipleDefinition {
                            kind,
                            name: Box::new(name.clone_static()),
                            previous: entry.get().0.clone(),
                        })
                        .emit();
                }
            }
        }

        map.into_iter()
            .map(|(NameKey(name), (location, ty))| (name.0.value.clone(), location, ty))
    }

    fn resolve_method<'a>(
        &mut self,
        class_ty: &ResolvedTy<'buf>,
        ast::Method {
            name,
            params,
            return_ty,
            ..
        }: &'a ast::Method<'buf>,
    ) -> (&'a Name<'buf>, DefinitionLocation, FunctionTy<'buf>) {
        let params = params
            .iter()
            .map(|ast::Formal { ty_name, .. }| {
                self.resolve_ty_name(
                    ty_name,
                    TyNameCtx::invariant()
                        .with_class_ty(class_ty)
                        .with_allow_self_ty(IllegalSelfTypePosition::Parameter)
                        .with_position(UnrecognizedTyPosition::Method(name.0.value.clone()))
                        .build(),
                )
            })
            .collect();
        let ret = Box::new(
            self.resolve_ty_name(
                return_ty,
                TyNameCtx::covariant()
                    .with_class_ty(class_ty)
                    .with_allow_self_ty(SelfTypeAllowed::Yes)
                    .with_position(UnrecognizedTyPosition::Method(name.0.value.clone()))
                    .build(),
            ),
        );

        (
            name,
            name.span().into_owned().into(),
            FunctionTy { params, ret },
        )
    }

    fn resolve_field<'a>(
        &mut self,
        class_ty: &ResolvedTy<'buf>,
        ast::Field(ast::Binding { name, ty_name, .. }): &'a ast::Field<'buf>,
    ) -> (&'a Name<'buf>, DefinitionLocation, ResolvedTy<'buf>) {
        let ty = self.resolve_ty_name(
            ty_name,
            TyNameCtx::invariant()
                .with_class_ty(class_ty)
                .with_allow_self_ty(SelfTypeAllowed::Yes)
                .with_position(UnrecognizedTyPosition::Field(name.0.value.clone()))
                .build(),
        );
        // ty can be Untyped

        (name, name.span().into_owned().into(), ty)
    }

    fn resolve_class<'a>(
        &mut self,
        class: &'a Class<'buf>,
    ) -> Option<(&'a TyName<'buf>, ClassIndex<'buf>)> {
        let class_ty = self.resolve_ty_name(
            &class.name,
            TyNameCtx::invariant()
                .with_class_ty(&ResolvedTy::Untyped)
                .with_allow_self_ty(IllegalSelfTypePosition::ClassName)
                .with_position(UnrecognizedTyPosition::Inherits)
                .build(),
        );

        if class_ty == ResolvedTy::Untyped {
            return None;
        }

        let parent = match &class.inherits {
            None => BuiltinClass::Object.into(),
            Some(parent) => {
                let ty = self.resolve_ty_name(
                    parent,
                    TyNameCtx::invariant()
                        .with_class_ty(&class_ty)
                        .with_allow_self_ty(IllegalSelfTypePosition::Inherits)
                        .with_position(UnrecognizedTyPosition::Inherits)
                        .build(),
                );

                if ty == ResolvedTy::Untyped {
                    return None;
                }

                ty
            }
        };

        let parent = parent
            .try_into()
            .expect("the parent class has a valid name");
        let mut index = ClassIndex::new(class.span().into_owned().into(), Some(parent));

        let (fields, methods): (Vec<_>, Vec<_>) =
            class
                .features
                .iter()
                .partition_map(|feature| match feature {
                    ast::Feature::Field(field) => Either::Left(field),
                    ast::Feature::Method(method) => Either::Right(method),
                });

        let methods = methods
            .into_iter()
            .map(|method| self.resolve_method(&class_ty, method))
            .collect::<Vec<_>>();
        index.add_methods(self.check_duplicate_definitions(
            methods,
            MultipleDefinitionKind::Method { inherited: false },
        ));

        let fields = fields
            .into_iter()
            .map(|field| self.resolve_field(&class_ty, field))
            .collect::<Vec<_>>();
        index.add_fields(self.check_duplicate_definitions(
            fields,
            MultipleDefinitionKind::Field { inherited: false },
        ));

        Some((&class.name, index))
    }
}
