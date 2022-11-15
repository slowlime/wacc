use std::borrow::{Borrow, Cow};
use std::collections::{HashMap, HashSet};
use std::iter::successors;

use itertools::{Either, Itertools};

use crate::analysis::error::{IllegalSelfTypePosition, TypeckError, UnrecognizedTy, UnrecognizedTyPosition};
use crate::analysis::typeck::{SelfTypeAllowed, Variance};
use crate::analysis::typectx::{ClassIndex, ClassName, DefinitionLocation, TypeCtx};
use crate::ast::ty::{BuiltinClass, FunctionTy, ResolvedTy};
use crate::ast::{self, Class, TyName};
use crate::errors::Diagnostics;
use crate::position::HasSpan;
use crate::util::CloneStatic;

fn builtin_ctx() -> TypeCtx<'static> {
    let mut ctx = TypeCtx::empty();

    ctx.add_class(
        &b"Object"[..],
        ClassIndex::new(None).with_methods([
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
        ClassIndex::new(Some(BuiltinClass::Object.into())).with_methods([
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
        ClassIndex::new(Some(BuiltinClass::Object.into())),
    );

    ctx.add_class(
        &b"String"[..],
        ClassIndex::new(Some(BuiltinClass::Object.into())).with_methods([
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
        ClassIndex::new(Some(BuiltinClass::Object.into())),
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

    pub fn contravariant() -> TyNameCtxBuilder<'a, 'buf> {
        Self::builder().with_variance(Variance::Contravariant)
    }

    pub fn covariant() -> TyNameCtxBuilder<'a, 'buf> {
        Self::builder().with_variance(Variance::Covariant)
    }

    pub fn invariant() -> TyNameCtxBuilder<'a, 'buf> {
        Self::builder().with_variance(Variance::Invariant)
    }
}

pub(super) struct ClassResolverResult<'buf> {
    pub unrecognized_tys: Vec<UnrecognizedTy<'buf>>,
    pub ctx: TypeCtx<'buf>,
    pub excluded: HashSet<ClassName<'buf>>,
}

pub(super) struct ClassResolver<'dia, 'buf, 'cls> {
    diagnostics: &'dia mut Diagnostics,
    unrecognized_tys: Vec<UnrecognizedTy<'buf>>,
    classes: &'cls [Class<'buf>],
    class_names: HashSet<ClassName<'buf>>,
    ctx: TypeCtx<'buf>,
}

impl<'dia, 'buf, 'cls> ClassResolver<'dia, 'buf, 'cls> {
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

    pub fn resolve(mut self) -> ClassResolverResult<'buf> {
        let (indexes, mut excluded): (HashMap<_, _>, HashSet<_>) = self
            .classes
            .into_iter()
            .map(|class| self.resolve_class(class).ok_or(class.name.borrow().into()))
            .partition_result();

        excluded.extend(self.add_to_ctx(indexes));

        ClassResolverResult {
            unrecognized_tys: self.unrecognized_tys,
            ctx: self.ctx,
            excluded,
        }
    }

    /// Returns names of clases excluded from the context due to inheritance cycles.
    fn add_to_ctx(
        &mut self,
        mut indexes: HashMap<ClassName<'buf>, ClassIndex<'buf>>,
    ) -> HashSet<ClassName<'buf>> {
        // perform a toposort on the indexes;
        // if a cycle is detected, remove the offending nodes and repeat

        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        enum State {
            Unvisited,
            Discovered,
            Visited,
            Ignored,
        }

        use State::*;

        fn get_parent<'a, 'buf>(
            indexes: &'a HashMap<ClassName<'buf>, ClassIndex<'buf>>,
            name: &ClassName<'buf>,
        ) -> Option<&'a ClassName<'buf>> {
            indexes
                .get(name)
                .expect("all names have been resolved")
                .parent()
        }

        fn handle_cycle<'a, 'buf>(
            this: &mut ClassResolver<'_, 'buf, '_>,
            offending: &'a ClassName<'buf>,
            ignored: &mut HashSet<ClassName<'buf>>,
            indexes: &'a HashMap<ClassName<'buf>, ClassIndex<'buf>>,
        ) {
            let cycle = successors(Some(offending), |node| {
                let parent = get_parent(&indexes, node)
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
                ignored.insert(name.clone());
            }

            let cycle = cycle.into_iter().map(ClassName::clone_static).collect();

            this.diagnostics
                .error()
                .with_span_and_error(TypeckError::InheritanceCycle {
                    ty_name: ty_name.clone_static(),
                    cycle,
                })
                .emit();
        }

        let mut ignored: HashSet<ClassName> = HashSet::new();

        let sorted: Vec<ClassName> = 'outer: loop {
            let mut visited = self
                .class_names
                .iter()
                .map(|name| {
                    (
                        name,
                        if ignored.contains(name) {
                            Ignored
                        } else {
                            Unvisited
                        },
                    )
                })
                .collect::<HashMap<_, _>>();

            let mut sorted = vec![];

            for name in indexes.keys() {
                match visited[name] {
                    Ignored | Visited => continue,

                    Discovered => {
                        unreachable!("Each DFS must traverse all nodes or reset the outer loop")
                    }

                    Unvisited => {
                        let mut stack = vec![];

                        fn discover<'a, 'buf>(
                            stack: &mut Vec<&'a ClassName<'buf>>,
                            visited: &mut HashMap<&'a ClassName<'buf>, State>,
                            name: &'a ClassName<'buf>,
                        ) {
                            stack.push(name);
                            visited.insert(name, Discovered);
                        }

                        discover(&mut stack, &mut visited, name);

                        while let Some(name) = stack.pop() {
                            match get_parent(&indexes, name).map(|parent| (parent, visited[parent]))
                            {
                                None | Some((_, Ignored | Visited)) => {}
                                Some((parent, Unvisited)) => {
                                    discover(&mut stack, &mut visited, parent)
                                }

                                Some((parent, Discovered)) => {
                                    handle_cycle(self, parent, &mut ignored, &indexes);

                                    continue 'outer;
                                }
                            }

                            sorted.push(name);
                            visited.insert(name, Visited);
                        }
                    }
                }
            }

            break sorted.into_iter().cloned().collect();
        };

        for name in sorted {
            let (name, index) = indexes
                .remove_entry(&name)
                .expect("toposort does not add new names");
            self.ctx.add_class(name, index);
        }

        ignored
            .into_iter()
            .map(|name| {
                indexes
                    .remove_entry(&name)
                    .expect("ignored contains only known names")
                    .0
            })
            .collect()
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
                        .error()
                        .with_span_and_error(TypeckError::IllegalSelfType {
                            ty_name: ty_name.clone_static(),
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
                    class_name: ty_name_ctx.class_ty.try_into().expect("class_ty is a class type"),
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

    fn resolve_method(
        &mut self,
        class_ty: &ResolvedTy<'buf>,
        ast::Method {
            name,
            params,
            return_ty,
            ..
        }: &ast::Method<'buf>,
    ) -> (Cow<'buf, [u8]>, DefinitionLocation, FunctionTy<'buf>) {
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

        (name.0.value.clone(), name.span().into_owned().into(), FunctionTy { params, ret }.into())
    }

    fn resolve_field(
        &mut self,
        class_ty: &ResolvedTy<'buf>,
        ast::Field(ast::Binding { name, ty_name, .. }): &ast::Field<'buf>,
    ) -> (Cow<'buf, [u8]>, DefinitionLocation, ResolvedTy<'buf>) {
        let ty = self.resolve_ty_name(
            ty_name,
            TyNameCtx::invariant()
                .with_class_ty(class_ty)
                .with_allow_self_ty(SelfTypeAllowed::Yes)
                .with_position(UnrecognizedTyPosition::Field(name.0.value.clone()))
                .build(),
        );
        // ty can be Untyped

        (name.0.value.clone(), name.span().into_owned().into(), ty)
    }

    fn resolve_class(
        &mut self,
        class: &Class<'buf>,
    ) -> Option<(ClassName<'buf>, ClassIndex<'buf>)> {
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
        let mut index = ClassIndex::new(Some(parent));

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
            .map(|method| self.resolve_method(&class_ty, method));
        index.add_methods(methods);

        let fields = fields
            .into_iter()
            .map(|field| self.resolve_field(&class_ty, field));
        index.add_fields(fields);

        Some((class.name.clone().into(), index))
    }
}
