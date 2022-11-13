use std::borrow::{Borrow, Cow};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt::{self, Display};
use std::iter::successors;

use itertools::Itertools;

use crate::analysis::typectx::{BindingMap, ClassIndex, TypeCtx};
use crate::ast::ty::{BuiltinClass, FunctionTy, ResolvedTy};
use crate::ast::{self, AstRecurse, Class, Name, TyName, VisitorMut};
use crate::errors::{DiagnosticMessage, Diagnostics};
use crate::position::HasSpan;
use crate::try_match;

use super::typectx::ClassName;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IllegalSelfTypePosition {
    ClassName,
    Inherits,
    Parameter,
}

impl Display for IllegalSelfTypePosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // NOTE: should make sense in the following context: `occured in the {}`
        write!(
            f,
            "{}",
            match self {
                Self::ClassName => "class name position",
                Self::Inherits => "inherits clause",
                Self::Parameter => "method parameter",
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

    BuiltinRedefined {
        ty_name: TyName<'static>,
        builtin: BuiltinClass,
    },

    InheritanceCycle {
        ty_name: TyName<'static>,
        cycle: Vec<ClassName<'static>>,
    },
}

impl Display for TypeckError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnrecognizedTy(ty_name) => {
                write!(f, "the type name `{}` is not recognized", ty_name)
            }

            Self::IllegalSelfType { position, .. } => {
                write!(f, "`SELF_TYPE` cannot occur in the {}", position)
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
        }
    }
}

impl Error for TypeckError {}

impl HasSpan for TypeckError {
    fn span(&self) -> Cow<'_, crate::position::Span> {
        match self {
            Self::UnrecognizedTy(ty_name) => ty_name.span(),
            Self::IllegalSelfType { ty_name, .. } => ty_name.span(),
            Self::BuiltinRedefined { ty_name, .. } => ty_name.span(),
            Self::InheritanceCycle { ty_name, .. } => ty_name.span(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Variance {
    Contravariant,
    Covariant,
    Invariant,
}

fn builtin_ctx() -> TypeCtx<'static> {
    let mut ctx = TypeCtx::empty();

    ctx.add_class(
        &b"Object"[..],
        ClassIndex::new(None).with_methods([
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
        &b"IO"[..],
        ClassIndex::new(Some(BuiltinClass::Object.into())).with_methods([
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

    ctx.add_class(
        &b"Int"[..],
        ClassIndex::new(Some(BuiltinClass::Object.into())),
    );

    ctx.add_class(
        &b"String"[..],
        ClassIndex::new(Some(BuiltinClass::Object.into())).with_methods([
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

    ctx.add_class(
        &b"Bool"[..],
        ClassIndex::new(Some(BuiltinClass::Object.into())),
    );

    ctx
}

enum SelfTypeAllowed<'a, 'buf> {
    Yes(&'a ResolvedTy<'buf>),
    No(IllegalSelfTypePosition),
}

impl<'a, 'buf> From<&'a ResolvedTy<'buf>> for SelfTypeAllowed<'a, 'buf> {
    fn from(ty: &'a ResolvedTy<'buf>) -> Self {
        Self::Yes(ty)
    }
}

impl<'a, 'buf> From<IllegalSelfTypePosition> for SelfTypeAllowed<'a, 'buf> {
    fn from(position: IllegalSelfTypePosition) -> Self {
        Self::No(position)
    }
}

struct TyNameCtxBuilder<'a, 'buf> {
    variance: Option<Variance>,
    self_type_allowed: Option<SelfTypeAllowed<'a, 'buf>>,
}

impl<'a, 'buf> TyNameCtxBuilder<'a, 'buf> {
    pub fn with_variance(self, variance: Variance) -> Self {
        Self {
            variance: Some(variance),
            ..self
        }
    }

    pub fn with_self_type_allowed(
        self,
        self_type_allowed: impl Into<SelfTypeAllowed<'a, 'buf>>,
    ) -> Self {
        Self {
            self_type_allowed: Some(self_type_allowed.into()),
            ..self
        }
    }

    pub fn build(self) -> TyNameCtx<'a, 'buf> {
        TyNameCtx {
            variance: self.variance.expect("variance must be set"),
            self_type_allowed: self
                .self_type_allowed
                .expect("self_type_allowed policy must be set"),
        }
    }
}

/// The context associated with an occurrence of a `TyName`.
struct TyNameCtx<'a, 'buf> {
    pub variance: Variance,
    pub self_type_allowed: SelfTypeAllowed<'a, 'buf>,
}

impl<'a, 'buf> TyNameCtx<'a, 'buf> {
    pub fn builder() -> TyNameCtxBuilder<'a, 'buf> {
        TyNameCtxBuilder {
            variance: None,
            self_type_allowed: None,
        }
    }

    pub fn contravariant() -> TyNameCtxBuilder<'a, 'buf> {
        TyNameCtxBuilder {
            variance: Some(Variance::Contravariant),
            self_type_allowed: None,
        }
    }

    pub fn covariant() -> TyNameCtxBuilder<'a, 'buf> {
        TyNameCtxBuilder {
            variance: Some(Variance::Covariant),
            self_type_allowed: None,
        }
    }

    pub fn invariant() -> TyNameCtxBuilder<'a, 'buf> {
        TyNameCtxBuilder {
            variance: Some(Variance::Invariant),
            self_type_allowed: None,
        }
    }
}

struct MethodResolverResult<'buf> {
    pub unrecognized_tys: Vec<TyName<'buf>>,
    pub ctx: TypeCtx<'buf>,
    pub excluded: HashSet<ClassName<'buf>>,
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
        let (indexes, mut excluded): (HashMap<_, _>, HashSet<_>) = self
            .classes
            .into_iter()
            .map(|class| {
                self.resolve_methods(class)
                    .ok_or(class.name.borrow().into())
            })
            .partition_result();

        excluded.extend(self.add_to_ctx(indexes));

        MethodResolverResult {
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
            this: &mut MethodResolver<'_, 'buf, '_>,
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
    /// - `Some(ResolvedTy::Bottom)` if the type occurs in a covariant position
    /// - `Some(ResolvedTy::Builtin(BuiltinClass::Object))` if the type occurs in a contravariant position
    /// - `None` if the type occurs in an invariant position
    fn resolve_ty_name<'a>(
        &mut self,
        ty_name: &TyName<'buf>,
        ty_name_ctx: TyNameCtx<'a, 'buf>,
    ) -> Option<ResolvedTy<'buf>> {
        let class_name: ClassName<'buf> = ty_name.clone().into();

        match class_name {
            ClassName::Builtin(builtin) => return Some(builtin.into()),

            ClassName::SelfType => match ty_name_ctx.self_type_allowed {
                SelfTypeAllowed::Yes(enclosed_class) => {
                    return Some(ResolvedTy::SelfType {
                        enclosed: Box::new(enclosed_class.clone()),
                    });
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
                return Some(ResolvedTy::Class(name))
            }

            ClassName::Named(_) => {
                self.unrecognized_tys.push(ty_name.clone());
            }
        }

        // the match above has emitted an error
        Some(match ty_name_ctx.variance {
            Variance::Covariant => ResolvedTy::Bottom,
            Variance::Contravariant => BuiltinClass::Object.into(),
            Variance::Invariant => return None,
        })
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
    ) -> (Cow<'buf, [u8]>, FunctionTy<'buf>) {
        let params = params
            .iter()
            .map(|ast::Formal { ty_name, .. }| {
                self.resolve_ty_name(
                    ty_name,
                    TyNameCtx::contravariant()
                        .with_self_type_allowed(IllegalSelfTypePosition::Parameter)
                        .build(),
                )
                .unwrap()
            })
            .collect();
        let ret = Box::new(
            self.resolve_ty_name(
                return_ty,
                TyNameCtx::covariant()
                    .with_self_type_allowed(class_ty)
                    .build(),
            )
            .unwrap(),
        );

        (name.0.value.clone(), FunctionTy { params, ret }.into())
    }

    fn resolve_methods(
        &mut self,
        class: &Class<'buf>,
    ) -> Option<(ClassName<'buf>, ClassIndex<'buf>)> {
        let Some(class_ty) = self
            .resolve_ty_name(
                &class.name,
                TyNameCtx::invariant()
                .with_self_type_allowed(IllegalSelfTypePosition::ClassName)
                .build()
            ) else { return None };

        let parent = match &class.inherits {
            None => BuiltinClass::Object.into(),
            Some(parent) => {
                let Some(ty) = self
                    .resolve_ty_name(
                        parent,
                        TyNameCtx::invariant()
                        .with_self_type_allowed(IllegalSelfTypePosition::Inherits)
                        .build()
                    ) else { return None };

                ty
            }
        };
        let parent = parent
            .try_into()
            .expect("the parent class has a valid name");

        let methods = class
            .features
            .iter()
            .filter_map(|feature| try_match!(feature, ast::Feature::Method(method) => method))
            .map(|method| self.resolve_method(&class_ty, method));
        let index = ClassIndex::new(Some(parent)).with_methods(methods);

        Some((class.name.clone().into(), index))
    }
}

pub struct TypeckResult<'buf> {
    pub classes: Vec<Class<'buf>>,
}

pub struct TypeChecker<'dia, 'buf> {
    diagnostics: &'dia mut Diagnostics,
    classes: Vec<Class<'buf>>,
    excluded: HashSet<ClassName<'buf>>,
    ctx: TypeCtx<'buf>,
    unrecognized_tys: Vec<TyName<'buf>>,
    unrecognized_names: Vec<Name<'buf>>,
}

impl<'dia, 'buf> TypeChecker<'dia, 'buf> {
    pub fn new(diagnostics: &'dia mut Diagnostics, classes: Vec<Class<'buf>>) -> Self {
        let method_resolver = MethodResolver::new(diagnostics, &classes);
        let MethodResolverResult {
            unrecognized_tys,
            ctx,
            excluded,
        } = method_resolver.resolve();

        Self {
            diagnostics,
            classes,
            ctx,
            excluded,
            unrecognized_tys,
            unrecognized_names: vec![],
        }
    }

    pub fn resolve(mut self) -> TypeckResult<'buf> {
        let mut visitor = TypeVisitor {
            diagnostics: self.diagnostics,
            ctx: self.ctx,
            bindings: BindingMap::new(),
            unrecognized_tys: &mut self.unrecognized_tys,
            unrecognized_names: &mut self.unrecognized_names,
        };

        for class in &mut self.classes {
            if !self.excluded.contains(&class.name.borrow().into()) {
                visitor.visit_class(class);
            }
        }

        todo!()
    }
}

struct TypeVisitor<'a, 'dia, 'buf> {
    diagnostics: &'dia mut Diagnostics,
    ctx: TypeCtx<'buf>,
    bindings: BindingMap<'buf>,
    unrecognized_tys: &'a mut Vec<TyName<'buf>>,
    unrecognized_names: &'a mut Vec<Name<'buf>>,
}

impl TypeVisitor<'_, '_, '_> {
    pub fn with_scope<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let mut result = None;

        take_mut::take(self, |mut this| {
            let mut diagnostics = Some(this.diagnostics);
            let mut ctx = Some(this.ctx);
            let mut unrecognized_tys = Some(this.unrecognized_tys);
            let mut unrecognized_names = Some(this.unrecognized_names);

            this.bindings.with_scope(|bindings| {
                take_mut::take(bindings, |bindings| {
                    let mut this = TypeVisitor {
                        diagnostics: diagnostics.take().unwrap(),
                        ctx: ctx.take().unwrap(),
                        bindings,
                        unrecognized_tys: unrecognized_tys.take().unwrap(),
                        unrecognized_names: unrecognized_names.take().unwrap(),
                    };

                    result = Some(f(&mut this));

                    diagnostics.replace(this.diagnostics);
                    ctx.replace(this.ctx);
                    unrecognized_tys.replace(this.unrecognized_tys);
                    unrecognized_names.replace(this.unrecognized_names);

                    this.bindings
                });
            });

            TypeVisitor {
                diagnostics: diagnostics.unwrap(),
                ctx: ctx.unwrap(),
                bindings: this.bindings,
                unrecognized_tys: unrecognized_tys.unwrap(),
                unrecognized_names: unrecognized_names.unwrap(),
            }
        });

        result.unwrap()
    }
}

impl<'buf> TypeVisitor<'_, '_, 'buf> {
    fn check_class_name(&mut self, ty_name: &TyName<'buf>) -> Option<ResolvedTy<'buf>> {
        let class_name: ClassName<'buf> = ty_name.into();

        let err = match class_name {
            ClassName::Named(name) => return Some(ResolvedTy::Class(name)),

            ClassName::SelfType => TypeckError::IllegalSelfType {
                position: IllegalSelfTypePosition::ClassName,
                ty_name: ty_name.clone_static(),
            },

            ClassName::Builtin(builtin) => TypeckError::BuiltinRedefined {
                builtin,
                ty_name: ty_name.clone_static(),
            },
        };

        self.diagnostics.error().with_span_and_error(err).emit();

        None
    }
}

impl<'buf> ast::VisitorMut<'buf> for TypeVisitor<'_, '_, 'buf> {
    type Output = ();

    fn visit_program(&mut self, program: &mut ast::Program<'buf>) {
        program.recurse_mut(self);
    }

    fn visit_class(&mut self, class: &mut Class<'buf>) {
        self.with_scope(|this| {
            // visit the fields first to populate the scope
            let (fields, methods) = class
                .features
                .iter_mut()
                .partition::<Vec<_>, _>(|feature| matches!(feature, ast::Feature::Field(_)));

            for feature in fields.into_iter().chain(methods) {
                this.visit_feature(feature);
            }

            if let Some(ty) = this.check_class_name(&class.name) {
                class.ty = ty.into();
            }
        });
    }

    fn visit_feature(&mut self, feature: &mut ast::Feature<'buf>) {
        feature.recurse_mut(self);
    }

    fn visit_method(&mut self, method: &mut ast::Method<'buf>) {
        method.recurse_mut(self);

        todo!()
    }

    fn visit_field(&mut self, field: &mut ast::Field<'buf>) {
        field.recurse_mut(self);

        todo!()
    }

    fn visit_expr(&mut self, expr: &mut ast::Expr<'buf>) {
        expr.recurse_mut(self);

        todo!()
    }

    fn visit_assignment(&mut self, expr: &mut ast::Assignment<'buf>) {
        expr.recurse_mut(self);

        todo!()
    }

    fn visit_call(&mut self, expr: &mut ast::Call<'buf>) {
        expr.recurse_mut(self);

        todo!()
    }

    fn visit_if(&mut self, expr: &mut ast::If<'buf>) {
        expr.recurse_mut(self);

        todo!()
    }

    fn visit_while(&mut self, expr: &mut ast::While<'buf>) {
        expr.recurse_mut(self);

        todo!()
    }

    fn visit_block(&mut self, expr: &mut ast::Block<'buf>) {
        expr.recurse_mut(self);

        todo!()
    }

    fn visit_let(&mut self, expr: &mut ast::Let<'buf>) {
        expr.recurse_mut(self);

        todo!()
    }

    fn visit_case(&mut self, expr: &mut ast::Case<'buf>) {
        expr.recurse_mut(self);

        todo!()
    }

    fn visit_new(&mut self, expr: &mut ast::New<'buf>) {
        expr.recurse_mut(self);

        todo!()
    }

    fn visit_bin_op(&mut self, expr: &mut ast::BinOpExpr<'buf>) {
        expr.recurse_mut(self);

        todo!()
    }

    fn visit_un_op(&mut self, expr: &mut ast::UnOpExpr<'buf>) {
        expr.recurse_mut(self);

        todo!()
    }

    fn visit_name_expr(&mut self, expr: &mut ast::NameExpr<'buf>) {
        expr.recurse_mut(self);

        todo!()
    }

    fn visit_formal(&mut self, formal: &mut ast::Formal<'buf>) {
        formal.recurse_mut(self);

        todo!()
    }

    fn visit_receiver(&mut self, recv: &mut ast::Receiver<'buf>) {
        recv.recurse_mut(self);

        todo!()
    }

    fn visit_case_arm(&mut self, arm: &mut ast::CaseArm<'buf>) {
        arm.recurse_mut(self);

        todo!()
    }

    fn visit_ty_name(&mut self, ty_name: &mut TyName<'buf>) {
        ty_name.recurse_mut(self);

        todo!()
    }

    fn visit_binding(&mut self, binding: &mut ast::Binding<'buf>) {
        binding.recurse_mut(self);

        todo!()
    }

    fn visit_name(&mut self, name: &mut Name<'buf>) {
        todo!()
    }

    fn visit_int_lit(&mut self, expr: &mut ast::IntLit) {
        todo!()
    }

    fn visit_string_lit(&mut self, expr: &mut ast::StringLit<'buf>) {
        todo!()
    }

    fn visit_bool_lit(&mut self, expr: &mut ast::BoolLit) {
        todo!()
    }
}
