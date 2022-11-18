use std::borrow::{Borrow, Cow};
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashSet;

use itertools::Itertools;

use crate::analysis::class_resolver::{ClassResolver, ClassResolverResult};
use crate::analysis::error::{
    IllegalSelfTypePosition, MultipleDefinitionKind, TypeckError, UnrecognizedName,
    UnrecognizedNamePosition, UnrecognizedTy, UnrecognizedTyPosition,
    UnrelatedTypesInCase, CaseArmSubsumed,
    MismatchedTypes,
};
use crate::analysis::typectx::{
    BindErrorKind, Binding, BindingKind, BindingMap, ClassIndex, ClassName, DefinitionLocation,
    TypeCtx,
};
use crate::ast::ty::{BuiltinClass, FunctionTy, HasExplicitTy, HasTy, ResolvedTy, Ty, TyExt};
use crate::ast::{self, AstRecurse, Class, Expr, Name, TyName, VisitorMut};
use crate::errors::{DiagnosticMessage, Diagnostics};
use crate::position::{HasSpan, Span};
use crate::try_match;
use crate::util::CloneStatic;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Variance {
    Contravariant,
    Covariant,
    Invariant,
}

/// The context associated with an occurrence of a `Name`.
struct NameCtx {
    pub variance: Variance,
    pub allow_self: bool,
}

pub(super) enum SelfTypeAllowed {
    Yes,
    No(IllegalSelfTypePosition),
}

impl From<IllegalSelfTypePosition> for SelfTypeAllowed {
    fn from(position: IllegalSelfTypePosition) -> Self {
        Self::No(position)
    }
}

/// The context associated with an occurrence of a `Name`.
struct TyNameCtx {
    pub variance: Variance,
    pub allow_self_ty: SelfTypeAllowed,
}

pub struct TypeckResult<'buf> {
    pub classes: Vec<Class<'buf>>,
    pub ctx: TypeCtx<'buf>,
}

pub struct TypeChecker<'dia, 'buf> {
    diagnostics: &'dia mut Diagnostics,
    classes: Vec<Class<'buf>>,
    excluded: HashSet<TyName<'buf>>,
    ctx: TypeCtx<'buf>,
    unrecognized_tys: Vec<UnrecognizedTy<'buf>>,
    unrecognized_names: Vec<UnrecognizedName<'buf>>,
}

impl<'dia, 'buf> TypeChecker<'dia, 'buf> {
    pub fn new(diagnostics: &'dia mut Diagnostics, classes: Vec<Class<'buf>>) -> Self {
        let method_resolver = ClassResolver::new(diagnostics, &classes);
        let ClassResolverResult {
            unrecognized_tys,
            ctx,
            excluded,
        } = method_resolver.resolve();
        let excluded = excluded.into_iter().cloned().collect();

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
        for class in &mut self.classes {
            if !self.excluded.contains(&class.name) {
                let mut visitor = TypeVisitor {
                    diagnostics: RefCell::new(self.diagnostics),
                    ctx: &mut self.ctx,
                    bindings: BindingMap::new(),
                    unrecognized_tys: RefCell::new(&mut self.unrecognized_tys),
                    unrecognized_names: RefCell::new(&mut self.unrecognized_names),
                    class_name: class.name.borrow().into(),
                    location: None,
                };

                visitor.visit_class(class);
            }
        }

        todo!("dedup unrecognized symbols, emit diagnostics, prepare the result")
    }
}

trait UnwrapResolvedTy<'buf>: HasTy<'buf> {
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

trait ResolvedTyExt<'buf> {
    fn to_builtin_ty(&self) -> Option<BuiltinClass>;

    /// Converts `self` to `ClassName`, mapping `ResolvedTy::SelfType` to `SelfType`.
    fn to_class_name(&self) -> Option<ClassName<'buf>>;

    fn as_function_ty(&self) -> Option<&FunctionTy<'buf>>;

    /// If `self` is `SelfType`, substitutes itself for the enclosing class.
    /// Otherwise returns `self`.
    fn reify_self_ty(&self) -> &ResolvedTy<'buf>;

    /// If `self` is `SelfType`, substitutes itself with `substitute`.
    /// Otherwise returns `self.
    fn reify_self_ty_with<'a>(&'a self, substitute: &'a ResolvedTy<'buf>) -> &'a ResolvedTy<'buf>;

    fn is_self_ty(&self) -> bool;
    fn is_class_ty(&self) -> bool;
    fn is_bottom_ty(&self) -> bool;
    fn is_untyped(&self) -> bool;
}

impl<'buf> ResolvedTyExt<'buf> for ResolvedTy<'buf> {
    fn to_builtin_ty(&self) -> Option<BuiltinClass> {
        try_match!(self, ResolvedTy::Builtin(builtin) => *builtin)
    }

    fn to_class_name(&self) -> Option<ClassName<'buf>> {
        self.try_into().ok()
    }

    fn as_function_ty(&self) -> Option<&FunctionTy<'buf>> {
        try_match!(self, ResolvedTy::Function(ty) => ty)
    }

    fn reify_self_ty(&self) -> &ResolvedTy<'buf> {
        match self {
            Self::SelfType { enclosed } => &enclosed,
            _ => self,
        }
    }

    fn reify_self_ty_with<'a>(&'a self, substitute: &'a ResolvedTy<'buf>) -> &'a ResolvedTy<'buf> {
        if self.is_self_ty() {
            substitute
        } else {
            self
        }
    }

    fn is_self_ty(&self) -> bool {
        matches!(self, ResolvedTy::SelfType { .. })
    }

    fn is_class_ty(&self) -> bool {
        matches!(self, ResolvedTy::Class(_) | ResolvedTy::Builtin(_))
    }

    fn is_bottom_ty(&self) -> bool {
        matches!(self, ResolvedTy::Bottom)
    }

    fn is_untyped(&self) -> bool {
        matches!(self, ResolvedTy::Untyped)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Location<'buf> {
    Method(Cow<'buf, [u8]>),
    Field(Cow<'buf, [u8]>),
}

impl<'buf> From<Location<'buf>> for UnrecognizedTyPosition<'buf> {
    fn from(location: Location<'buf>) -> Self {
        match location {
            Location::Method(name) => Self::Method(name),
            Location::Field(name) => Self::Field(name),
        }
    }
}

impl<'buf> From<Location<'buf>> for UnrecognizedNamePosition<'buf> {
    fn from(location: Location<'buf>) -> Self {
        match location {
            Location::Method(name) => Self::Method(name),
            Location::Field(name) => Self::Field(name),
        }
    }
}

struct TypeVisitor<'a, 'dia, 'buf> {
    diagnostics: RefCell<&'dia mut Diagnostics>,
    ctx: &'a mut TypeCtx<'buf>,
    bindings: BindingMap<'buf>,
    unrecognized_tys: RefCell<&'a mut Vec<UnrecognizedTy<'buf>>>,
    unrecognized_names: RefCell<&'a mut Vec<UnrecognizedName<'buf>>>,
    class_name: ClassName<'buf>,
    location: Option<Location<'buf>>,
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
            let mut class_name = Some(this.class_name);
            let mut location = Some(this.location);

            this.bindings.with_scope(|bindings| {
                take_mut::take(bindings, |bindings| {
                    let mut this = TypeVisitor {
                        diagnostics: diagnostics.take().unwrap(),
                        ctx: ctx.take().unwrap(),
                        bindings,
                        unrecognized_tys: unrecognized_tys.take().unwrap(),
                        unrecognized_names: unrecognized_names.take().unwrap(),
                        class_name: class_name.take().unwrap(),
                        location: location.take().unwrap(),
                    };

                    result = Some(f(&mut this));

                    diagnostics.replace(this.diagnostics);
                    ctx.replace(this.ctx);
                    unrecognized_tys.replace(this.unrecognized_tys);
                    unrecognized_names.replace(this.unrecognized_names);
                    class_name.replace(this.class_name);
                    location.replace(this.location);

                    this.bindings
                });
            });

            TypeVisitor {
                diagnostics: diagnostics.unwrap(),
                ctx: ctx.unwrap(),
                bindings: this.bindings,
                unrecognized_tys: unrecognized_tys.unwrap(),
                unrecognized_names: unrecognized_names.unwrap(),
                class_name: class_name.unwrap(),
                location: location.take().unwrap(),
            }
        });

        result.unwrap()
    }

    fn add_inherited_fields(&mut self) {
        let supertys = self
            .ctx
            .inheritance_chain(&self.class_name)
            .collect::<Vec<_>>();

        supertys
            .into_iter()
            .rev()
            .flat_map(|(_, index)| index.fields())
            .for_each(|(name, location, ty)| {
                // ignore the error here: it'll get emitted while typecking the offending class
                let _ = self.bindings.innermost_mut().bind_if_empty(
                    name.clone(),
                    Binding {
                        kind: BindingKind::Field { inherited: true },
                        ty: ty.clone(),
                        location: location.clone(),
                    },
                );
            });
    }
}

impl<'buf> TypeVisitor<'_, '_, 'buf> {
    fn class_index(&self) -> &ClassIndex<'buf> {
        self.ctx
            .get_class(&self.class_name)
            .expect("the current class is present in the ctx")
    }

    fn inherits_from(&self, lhs: &ClassName<'buf>, rhs: &ClassName<'buf>) -> bool {
        self.ctx.is_subtype(lhs, rhs)
    }

    fn is_subtype(&self, lhs: &ResolvedTy<'buf>, rhs: &ResolvedTy<'buf>) -> bool {
        if lhs == rhs {
            return true;
        }

        match (lhs, rhs) {
            // functions are special-cased for simplicity:
            // since neither ⊥ ≮: f nor f ≮: ⊤ holds, they aren't proper types per se
            (ResolvedTy::Function(_), _) | (_, ResolvedTy::Function(_)) => false,

            (ResolvedTy::Untyped, _) | (_, ResolvedTy::Untyped) => true,

            (ResolvedTy::Bottom, _) => true,
            // the lhs == bottom case is already handled
            (_, ResolvedTy::Bottom) => false,

            (ResolvedTy::SelfType { enclosed }, _) => self.is_subtype(&enclosed, rhs),
            // T <: SELF_TYPE[C] iff T = ⊥, but that's already handled
            (_, ResolvedTy::SelfType { .. }) => false,

            (
                ResolvedTy::Builtin(_) | ResolvedTy::Class(_),
                ResolvedTy::Builtin(_) | ResolvedTy::Class(_),
            ) => self.inherits_from(&lhs.to_class_name().unwrap(), &rhs.to_class_name().unwrap()),
        }
    }

    fn join_classes(
        &self,
        lhs_class: ClassName<'buf>,
        rhs_class: ClassName<'buf>,
    ) -> ResolvedTy<'buf> {
        let lhs_chain: Vec<_> = self
            .ctx
            .inheritance_chain(&lhs_class)
            .map(|(name, _)| name)
            .collect();
        let rhs_chain: Vec<_> = self
            .ctx
            .inheritance_chain(&rhs_class)
            .map(|(name, _)| name)
            .collect();

        let join = lhs_chain
            .into_iter()
            .rev()
            .zip(rhs_chain.into_iter().rev())
            .take_while(|(l, r)| l == r)
            .last();

        join.expect("lhs and rhs have at least one common ancestor (Object)")
            .0
            .clone()
            .try_into()
            .expect("the join is not a SELF_TYPE")
    }

    fn join_tys(&self, lhs: &ResolvedTy<'buf>, rhs: &ResolvedTy<'buf>) -> ResolvedTy<'buf> {
        let mut lhs = lhs;
        let mut rhs = rhs;

        loop {
            break match (lhs, rhs) {
                (_, _) if lhs == rhs => lhs.clone(),

                // distinct function types are unrelated
                (ResolvedTy::Function(_), _) | (_, ResolvedTy::Function(_)) => ResolvedTy::Untyped,

                // propagate untypedness
                (ResolvedTy::Untyped, _) | (_, ResolvedTy::Untyped) => ResolvedTy::Untyped,

                // the bottom type is a subtype of the other
                (ResolvedTy::Bottom, ty) | (ty, ResolvedTy::Bottom) => ty.clone(),

                (ResolvedTy::SelfType { enclosed }, ty)
                | (ty, ResolvedTy::SelfType { enclosed }) => {
                    lhs = enclosed;
                    rhs = ty;

                    continue;
                }

                (
                    ResolvedTy::Builtin(_) | ResolvedTy::Class(_),
                    ResolvedTy::Builtin(_) | ResolvedTy::Class(_),
                ) => {
                    let lhs_class = lhs.to_class_name().unwrap();
                    let rhs_class = rhs.to_class_name().unwrap();

                    self.join_classes(lhs_class, rhs_class)
                }
            };
        }
    }

    fn bind(&mut self, name: &Name<'buf>, ty: ResolvedTy<'buf>, binding_kind: BindingKind) {
        let binding = Binding {
            kind: binding_kind,
            ty,
            location: DefinitionLocation::UserCode(name.span().into_owned()),
        };

        if let Err(e) = self
            .bindings
            .innermost_mut()
            .bind(name.0.value.clone(), binding)
        {
            match e.kind {
                BindErrorKind::DoubleDefinition { previous } => self
                    .diagnostics
                    .borrow_mut()
                    .error()
                    .with_span_and_error(TypeckError::MultipleDefinition {
                        kind: match previous.kind {
                            BindingKind::Local => panic!(
                                "a local binding was attempted without introducing a new scope"
                            ),
                            BindingKind::Field { inherited } => {
                                MultipleDefinitionKind::Field { inherited }
                            }
                            BindingKind::Parameter => MultipleDefinitionKind::Parameter,
                        },

                        name: Box::new(name.clone_static()),
                        previous: previous.location.clone(),
                    })
                    .emit(),

                BindErrorKind::BindingToSelf => self
                    .diagnostics
                    .borrow_mut()
                    .error()
                    .with_span_and_error(TypeckError::IllegalSelf(Box::new(name.clone_static())))
                    .emit(),
            }
        }
    }

    fn push_unrecognized_name(&self, name: Name<'buf>) {
        self.unrecognized_names.borrow_mut().push(UnrecognizedName {
            name: Box::new(name),
            class_name: self.class_name.clone(),
            position: self.location.clone().expect("location must be set").into(),
        });
    }

    fn push_unrecognized_ty(&self, ty_name: TyName<'buf>) {
        self.unrecognized_tys.borrow_mut().push(UnrecognizedTy {
            ty_name,
            class_name: self.class_name.clone(),
            position: self.location.clone().expect("location must be set").into(),
        });
    }

    /// Resolves a name in the binding map.
    ///
    /// Returns the type of the binding if it's present.
    /// Otherwise adds the name to the list of unrecognized names and returns the following:
    /// - `ResolvedTy::Bottom` if the name occurs in a covariant position
    /// - `ResolvedTy::Builtin(BuiltinClass::Object)` if the name occurs in a contravariant position
    /// - `ResolvedTy::Untyped` if the type occurs in an invariant position
    fn resolve_name(&self, name: &Name<'buf>, name_ctx: NameCtx) -> ResolvedTy<'buf> {
        use Variance::*;

        match name.as_slice() {
            b"self" if name_ctx.allow_self => {
                return ResolvedTy::SelfType {
                    enclosed: Box::new(self.class_name.clone().try_into().unwrap()),
                }
            }

            b"self" => {
                self.diagnostics
                    .borrow_mut()
                    .error()
                    .with_span_and_error(TypeckError::IllegalSelf(Box::new(name.clone_static())))
                    .emit();
            }

            _ => {
                if let Some(Binding { ty, .. }) = self.bindings.resolve(name.as_slice()) {
                    return ty.clone();
                }

                self.push_unrecognized_name(name.clone());
            }
        }

        match name_ctx.variance {
            Covariant => ResolvedTy::Bottom,
            Contravariant => BuiltinClass::Object.into(),
            Invariant => ResolvedTy::Untyped,
        }
    }

    fn make_self_ty(&self) -> ResolvedTy<'buf> {
        ResolvedTy::SelfType {
            enclosed: Box::new(self.class_name.clone().try_into().unwrap()),
        }
    }

    /// Resolves a type name in the current type context.
    ///
    /// Returns the resolved type if it's present.
    /// Otherwise adds the name to the list of unrecognized type names and returns the following:
    /// - `ResolvedTy::Bottom` if the type name occurs in a covariant position
    /// - `ResolvedTy::Builtin(BuiltinClass::Object)` if the type name occurs in a contravariant position
    /// - `ResolvedTy::Untyped` if the type name occurs in an invariant position
    fn resolve_ty_name(&self, ty_name: &TyName<'buf>, ty_name_ctx: TyNameCtx) -> ResolvedTy<'buf> {
        use Variance::*;

        let class_name = ty_name.into();

        match class_name {
            ClassName::SelfType => match ty_name_ctx.allow_self_ty {
                SelfTypeAllowed::Yes => return self.make_self_ty(),

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

            ClassName::Named(_) | ClassName::Builtin(_) => {
                if self.ctx.get_class(&class_name).is_some() {
                    return ResolvedTy::try_from(class_name).unwrap();
                } else {
                    self.push_unrecognized_ty(ty_name.clone());
                }
            }
        }

        match ty_name_ctx.variance {
            Covariant => ResolvedTy::Bottom,
            Contravariant => BuiltinClass::Object.into(),
            Invariant => ResolvedTy::Untyped,
        }
    }

    fn check_class_name(&self, ty_name: &TyName<'buf>) -> Option<ResolvedTy<'buf>> {
        let class_name: ClassName<'buf> = ty_name.into();

        let err = match class_name {
            ClassName::Named(name) => return Some(ResolvedTy::Class(name)),

            ClassName::SelfType => TypeckError::IllegalSelfType {
                position: IllegalSelfTypePosition::ClassName,
                ty_name: Box::new(ty_name.clone_static()),
            },

            ClassName::Builtin(builtin) => TypeckError::BuiltinRedefined {
                builtin,
                ty_name: Box::new(ty_name.clone_static()),
            },
        };

        self.diagnostics
            .borrow_mut()
            .error()
            .with_span_and_error(err)
            .emit();

        None
    }

    fn check_expr_conforms(&self, expr: &Expr<'buf>, expected_ty: &ResolvedTy<'buf>) {
        if !self.is_subtype(&expr.unwrap_res_ty(), expected_ty) {
            self.diagnostics
                .borrow_mut()
                .error()
                .with_span_and_error(TypeckError::MismatchedTypes(Box::new(MismatchedTypes {
                    span: expr.span().into_owned(),
                    expected_ty: expected_ty.clone_static(),
                    actual_ty: expr.unwrap_res_ty().into_owned().clone_static(),
                })))
                .emit();
        }
    }

    fn check_case_arm_tys_related(
        &self,
        expr: &ast::Case<'buf>,
        scrutinee_ty: Cow<ResolvedTy<'buf>>,
    ) {
        for arm in &expr.arms {
            let arm_ty = arm.binding_ty.unwrap_res_ty();

            if !self.is_subtype(&arm_ty, &scrutinee_ty) && !self.is_subtype(&scrutinee_ty, &arm_ty)
            {
                self.diagnostics
                    .borrow_mut()
                    .warn()
                    .with_span_and_error(TypeckError::UnrelatedTypesInCase(Box::new(UnrelatedTypesInCase {
                        scrutinee_span: expr.scrutinee.span().into_owned(),
                        scrutinee_ty: scrutinee_ty.clone().into_owned().clone_static(),
                        arm_span: arm.span.clone(),
                        arm_ty: arm_ty.clone().into_owned().clone_static(),
                    })))
                    .emit();
            }
        }
    }

    fn check_case_subsumption(&self, expr: &ast::Case<'buf>) {
        // since the algorithm is quadratic in the number of case arms,
        // we skip the check once the number gets too large
        const MAX_ARM_COUNT: usize = 200;

        if expr.arms.len() > MAX_ARM_COUNT {
            return;
        }

        'lower: for (i, arm_lower) in expr.arms.iter().enumerate() {
            let arm_lower_ty = arm_lower.binding_ty.unwrap_res_ty();

            if arm_lower_ty.is_untyped() {
                continue;
            }

            for arm_upper in expr.arms.iter().take(i.saturating_sub(1)) {
                let arm_upper_ty = arm_upper.binding_ty.unwrap_res_ty();

                if arm_upper_ty.is_untyped() {
                    continue;
                }

                if self.is_subtype(&arm_lower_ty, &arm_upper_ty) {
                    self.diagnostics
                        .borrow_mut()
                        .warn()
                        .with_span_and_error(TypeckError::CaseArmSubsumed(Box::new(CaseArmSubsumed {
                            subsuming_arm_ty: arm_upper_ty.into_owned().clone_static(),
                            subsuming_arm_span: arm_upper.span.clone(),
                            subsumed_arm_ty: arm_lower_ty.into_owned().clone_static(),
                            subsumed_arm_span: arm_lower.span.clone(),
                        })))
                        .emit();

                    // report only the first error for each lower-precedence arm
                    continue 'lower;
                }
            }
        }
    }

    fn check_equals(&self, lhs: &Expr<'buf>, rhs: &Expr<'buf>) {
        let lhs_ty = lhs.unwrap_res_ty();
        let rhs_ty = rhs.unwrap_res_ty();

        let is_primitive_ty = |ty: &ResolvedTy<'buf>| {
            matches!(
                ty,
                ResolvedTy::Builtin(BuiltinClass::Int | BuiltinClass::String | BuiltinClass::Bool)
            )
        };

        match (lhs_ty.as_ref(), rhs_ty.as_ref()) {
            (ResolvedTy::Bottom | ResolvedTy::Untyped, _) => return,
            (_, ResolvedTy::Bottom | ResolvedTy::Untyped) => return,

            (ResolvedTy::Function(_), _) | (_, ResolvedTy::Function(_)) => {
                unreachable!("exprs cannot have a function type")
            }

            (lhs_ty, rhs_ty) if is_primitive_ty(lhs_ty) || is_primitive_ty(rhs_ty) => {}

            _ => return,
        }

        // at least one of the operands has a primitive type: ensure types match
        if lhs_ty != rhs_ty {
            let err = if is_primitive_ty(&*lhs_ty) {
                TypeckError::MismatchedTypes(Box::new(MismatchedTypes {
                    span: rhs.span().into_owned(),
                    expected_ty: lhs_ty.into_owned().clone_static(),
                    actual_ty: rhs_ty.into_owned().clone_static(),
                }))
            } else {
                TypeckError::MismatchedTypes(Box::new(MismatchedTypes {
                    span: lhs.span().into_owned(),
                    expected_ty: rhs_ty.into_owned().clone_static(),
                    actual_ty: lhs_ty.into_owned().clone_static(),
                }))
            };

            self.diagnostics
                .borrow_mut()
                .error()
                .with_span_and_error(err)
                .emit();
        }
    }

    fn visit_binding(&mut self, binding: &mut ast::Binding<'buf>, binding_kind: BindingKind) {
        let ty = self.resolve_ty_name(
            &binding.ty_name,
            TyNameCtx {
                variance: Variance::Invariant,
                allow_self_ty: SelfTypeAllowed::Yes,
            },
        );

        self.bind(&binding.name, ty, binding_kind);
    }
}

impl<'buf> ast::VisitorMut<'buf> for TypeVisitor<'_, '_, 'buf> {
    type Output = ();

    fn visit_program(&mut self, program: &mut ast::Program<'buf>) {
        program.recurse_mut(self);
    }

    fn visit_class(&mut self, class: &mut Class<'buf>) {
        assert_eq!(ClassName::from(class.name.borrow()), self.class_name);

        self.with_scope(|this| {
            this.add_inherited_fields();

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
        self.with_scope(|this| {
            this.location
                .replace(Location::Method(method.name.0.value.clone()));

            let (_, ty) = this
                .class_index()
                .get_method_ty(method.name.as_slice())
                .expect("the current method is present in the class index");
            method.ty = ty.clone().into();

            for param in &mut method.params {
                this.visit_formal(param);
            }

            this.with_scope(|this| {
                this.visit_expr(&mut method.body);
                this.check_expr_conforms(
                    &method.body,
                    &method.ty.unwrap_res_ty().as_function_ty().unwrap().ret,
                );
            });

            this.location.take();
        });
    }

    fn visit_field(&mut self, field: &mut ast::Field<'buf>) {
        self.location
            .replace(Location::Field(field.0.name.0.value.clone()));

        field.recurse_mut(self);

        self.location.take();
    }

    fn visit_expr(&mut self, expr: &mut ast::Expr<'buf>) {
        expr.recurse_mut(self);
    }

    fn visit_assignment(&mut self, expr: &mut ast::Assignment<'buf>) {
        let ty = self.resolve_name(
            &expr.name,
            NameCtx {
                variance: Variance::Contravariant,
                allow_self: false,
            },
        );
        self.visit_expr(&mut expr.expr);
        self.check_expr_conforms(&expr.expr, &ty);
    }

    fn visit_call(&mut self, expr: &mut ast::Call<'buf>) {
        self.visit_receiver(&mut expr.receiver);
        let recv_ty = expr.receiver.unwrap_res_ty();

        for arg in &mut expr.args {
            self.visit_expr(arg);
        }

        let ty = match recv_ty.reify_self_ty() {
            ResolvedTy::SelfType { .. } => unreachable!("SELF_TYPE should have been reified"),
            ResolvedTy::Function(..) => unreachable!("objects cannot have a function type"),
            ResolvedTy::Bottom | ResolvedTy::Untyped => recv_ty.clone().into_owned(),
            reified_recv_ty @ (ResolvedTy::Class(_) | ResolvedTy::Builtin(_)) => {
                let class_name = reified_recv_ty
                    .to_class_name()
                    .expect("the type of the receiver is a class type");

                match self.ctx.get_method_ty(&class_name, expr.method.as_slice()) {
                    Some((_, _, FunctionTy { params, ret })) => {
                        for (arg, param_ty) in expr.args.iter().zip(params.iter()) {
                            self.check_expr_conforms(arg, param_ty);
                        }

                        if params.len() != expr.args.len() {
                            self.diagnostics
                                .borrow_mut()
                                .error()
                                .with_span_and_error(TypeckError::InvalidNumberOfArguments {
                                    call_span: expr.span.clone(),
                                    expected_count: params.len(),
                                    supplied_count: expr.args.len(),
                                })
                                .emit();
                        }

                        ret.reify_self_ty_with(&recv_ty).clone()
                    }

                    None => {
                        // does not have to be deduped, emit right away
                        todo!("emit and error")
                    }
                }
            }
        };

        expr.ty = Some(ty.into());
    }

    fn visit_if(&mut self, expr: &mut ast::If<'buf>) {
        expr.recurse_mut(self);
        self.check_expr_conforms(&expr.antecedent, &BuiltinClass::Bool.into());

        let consequent_ty = expr.consequent.unwrap_res_ty();
        let alternative_ty = expr.consequent.unwrap_res_ty();
        let ty = self.join_tys(&consequent_ty, &alternative_ty);
        expr.ty = Some(ty.into());
    }

    fn visit_while(&mut self, expr: &mut ast::While<'buf>) {
        expr.recurse_mut(self);
        self.check_expr_conforms(&expr.condition, &BuiltinClass::Bool.into());
    }

    fn visit_block(&mut self, expr: &mut ast::Block<'buf>) {
        expr.recurse_mut(self);
    }

    fn visit_let(&mut self, expr: &mut ast::Let<'buf>) {
        self.with_scope(|this| {
            ast::VisitorMut::visit_binding(this, &mut expr.binding);
            this.visit_expr(&mut expr.expr);
        })
    }

    fn visit_case(&mut self, expr: &mut ast::Case<'buf>) {
        expr.recurse_mut(self);

        let scrutinee_ty = expr.scrutinee.unwrap_res_ty();
        self.check_case_arm_tys_related(expr, scrutinee_ty);
        self.check_case_subsumption(expr);

        let ty = expr
            .arms
            .iter()
            .map(|arm| arm.unwrap_res_ty())
            .reduce(|join, arm| Cow::Owned(self.join_tys(&join, &arm)))
            .expect("a case expression has at least one arm");

        expr.ty = Some(ty.into_owned().into());
    }

    fn visit_new(&mut self, expr: &mut ast::New<'buf>) {
        let ty = self.resolve_ty_name(
            &expr.ty_name,
            TyNameCtx {
                variance: Variance::Covariant,
                allow_self_ty: SelfTypeAllowed::Yes,
            },
        );

        expr.ty = ty.into();
    }

    fn visit_bin_op(&mut self, expr: &mut ast::BinOpExpr<'buf>) {
        use ast::BinOpKind::*;

        expr.recurse_mut(self);

        let ty: ResolvedTy<'buf> = match expr.op {
            Add | Subtract | Multiply | Divide | LessThan | LessEquals => {
                self.check_expr_conforms(&expr.lhs, &BuiltinClass::Int.into());
                self.check_expr_conforms(&expr.rhs, &BuiltinClass::Int.into());

                if let LessThan | LessEquals = expr.op {
                    BuiltinClass::Bool.into()
                } else {
                    BuiltinClass::Int.into()
                }
            }

            Equals => {
                self.check_equals(&expr.lhs, &expr.rhs);

                BuiltinClass::Bool.into()
            }
        };

        expr.ty = Some(ty.into());
    }

    fn visit_un_op(&mut self, expr: &mut ast::UnOpExpr<'buf>) {
        use ast::UnOpKind::*;

        expr.recurse_mut(self);

        let ty: ResolvedTy<'buf> = match expr.op {
            IsVoid => BuiltinClass::Bool.into(),

            Complement => {
                self.check_expr_conforms(&expr.expr, &BuiltinClass::Int.into());

                BuiltinClass::Int.into()
            }

            Not => {
                self.check_expr_conforms(&expr.expr, &BuiltinClass::Bool.into());

                BuiltinClass::Bool.into()
            }
        };

        expr.ty = Some(ty.into());
    }

    fn visit_name_expr(&mut self, expr: &mut ast::NameExpr<'buf>) {
        let ty = self.resolve_name(
            &expr.name,
            NameCtx {
                variance: Variance::Covariant,
                allow_self: true,
            },
        );

        expr.ty = Some(ty.into());
    }

    fn visit_formal(&mut self, formal: &mut ast::Formal<'buf>) {
        // a scope has already been introduced in visit_method
        let ty = self.resolve_ty_name(
            &formal.ty_name,
            TyNameCtx {
                variance: Variance::Invariant,
                allow_self_ty: IllegalSelfTypePosition::Parameter.into(),
            },
        );

        self.bind(&formal.name, ty.clone(), BindingKind::Parameter);
        formal.ty = ty.into();
    }

    fn visit_receiver(&mut self, recv: &mut ast::Receiver<'buf>) {
        use ast::Receiver;

        recv.recurse_mut(self);

        match recv {
            Receiver::SelfType { ty } => {
                *ty = self.make_self_ty().into();
            }

            Receiver::Dynamic(_expr) => {
                // typecked in the recurse_mut call above
            }

            Receiver::Static {
                object,
                ty_name,
                ty: recv_ty,
            } => {
                let ty = self.resolve_ty_name(
                    ty_name,
                    TyNameCtx {
                        variance: Variance::Invariant,
                        allow_self_ty: IllegalSelfTypePosition::StaticDispatch.into(),
                    },
                );

                self.check_expr_conforms(&object, &ty);
                *recv_ty = ty.into();
            }
        }
    }

    fn visit_case_arm(&mut self, arm: &mut ast::CaseArm<'buf>) {
        let binding_ty = self.resolve_ty_name(
            &arm.binding_ty_name,
            TyNameCtx {
                variance: Variance::Invariant,
                allow_self_ty: IllegalSelfTypePosition::CaseArm.into(),
            },
        );
        arm.binding_ty = binding_ty.clone().into();

        self.with_scope(|this| {
            this.bind(&arm.name, binding_ty, BindingKind::Local);
            this.visit_expr(&mut arm.expr);
        })
    }

    fn visit_ty_name(&mut self, ty_name: &mut TyName<'buf>) {
        ty_name.recurse_mut(self);
    }

    fn visit_binding(&mut self, binding: &mut ast::Binding<'buf>) {
        // a scope has already been introduced in the relevant method
        self.visit_binding(binding, BindingKind::Local);
    }

    fn visit_name(&mut self, name: &mut Name<'buf>) {}

    fn visit_int_lit(&mut self, expr: &mut ast::IntLit) {}

    fn visit_string_lit(&mut self, expr: &mut ast::StringLit<'buf>) {}

    fn visit_bool_lit(&mut self, expr: &mut ast::BoolLit) {}
}
