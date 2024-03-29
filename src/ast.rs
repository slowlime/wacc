pub mod dump;
pub mod ty;
mod visit;

use std::borrow::Cow;
use std::fmt;

use serde::{Serialize, Serializer};

use self::ty::{BuiltinClass, HasExplicitTy, HasTy, ResolvedTy, Ty};
use crate::analysis::BindingId;
use crate::parse::token::Symbol;
use crate::position::{HasSpan, Span, Spanned};
use crate::util::{slice_formatter, CloneStatic};

pub use self::visit::{AstRecurse, DefaultVisitor, DefaultVisitorMut, Visitor, VisitorMut};

fn serialize_name_as_string<S>(name: &Spanned<Cow<'_, [u8]>>, ser: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let substitute = Spanned {
        value: String::from_utf8_lossy(&name.value),
        span: name.span.clone(),
    };

    substitute.serialize(ser)
}

macro_rules! impl_recurse {
    (|$s:ident: $type:ty, $visitor:ident| { const => $body_const:expr, mut => $body_mut:expr $(,)?}) => {
        impl<'buf> AstRecurse<'buf> for $type {
            fn recurse<V: Visitor<'buf, Output = ()>>(&$s, $visitor: &mut V) {
                $body_const;
            }

            fn recurse_mut<V: VisitorMut<'buf, Output = ()>>(&mut $s, $visitor: &mut V) {
                $body_mut;
            }
        }
    };
}

macro_rules! impl_has_span {
    ($type:ty) => {
        impl HasSpan for $type {
            fn span(&self) -> Cow<'_, Span> {
                Cow::Borrowed(&self.span)
            }
        }
    };

    (|&$s:ident: $type:ty| $body:expr) => {
        impl HasSpan for $type {
            fn span(&$s) -> Cow<'_, Span> {
                Cow::Borrowed($body)
            }
        }
    };

    (|$s:ident: $type:ty| $body:expr) => {
        impl HasSpan for $type {
            fn span(&$s) -> Cow<'_, Span> {
                Cow::Owned($body)
            }
        }
    };

    (&$s:ident: $type:ty => $body:expr) => {
        impl HasSpan for $type {
            fn span(&$s) -> Cow<'_, Span> {
                $body
            }
        }
    };
}

macro_rules! impl_has_ty {
    (? $type:ty) => {
        impl<'buf> HasTy<'buf> for $type {
            fn ty(&self) -> Option<Cow<'_, Ty<'buf>>> {
                self.ty.as_ref().map(Cow::Borrowed)
            }
        }
    };

    (? |&$s:ident: $type:ty| $body:expr) => {
        impl<'buf> HasTy<'buf> for $type {
            fn ty(&$s) -> Option<Cow<'_, Ty<'buf>>> {
                $body
            }
        }
    };

    ($type:ty) => {
        impl<'buf> HasExplicitTy<'buf> for $type {
            fn explicit_ty(&self) -> Cow<'_, Ty<'buf>> {
                Cow::Borrowed(&self.ty)
            }
        }
    };

    (|&$s:ident: $type:ty| $body:expr) => {
        impl<'buf> HasExplicitTy<'buf> for $type {
            fn explicit_ty(&$s) -> Cow<'_, Ty<'buf>> {
                $body
            }
        }
    };
}

macro_rules! impl_clone_static {
    (|&$s:ident: $type:ident| $body:expr) => {
        impl CloneStatic<$type<'static>> for $type<'_> {
            fn clone_static(&$s) -> $type<'static> {
                $body
            }
        }
    };
}

macro_rules! define_op_kind {
    ($name:ident { $( $op:ident => $symbol:ident, )+ }) => {
        #[derive(Serialize, Debug, Clone, Copy, Eq, PartialEq, Hash)]
        pub enum $name {
            $( $op, )+
        }

        impl $name {
            pub fn symbol(self) -> Symbol {
                match self {
                    $( Self::$op => Symbol::$symbol, )+
                }
            }
        }

        impl TryFrom<Symbol> for $name {
            type Error = ();

            fn try_from(symbol: Symbol) -> Result<Self, Self::Error> {
                match symbol {
                    $( Symbol::$symbol => Ok(Self::$op), )+
                    _ => Err(()),
                }
            }
        }
    }
}

#[derive(Serialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct Program<'buf> {
    pub classes: Vec<Class<'buf>>,
    pub span: Span,
}

impl_clone_static!(|&self: Program| Program {
    classes: self.classes.iter().map(Class::clone_static).collect(),
    span: self.span.clone(),
});

impl_recurse!(|self: Program<'buf>, visitor| {
    const => for class in &self.classes {
        visitor.visit_class(class);
    },

    mut => for class in &mut self.classes {
        visitor.visit_class(class);
    },
});

impl_has_span!(Program<'_>);

#[derive(Serialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct Class<'buf> {
    pub name: TyName<'buf>,
    pub inherits: Option<TyName<'buf>>,
    pub features: Vec<Feature<'buf>>,
    pub span: Span,
    pub ty: Ty<'buf>,
}

impl_clone_static!(|&self: Class| Class {
    name: self.name.clone_static(),
    inherits: self.inherits.clone_static(),
    features: self.features.clone_static(),
    span: self.span.clone(),
    ty: self.ty.clone_static(),
});

impl_recurse!(|self: Class<'buf>, visitor| {
    const => {
        visitor.visit_ty_name(&self.name);

        if let Some(inherits) = &self.inherits {
            visitor.visit_ty_name(inherits);
        }

        for feature in &self.features {
            visitor.visit_feature(feature);
        }
    },

    mut => {
        visitor.visit_ty_name(&mut self.name);

        if let Some(inherits) = &mut self.inherits {
            visitor.visit_ty_name(inherits);
        }

        for feature in &mut self.features {
            visitor.visit_feature(feature);
        }
    },
});

impl_has_span!(Class<'_>);
impl_has_ty!(Class<'buf>);

#[derive(Serialize, Clone, Eq, PartialEq, Hash)]
pub struct Name<'buf>(
    #[serde(serialize_with = "serialize_name_as_string")]
    pub Spanned<Cow<'buf, [u8]>>
);

impl<'buf> Name<'buf> {
    pub fn as_slice(&self) -> &[u8] {
        &self.0.value
    }
}

impl_clone_static!(|&self: Name| Name(Spanned {
    span: self.0.span.clone(),
    value: self.0.value.clone_static(),
}));

impl_has_span!(|&self: Name<'_>| &self.0.span);

impl fmt::Debug for Name<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Name")
            .field("span", &self.0.span)
            .field("value", &slice_formatter(&self.0.value))
            .finish()
    }
}

impl fmt::Display for Name<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", slice_formatter(&self.0.value))
    }
}

/// A name occuring in a type position.
#[derive(Serialize, Clone, Eq, PartialEq, Hash)]
pub struct TyName<'buf>(pub Name<'buf>);

impl_clone_static!(|&self: TyName| TyName(self.0.clone_static()));

impl_has_span!(|&self: TyName<'_>| &self.0 .0.span);

impl_recurse!(|self: TyName<'buf>, visitor| {
    const => visitor.visit_name(&self.0),
    mut => visitor.visit_name(&mut self.0),
});

impl fmt::Debug for TyName<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TyName")
            .field("span", &self.0 .0.span)
            .field("value", &slice_formatter(&self.0 .0.value))
            .finish()
    }
}

impl fmt::Display for TyName<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.0)
    }
}

#[derive(Serialize, Debug, Clone, Eq, PartialEq, Hash)]
pub enum Feature<'buf> {
    Method(Method<'buf>),
    Field(Field<'buf>),
}

impl_clone_static!(|&self: Feature| match self {
    Self::Method(method) => Feature::Method(method.clone_static()),
    Self::Field(field) => Feature::Field(field.clone_static()),
});

impl_recurse!(|self: Feature<'buf>, visitor| {
    const => match self {
        Feature::Method(method) => visitor.visit_method(method),
        Feature::Field(field) => visitor.visit_field(field),
    },

    mut => match self {
        Feature::Method(method) => visitor.visit_method(method),
        Feature::Field(field) => visitor.visit_field(field),
    },
});

impl_has_span!(&self: Feature<'_> => match self {
    Self::Method(method) => method.span(),
    Self::Field(binding) => binding.span(),
});

impl_has_ty!(|&self: Feature<'buf>| match self {
    Self::Method(method) => method.explicit_ty(),
    Self::Field(field) => field.explicit_ty(),
});

#[derive(Serialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct Method<'buf> {
    pub name: Name<'buf>,
    pub params: Vec<Formal<'buf>>,
    pub return_ty: TyName<'buf>,
    pub body: Box<Expr<'buf>>,
    pub span: Span,
    pub ty: Ty<'buf>,
}

impl_clone_static!(|&self: Method| Method {
    name: self.name.clone_static(),
    params: self.params.clone_static(),
    return_ty: self.return_ty.clone_static(),
    body: Box::new(self.body.clone_static()),
    span: self.span.clone(),
    ty: self.ty.clone_static(),
});

impl_recurse!(|self: Method<'buf>, visitor| {
    const => {
        visitor.visit_name(&self.name);

        for param in &self.params {
            visitor.visit_formal(param);
        }

        visitor.visit_ty_name(&self.return_ty);
        visitor.visit_expr(&self.body);
    },

    mut => {
        visitor.visit_name(&mut self.name);

        for param in &mut self.params {
            visitor.visit_formal(param);
        }

        visitor.visit_ty_name(&mut self.return_ty);
        visitor.visit_expr(&mut self.body);
    },
});

impl_has_span!(Method<'_>);
impl_has_ty!(Method<'buf>);

#[derive(Serialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct Formal<'buf> {
    pub name: Name<'buf>,
    pub binding_id: Option<BindingId>,
    pub ty_name: TyName<'buf>,
    pub span: Span,
    pub ty: Ty<'buf>,
}

impl_clone_static!(|&self: Formal| Formal {
    name: self.name.clone_static(),
    binding_id: self.binding_id,
    ty_name: self.ty_name.clone_static(),
    span: self.span.clone(),
    ty: self.ty.clone_static(),
});

impl_recurse!(|self: Formal<'buf>, visitor| {
    const => {
        visitor.visit_name(&self.name);
        visitor.visit_ty_name(&self.ty_name);
    },

    mut => {
        visitor.visit_name(&mut self.name);
        visitor.visit_ty_name(&mut self.ty_name);
    }
});

impl_has_span!(Formal<'_>);
impl_has_ty!(Formal<'buf>);

#[derive(Serialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct Field<'buf>(pub Binding<'buf>);

impl_clone_static!(|&self: Field| Field(self.0.clone_static()));

impl_recurse!(|self: Field<'buf>, visitor| {
    const => visitor.visit_binding(&self.0),
    mut => visitor.visit_binding(&mut self.0),
});

impl_has_span!(|&self: Field<'_>| &self.0.span);
impl_has_ty!(|&self: Field<'buf>| self.0.explicit_ty());

#[derive(Serialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct Binding<'buf> {
    pub name: Name<'buf>,
    pub binding_id: Option<BindingId>,
    pub ty_name: TyName<'buf>,
    pub init: Option<Box<Expr<'buf>>>,
    pub span: Span,
    pub ty: Ty<'buf>,
}

impl_clone_static!(|&self: Binding| Binding {
    name: self.name.clone_static(),
    binding_id: self.binding_id,
    ty_name: self.ty_name.clone_static(),
    init: self.init.as_ref().map(|expr| Box::new(expr.clone_static())),
    span: self.span.clone(),
    ty: self.ty.clone_static(),
});

impl_recurse!(|self: Binding<'buf>, visitor| {
    const => {
        visitor.visit_name(&self.name);
        visitor.visit_ty_name(&self.ty_name);

        if let Some(expr) = &self.init {
            visitor.visit_expr(expr);
        }
    },

    mut => {
        visitor.visit_name(&mut self.name);
        visitor.visit_ty_name(&mut self.ty_name);

        if let Some(expr) = &mut self.init {
            visitor.visit_expr(expr);
        }
    },
});

impl_has_span!(Binding<'_>);
impl_has_ty!(Binding<'buf>);

#[derive(Serialize, Debug, Clone, Eq, PartialEq, Hash)]
pub enum Expr<'buf> {
    Assignment(Assignment<'buf>),
    Call(Call<'buf>),
    If(If<'buf>),
    While(While<'buf>),
    Block(Block<'buf>),
    Let(Let<'buf>),
    Case(Case<'buf>),
    New(New<'buf>),
    BinOp(BinOpExpr<'buf>),
    UnOp(UnOpExpr<'buf>),
    Name(NameExpr<'buf>),
    Int(IntLit),
    String(StringLit<'buf>),
    Bool(BoolLit),
}

impl_clone_static!(|&self: Expr| match self {
    Self::Assignment(expr) => Expr::Assignment(expr.clone_static()),
    Self::Call(expr) => Expr::Call(expr.clone_static()),
    Self::If(expr) => Expr::If(expr.clone_static()),
    Self::While(expr) => Expr::While(expr.clone_static()),
    Self::Block(expr) => Expr::Block(expr.clone_static()),
    Self::Let(expr) => Expr::Let(expr.clone_static()),
    Self::Case(expr) => Expr::Case(expr.clone_static()),
    Self::New(expr) => Expr::New(expr.clone_static()),
    Self::BinOp(expr) => Expr::BinOp(expr.clone_static()),
    Self::UnOp(expr) => Expr::UnOp(expr.clone_static()),
    Self::Name(expr) => Expr::Name(expr.clone_static()),
    Self::Int(expr) => Expr::Int(expr.clone()),
    Self::String(expr) => Expr::String(expr.clone_static()),
    Self::Bool(expr) => Expr::Bool(expr.clone()),
});

impl_recurse!(|self: Expr<'buf>, visitor| {
    const => match self {
        Self::Assignment(expr) => visitor.visit_assignment(expr),
        Self::Call(expr) => visitor.visit_call(expr),
        Self::If(expr) => visitor.visit_if(expr),
        Self::While(expr) => visitor.visit_while(expr),
        Self::Block(expr) => visitor.visit_block(expr),
        Self::Let(expr) => visitor.visit_let(expr),
        Self::Case(expr) => visitor.visit_case(expr),
        Self::New(expr) => visitor.visit_new(expr),
        Self::BinOp(expr) => visitor.visit_bin_op(expr),
        Self::UnOp(expr) => visitor.visit_un_op(expr),
        Self::Name(expr) => visitor.visit_name_expr(expr),
        Self::Int(expr) => visitor.visit_int_lit(expr),
        Self::String(expr) => visitor.visit_string_lit(expr),
        Self::Bool(expr) => visitor.visit_bool_lit(expr),
    },

    mut => match self {
        Self::Assignment(expr) => visitor.visit_assignment(expr),
        Self::Call(expr) => visitor.visit_call(expr),
        Self::If(expr) => visitor.visit_if(expr),
        Self::While(expr) => visitor.visit_while(expr),
        Self::Block(expr) => visitor.visit_block(expr),
        Self::Let(expr) => visitor.visit_let(expr),
        Self::Case(expr) => visitor.visit_case(expr),
        Self::New(expr) => visitor.visit_new(expr),
        Self::BinOp(expr) => visitor.visit_bin_op(expr),
        Self::UnOp(expr) => visitor.visit_un_op(expr),
        Self::Name(expr) => visitor.visit_name_expr(expr),
        Self::Int(expr) => visitor.visit_int_lit(expr),
        Self::String(expr) => visitor.visit_string_lit(expr),
        Self::Bool(expr) => visitor.visit_bool_lit(expr),
    },
});

impl_has_span!(&self: Expr<'_> => match self {
    Self::Assignment(expr) => expr.span(),
    Self::Call(expr) => expr.span(),
    Self::If(expr) => expr.span(),
    Self::While(expr) => expr.span(),
    Self::Block(expr) => expr.span(),
    Self::Let(expr) => expr.span(),
    Self::Case(expr) => expr.span(),
    Self::New(expr) => expr.span(),
    Self::BinOp(expr) => expr.span(),
    Self::UnOp(expr) => expr.span(),
    Self::Name(expr) => expr.span(),
    Self::Int(expr) => expr.span(),
    Self::String(expr) => expr.span(),
    Self::Bool(expr) => expr.span(),
});

impl_has_ty!(? |&self: Expr<'buf>| match self {
    Self::Assignment(expr) => expr.ty(),
    Self::Call(expr) => expr.ty(),
    Self::If(expr) => expr.ty(),
    Self::While(expr) => expr.ty(),
    Self::Block(expr) => expr.ty(),
    Self::Let(expr) => expr.ty(),
    Self::Case(expr) => expr.ty(),
    Self::New(expr) => expr.ty(),
    Self::BinOp(expr) => expr.ty(),
    Self::UnOp(expr) => expr.ty(),
    Self::Name(expr) => expr.ty(),
    Self::Int(expr) => expr.ty(),
    Self::String(expr) => expr.ty(),
    Self::Bool(expr) => expr.ty(),
});

impl<'buf> Expr<'buf> {
    pub fn recurse<V: Visitor<'buf>>(&self, visitor: &mut V) -> V::Output {
        match self {
            Self::Assignment(expr) => visitor.visit_assignment(expr),
            Self::Call(expr) => visitor.visit_call(expr),
            Self::If(expr) => visitor.visit_if(expr),
            Self::While(expr) => visitor.visit_while(expr),
            Self::Block(expr) => visitor.visit_block(expr),
            Self::Let(expr) => visitor.visit_let(expr),
            Self::Case(expr) => visitor.visit_case(expr),
            Self::New(expr) => visitor.visit_new(expr),
            Self::BinOp(expr) => visitor.visit_bin_op(expr),
            Self::UnOp(expr) => visitor.visit_un_op(expr),
            Self::Name(expr) => visitor.visit_name_expr(expr),
            Self::Int(expr) => visitor.visit_int_lit(expr),
            Self::String(expr) => visitor.visit_string_lit(expr),
            Self::Bool(expr) => visitor.visit_bool_lit(expr),
        }
    }

    pub fn recurse_mut<V: VisitorMut<'buf>>(&mut self, visitor: &mut V) -> V::Output {
        match self {
            Self::Assignment(expr) => visitor.visit_assignment(expr),
            Self::Call(expr) => visitor.visit_call(expr),
            Self::If(expr) => visitor.visit_if(expr),
            Self::While(expr) => visitor.visit_while(expr),
            Self::Block(expr) => visitor.visit_block(expr),
            Self::Let(expr) => visitor.visit_let(expr),
            Self::Case(expr) => visitor.visit_case(expr),
            Self::New(expr) => visitor.visit_new(expr),
            Self::BinOp(expr) => visitor.visit_bin_op(expr),
            Self::UnOp(expr) => visitor.visit_un_op(expr),
            Self::Name(expr) => visitor.visit_name_expr(expr),
            Self::Int(expr) => visitor.visit_int_lit(expr),
            Self::String(expr) => visitor.visit_string_lit(expr),
            Self::Bool(expr) => visitor.visit_bool_lit(expr),
        }
    }
}

#[derive(Serialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct Assignment<'buf> {
    pub name: Name<'buf>,
    pub binding_id: Option<BindingId>,
    pub expr: Box<Expr<'buf>>,
    pub span: Span,
}

impl_clone_static!(|&self: Assignment| Assignment {
    name: self.name.clone_static(),
    binding_id: self.binding_id,
    expr: Box::new(self.expr.clone_static()),
    span: self.span.clone(),
});

impl_recurse!(|self: Assignment<'buf>, visitor| {
    const => {
        visitor.visit_name(&self.name);
        visitor.visit_expr(&self.expr);
    },

    mut => {
        visitor.visit_name(&mut self.name);
        visitor.visit_expr(&mut self.expr);
    },
});

impl_has_span!(Assignment<'_>);
impl_has_ty!(? |&self: Assignment<'buf>| self.expr.ty());

#[derive(Serialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct Call<'buf> {
    pub receiver: Receiver<'buf>,
    pub method: Name<'buf>,
    pub args: Vec<Box<Expr<'buf>>>,
    pub span: Span,
    pub ty: Option<Ty<'buf>>,
}

impl_clone_static!(|&self: Call| Call {
    receiver: self.receiver.clone_static(),
    method: self.method.clone_static(),
    args: self
        .args
        .iter()
        .map(|arg| Box::new(arg.clone_static()))
        .collect(),
    span: self.span.clone(),
    ty: self.ty.clone_static(),
});

impl_recurse!(|self: Call<'buf>, visitor| {
    const => {
        visitor.visit_receiver(&self.receiver);
        visitor.visit_name(&self.method);

        for arg in &self.args {
            visitor.visit_expr(arg);
        }
    },

    mut => {
        visitor.visit_receiver(&mut self.receiver);
        visitor.visit_name(&mut self.method);

        for arg in &mut self.args {
            visitor.visit_expr(arg);
        }
    },
});

impl_has_span!(Call<'_>);
impl_has_ty!(?Call<'buf>);

#[derive(Serialize, Debug, Clone, Eq, PartialEq, Hash)]
pub enum Receiver<'buf> {
    SelfType {
        ty: Ty<'buf>,
        method_name_span: Span,
    },

    Dynamic(Box<Expr<'buf>>),

    Static {
        object: Box<Expr<'buf>>,
        ty_name: TyName<'buf>,
        ty: Ty<'buf>,
    },
}

impl_clone_static!(|&self: Receiver| match self {
    Self::SelfType {
        ty,
        method_name_span,
    } => Receiver::SelfType {
        ty: ty.clone_static(),
        method_name_span: method_name_span.clone(),
    },

    Self::Dynamic(expr) => Receiver::Dynamic(Box::new(expr.clone_static())),

    Self::Static {
        object,
        ty_name,
        ty,
    } => Receiver::Static {
        object: Box::new(object.clone_static()),
        ty_name: ty_name.clone_static(),
        ty: ty.clone_static(),
    },
});

impl_recurse!(|self: Receiver<'buf>, visitor| {
    const => match self {
        Self::SelfType { .. } => {},
        Self::Dynamic(expr) => visitor.visit_expr(expr),

        Self::Static { object, ty_name, .. } => {
            visitor.visit_expr(object);
            visitor.visit_ty_name(ty_name);
        }
    },

    mut => match self {
        Self::SelfType { .. } => {},
        Self::Dynamic(expr) => visitor.visit_expr(expr),

        Self::Static { object, ty_name, .. } => {
            visitor.visit_expr(object);
            visitor.visit_ty_name(ty_name);
        }
    },
});

impl_has_span!(&self: Receiver<'_> => match self {
    Self::SelfType { method_name_span, .. } => Cow::Borrowed(method_name_span),
    Self::Dynamic(expr) => expr.span(),
    Self::Static { object, .. } => object.span(),
});

impl_has_ty!(? |&self: Receiver<'buf>| match self {
    Self::SelfType { ty, .. } => Some(Cow::Borrowed(ty)),
    Self::Dynamic(expr) => expr.ty(),
    Self::Static { ty, .. } => Some(Cow::Borrowed(ty)),
});

#[derive(Serialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct If<'buf> {
    pub antecedent: Box<Expr<'buf>>,
    pub consequent: Box<Expr<'buf>>,
    pub alternative: Box<Expr<'buf>>,
    pub span: Span,
    pub ty: Option<Ty<'buf>>,
}

impl_clone_static!(|&self: If| If {
    antecedent: Box::new(self.antecedent.clone_static()),
    consequent: Box::new(self.consequent.clone_static()),
    alternative: Box::new(self.alternative.clone_static()),
    span: self.span.clone(),
    ty: self.ty.clone_static(),
});

impl_recurse!(|self: If<'buf>, visitor| {
    const => {
        visitor.visit_expr(&self.antecedent);
        visitor.visit_expr(&self.consequent);
        visitor.visit_expr(&self.alternative);
    },

    mut => {
        visitor.visit_expr(&mut self.antecedent);
        visitor.visit_expr(&mut self.consequent);
        visitor.visit_expr(&mut self.alternative);
    },
});

impl_has_span!(If<'_>);
impl_has_ty!(?If<'buf>);

#[derive(Serialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct While<'buf> {
    pub condition: Box<Expr<'buf>>,
    pub body: Box<Expr<'buf>>,
    pub span: Span,
}

impl_clone_static!(|&self: While| While {
    condition: Box::new(self.condition.clone_static()),
    body: Box::new(self.body.clone_static()),
    span: self.span.clone(),
});

impl_recurse!(|self: While<'buf>, visitor| {
    const => {
        visitor.visit_expr(&self.condition);
        visitor.visit_expr(&self.body);
    },

    mut => {
        visitor.visit_expr(&mut self.condition);
        visitor.visit_expr(&mut self.body);
    },
});

impl_has_span!(While<'_>);
impl_has_ty!(|&self: While<'buf>| Cow::Owned(ResolvedTy::Builtin(BuiltinClass::Object).into()));

#[derive(Serialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct Block<'buf> {
    pub body: Vec<Box<Expr<'buf>>>,
    pub span: Span,
}

impl_clone_static!(|&self: Block| Block {
    body: self
        .body
        .iter()
        .map(|expr| Box::new(expr.clone_static()))
        .collect(),
    span: self.span.clone(),
});

impl_recurse!(|self: Block<'buf>, visitor| {
    const => for expr in &self.body {
        visitor.visit_expr(expr);
    },

    mut => for expr in &mut self.body {
        visitor.visit_expr(expr);
    },
});

impl_has_span!(Block<'_>);
impl_has_ty!(? |&self: Block<'buf>| self.body.last().unwrap().ty());

#[derive(Serialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct Let<'buf> {
    pub binding: Binding<'buf>,
    pub expr: Box<Expr<'buf>>,
    pub span: Span,
}

impl_clone_static!(|&self: Let| Let {
    binding: self.binding.clone_static(),
    expr: Box::new(self.expr.clone_static()),
    span: self.span.clone(),
});

impl_recurse!(|self: Let<'buf>, visitor| {
    const => {
        visitor.visit_binding(&self.binding);
        visitor.visit_expr(&self.expr);
    },

    mut => {
        visitor.visit_binding(&mut self.binding);
        visitor.visit_expr(&mut self.expr);
    },
});

impl_has_span!(Let<'_>);
impl_has_ty!(? |&self: Let<'buf>| self.expr.ty());

#[derive(Serialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct Case<'buf> {
    pub scrutinee: Box<Expr<'buf>>,
    pub arms: Vec<CaseArm<'buf>>,
    pub span: Span,
    pub ty: Option<Ty<'buf>>,
}

impl_clone_static!(|&self: Case| Case {
    scrutinee: Box::new(self.scrutinee.clone_static()),
    arms: self.arms.clone_static(),
    span: self.span.clone(),
    ty: self.ty.clone_static(),
});

impl_recurse!(|self: Case<'buf>, visitor| {
    const => {
        visitor.visit_expr(&self.scrutinee);

        for arm in &self.arms {
            visitor.visit_case_arm(arm);
        }
    },

    mut => {
        visitor.visit_expr(&mut self.scrutinee);

        for arm in &mut self.arms {
            visitor.visit_case_arm(arm);
        }
    },
});

impl_has_span!(Case<'_>);
impl_has_ty!(?Case<'buf>);

#[derive(Serialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct CaseArm<'buf> {
    pub name: Name<'buf>,
    pub binding_ty_name: TyName<'buf>,
    pub expr: Box<Expr<'buf>>,
    pub span: Span,
    pub binding_ty: Ty<'buf>,
    pub binding_id: Option<BindingId>,
}

impl_clone_static!(|&self: CaseArm| CaseArm {
    name: self.name.clone_static(),
    binding_ty_name: self.binding_ty_name.clone_static(),
    expr: Box::new(self.expr.clone_static()),
    span: self.span.clone(),
    binding_ty: self.binding_ty.clone_static(),
    binding_id: self.binding_id,
});

impl_recurse!(|self: CaseArm<'buf>, visitor| {
    const => {
        visitor.visit_name(&self.name);
        visitor.visit_ty_name(&self.binding_ty_name);
        visitor.visit_expr(&self.expr);
    },

    mut => {
        visitor.visit_name(&mut self.name);
        visitor.visit_ty_name(&mut self.binding_ty_name);
        visitor.visit_expr(&mut self.expr);
    },
});

impl_has_span!(CaseArm<'_>);
impl_has_ty!(? |&self: CaseArm<'buf>| self.expr.ty());

#[derive(Serialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct New<'buf> {
    pub ty_name: TyName<'buf>,
    pub span: Span,
    pub ty: Ty<'buf>,
}

impl_clone_static!(|&self: New| New {
    ty_name: self.ty_name.clone_static(),
    span: self.span.clone(),
    ty: self.ty.clone_static(),
});

impl_recurse!(|self: New<'buf>, visitor| {
    const => visitor.visit_ty_name(&self.ty_name),
    mut => visitor.visit_ty_name(&mut self.ty_name),
});

impl_has_span!(New<'_>);
impl_has_ty!(New<'buf>);

#[derive(Serialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct BinOpExpr<'buf> {
    pub op: BinOpKind,
    pub lhs: Box<Expr<'buf>>,
    pub rhs: Box<Expr<'buf>>,
    pub span: Span,
    pub ty: Option<Ty<'buf>>,
}

impl_clone_static!(|&self: BinOpExpr| BinOpExpr {
    op: self.op,
    lhs: Box::new(self.lhs.clone_static()),
    rhs: Box::new(self.rhs.clone_static()),
    span: self.span.clone(),
    ty: self.ty.clone_static(),
});

impl_recurse!(|self: BinOpExpr<'buf>, visitor| {
    const => {
        visitor.visit_expr(&self.lhs);
        visitor.visit_expr(&self.rhs);
    },

    mut => {
        visitor.visit_expr(&mut self.lhs);
        visitor.visit_expr(&mut self.rhs);
    },
});

impl_has_span!(BinOpExpr<'_>);
impl_has_ty!(?BinOpExpr<'buf>);

define_op_kind!(BinOpKind {
    Add => Plus,
    Subtract => Minus,
    Multiply => Asterisk,
    Divide => Slash,
    LessThan => Less,
    LessEquals => LessEquals,
    Equals => Equals,
});

#[derive(Serialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct UnOpExpr<'buf> {
    pub op: UnOpKind,
    pub expr: Box<Expr<'buf>>,
    pub span: Span,
    pub ty: Option<Ty<'buf>>,
}

impl_clone_static!(|&self: UnOpExpr| UnOpExpr {
    op: self.op,
    expr: Box::new(self.expr.clone_static()),
    span: self.span.clone(),
    ty: self.ty.clone_static(),
});

impl_recurse!(|self: UnOpExpr<'buf>, visitor| {
    const => visitor.visit_expr(&self.expr),
    mut => visitor.visit_expr(&mut self.expr),
});

impl_has_span!(UnOpExpr<'_>);
impl_has_ty!(?UnOpExpr<'buf>);

define_op_kind!(UnOpKind {
    IsVoid => IsVoid,
    Complement => Tilde,
    Not => Not,
});

#[derive(Serialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct NameExpr<'buf> {
    pub name: Name<'buf>,
    pub binding_id: Option<BindingId>,
    pub ty: Option<Ty<'buf>>,
}

impl_clone_static!(|&self: NameExpr| NameExpr {
    name: self.name.clone_static(),
    binding_id: self.binding_id,
    ty: self.ty.clone_static(),
});

impl_recurse!(|self: NameExpr<'buf>, visitor| {
    const => visitor.visit_name(&self.name),
    mut => visitor.visit_name(&mut self.name),
});

impl_has_span!(&self: NameExpr<'_> => self.name.span());
impl_has_ty!(?NameExpr<'buf>);

#[derive(Serialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct IntLit(pub Spanned<i32>);

impl_has_span!(|&self: IntLit| &self.0.span);
impl_has_ty!(|&self: IntLit| Cow::Owned(ResolvedTy::Builtin(BuiltinClass::Int).into()));

#[derive(Serialize, Clone, Eq, PartialEq, Hash)]
pub struct StringLit<'buf>(
    #[serde(serialize_with = "serialize_name_as_string")]
    pub Spanned<Cow<'buf, [u8]>>
);

impl fmt::Debug for StringLit<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Name")
            .field("span", &self.0.span)
            .field("value", &slice_formatter(self.0.value.as_ref()))
            .finish()
    }
}

impl_clone_static!(|&self: StringLit| StringLit(Spanned {
    span: self.0.span.clone(),
    value: self.0.value.clone_static(),
}));

impl_has_span!(|&self: StringLit<'_>| &self.0.span);
impl_has_ty!(|&self: StringLit<'buf>| Cow::Owned(ResolvedTy::Builtin(BuiltinClass::String).into()));

#[derive(Serialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct BoolLit(pub Spanned<bool>);

impl_has_span!(|&self: BoolLit| &self.0.span);
impl_has_ty!(|&self: BoolLit| Cow::Owned(ResolvedTy::Builtin(BuiltinClass::Bool).into()));
