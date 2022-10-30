use std::borrow::Cow;
use std::fmt;

use byte_string::ByteStr;

use crate::token::Symbol;
use crate::position::{HasSpan, Span, Spanned};

pub trait AstRecurse<'buf> {
    fn recurse<V: DefaultVisitor<'buf>>(&self, visitor: &mut V);
    fn recurse_mut<V: DefaultVisitorMut<'buf>>(&mut self, visitor: &mut V);
}

macro_rules! define_visitor {
    ($( $type:ident { $( $name:ident ( $arg:ident : $ty:ty ) );+ $(;)? } )+) => {
        pub trait Visitor<'buf>
        where
            Self: Sized,
        {
            type Output;

            $(
                $(
                    fn $name(&mut self, $arg: &$ty) -> Self::Output;
                )+
            )+
        }

        pub trait VisitorMut<'buf>
        where
            Self: Sized,
        {
            type Output;

            $(
                $(
                    fn $name(&mut self, $arg: &mut $ty) -> Self::Output;
                )+
            )+
        }

        pub trait DefaultVisitor<'buf>
        where
            Self: Sized,
        {
            $( define_visitor!(@ $type { $( $name ( $arg : &$ty ) => recurse; )+ } ); )+
        }

        impl<'buf, T> Visitor<'buf> for T
        where
            T: DefaultVisitor<'buf>,
        {
            type Output = ();

            $(
                $(
                    fn $name(&mut self, $arg: &$ty) {
                        <Self as DefaultVisitor<'buf>>::$name(self, $arg);
                    }
                )+
            )+
        }

        pub trait DefaultVisitorMut<'buf>
        where
            Self: Sized,
        {
            $( define_visitor!(@ $type { $( $name ( $arg : &mut $ty ) => recurse_mut; )+ } ); )+
        }

        impl<'buf, T> VisitorMut<'buf> for T
        where
            T: DefaultVisitorMut<'buf>,
        {
            type Output = ();

            $(
                $(
                    fn $name(&mut self, $arg: &mut $ty) {
                        <Self as DefaultVisitorMut<'buf>>::$name(self, $arg);
                    }
                )+
            )+
        }
    };

    (@ NonTerminal { $( $name:ident ( $arg:ident : $ty:ty ) => $recurse:ident; )+ }) => {
        $(
            fn $name(&mut self, $arg: $ty) {
                $arg.$recurse(self);
            }
        )+
    };

    (@ Terminal { $( $name:ident ( $arg:ident : $ty:ty ) => $recurse:ident; )+ }) => {
        $(
            #[allow(unused_variables)]
            fn $name(&mut self, $arg: $ty) {}
        )+
    };
}

macro_rules! impl_recurse {
    (|$s:ident: $type:ty, $visitor:ident| { const => $body_const:expr, mut => $body_mut:expr $(,)?}) => {
        impl<'buf> AstRecurse<'buf> for $type {
            fn recurse<V: DefaultVisitor<'buf>>(&$s, $visitor: &mut V) {
                $body_const;
            }

            fn recurse_mut<V: DefaultVisitorMut<'buf>>(&mut $s, $visitor: &mut V) {
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

define_visitor! {
    NonTerminal {
        // statements
        visit_program(program: Program<'buf>);
        visit_class(class: Class<'buf>);
        visit_feature(feature: Feature<'buf>);
        visit_method(method: Method<'buf>);
        visit_field(field: Field<'buf>);

        // expr
        visit_expr(expr: Expr<'buf>);
        visit_assignment(expr: Assignment<'buf>);
        visit_call(expr: Call<'buf>);
        visit_if(expr: If<'buf>);
        visit_while(expr: While<'buf>);
        visit_block(expr: Block<'buf>);
        visit_let(expr: Let<'buf>);
        visit_case(expr: Case<'buf>);
        visit_new(expr: New<'buf>);
        visit_bin_op(expr: BinOpExpr<'buf>);
        visit_un_op(expr: UnOpExpr<'buf>);
        visit_name_expr(expr: NameExpr<'buf>);

        // neither stmt nor expr
        visit_formal(formal: Formal<'buf>);
        visit_receiver(recv: Receiver<'buf>);
        visit_case_arm(arm: CaseArm<'buf>);
    }

    Terminal {
        visit_name(name: Name<'buf>);
        visit_binding(binding: Binding<'buf>);
        visit_int_lit(expr: IntLit);
        visit_string_lit(expr: StringLit<'buf>);
        visit_bool_lit(expr: BoolLit);
    }
}

macro_rules! define_op_kind {
    ($name:ident { $( $op:ident => $symbol:ident, )+ }) => {
        #[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Program<'buf> {
    pub classes: Vec<Class<'buf>>,
    pub span: Span,
}

impl_recurse!(|self: Program<'buf>, visitor| {
    const => for class in &self.classes {
        visitor.visit_class(class);
    },

    mut => for class in &mut self.classes {
        visitor.visit_class(class);
    },
});

impl_has_span!(Program<'_>);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Class<'buf> {
    pub name: Name<'buf>,
    pub inherits: Option<Name<'buf>>,
    pub features: Vec<Feature<'buf>>,
    pub span: Span,
}

impl_recurse!(|self: Class<'buf>, visitor| {
    const => {
        visitor.visit_name(&self.name);

        if let Some(inherits) = &self.inherits {
            visitor.visit_name(inherits);
        }

        for feature in &self.features {
            visitor.visit_feature(feature);
        }
    },

    mut => {
        visitor.visit_name(&mut self.name);

        if let Some(inherits) = &mut self.inherits {
            visitor.visit_name(inherits);
        }

        for feature in &mut self.features {
            visitor.visit_feature(feature);
        }
    },
});

impl_has_span!(Class<'_>);

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Name<'buf>(pub Spanned<&'buf [u8]>);

impl_has_span!(|&self: Name<'_>| &self.0.span);

impl fmt::Debug for Name<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Name")
            .field("span", &self.0.span)
            .field("value", &ByteStr::new(self.0.value))
            .finish()
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Feature<'buf> {
    Method(Method<'buf>),
    Field(Field<'buf>),
}

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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Method<'buf> {
    pub name: Name<'buf>,
    pub params: Vec<Formal<'buf>>,
    pub return_ty: Name<'buf>,
    pub body: Box<Expr<'buf>>,
    pub span: Span,
}

impl_recurse!(|self: Method<'buf>, visitor| {
    const => {
        visitor.visit_name(&self.name);

        for param in &self.params {
            visitor.visit_formal(param);
        }

        visitor.visit_name(&self.return_ty);
        visitor.visit_expr(&self.body);
    },

    mut => {
        visitor.visit_name(&mut self.name);

        for param in &mut self.params {
            visitor.visit_formal(param);
        }

        visitor.visit_name(&mut self.return_ty);
        visitor.visit_expr(&mut self.body);
    },
});

impl_has_span!(Method<'_>);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Formal<'buf> {
    pub name: Name<'buf>,
    pub ty: Name<'buf>,
    pub span: Span,
}

impl_recurse!(|self: Formal<'buf>, visitor| {
    const => {
        visitor.visit_name(&self.name);
        visitor.visit_name(&self.ty);
    },

    mut => {
        visitor.visit_name(&mut self.name);
        visitor.visit_name(&mut self.ty);
    }
});

impl_has_span!(Formal<'_>);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Field<'buf>(pub Binding<'buf>);

impl_recurse!(|self: Field<'buf>, visitor| {
    const => visitor.visit_binding(&self.0),
    mut => visitor.visit_binding(&mut self.0),
});

impl_has_span!(|&self: Field<'_>| &self.0.span);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Binding<'buf> {
    pub name: Name<'buf>,
    pub ty: Name<'buf>,
    pub init: Option<Box<Expr<'buf>>>,
    pub span: Span,
}

impl_recurse!(|self: Binding<'buf>, visitor| {
    const => {
        visitor.visit_name(&self.name);
        visitor.visit_name(&self.ty);

        if let Some(expr) = &self.init {
            visitor.visit_expr(expr);
        }
    },

    mut => {
        visitor.visit_name(&mut self.name);
        visitor.visit_name(&mut self.ty);

        if let Some(expr) = &mut self.init {
            visitor.visit_expr(expr);
        }
    },
});

impl_has_span!(Binding<'_>);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Assignment<'buf> {
    pub name: Name<'buf>,
    pub expr: Box<Expr<'buf>>,
    pub span: Span,
}

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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Call<'buf> {
    pub receiver: Receiver<'buf>,
    pub method: Name<'buf>,
    pub args: Vec<Box<Expr<'buf>>>,
    pub span: Span,
}

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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Receiver<'buf> {
    SelfType,
    Dynamic(Box<Expr<'buf>>),
    Static {
        object: Box<Expr<'buf>>,
        ty: Name<'buf>,
    },
}

impl_recurse!(|self: Receiver<'buf>, visitor| {
    const => match self {
        Self::SelfType => {},
        Self::Dynamic(expr) => visitor.visit_expr(expr),

        Self::Static { object, ty } => {
            visitor.visit_expr(object);
            visitor.visit_name(ty);
        }
    },

    mut => match self {
        Self::SelfType => {},
        Self::Dynamic(expr) => visitor.visit_expr(expr),

        Self::Static { object, ty } => {
            visitor.visit_expr(object);
            visitor.visit_name(ty);
        }
    },
});

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct If<'buf> {
    pub antecedent: Box<Expr<'buf>>,
    pub consequent: Box<Expr<'buf>>,
    pub alternative: Box<Expr<'buf>>,
    pub span: Span,
}

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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct While<'buf> {
    pub condition: Box<Expr<'buf>>,
    pub body: Box<Expr<'buf>>,
    pub span: Span,
}

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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Block<'buf> {
    pub body: Vec<Box<Expr<'buf>>>,
    pub span: Span,
}

impl_recurse!(|self: Block<'buf>, visitor| {
    const => for expr in &self.body {
        visitor.visit_expr(expr);
    },

    mut => for expr in &mut self.body {
        visitor.visit_expr(expr);
    },
});

impl_has_span!(Block<'_>);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Let<'buf> {
    pub binding: Binding<'buf>,
    pub expr: Box<Expr<'buf>>,
    pub span: Span,
}

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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Case<'buf> {
    pub scrutinee: Box<Expr<'buf>>,
    pub arms: Vec<CaseArm<'buf>>,
    pub span: Span,
}

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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct CaseArm<'buf> {
    pub name: Name<'buf>,
    pub ty: Name<'buf>,
    pub expr: Box<Expr<'buf>>,
    pub span: Span,
}

impl_recurse!(|self: CaseArm<'buf>, visitor| {
    const => {
        visitor.visit_name(&self.name);
        visitor.visit_name(&self.ty);
        visitor.visit_expr(&self.expr);
    },

    mut => {
        visitor.visit_name(&mut self.name);
        visitor.visit_name(&mut self.ty);
        visitor.visit_expr(&mut self.expr);
    },
});

impl_has_span!(CaseArm<'_>);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct New<'buf> {
    pub ty: Name<'buf>,
    pub span: Span,
}

impl_recurse!(|self: New<'buf>, visitor| {
    const => visitor.visit_name(&self.ty),
    mut => visitor.visit_name(&mut self.ty),
});

impl_has_span!(New<'_>);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct BinOpExpr<'buf> {
    pub op: BinOpKind,
    pub lhs: Box<Expr<'buf>>,
    pub rhs: Box<Expr<'buf>>,
    pub span: Span,
}

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

define_op_kind!(BinOpKind {
    Add => Plus,
    Subtract => Minus,
    Multiply => Asterisk,
    Divide => Slash,
    LessThan => Less,
    LessEquals => LessEquals,
    Equals => Equals,
});

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct UnOpExpr<'buf> {
    pub op: UnOpKind,
    pub expr: Box<Expr<'buf>>,
    pub span: Span,
}

impl_recurse!(|self: UnOpExpr<'buf>, visitor| {
    const => visitor.visit_expr(&self.expr),
    mut => visitor.visit_expr(&mut self.expr),
});

impl_has_span!(UnOpExpr<'_>);

define_op_kind!(UnOpKind {
    IsVoid => IsVoid,
    Complement => Tilde,
    Not => Not,
});

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct NameExpr<'buf>(pub Name<'buf>);

impl_recurse!(|self: NameExpr<'buf>, visitor| {
    const => visitor.visit_name(&self.0),
    mut => visitor.visit_name(&mut self.0),
});

impl_has_span!(&self: NameExpr<'_> => self.0.span());

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct IntLit(pub Spanned<i32>);

impl_has_span!(|&self: IntLit| &self.0.span);

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct StringLit<'buf>(pub Spanned<Cow<'buf, [u8]>>);

impl fmt::Debug for StringLit<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Name")
            .field("span", &self.0.span)
            .field("value", &ByteStr::new(self.0.value.as_ref()))
            .finish()
    }
}

impl_has_span!(|&self: StringLit<'_>| &self.0.span);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct BoolLit(pub Spanned<bool>);

impl_has_span!(|&self: BoolLit| &self.0.span);
