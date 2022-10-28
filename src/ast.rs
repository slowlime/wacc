use std::borrow::Cow;

use crate::position::Spanned;
use crate::token::Symbol;

pub trait AstRecurse<'buf> {
    fn recurse<V: Visitor<'buf>>(&self, visitor: &mut V);
    fn recurse_mut<V: VisitorMut<'buf>>(&mut self, visitor: &mut V);
}

macro_rules! define_visitor {
    ($( $type:ident { $( $name:ident ( $arg:ident : $ty:ty ) );+ $(;)? } )+) => {
        pub trait Visitor<'buf>
        where
            Self: Sized,
        {
            $( define_visitor!(@ $type { $( $name ( $arg : &$ty ) => recurse; )+ } ); )+
        }

        pub trait VisitorMut<'buf>
        where
            Self: Sized,
        {
            $( define_visitor!(@ $type { $( $name ( $arg : &mut $ty ) => recurse_mut; )+ } ); )+
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
            fn $name(&mut self, $arg: $ty) {}
        )+
    };
}

macro_rules! impl_recurse {
    (|$s:ident: $type:ty, $visitor:ident| { const => $body_const:expr, mut => $body_mut:expr $(,)?}) => {
        impl<'buf> AstRecurse<'buf> for $type {
            fn recurse<V: Visitor<'buf>>(&$s, $visitor: &mut V) {
                $body_const;
            }

            fn recurse_mut<V: VisitorMut<'buf>>(&mut $s, $visitor: &mut V) {
                $body_mut;
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
}

impl_recurse!(|self: Program<'buf>, visitor| {
    const => for class in &self.classes {
        visitor.visit_class(class);
    },

    mut => for class in &mut self.classes {
        visitor.visit_class(class);
    },
});

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Class<'buf> {
    pub name: Name<'buf>,
    pub inherits: Option<Name<'buf>>,
    pub features: Vec<Feature<'buf>>,
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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Name<'buf>(pub Spanned<&'buf [u8]>);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Feature<'buf> {
    Method(Method<'buf>),
    Field(Binding<'buf>),
}

impl_recurse!(|self: Feature<'buf>, visitor| {
    const => match self {
        Feature::Method(method) => visitor.visit_method(method),
        Feature::Field(binding) => visitor.visit_binding(binding),
    },

    mut => match self {
        Feature::Method(method) => visitor.visit_method(method),
        Feature::Field(binding) => visitor.visit_binding(binding),
    },
});

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Method<'buf> {
    pub name: Name<'buf>,
    pub params: Vec<Formal<'buf>>,
    pub return_ty: Name<'buf>,
    pub body: Box<Expr<'buf>>,
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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Formal<'buf> {
    pub name: Name<'buf>,
    pub ty: Name<'buf>,
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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Binding<'buf> {
    pub name: Name<'buf>,
    pub ty: Name<'buf>,
    pub init: Option<Box<Expr<'buf>>>,
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
    Name(Name<'buf>),
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
        Self::Name(name) => visitor.visit_name(name),
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
        Self::Name(name) => visitor.visit_name(name),
        Self::Int(expr) => visitor.visit_int_lit(expr),
        Self::String(expr) => visitor.visit_string_lit(expr),
        Self::Bool(expr) => visitor.visit_bool_lit(expr),
    },
});

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Assignment<'buf> {
    pub name: Name<'buf>,
    pub expr: Box<Expr<'buf>>,
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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Call<'buf> {
    pub receiver: Receiver<'buf>,
    pub method: Name<'buf>,
    pub args: Vec<Box<Expr<'buf>>>,
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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct While<'buf> {
    pub condition: Box<Expr<'buf>>,
    pub body: Box<Expr<'buf>>,
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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Block<'buf> {
    pub body: Vec<Box<Expr<'buf>>>,
}

impl_recurse!(|self: Block<'buf>, visitor| {
    const => for expr in &self.body {
        visitor.visit_expr(expr);
    },

    mut => for expr in &mut self.body {
        visitor.visit_expr(expr);
    },
});

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Let<'buf> {
    pub bindings: Vec<Binding<'buf>>,
    pub expr: Box<Expr<'buf>>,
}

impl_recurse!(|self: Let<'buf>, visitor| {
    const => {
        for binding in &self.bindings {
            visitor.visit_binding(binding);
        }

        visitor.visit_expr(&self.expr);
    },

    mut => {
        for binding in &mut self.bindings {
            visitor.visit_binding(binding);
        }

        visitor.visit_expr(&mut self.expr);
    },
});

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Case<'buf> {
    pub scrutinee: Box<Expr<'buf>>,
    pub arms: Vec<CaseArm<'buf>>,
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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct CaseArm<'buf> {
    pub name: Name<'buf>,
    pub ty: Name<'buf>,
    pub expr: Box<Expr<'buf>>,
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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct New<'buf>(pub Name<'buf>);

impl_recurse!(|self: New<'buf>, visitor| {
    const => visitor.visit_name(&self.0),
    mut => visitor.visit_name(&mut self.0),
});

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct BinOpExpr<'buf> {
    pub op: BinOpKind,
    pub lhs: Box<Expr<'buf>>,
    pub rhs: Box<Expr<'buf>>,
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
}

impl_recurse!(|self: UnOpExpr<'buf>, visitor| {
    const => visitor.visit_expr(&self.expr),
    mut => visitor.visit_expr(&mut self.expr),
});

define_op_kind!(UnOpKind {
    IsVoid => IsVoid,
    Complement => Tilde,
    Not => Not,
});

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct IntLit(pub Spanned<i32>);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct StringLit<'buf>(pub Spanned<Cow<'buf, [u8]>>);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct BoolLit(pub Spanned<bool>);
