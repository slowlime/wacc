use std::borrow::Cow;

use crate::position::Spanned;
use crate::token::Symbol;

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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Class<'buf> {
    pub name: Name<'buf>,
    pub inherits: Option<Name<'buf>>,
    pub features: Vec<Feature<'buf>>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Name<'buf>(pub Spanned<&'buf [u8]>);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Feature<'buf> {
    Method(Method<'buf>),
    Field(Binding<'buf>),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Method<'buf> {
    pub name: Name<'buf>,
    pub params: Vec<Formal<'buf>>,
    pub return_ty: Name<'buf>,
    pub body: Box<Expr<'buf>>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Formal<'buf> {
    pub name: Name<'buf>,
    pub ty: Name<'buf>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Binding<'buf> {
    pub name: Name<'buf>,
    pub ty: Name<'buf>,
    pub init: Option<Box<Expr<'buf>>>,
}

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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Assignment<'buf> {
    pub name: Name<'buf>,
    pub expr: Box<Expr<'buf>>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Call<'buf> {
    pub receiver: Receiver<'buf>,
    pub method: Name<'buf>,
    pub args: Vec<Box<Expr<'buf>>>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Receiver<'buf> {
    SelfType,
    Dynamic(Box<Expr<'buf>>),
    Static {
        object: Box<Expr<'buf>>,
        ty: Name<'buf>,
    },
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct If<'buf> {
    pub antecedent: Box<Expr<'buf>>,
    pub consequent: Box<Expr<'buf>>,
    pub alternative: Box<Expr<'buf>>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct While<'buf> {
    pub condition: Box<Expr<'buf>>,
    pub body: Box<Expr<'buf>>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Block<'buf> {
    pub body: Vec<Box<Expr<'buf>>>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Let<'buf> {
    pub bindings: Vec<Binding<'buf>>,
    pub expr: Box<Expr<'buf>>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Case<'buf> {
    pub scrutinee: Box<Expr<'buf>>,
    pub arms: Vec<CaseArm<'buf>>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct CaseArm<'buf> {
    pub name: Name<'buf>,
    pub ty: Name<'buf>,
    pub expr: Box<Expr<'buf>>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct New<'buf>(pub Name<'buf>);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct BinOpExpr<'buf> {
    pub op: BinOpKind,
    pub lhs: Box<Expr<'buf>>,
    pub rhs: Box<Expr<'buf>>,
}

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
