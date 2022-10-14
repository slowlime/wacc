use crate::token::{Symbol, Token};

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
pub struct Program<'a> {
    pub classes: Vec<Class<'a>>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Class<'a> {
    pub name: Name<'a>,
    pub inherits: Option<Name<'a>>,
    pub features: Vec<Feature<'a>>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Name<'a>(pub Token<'a>);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Feature<'a> {
    Method(Method<'a>),
    Field(Binding<'a>),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Method<'a> {
    pub name: Name<'a>,
    pub params: Vec<Formal<'a>>,
    pub return_ty: Name<'a>,
    pub body: Expr<'a>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Formal<'a> {
    pub name: Name<'a>,
    pub ty: Name<'a>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Binding<'a> {
    pub name: Name<'a>,
    pub ty: Name<'a>,
    pub init: Option<Expr<'a>>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Expr<'a> {
    Assignment(Assignment<'a>),
    Call(Call<'a>),
    If(If<'a>),
    While(While<'a>),
    Block(Block<'a>),
    Let(Let<'a>),
    Case(Case<'a>),
    New(New<'a>),
    BinOp(BinOpExpr<'a>),
    UnOp(UnOpExpr<'a>),
    Name(Name<'a>),
    Int(IntLit<'a>),
    String(StringLit<'a>),
    Bool(BoolLit<'a>),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Assignment<'a> {
    pub name: Name<'a>,
    pub expr: Box<Expr<'a>>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Call<'a> {
    pub receiver: Receiver<'a>,
    pub method: Name<'a>,
    pub args: Vec<Expr<'a>>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Receiver<'a> {
    SelfType,
    Dynamic(Box<Expr<'a>>),
    Static {
        object: Box<Expr<'a>>,
        ty: Name<'a>,
    },
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct If<'a> {
    pub condition: Box<Expr<'a>>,
    pub consequent: Box<Expr<'a>>,
    pub alternative: Box<Expr<'a>>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct While<'a> {
    pub condition: Box<Expr<'a>>,
    pub body: Box<Expr<'a>>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Block<'a> {
    pub body: Vec<Expr<'a>>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Let<'a> {
    pub bindings: Vec<Binding<'a>>,
    pub expr: Box<Expr<'a>>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Case<'a> {
    pub scrutinee: Box<Expr<'a>>,
    pub arms: Vec<CaseArm<'a>>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct CaseArm<'a> {
    pub name: Name<'a>,
    pub ty: Name<'a>,
    pub expr: Box<Expr<'a>>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct New<'a>(pub Name<'a>);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct BinOpExpr<'a> {
    pub op: BinOpKind,
    pub lhs: Box<Expr<'a>>,
    pub rhs: Box<Expr<'a>>,
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
pub struct UnOpExpr<'a> {
    pub op: UnOpKind,
    pub expr: Box<Expr<'a>>,
}

define_op_kind!(UnOpKind {
    IsVoid => IsVoid,
    Complement => Tilde,
    Not => Not,
});

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct IntLit<'a>(pub Token<'a>);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct StringLit<'a>(pub Token<'a>);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct BoolLit<'a>(pub Token<'a>);
