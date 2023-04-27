use core::slice;

use slotmap::new_key_type;

use crate::util::define_byte_string;

use super::bb::Block;
use super::func::Func;
use super::ty::{IrClassName, IrTy};
use super::value::Value;

macro_rules! impl_into_instr {
    ($struct:ident) => {
        impl<'a> From<$struct> for InstrKind<'a> {
            fn from(instr: $struct) -> Self {
                Self::$struct(instr)
            }
        }
    };

    ($struct:ident<'a>) => {
        impl<'a> From<$struct<'a>> for InstrKind<'a> {
            fn from(instr: $struct<'a>) -> Self {
                Self::$struct(instr)
            }
        }
    };
}

macro_rules! delegate_instr_operands {
    ($ty:ty => |$self:ident| $delegatee:expr) => {
        impl InstrOperands for $ty {
            type Operand = Value;

            fn operands(&$self) -> &[Value] {
                $delegatee.operands()
            }

            fn operands_mut(&mut $self) -> &mut [Value] {
                $delegatee.operands_mut()
            }
        }
    };
}

define_byte_string! {
    pub struct MethodName<'a>;
    pub struct FieldName<'a>;
    pub struct IrBytes<'a>;
}

pub trait InstrOperands {
    type Operand;

    fn operands(&self) -> &[Self::Operand];
    fn operands_mut(&mut self) -> &mut [Self::Operand];
}

impl<const N: usize> InstrOperands for [Value; N] {
    type Operand = Value;

    fn operands(&self) -> &[Value] {
        self
    }

    fn operands_mut(&mut self) -> &mut [Value] {
        self
    }
}

impl InstrOperands for Vec<Value> {
    type Operand = Value;

    fn operands(&self) -> &[Value] {
        self
    }

    fn operands_mut(&mut self) -> &mut [Value] {
        self
    }
}

impl InstrOperands for Value {
    type Operand = Value;

    fn operands(&self) -> &[Value] {
        slice::from_ref(self)
    }

    fn operands_mut(&mut self) -> &mut [Value] {
        slice::from_mut(self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VTableLookup<'a> {
    pub obj: Value,
    pub method_name: MethodName<'a>,
}

impl_into_instr!(VTableLookup<'a>);
delegate_instr_operands!(VTableLookup<'_> => |self| self.obj);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MethodLookup<'a> {
    pub class_name: IrClassName<'a>,
    pub method_name: MethodName<'a>,
}

impl_into_instr!(MethodLookup<'a>);

impl InstrOperands for MethodLookup<'_> {
    type Operand = Value;

    fn operands(&self) -> &[Value] {
        &[]
    }

    fn operands_mut(&mut self) -> &mut [Value] {
        &mut []
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CallRef(Vec<Value>);

impl_into_instr!(CallRef);
delegate_instr_operands!(CallRef => |self| self.0);

impl CallRef {
    fn new(func_ref: Value, args: &[Value]) -> Self {
        let mut result = Self(Vec::with_capacity(1 + args.len()));
        result.0.extend_from_slice(args);

        result
    }

    fn func_ref(&self) -> Value {
        self.0[0]
    }

    fn func_ref_mut(&mut self) -> &mut Value {
        &mut self.0[0]
    }

    fn args(&self) -> &[Value] {
        &self.0[1..]
    }

    fn args_mut(&mut self) -> &mut [Value] {
        &mut self.0[1..]
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Call<'a> {
    pub func: Func<'a>,
    pub args: Vec<Value>,
}

impl_into_instr!(Call<'a>);
delegate_instr_operands!(Call<'_> => |self| self.args);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FieldGet<'a> {
    pub obj: Value,
    pub field: FieldName<'a>,
}

impl_into_instr!(FieldGet<'a>);
delegate_instr_operands!(FieldGet<'_> => |self| self.obj);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FieldSet<'a> {
    pub field: FieldName<'a>,
    args: [Value; 2],
}

impl_into_instr!(FieldSet<'a>);
delegate_instr_operands!(FieldSet<'_> => |self| self.args);

impl<'a> FieldSet<'a> {
    pub fn new(obj: Value, field: FieldName<'a>, value: Value) -> Self {
        Self {
            field,
            args: [obj, value],
        }
    }

    pub fn obj(&self) -> Value {
        self.args[0]
    }

    pub fn value(&self) -> Value {
        self.args[1]
    }

    pub fn obj_mut(&mut self) -> &mut Value {
        &mut self.args[0]
    }

    pub fn value_mut(&mut self) -> &mut Value {
        &mut self.args[1]
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Cast<'a> {
    pub obj: Value,
    pub to_ty: IrTy<'a>,
}

impl_into_instr!(Cast<'a>);
delegate_instr_operands!(Cast<'_> => |self| self.obj);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InstrKind<'a> {
    VTableLookup(VTableLookup<'a>),
    MethodLookup(MethodLookup<'a>),
    CallRef(CallRef),
    Call(Call<'a>),
    FieldGet(FieldGet<'a>),
    FieldSet(FieldSet<'a>),
    Box(Value),
    Unbox(Value),
    Cast(Cast<'a>),
    UncheckedCast(Cast<'a>),
    New(IrTy<'a>),
    IsNull(Value),
    Add(Value, Value),
    Sub(Value, Value),
    Mul(Value, Value),
    Div(Value, Value),
    Lt(Value, Value),
    Gt(Value, Value),
    Le(Value, Value),
    Ge(Value, Value),
    Eq(Value, Value),
    Inv(Value),
    Not(Value),
    I32(i32),
    Bytes(IrBytes<'a>),
    Bool(bool),
}

new_key_type! {
    pub struct Instr;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BlockJump {
    pub bb: Block,
    pub args: Vec<Value>,
}

delegate_instr_operands!(BlockJump => |self| self.args);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Branch {
    pub cond: Value,
    jumps: [BlockJump; 2],
}

impl InstrOperands for Branch {
    type Operand = BlockJump;

    fn operands(&self) -> &[BlockJump] {
        &self.jumps
    }

    fn operands_mut(&mut self) -> &mut [BlockJump] {
        &mut self.jumps
    }
}

impl Branch {
    pub fn new(cond: Value, on_true: BlockJump, on_false: BlockJump) -> Self {
        Self {
            cond,
            jumps: [on_true, on_false],
        }
    }

    pub fn on_true(&self) -> &BlockJump {
        &self.jumps[0]
    }

    pub fn on_false(&self) -> &BlockJump {
        &self.jumps[1]
    }

    pub fn on_true_mut(&mut self) -> &mut BlockJump {
        &mut self.jumps[0]
    }

    pub fn on_false_mut(&mut self) -> &mut BlockJump {
        &mut self.jumps[1]
    }
}

#[derive(Debug, Clone)]
pub enum TermInstrKind {
    Branch(Branch),
    Jump(BlockJump),
    Return(Option<Value>),
    /// The block never reaches the end.
    Diverge,
}

impl TermInstrKind {
    pub fn jumps_to<'s>(&'s self, bb: Block) -> impl Iterator<Item = &'s BlockJump> + 's {
        self.operands().iter().filter(move |jmp| jmp.bb == bb)
    }

    pub fn jumps_to_mut<'s>(&'s mut self, bb: Block) -> impl Iterator<Item = &'s mut BlockJump> + 's {
        self.operands_mut().iter_mut().filter(move |jmp| jmp.bb == bb)
    }
}

impl InstrOperands for TermInstrKind {
    type Operand = BlockJump;

    fn operands(&self) -> &[BlockJump] {
        match self {
            Self::Branch(br) => br.operands(),
            Self::Jump(jmp) => slice::from_ref(jmp),
            Self::Return(_) | Self::Diverge => &[],
        }
    }

    fn operands_mut(&mut self) -> &mut [BlockJump] {
        match self {
            Self::Branch(br) => br.operands_mut(),
            Self::Jump(jmp) => slice::from_mut(jmp),
            Self::Return(_) | Self::Diverge => &mut [],
        }
    }
}

new_key_type! {
    pub struct TermInstr;
}
