use core::slice;

use slotmap::new_key_type;

use crate::util::define_byte_string;

use super::bb::Block;
use super::func::FuncId;
use super::ty::IrClassName;
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
    pub class: IrClassName<'a>,
    pub method_name: MethodName<'a>,
}

impl_into_instr!(VTableLookup<'a>);
delegate_instr_operands!(VTableLookup<'_> => |self| self.obj);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MethodLookup<'a> {
    pub class: IrClassName<'a>,
    pub method: MethodName<'a>,
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
    pub fn new(func_ref: Value, args: &[Value]) -> Self {
        let mut result = Self(Vec::with_capacity(1 + args.len()));
        result.0.push(func_ref);
        result.0.extend_from_slice(args);

        result
    }

    pub fn func_ref(&self) -> Value {
        self.0[0]
    }

    pub fn func_ref_mut(&mut self) -> &mut Value {
        &mut self.0[0]
    }

    pub fn args(&self) -> &[Value] {
        &self.0[1..]
    }

    pub fn args_mut(&mut self) -> &mut [Value] {
        &mut self.0[1..]
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Call {
    pub func: FuncId,
    pub args: Vec<Value>,
}

impl_into_instr!(Call);
delegate_instr_operands!(Call => |self| self.args);

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
    pub value: Value,
    pub to_class: IrClassName<'a>,
}

impl_into_instr!(Cast<'a>);
delegate_instr_operands!(Cast<'_> => |self| self.value);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BytesGet([Value; 2]);

impl BytesGet {
    pub fn new(bytes: Value, idx: Value) -> Self {
        Self([bytes, idx])
    }

    pub fn bytes(&self) -> Value {
        self.0[0]
    }

    pub fn idx(&self) -> Value {
        self.0[1]
    }
}

impl_into_instr!(BytesGet);
delegate_instr_operands!(BytesGet => |self| self.0);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BytesSet([Value; 3]);

impl BytesSet {
    pub fn new(bytes: Value, idx: Value, byte: Value) -> Self {
        Self([bytes, idx, byte])
    }

    pub fn bytes(&self) -> Value {
        self.0[0]
    }

    pub fn idx(&self) -> Value {
        self.0[1]
    }

    pub fn byte(&self) -> Value {
        self.0[2]
    }
}

impl_into_instr!(BytesSet);
delegate_instr_operands!(BytesSet => |self| self.0);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InstrKind<'a> {
    VTableLookup(VTableLookup<'a>),
    MethodLookup(MethodLookup<'a>),
    CallRef(CallRef),
    Call(Call),
    FieldGet(FieldGet<'a>),
    FieldSet(FieldSet<'a>),
    Box(Value),
    Unbox(Value),
    Cast(Cast<'a>),
    New(IrClassName<'a>),
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
    Null(IrClassName<'a>),
    BytesNew(Value),
    BytesGet(BytesGet),
    BytesSet(BytesSet),
    BytesLen(Value),
    I32(i32),
    Bytes(IrBytes<'a>),
    Bool(bool),
    Unit,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CastBranch<'a> {
    pub cast: Cast<'a>,
    jumps: [BlockJump; 2],
}

impl<'a> CastBranch<'a> {
    pub fn new(cast: Cast<'a>, on_success: BlockJump, on_fail: BlockJump) -> Self {
        Self {
            cast,
            jumps: [on_success, on_fail],
        }
    }

    pub fn on_success(&self) -> &BlockJump {
        &self.jumps[0]
    }

    pub fn on_fail(&self) -> &BlockJump {
        &self.jumps[1]
    }

    pub fn on_success_mut(&mut self) -> &mut BlockJump {
        &mut self.jumps[0]
    }

    pub fn on_fail_mut(&mut self) -> &mut BlockJump {
        &mut self.jumps[1]
    }
}

impl<'a> InstrOperands for CastBranch<'a> {
    type Operand = BlockJump;

    fn operands(&self) -> &[BlockJump] {
        &self.jumps
    }

    fn operands_mut(&mut self) -> &mut [BlockJump] {
        &mut self.jumps
    }
}

#[derive(Debug, Clone)]
pub enum TermInstrKind<'a> {
    Branch(Branch),
    CastBranch(CastBranch<'a>),
    Jump(BlockJump),
    Return(Value),

    /// The block never reaches the end.
    Diverge,
}

impl<'a> TermInstrKind<'a> {
    pub fn jumps_to<'s>(&'s self, bb: Block) -> impl Iterator<Item = &'s BlockJump> + 's {
        self.operands().iter().filter(move |jmp| jmp.bb == bb)
    }

    pub fn jumps_to_mut<'s>(
        &'s mut self,
        bb: Block,
    ) -> impl Iterator<Item = &'s mut BlockJump> + 's {
        self.operands_mut()
            .iter_mut()
            .filter(move |jmp| jmp.bb == bb)
    }

    pub fn succ_bbs(&self) -> impl Iterator<Item = Block> + '_ {
        self.operands().iter().map(|jmp| jmp.bb)
    }
}

impl<'a> From<Branch> for TermInstrKind<'a> {
    fn from(branch: Branch) -> Self {
        TermInstrKind::Branch(branch)
    }
}

impl<'a> From<CastBranch<'a>> for TermInstrKind<'a> {
    fn from(branch: CastBranch<'a>) -> Self {
        TermInstrKind::CastBranch(branch)
    }
}

impl<'a> From<BlockJump> for TermInstrKind<'a> {
    fn from(jmp: BlockJump) -> Self {
        TermInstrKind::Jump(jmp)
    }
}

impl<'a> InstrOperands for TermInstrKind<'a> {
    type Operand = BlockJump;

    fn operands(&self) -> &[BlockJump] {
        match self {
            Self::Branch(br) => br.operands(),
            Self::CastBranch(br) => br.operands(),
            Self::Jump(jmp) => slice::from_ref(jmp),
            Self::Return(_) | Self::Diverge => &[],
        }
    }

    fn operands_mut(&mut self) -> &mut [BlockJump] {
        match self {
            Self::Branch(br) => br.operands_mut(),
            Self::CastBranch(br) => br.operands_mut(),
            Self::Jump(jmp) => slice::from_mut(jmp),
            Self::Return(_) | Self::Diverge => &mut [],
        }
    }
}

new_key_type! {
    pub struct TermInstr;
}
