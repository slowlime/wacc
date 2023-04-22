use std::cell::RefCell;
use std::ops::Deref;

use slotmap::SlotMap;

use crate::util::define_byte_string;

use super::bb::{Block, BlockId};
use super::instr::{InstrId, Instr};
use super::ty::{IrTy, HasIrTy};
use super::util::derive_ref_eq;
use super::value::{Value, ValueId};

define_byte_string! {
    pub struct FuncName<'a>;
}

#[derive(Debug)]
pub struct Param<'a> {
    pub func: Func<'a>,
    pub ty: IrTy<'a>,
    pub index: usize,
}

impl<'a> HasIrTy<'a> for Param<'a> {
    fn ty(&self) -> &IrTy<'a> {
        &self.ty
    }
}

#[derive(Debug)]
pub struct FuncInner<'a> {
    pub name: FuncName<'a>,
    pub bbs: SlotMap<BlockId, Block<'a>>,
    pub instrs: SlotMap<InstrId, Instr<'a>>,
    pub values: SlotMap<ValueId, Value<'a>>,
    pub params: Vec<ValueId>,
}

#[derive(Debug)]
pub struct FuncCell<'a>(RefCell<FuncInner<'a>>);

impl<'a> Deref for FuncCell<'a> {
    type Target = RefCell<FuncInner<'a>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub type Func<'a> = &'a FuncCell<'a>;
derive_ref_eq!(&'a FuncCell<'a>);
