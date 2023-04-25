use crate::ast::ty::ResolvedTy;
use crate::ir::ty::IrTy;

use super::GlobalCtx;

impl<'a> GlobalCtx<'a> {
    pub fn lower_ty(&self, ty: &ResolvedTy<'_>) -> IrTy<'a> {
        todo!()
    }
}
