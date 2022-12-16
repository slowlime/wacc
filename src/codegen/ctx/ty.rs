use indexmap::IndexMap;
use once_cell::sync::Lazy;

use crate::analysis::{ClassName, TypeCtx};
use crate::ast::ty::{BuiltinClass, FunctionTy, ResolvedTy};
use crate::try_match;
use crate::util::slice_formatter;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RegularTy<'buf> {
    /// The unboxed i32 type.
    I32,
    ByteArray,
    Class(ClassName<'buf>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum WasmTy<'buf> {
    Regular(RegularTy<'buf>),

    Func {
        params: Vec<RegularTy<'buf>>,
        ret: Option<RegularTy<'buf>>,
    },
}

impl<'buf> RegularTy<'buf> {
    pub fn class_name(&self) -> Option<&ClassName<'buf>> {
        try_match!(self, Self::Class(class_name) => class_name)
    }
}

impl<'buf> WasmTy<'buf> {
    pub fn is_boxed(&self) -> bool {
        !matches!(self, Self::Regular(RegularTy::I32))
    }

    pub fn class_name(&self) -> Option<&ClassName<'buf>> {
        try_match!(self, Self::Regular(RegularTy::Class(class_name)) => class_name)
    }
}

impl<'buf> TryFrom<ResolvedTy<'buf>> for RegularTy<'buf> {
    type Error = ();

    fn try_from(ty: ResolvedTy<'buf>) -> Result<RegularTy<'buf>, ()> {
        Ok(match ty {
            ResolvedTy::Builtin(builtin) => Self::Class(builtin.into()),
            ResolvedTy::SelfType { enclosed } => (*enclosed).try_into().unwrap(),
            ResolvedTy::Class(name) => Self::Class(name.into()),
            ResolvedTy::Function(_) => return Err(()),
            ResolvedTy::Bottom | ResolvedTy::Untyped => panic!("the ast must have passed typeck"),
        })
    }
}

impl<'buf> From<BuiltinClass> for RegularTy<'buf> {
    fn from(builtin: BuiltinClass) -> RegularTy<'buf> {
        RegularTy::Class(builtin.into())
    }
}

impl<'buf> From<ClassName<'buf>> for RegularTy<'buf> {
    fn from(class_name: ClassName<'buf>) -> RegularTy<'buf> {
        RegularTy::Class(class_name)
    }
}

impl<'buf> From<FunctionTy<'buf>> for WasmTy<'buf> {
    fn from(FunctionTy { params, ret }: FunctionTy<'buf>) -> Self {
        Self::Func {
            params: params
                .into_iter()
                .map(|param| param.try_into().unwrap())
                .collect(),
            ret: Some((*ret).try_into().unwrap()),
        }
    }
}

impl<'buf> From<ClassName<'buf>> for WasmTy<'buf> {
    fn from(class_name: ClassName<'buf>) -> Self {
        assert_ne!(class_name, ClassName::SelfType);

        Self::Regular(RegularTy::Class(class_name))
    }
}

impl<'buf> From<BuiltinClass> for WasmTy<'buf> {
    fn from(builtin: BuiltinClass) -> Self {
        Self::Regular(RegularTy::Class(builtin.into()))
    }
}

impl<'buf> From<ResolvedTy<'buf>> for WasmTy<'buf> {
    fn from(ty: ResolvedTy<'buf>) -> WasmTy<'buf> {
        match ty {
            ResolvedTy::Builtin(_) | ResolvedTy::SelfType { .. } | ResolvedTy::Class(_) => {
                Self::Regular(ty.try_into().unwrap())
            }
            ResolvedTy::Function(ty) => ty.into(),

            ResolvedTy::Bottom | ResolvedTy::Untyped => panic!("the ast must have passed typeck"),
        }
    }
}

impl<'buf> From<RegularTy<'buf>> for WasmTy<'buf> {
    fn from(ty: RegularTy<'buf>) -> WasmTy<'buf> {
        Self::Regular(ty)
    }
}

pub fn get_method_ty<'buf>(
    ty_ctx: &TypeCtx<'buf>,
    class_name: &ClassName<'buf>,
    method_name: &[u8],
) -> WasmTy<'buf> {
    let chain = ty_ctx.inheritance_chain(class_name).collect::<Vec<_>>();
    let method = chain
        .into_iter()
        .rev()
        .find_map(|(name, index)| index.get_method_ty(method_name).map(|(_, ty)| (name, ty)));
    let Some((def_class_name, method_ty)) = method else {
        panic!("Class {} does not have method {}", class_name, slice_formatter(method_name));
    };
    let mut method_ty = method_ty.clone();
    method_ty
        .params
        .insert(0, def_class_name.clone().try_into().unwrap());

    method_ty.into()
}

macro_rules! define_well_known_types {
    ($( $key:ident => $ty:expr ),* $(,)?) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
        pub enum WellKnownTyKey {
            $( $key, )*
        }

        static WELL_KNOWN_TY_MAP: Lazy<IndexMap<WellKnownTyKey, WasmTy<'static>>> = Lazy::new(|| {
            let mut map = IndexMap::new();

            $(
                map.insert(WellKnownTyKey::$key, $ty);
            )*

            map
        });

        pub struct WellKnownTyProvider(());

        pub const WELL_KNOWN_TYS: WellKnownTyProvider = WellKnownTyProvider(());

        impl WellKnownTyProvider {
            pub fn get(&self, key: WellKnownTyKey) -> &'static WasmTy<'static> {
                WELL_KNOWN_TY_MAP.get(&key).unwrap()
            }

            pub fn iter(&self) -> impl Iterator<Item = (WellKnownTyKey, &'static WasmTy<'static>)> {
                WELL_KNOWN_TY_MAP.iter()
                    .map(|(&key, wasm_ty)| (key, wasm_ty))
            }
        }
    }
}

define_well_known_types! {
    ByteArray => RegularTy::ByteArray.into(),

    FuncToI32 => WasmTy::Func {
        params: vec![],
        ret: Some(RegularTy::I32),
    },

    FuncBytesToBytes => WasmTy::Func {
        params: vec![RegularTy::ByteArray],
        ret: Some(RegularTy::ByteArray),
    },

    FuncEmpty => WasmTy::Func {
        params: vec![],
        ret: None,
    },
}
