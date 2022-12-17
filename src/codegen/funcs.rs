use std::borrow::Cow;

use indexmap::IndexMap;
use once_cell::sync::Lazy;
use paste::paste;

use crate::ast::ty::BuiltinClass;

use super::ctx::ty::{RegularTy, WasmTy};
use super::ctx::FuncName;

pub struct SpecialFunc {
    pub name: FuncName<'static>,
    pub ty: WasmTy<'static>,
}

macro_rules! define_special_funcs {
    ($( $section:ident { $( $key:ident => $name:literal : $ty:expr ),* $(,)? } )+) => {
        $( define_special_funcs!(@ $section, $( $key => $name : $ty ),*); )+

        paste! {
            #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
            pub enum SpecialFuncKey {
                $( [< $section:camel >]([< $section:camel FuncKey >]), )+
            }

            $(
                impl From<[< $section:camel FuncKey >]> for SpecialFuncKey {
                    fn from(key: [< $section:camel FuncKey >]) -> SpecialFuncKey {
                        Self::[< $section:camel >](key)
                    }
                }
            )+

            pub fn specials() -> impl Iterator<Item = (SpecialFuncKey, &'static SpecialFunc)> {
                std::iter::empty()
                $( .chain([< $section:upper _FUNCS >].iter().map(|(key, func)| (key.into(), func))) )+
            }
        }
    };

    (@ $section:ident, $( $key:ident => $name:literal : $ty:expr ),* ) => {
        paste! {
            #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
            pub enum [< $section:camel FuncKey >] {
                $( $key, )*
            }

            impl [< $section:camel FuncKey >] {
                pub fn as_str(&self) -> &'static str {
                    match self {
                        $(
                            Self::$key => $name,
                        )*
                    }
                }
            }

            static [< $section:upper _FUNC_TYS >]: Lazy<IndexMap<[< $section:camel FuncKey >], SpecialFunc>> = Lazy::new(|| {
                let mut map = IndexMap::new();

                $(
                    map.insert([< $section:camel FuncKey >]::$key, SpecialFunc {
                        name: FuncName::Plain(Cow::Borrowed($name)),
                        ty: $ty,
                    });
                )*

                map
            });

            pub struct [< $section:camel Provider >](());

            pub static [< $section:upper _FUNCS >]: [< $section:camel Provider >] = [< $section:camel Provider >](());

            impl [< $section:camel Provider >] {
                pub fn get(&self, key: [< $section:camel FuncKey >]) -> &'static SpecialFunc {
                    [< $section:upper _FUNC_TYS >].get(&key).unwrap()
                }

                pub fn iter(&self) -> impl Iterator<Item = ([< $section:camel FuncKey >], &'static SpecialFunc)> {
                    [< $section:upper _FUNC_TYS >].iter().map(|(&key, func)| (key, func))
                }
            }
        }
    }
}

define_special_funcs! {
    Imported {
        Abort => "abort": WasmTy::Func {
            params: vec![],
            ret: None,
        },

        PrintBytes => "print_bytes": WasmTy::Func {
            params: vec![RegularTy::ByteArray],
            ret: None,
        },

        PrintInt => "print_int": WasmTy::Func {
            params: vec![RegularTy::I32],
            ret: None,
        },

        ReadLine => "read_line": WasmTy::Func {
            params: vec![],
            ret: Some(RegularTy::ByteArray),
        },

        ReadInt => "read_int": WasmTy::Func {
            params: vec![],
            ret: Some(RegularTy::I32),
        },
    }

    Builtin {
        StringEq => "string_eq": WasmTy::Func {
            params: vec![RegularTy::ByteArray; 2],
            ret: Some(RegularTy::I32),
        },

        StringConcat => "string_concat": WasmTy::Func {
            params: vec![RegularTy::ByteArray; 2],
            ret: Some(RegularTy::ByteArray),
        },

        StringSubstr => "string_substr": WasmTy::Func {
            params: vec![RegularTy::ByteArray, RegularTy::I32, RegularTy::I32],
            ret: Some(RegularTy::ByteArray),
        },

        Start => "start": WasmTy::Func {
            params: vec![],
            ret: None,
        },
    }
}

pub struct SpecialMethod {
    pub name: &'static [u8],
    pub ty: WasmTy<'static>,
}

macro_rules! define_special_methods {
    ($( $key:ident => $name:literal: $ty:expr ),* $(,)?) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
        pub enum SpecialMethodKey {
            $( $key, )*
        }

        static SPECIAL_METHOD_MAP: Lazy<IndexMap<SpecialMethodKey, SpecialMethod>> = Lazy::new(|| {
            let mut map = IndexMap::new();

            $(
                map.insert(SpecialMethodKey::$key, SpecialMethod {
                    name: $name,
                    ty: $ty,
                });
            )*

            map
        });

        pub struct SpecialMethodProvider(());

        pub const SPECIAL_METHODS: SpecialMethodProvider = SpecialMethodProvider(());

        impl SpecialMethodProvider {
            pub fn get(&self, key: SpecialMethodKey) -> &'static SpecialMethod {
                SPECIAL_METHOD_MAP.get(&key).unwrap()
            }

            pub fn iter(&self) -> impl Iterator<Item = (SpecialMethodKey, &'static SpecialMethod)> {
                SPECIAL_METHOD_MAP.iter()
                    .map(|(&key, method)| (key, method))
            }
        }
    }
}

define_special_methods! {
    // actually it's () -> SELF_TYPE
    Constructor => b"{new}": WasmTy::Func {
        params: vec![],
        ret: Some(BuiltinClass::Object.into()),
    },

    // actually it's (SELF_TYPE) -> SELF_TYPE
    Initializer => b"{init}": WasmTy::Func {
        params: vec![BuiltinClass::Object.into()],
        ret: Some(BuiltinClass::Object.into()),
    },

    // actually it's (SELF_TYPE) -> SELF_TYPE
    Copy => b"{copy}": WasmTy::Func {
        params: vec![BuiltinClass::Object.into()],
        ret: Some(BuiltinClass::Object.into()),
    },

    // actually it's () -> bytes
    TypeName => b"{type_name}": WasmTy::Func {
        params: vec![],
        ret: Some(RegularTy::ByteArray.into()),
    },
}
