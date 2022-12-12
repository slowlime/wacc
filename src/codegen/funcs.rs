use std::borrow::Cow;
use std::collections::HashMap;

use once_cell::sync::Lazy;
use paste::paste;

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
            #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

            static [< $section:upper _FUNC_TYS >]: Lazy<HashMap<[< $section:camel FuncKey >], SpecialFunc>> = Lazy::new(|| {
                let mut map = HashMap::new();

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
    Builtin {
        StringEq => "string_eq": WasmTy::Func {
            params: vec![RegularTy::ByteArray; 2],
            ret: Some(RegularTy::I32),
        },
    }

    Imported {
        Abort => "abort": WasmTy::Func {
            params: vec![],
            ret: None,
        },
    }
}
