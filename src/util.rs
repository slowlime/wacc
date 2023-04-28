use std::borrow::Cow;
use std::fmt::{self, Debug, Display};

#[macro_export]
macro_rules! try_match {
    ($e:expr, $(|)? $pattern:pat $( if $guard:expr )? $(,)? => $v:expr) => {
        match $e {
            $pattern $( if $guard )? => Some($v),
            _ => None,
        }
    }
}

pub fn try_min<'a, A: PartialOrd>(lhs: &'a A, rhs: &'a A) -> Option<&'a A> {
    use std::cmp::Ordering::*;

    lhs.partial_cmp(rhs).map(|cmp| match cmp {
        Greater => rhs,
        _ => lhs,
    })
}

pub fn try_max<'a, A: PartialOrd>(lhs: &'a A, rhs: &'a A) -> Option<&'a A> {
    try_min(rhs, lhs)
}

pub fn slice_formatter(slice: &[u8]) -> impl Debug + Display + '_ {
    use byte_string::ByteStr;

    struct SliceFormatter<'a> {
        slice: &'a [u8],
    }

    impl Debug for SliceFormatter<'_> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{:?}", ByteStr::new(self.slice))
        }
    }

    impl Display for SliceFormatter<'_> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", String::from_utf8_lossy(self.slice))
        }
    }

    SliceFormatter { slice }
}

pub fn serialize_bytes_as_string<S>(bytes: &[u8], ser: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    serde::Serialize::serialize(&String::from_utf8_lossy(bytes), ser)
}

pub trait CloneStatic<T>
where
    T: 'static,
{
    fn clone_static(&self) -> T;
}

impl<T: ?Sized + ToOwned> CloneStatic<Cow<'static, T>> for Cow<'_, T>
where
    <T as ToOwned>::Owned: Clone,
    <T as ToOwned>::Owned: 'static,
{
    fn clone_static(&self) -> Cow<'static, T> {
        match self {
            Cow::Borrowed(borrowed) => Cow::Owned((*borrowed).to_owned()),
            Cow::Owned(owned) => Cow::Owned(owned.clone()),
        }
    }
}

impl<T, U> CloneStatic<Option<U>> for Option<T>
where
    T: CloneStatic<U>,
    U: 'static,
{
    fn clone_static(&self) -> Option<U> {
        self.as_ref().map(T::clone_static)
    }
}

impl<T, U> CloneStatic<Vec<U>> for Vec<T>
where
    T: CloneStatic<U>,
    U: 'static,
{
    fn clone_static(&self) -> Vec<U> {
        self.iter().map(T::clone_static).collect()
    }
}

macro_rules! define_byte_string {
    ($($vis:vis struct $name:ident<$lifetime:lifetime>;)+) => {
        $(
            #[derive(Clone, Copy, PartialEq, Eq, Hash)]
            $vis struct $name<$lifetime>(&$lifetime [u8]);

            impl ::std::fmt::Debug for $name<'_> {
                fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                    f.debug_tuple(stringify!($name))
                        .field(&$crate::util::slice_formatter(&self.0))
                        .finish()
                }
            }

            impl ::std::fmt::Display for $name<'_> {
                fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                    write!(f, "{}", $crate::util::slice_formatter(&self.0))
                }
            }

            impl<$lifetime> ::std::convert::From<&$lifetime [u8]> for $name<$lifetime> {
                fn from(value: &$lifetime [u8]) -> Self {
                    Self(value)
                }
            }
        )+
    };
}

pub(crate) use define_byte_string;
