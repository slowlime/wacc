use std::borrow::{Cow, Borrow};
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
