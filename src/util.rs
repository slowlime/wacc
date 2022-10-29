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
