#[macro_export]
macro_rules! try_match {
    ($e:expr, $(|)? $pattern:pat $( if $guard:expr )? $(,)? => $v:expr) => {
        match $e {
            $pattern $( if $guard )? => Some($v),
            _ => None,
        }
    }
}
