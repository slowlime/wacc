/// Derives `PartialEq`, `Eq`, and `Hash` on the reference to the provided type.
///
/// It's assumed a value does not move, so the address uniquely identifies it.
macro_rules! derive_ref_eq {
    (&$lifetime:lifetime $ty:ty) => {
        impl<$lifetime> PartialEq for &$lifetime $ty {
            fn eq(&self, other: &Self) -> bool {
                return ::std::ptr::eq(self, other);
            }
        }

        impl<$lifetime> Eq for &$lifetime $ty {}

        impl<$lifetime> ::std::hash::Hash for &$lifetime $ty {
            fn hash<H>(&self, state: &mut H)
            where
                H: ::std::hash::Hasher,
            {
                ::std::ptr::hash(self, state)
            }
        }
    }
}

pub(crate) use derive_ref_eq;
