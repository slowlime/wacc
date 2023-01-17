use std::borrow::Cow;

use once_cell::unsync::OnceCell;

pub struct Dump<'a> {
    bytes: Cow<'a, [u8]>,
    string: OnceCell<String>,
}

impl PartialEq for Dump<'_> {
    fn eq(&self, other: &Dump) -> bool {
        self.bytes == other.bytes
    }
}

impl AsRef<str> for Dump<'_> {
    fn as_ref(&self) -> &str {
        self.string.get_or_init(|| {
            String::from_utf8_lossy(&self.bytes).into_owned()
        })
    }
}

impl<'a> From<&'a [u8]> for Dump<'a> {
    fn from(bytes: &'a [u8]) -> Self {
        Self {
            bytes: Cow::Borrowed(bytes),
            string: OnceCell::new(),
        }
    }
}

impl From<Vec<u8>> for Dump<'_> {
    fn from(bytes: Vec<u8>) -> Self {
        Self {
            bytes: Cow::Owned(bytes),
            string: OnceCell::new(),
        }
    }
}
