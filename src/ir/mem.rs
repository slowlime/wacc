use elsa::FrozenMap;

#[derive(Default)]
pub struct Arena {
    strings: FrozenMap<Vec<u8>, Box<()>>,
}

impl Arena {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn alloc_bytes<'a>(&'a self, bytes: &[u8]) -> &'a [u8] {
        if let Some((bytes, _)) = self.strings.get_key_value(bytes) {
            return bytes;
        }

        self.strings.insert(bytes.to_owned(), Box::new(()));
        self.strings.get_key_value(bytes).unwrap().0
    }

    pub fn alloc<'a, T: From<&'a [u8]>>(&'a self, bytes: &[u8]) -> T {
        self.alloc_bytes(bytes).into()
    }
}

pub type ArenaRef<'a> = &'a Arena;
