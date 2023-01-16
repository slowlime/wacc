use std::fs;
use std::io;
use std::num::NonZeroUsize;
use std::path::{Path, PathBuf};

use elsa::FrozenVec;
use serde::Serialize;

#[derive(Serialize, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourceId(NonZeroUsize);

#[derive(Default)]
pub struct SourceBuffer(FrozenVec<Vec<u8>>);

impl SourceBuffer {
    pub fn new() -> Self {
        Self(FrozenVec::new())
    }
}

pub struct Source<'buf> {
    buf: &'buf SourceBuffer,
    files: Vec<SourceFile<'buf>>,
}

#[derive(Debug)]
pub struct SourceFile<'buf> {
    id: SourceId,
    path: PathBuf,
    buf: &'buf [u8],
}

impl<'buf> Source<'buf> {
    pub fn new(buf: &'buf mut SourceBuffer) -> Self {
        // take a &mut to ensure exclusivity of the borrow

        Self {
            buf: &*buf,
            files: Vec::new(),
        }
    }

    pub fn load_from_string(&mut self, path: PathBuf, buf: Vec<u8>) -> SourceId {
        let idx = self.files.len();
        assert_eq!(self.buf.0.len(), idx);
        let id = SourceId((idx + 1).try_into().unwrap());

        self.buf.0.push(buf);
        let buf: &'buf [u8] = &self.buf.0[idx];

        self.files.push(SourceFile { id, path, buf });

        id
    }

    pub fn load(&mut self, path: PathBuf) -> io::Result<SourceId> {
        let buf = fs::read(&path)?;

        Ok(self.load_from_string(path, buf))
    }

    pub fn get(&self, id: SourceId) -> Option<&SourceFile<'buf>> {
        self.files.get(usize::from(id.0) - 1)
    }

    pub fn iter(&self) -> impl Iterator<Item = &SourceFile<'buf>> {
        self.files.iter()
    }
}

impl<'buf> SourceFile<'buf> {
    pub fn id(&self) -> SourceId {
        self.id
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn buf(&self) -> &'buf [u8] {
        self.buf
    }
}
