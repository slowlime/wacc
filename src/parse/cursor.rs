use std::slice;

use crate::position::Position;
use crate::source::{SourceFile, SourceId};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CursorState {
    Eof,
    HeadByte,
    ContinuationByte(u8),
}

#[derive(Debug, Clone)]
pub struct Cursor<'buf> {
    buf: &'buf [u8],
    source_id: SourceId,
    iter: slice::Iter<'buf, u8>,
    pos: Position,
    prev_pos: Option<Position>,
    state: CursorState,
}

impl<'buf> Cursor<'buf> {
    pub fn new(src_file: &SourceFile<'buf>) -> Self {
        let buf = src_file.buf();

        Self {
            buf,
            source_id: src_file.id(),
            iter: buf.iter(),
            pos: Position::with_source_file(src_file),
            prev_pos: None,
            state: CursorState::HeadByte,
        }
    }

    /// Returns the position of the immediately following character.
    pub fn pos(&self) -> Position {
        self.pos
    }

    /// Returns the position of the previously returned character.
    pub fn prev_pos(&self) -> Position {
        self.prev_pos.unwrap_or_else(|| Position::with_source_id(self.source_id))
    }

    pub fn peek(&self) -> Option<u8> {
        self.iter.clone().next().copied()
    }

    pub fn remaining(&self) -> &'buf [u8] {
        &self.buf[self.pos.byte..]
    }

    pub fn starts_with(&self, value: &[u8]) -> bool {
        self.remaining().starts_with(value)
    }

    pub fn consume_expecting(&mut self, expected: &[u8]) -> Option<&'buf [u8]> {
        self.starts_with(expected)
            .then(|| self.consume_n(expected.len()))
    }

    pub fn consume_n(&mut self, n: usize) -> &'buf [u8] {
        let start = self.pos.byte;

        for _ in 0..n {
            self.next();
        }

        let end = self.pos.byte;

        &self.buf[start..end]
    }

    pub fn consume_while(&mut self, mut predicate: impl FnMut(&u8) -> bool) -> &'buf [u8] {
        self.consume_n(self.iter.clone().take_while(|&c| predicate(c)).count())
    }
}

impl<'buf> Iterator for Cursor<'buf> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        use CursorState::*;

        if let Eof = self.state {
            return None;
        }

        self.prev_pos = Some(self.pos);

        let c = self.iter.next().copied();

        let c = if let Some(c) = c {
            c
        } else {
            self.state = Eof;

            return None;
        };

        self.pos.byte += 1;

        self.state = match self.state {
            Eof => unreachable!(),

            HeadByte if c == b'\r' && self.peek() == Some(b'\n') => HeadByte,

            HeadByte if c == b'\n' || c == b'\r' => {
                self.pos.line.checked_add(1).unwrap();
                self.pos.col = 1.try_into().unwrap();

                HeadByte
            }

            HeadByte => {
                self.pos.col.checked_add(1).unwrap();

                match c {
                    // `c` is a single-byte utf-8 sequence
                    0b0000_0000..=0b0111_1111 => HeadByte,

                    // `c` is a utf-8 continuation byte in head position
                    0b1000_0000..=0b1011_1111 => HeadByte,

                    // `c` begins a 2-byte utf-8 sequence
                    0b1100_0000..=0b1101_1111 => ContinuationByte(1),

                    // `c` begins a 3-byte utf-8 sequence
                    0b1110_0000..=0b1110_1111 => ContinuationByte(2),

                    // `c` begins a 4-byte utf-8 sequence
                    0b1111_0000..=0b1111_0111 => ContinuationByte(3),

                    // `c` is an invalid head byte
                    _ => HeadByte,
                }
            }

            ContinuationByte(1) => HeadByte,

            ContinuationByte(remaining) => {
                if (0b1000_0000..=0b1011_1111).contains(&c) {
                    ContinuationByte(remaining - 1)
                } else {
                    // expected a continuation byte, got something wild
                    HeadByte
                }
            }
        };

        Some(c)
    }
}
