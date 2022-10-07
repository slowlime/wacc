use std::slice;

use crate::position::Position;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum CursorState {
    Eof,
    HeadByte,
    ContinuationByte(u8),
}

#[derive(Debug, Clone)]
pub struct Cursor<'a> {
    buf: &'a [u8],
    iter: slice::Iter<'a, u8>,
    pos: Position,
    prev_pos: Option<Position>,
    state: CursorState,
}

impl<'a> Cursor<'a> {
    pub fn new(buf: &'a [u8]) -> Self {
        Self {
            buf,
            iter: buf.iter(),
            pos: Default::default(),
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
        self.prev_pos.unwrap_or_default()
    }

    pub fn peek(&self) -> Option<u8> {
        self.iter.clone().next().copied()
    }

    pub fn starts_with(&self, value: &[u8]) -> bool {
        self.buf[self.pos.byte..].starts_with(value)
    }
}

impl<'a> Iterator for Cursor<'a> {
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
                self.pos.line += 1;
                self.pos.col = 1;

                HeadByte
            }

            HeadByte => {
                self.pos.col += 1;

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

            ContinuationByte(remaining) => if (0b1000_0000..=0b1011_1111).contains(&c) {
                ContinuationByte(remaining - 1)
            } else {
                // expected a continuation byte, got something wild
                HeadByte
            }
        };

        Some(c)
    }
}
