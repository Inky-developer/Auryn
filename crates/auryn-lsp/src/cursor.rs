use std::str::Chars;

use tower_lsp_server::ls_types::Position;

pub struct Cursor<'a> {
    chars: Chars<'a>,
    pub pos: Position,
}

impl<'a> Cursor<'a> {
    pub fn advance_bytes(&mut self, mut num_bytes: usize) {
        while num_bytes > 0 {
            let Some((_, char)) = self.next() else {
                return;
            };
            num_bytes = num_bytes.saturating_sub(char.len_utf8());
        }
    }
}

impl<'a> From<&'a str> for Cursor<'a> {
    fn from(value: &'a str) -> Self {
        Self {
            chars: value.chars(),
            pos: Position::default(),
        }
    }
}

impl<'a> Iterator for Cursor<'a> {
    type Item = (Position, char);

    fn next(&mut self) -> Option<Self::Item> {
        let char = self.chars.next()?;
        let pos = self.pos;
        match char {
            '\n' => {
                self.pos.line += 1;
                self.pos.character = 0;
            }
            _ => {
                self.pos.character += char.len_utf16() as u32;
            }
        };
        Some((pos, char))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.chars.size_hint()
    }
}
