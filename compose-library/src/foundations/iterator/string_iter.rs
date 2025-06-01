use ecow::EcoString;

#[derive(Debug, Clone)]
pub struct StringIterator {
    s: EcoString,
    byte_pos: usize,
}

impl StringIterator {
    pub fn new(s: EcoString) -> Self {
        Self { s, byte_pos: 0 }
    }
}

impl Iterator for StringIterator {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        if self.byte_pos >= self.s.len() {
            return None;
        }

        let c = self.s[self.byte_pos..].chars().next()?;
        self.byte_pos += c.len_utf8();
        Some(c)
    }
}

