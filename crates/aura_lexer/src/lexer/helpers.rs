use super::Lexer;

impl<'a> Lexer<'a> {
    pub(crate) fn byte_pos(&self) -> u32 {
        if self.pos >= self.chars.len() {
            self.source.len() as u32
        } else {
            self.chars[..self.pos]
                .iter()
                .map(|c| c.len_utf8() as u32)
                .sum()
        }
    }

    pub(crate) fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    pub(crate) fn peek_next(&self) -> Option<char> {
        self.chars.get(self.pos + 1).copied()
    }

    pub(crate) fn advance(&mut self) -> Option<char> {
        let c = self.chars.get(self.pos).copied();
        self.pos += 1;
        c
    }
}
