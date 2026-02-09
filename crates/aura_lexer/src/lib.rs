pub mod chumsky_lexer;
pub mod interp;
pub mod token;

#[cfg(test)]
mod tests;

pub use token::{Token, TokenKind};

/// Lex source code into a `Vec<Token>`, handling string interpolation.
///
/// This is the primary API — uses chumsky for core tokenization
/// with a post-pass for string interpolation.
pub fn lex(source: &str, file_id: u32) -> Vec<Token> {
    let raw = chumsky_lexer::lex(source, file_id);
    let expanded = interp::expand_interpolations(source, file_id, raw);
    expanded
        .into_iter()
        .map(|(kind, span)| Token { kind, span })
        .collect()
}

/// Backward-compatible lexer struct.
pub struct Lexer<'a> {
    source: &'a str,
    file_id: u32,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str, file_id: u32) -> Self {
        Self { source, file_id }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = lex(self.source, self.file_id);
        // The old API always appends Eof
        let eof_span = if let Some(last) = tokens.last() {
            aura_common::Span::new(self.file_id, last.span.end, last.span.end)
        } else {
            aura_common::Span::new(self.file_id, 0, 0)
        };
        tokens.push(Token {
            kind: TokenKind::Eof,
            span: eof_span,
        });
        tokens
    }
}
