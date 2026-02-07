use crate::token::{Token, TokenKind};
use aura_common::Span;

use super::Lexer;

impl<'a> Lexer<'a> {
    pub(crate) fn lex_identifier(&mut self, start: u32, first: char) -> Token {
        let mut ident = String::new();
        ident.push(first);

        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                ident.push(c);
                self.advance();
            } else {
                break;
            }
        }

        let end = self.byte_pos();
        // Check for sole underscore (wildcard pattern)
        if ident == "_" {
            return Token {
                kind: TokenKind::Underscore,
                span: Span::new(self.file_id, start, end),
            };
        }

        // Check keywords
        if let Some(kw) = TokenKind::keyword_from_str(&ident) {
            return Token {
                kind: kw,
                span: Span::new(self.file_id, start, end),
            };
        }

        // Distinguish upper vs lower identifiers
        let kind = if first.is_uppercase() {
            TokenKind::UpperIdent(ident)
        } else {
            TokenKind::Ident(ident)
        };

        Token {
            kind,
            span: Span::new(self.file_id, start, end),
        }
    }
}
