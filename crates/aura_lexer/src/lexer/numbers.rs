use crate::token::{Token, TokenKind};
use aura_common::Span;

use super::Lexer;

impl<'a> Lexer<'a> {
    pub(crate) fn lex_number(&mut self, start: u32, first: char) -> Token {
        // Check for hex (0x), binary (0b), octal (0o)
        if first == '0' {
            match self.peek() {
                Some('x') | Some('X') => {
                    self.advance();
                    return self.lex_radix_int(start, 16, |c| c.is_ascii_hexdigit());
                }
                Some('b') | Some('B') => {
                    self.advance();
                    return self.lex_radix_int(start, 2, |c| c == '0' || c == '1');
                }
                Some('o') | Some('O') => {
                    self.advance();
                    return self.lex_radix_int(start, 8, |c| ('0'..='7').contains(&c));
                }
                _ => {}
            }
        }

        let mut num_str = String::new();
        num_str.push(first);
        let mut is_float = false;

        while let Some(c) = self.peek() {
            if c.is_ascii_digit() || c == '_' {
                if c != '_' {
                    num_str.push(c);
                }
                self.advance();
            } else if c == '.' && self.peek_next() != Some('.') && !is_float {
                // Check it's not .. range operator
                if let Some(next) = self.peek_next() {
                    if next.is_ascii_digit() {
                        is_float = true;
                        num_str.push('.');
                        self.advance();
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        let end = self.byte_pos();
        let kind = if is_float {
            match num_str.parse::<f64>() {
                Ok(v) => TokenKind::FloatLit(v),
                Err(e) => TokenKind::Error(format!("invalid float literal: {e}")),
            }
        } else {
            match num_str.parse::<i64>() {
                Ok(v) => TokenKind::IntLit(v),
                Err(e) => TokenKind::Error(format!("invalid integer literal: {e}")),
            }
        };
        Token {
            kind,
            span: Span::new(self.file_id, start, end),
        }
    }

    pub(crate) fn lex_radix_int(
        &mut self,
        start: u32,
        radix: u32,
        is_digit: impl Fn(char) -> bool,
    ) -> Token {
        let mut num_str = String::new();
        while let Some(c) = self.peek() {
            if is_digit(c) {
                num_str.push(c);
                self.advance();
            } else if c == '_' {
                self.advance();
            } else {
                break;
            }
        }
        let end = self.byte_pos();
        if num_str.is_empty() {
            return Token {
                kind: TokenKind::Error("expected digits after radix prefix".into()),
                span: Span::new(self.file_id, start, end),
            };
        }
        let kind = match i64::from_str_radix(&num_str, radix) {
            Ok(v) => TokenKind::IntLit(v),
            Err(e) => TokenKind::Error(format!("invalid integer literal: {e}")),
        };
        Token {
            kind,
            span: Span::new(self.file_id, start, end),
        }
    }
}
