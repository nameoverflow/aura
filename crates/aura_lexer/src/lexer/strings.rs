use crate::token::{Token, TokenKind};
use aura_common::Span;

use super::Lexer;

impl<'a> Lexer<'a> {
    pub(crate) fn lex_string(&mut self, start: u32) -> Token {
        let mut text = String::new();
        let has_interpolation = false;

        loop {
            match self.peek() {
                None => {
                    let end = self.byte_pos();
                    return Token {
                        kind: TokenKind::Error("unterminated string literal".into()),
                        span: Span::new(self.file_id, start, end),
                    };
                }
                Some('"') => {
                    self.advance();
                    let end = self.byte_pos();
                    if has_interpolation {
                        return Token {
                            kind: TokenKind::StringEnd(text),
                            span: Span::new(self.file_id, start, end),
                        };
                    } else {
                        return Token {
                            kind: TokenKind::StringLit(text),
                            span: Span::new(self.file_id, start, end),
                        };
                    }
                }
                Some('{') => {
                    // Check for escaped brace {{
                    if self.peek_next() == Some('{') {
                        self.advance();
                        self.advance();
                        text.push('{');
                        continue;
                    }
                    // Start interpolation
                    self.advance(); // consume {
                    self.interp_stack.push(0);
                    let end = self.byte_pos();
                    let kind = if has_interpolation {
                        TokenKind::StringMid(text)
                    } else {
                        TokenKind::StringStart(text)
                    };
                    return Token {
                        kind,
                        span: Span::new(self.file_id, start, end),
                    };
                }
                Some('}') if self.peek_next() == Some('}') => {
                    self.advance();
                    self.advance();
                    text.push('}');
                }
                Some('\\') => {
                    self.advance();
                    match self.advance() {
                        Some('n') => text.push('\n'),
                        Some('t') => text.push('\t'),
                        Some('r') => text.push('\r'),
                        Some('\\') => text.push('\\'),
                        Some('"') => text.push('"'),
                        Some('0') => text.push('\0'),
                        Some(c) => {
                            text.push('\\');
                            text.push(c);
                        }
                        None => {
                            let end = self.byte_pos();
                            return Token {
                                kind: TokenKind::Error("unterminated string escape".into()),
                                span: Span::new(self.file_id, start, end),
                            };
                        }
                    }
                }
                Some(c) => {
                    self.advance();
                    text.push(c);
                    // Track if we ever had interpolation for StringEnd
                    // (This will be overwritten if we actually hit interpolation)
                }
            }
        }
    }

    pub(crate) fn lex_string_continuation(&mut self, start: u32) -> Token {
        // We just consumed the } that ended an interpolation.
        // Now continue scanning the string.
        let mut text = String::new();

        loop {
            match self.peek() {
                None => {
                    let end = self.byte_pos();
                    return Token {
                        kind: TokenKind::Error("unterminated string literal".into()),
                        span: Span::new(self.file_id, start, end),
                    };
                }
                Some('"') => {
                    self.advance();
                    let end = self.byte_pos();
                    return Token {
                        kind: TokenKind::StringEnd(text),
                        span: Span::new(self.file_id, start, end),
                    };
                }
                Some('{') => {
                    if self.peek_next() == Some('{') {
                        self.advance();
                        self.advance();
                        text.push('{');
                        continue;
                    }
                    self.advance();
                    self.interp_stack.push(0);
                    let end = self.byte_pos();
                    return Token {
                        kind: TokenKind::StringMid(text),
                        span: Span::new(self.file_id, start, end),
                    };
                }
                Some('}') if self.peek_next() == Some('}') => {
                    self.advance();
                    self.advance();
                    text.push('}');
                }
                Some('\\') => {
                    self.advance();
                    match self.advance() {
                        Some('n') => text.push('\n'),
                        Some('t') => text.push('\t'),
                        Some('r') => text.push('\r'),
                        Some('\\') => text.push('\\'),
                        Some('"') => text.push('"'),
                        Some('0') => text.push('\0'),
                        Some(c) => {
                            text.push('\\');
                            text.push(c);
                        }
                        None => {
                            let end = self.byte_pos();
                            return Token {
                                kind: TokenKind::Error("unterminated string escape".into()),
                                span: Span::new(self.file_id, start, end),
                            };
                        }
                    }
                }
                Some(c) => {
                    self.advance();
                    text.push(c);
                }
            }
        }
    }
}
