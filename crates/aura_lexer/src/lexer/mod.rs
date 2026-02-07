mod helpers;
mod identifiers;
mod numbers;
mod strings;

#[cfg(test)]
mod tests;

use crate::token::{Token, TokenKind};
use aura_common::Span;

pub struct Lexer<'a> {
    pub(crate) source: &'a str,
    pub(crate) chars: Vec<char>,
    pub(crate) pos: usize,
    pub(crate) file_id: u32,
    /// Stack for tracking string interpolation nesting.
    /// Each entry is the brace depth within an interpolation.
    pub(crate) interp_stack: Vec<u32>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str, file_id: u32) -> Self {
        Self {
            source,
            chars: source.chars().collect(),
            pos: 0,
            file_id,
            interp_stack: Vec::new(),
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        loop {
            let tok = self.next_token();
            let is_eof = tok.kind == TokenKind::Eof;
            tokens.push(tok);
            if is_eof {
                break;
            }
        }
        tokens
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            // Skip whitespace
            while let Some(c) = self.peek() {
                if c.is_whitespace() {
                    self.advance();
                } else {
                    break;
                }
            }

            // Skip line comments (but not doc comments)
            if self.peek() == Some('/') && self.peek_next() == Some('/') {
                // Check for doc comment ///
                if self.chars.get(self.pos + 2) == Some(&'/') {
                    break; // Don't skip doc comments
                }
                // Regular comment, skip to end of line
                while let Some(c) = self.advance() {
                    if c == '\n' {
                        break;
                    }
                }
                continue;
            }

            break;
        }
    }

    fn make_token(&self, kind: TokenKind, start: u32) -> Token {
        Token {
            kind,
            span: Span::new(self.file_id, start, self.byte_pos()),
        }
    }

    fn next_token(&mut self) -> Token {
        // If we're inside a string interpolation and see }, end the interpolation
        if !self.interp_stack.is_empty() {
            if let Some(&depth) = self.interp_stack.last() {
                if depth == 0 && self.peek() == Some('}') {
                    let start = self.byte_pos();
                    self.advance();
                    self.interp_stack.pop();
                    // Continue scanning string after interpolation
                    return self.lex_string_continuation(start);
                }
            }
        }

        self.skip_whitespace_and_comments();

        let start = self.byte_pos();
        let ch = match self.advance() {
            Some(c) => c,
            None => {
                return Token {
                    kind: TokenKind::Eof,
                    span: Span::new(self.file_id, start, start),
                };
            }
        };

        match ch {
            // Doc comments
            '/' if self.peek() == Some('/') && self.chars.get(self.pos + 1) == Some(&'/') => {
                self.advance(); // second /
                self.advance(); // third /
                                // Skip optional leading space
                if self.peek() == Some(' ') {
                    self.advance();
                }
                let mut text = String::new();
                while let Some(c) = self.peek() {
                    if c == '\n' {
                        break;
                    }
                    text.push(c);
                    self.advance();
                }
                let end = self.byte_pos();
                Token {
                    kind: TokenKind::DocComment(text),
                    span: Span::new(self.file_id, start, end),
                }
            }

            // String literals with interpolation
            '"' => self.lex_string(start),

            // Numbers
            c if c.is_ascii_digit() => self.lex_number(start, c),

            // Identifiers & keywords
            c if c.is_alphabetic() || c == '_' => self.lex_identifier(start, c),

            // Operators & delimiters
            '+' => self.make_token(TokenKind::Plus, start),
            '-' => {
                if self.peek() == Some('>') {
                    self.advance();
                    self.make_token(TokenKind::Arrow, start)
                } else {
                    self.make_token(TokenKind::Minus, start)
                }
            }
            '*' => self.make_token(TokenKind::Star, start),
            '/' => self.make_token(TokenKind::Slash, start),
            '%' => self.make_token(TokenKind::Percent, start),
            '=' => {
                if self.peek() == Some('=') {
                    self.advance();
                    self.make_token(TokenKind::Eq, start)
                } else if self.peek() == Some('>') {
                    self.advance();
                    self.make_token(TokenKind::FatArrow, start)
                } else {
                    self.make_token(TokenKind::Assign, start)
                }
            }
            '!' => {
                if self.peek() == Some('=') {
                    self.advance();
                    self.make_token(TokenKind::NotEq, start)
                } else {
                    self.make_token(TokenKind::Error("unexpected '!'".into()), start)
                }
            }
            '<' => {
                if self.peek() == Some('=') {
                    self.advance();
                    self.make_token(TokenKind::LtEq, start)
                } else {
                    self.make_token(TokenKind::Lt, start)
                }
            }
            '>' => {
                if self.peek() == Some('=') {
                    self.advance();
                    self.make_token(TokenKind::GtEq, start)
                } else {
                    self.make_token(TokenKind::Gt, start)
                }
            }
            '|' => {
                if self.peek() == Some('>') {
                    self.advance();
                    self.make_token(TokenKind::Pipeline, start)
                } else {
                    self.make_token(TokenKind::Pipe, start)
                }
            }
            '?' => self.make_token(TokenKind::Question, start),
            '.' => {
                if self.peek() == Some('.') {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        self.make_token(TokenKind::DotDotEq, start)
                    } else {
                        self.make_token(TokenKind::DotDot, start)
                    }
                } else {
                    self.make_token(TokenKind::Dot, start)
                }
            }
            '(' => self.make_token(TokenKind::LParen, start),
            ')' => self.make_token(TokenKind::RParen, start),
            '{' => {
                // Track brace depth inside interpolation
                if let Some(depth) = self.interp_stack.last_mut() {
                    *depth += 1;
                }
                self.make_token(TokenKind::LBrace, start)
            }
            '}' => {
                // Track brace depth inside interpolation
                if let Some(depth) = self.interp_stack.last_mut() {
                    *depth = depth.saturating_sub(1);
                }
                self.make_token(TokenKind::RBrace, start)
            }
            '[' => self.make_token(TokenKind::LBracket, start),
            ']' => self.make_token(TokenKind::RBracket, start),
            ',' => self.make_token(TokenKind::Comma, start),
            ':' => {
                if self.peek() == Some(':') {
                    self.advance();
                    self.make_token(TokenKind::ColonColon, start)
                } else {
                    self.make_token(TokenKind::Colon, start)
                }
            }
            ';' => self.make_token(TokenKind::Semicolon, start),
            '@' => self.make_token(TokenKind::At, start),

            c => {
                let end = self.byte_pos();
                Token {
                    kind: TokenKind::Error(format!("unexpected character '{c}'")),
                    span: Span::new(self.file_id, start, end),
                }
            }
        }
    }
}
