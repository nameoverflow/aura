mod exprs;
mod items;
mod lookahead;
pub(crate) mod operators;
mod patterns;
mod types;

#[cfg(test)]
mod tests;

use crate::ast::*;
use aura_common::Span;
use aura_lexer::token::{Token, TokenKind};

pub struct Parser {
    pub(crate) tokens: Vec<Token>,
    pub(crate) pos: usize,
    pub(crate) errors: Vec<ParseError>,
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "parse error at {}..{}: {}",
            self.span.start, self.span.end, self.message
        )
    }
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            pos: 0,
            errors: Vec::new(),
        }
    }

    pub fn parse_module(&mut self) -> Result<Module, Vec<ParseError>> {
        let start = self.current_span();
        let mut items = Vec::new();

        while !self.at_eof() {
            match self.parse_item() {
                Ok(item) => items.push(item),
                Err(e) => {
                    self.errors.push(e);
                    self.recover_to_next_item();
                }
            }
        }

        if self.errors.is_empty() {
            let end = self.current_span();
            Ok(Module {
                name: None,
                items,
                span: start.merge(end),
            })
        } else {
            Err(self.errors.clone())
        }
    }

    pub fn errors(&self) -> &[ParseError] {
        &self.errors
    }

    // ── Token helpers ──

    pub(crate) fn peek(&self) -> &TokenKind {
        self.tokens
            .get(self.pos)
            .map(|t| &t.kind)
            .unwrap_or(&TokenKind::Eof)
    }

    pub(crate) fn current_span(&self) -> Span {
        self.tokens
            .get(self.pos)
            .map(|t| t.span)
            .unwrap_or(Span::dummy())
    }

    pub(crate) fn advance(&mut self) -> &Token {
        let tok = &self.tokens[self.pos];
        if self.pos < self.tokens.len() - 1 {
            self.pos += 1;
        }
        tok
    }

    pub(crate) fn at_eof(&self) -> bool {
        matches!(self.peek(), TokenKind::Eof)
    }

    pub(crate) fn expect(&mut self, kind: &TokenKind) -> Result<Span, ParseError> {
        if std::mem::discriminant(self.peek()) == std::mem::discriminant(kind) {
            let span = self.current_span();
            self.advance();
            Ok(span)
        } else {
            Err(ParseError {
                message: format!("expected {:?}, found {:?}", kind, self.peek()),
                span: self.current_span(),
            })
        }
    }

    pub(crate) fn expect_ident(&mut self) -> Result<(String, Span), ParseError> {
        match self.peek().clone() {
            TokenKind::Ident(name) => {
                let span = self.current_span();
                self.advance();
                Ok((name, span))
            }
            _ => Err(ParseError {
                message: format!("expected identifier, found {:?}", self.peek()),
                span: self.current_span(),
            }),
        }
    }

    pub(crate) fn expect_upper_ident(&mut self) -> Result<(String, Span), ParseError> {
        match self.peek().clone() {
            TokenKind::UpperIdent(name) => {
                let span = self.current_span();
                self.advance();
                Ok((name, span))
            }
            _ => Err(ParseError {
                message: format!("expected type name, found {:?}", self.peek()),
                span: self.current_span(),
            }),
        }
    }

    pub(crate) fn any_ident(&mut self) -> Result<(String, Span), ParseError> {
        match self.peek().clone() {
            TokenKind::Ident(name) | TokenKind::UpperIdent(name) => {
                let span = self.current_span();
                self.advance();
                Ok((name, span))
            }
            _ => Err(ParseError {
                message: format!("expected identifier, found {:?}", self.peek()),
                span: self.current_span(),
            }),
        }
    }

    pub(crate) fn check(&self, kind: &TokenKind) -> bool {
        std::mem::discriminant(self.peek()) == std::mem::discriminant(kind)
    }

    pub(crate) fn eat(&mut self, kind: &TokenKind) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn recover_to_next_item(&mut self) {
        loop {
            match self.peek() {
                TokenKind::Eof => break,
                TokenKind::Def
                | TokenKind::Type
                | TokenKind::Pub
                | TokenKind::Use
                | TokenKind::Module
                | TokenKind::Concept
                | TokenKind::Instance => break,
                _ => {
                    self.advance();
                }
            }
        }
    }
}
