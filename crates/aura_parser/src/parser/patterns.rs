use crate::ast::*;
use aura_lexer::token::TokenKind;

use super::operators::pattern_span;
use super::{ParseError, Parser};

impl Parser {
    pub(crate) fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        let first = self.parse_pattern_atom()?;
        if !self.eat(&TokenKind::Pipe) {
            return Ok(first);
        }

        let mut patterns = vec![first];
        loop {
            patterns.push(self.parse_pattern_atom()?);
            if !self.eat(&TokenKind::Pipe) {
                break;
            }
        }

        let span = patterns
            .first()
            .map(pattern_span)
            .unwrap_or_else(|| self.current_span())
            .merge(patterns.last().map(pattern_span).unwrap_or_else(|| self.current_span()));
        Ok(Pattern::Or(patterns, span))
    }

    fn parse_pattern_atom(&mut self) -> Result<Pattern, ParseError> {
        match self.peek().clone() {
            TokenKind::Underscore => {
                let span = self.current_span();
                self.advance();
                Ok(Pattern::Wildcard(span))
            }
            TokenKind::IntLit(n) => {
                let span = self.current_span();
                self.advance();
                Ok(Pattern::Literal(LitPattern::Int(n), span))
            }
            TokenKind::FloatLit(n) => {
                let span = self.current_span();
                self.advance();
                Ok(Pattern::Literal(LitPattern::Float(n), span))
            }
            TokenKind::StringLit(s) => {
                let span = self.current_span();
                self.advance();
                Ok(Pattern::Literal(LitPattern::String(s), span))
            }
            TokenKind::True => {
                let span = self.current_span();
                self.advance();
                Ok(Pattern::Literal(LitPattern::Bool(true), span))
            }
            TokenKind::False => {
                let span = self.current_span();
                self.advance();
                Ok(Pattern::Literal(LitPattern::Bool(false), span))
            }
            TokenKind::Ident(name) => {
                let span = self.current_span();
                self.advance();
                Ok(Pattern::Ident(name, span))
            }
            TokenKind::UpperIdent(name) => {
                let span = self.current_span();
                self.advance();
                if self.eat(&TokenKind::LBrace) {
                    let mut fields = Vec::new();
                    let mut has_rest = false;
                    if !self.check(&TokenKind::RBrace) {
                        loop {
                            if self.eat(&TokenKind::DotDot) {
                                has_rest = true;
                            } else {
                                let (field_name, fspan) = self.expect_ident()?;
                                let field_pattern = if self.eat(&TokenKind::Colon) {
                                    self.parse_pattern()?
                                } else {
                                    Pattern::Ident(field_name.clone(), fspan)
                                };
                                let f_end = pattern_span(&field_pattern);
                                fields.push(FieldPattern {
                                    name: field_name,
                                    pattern: field_pattern,
                                    span: fspan.merge(f_end),
                                });
                            }
                            if !self.eat(&TokenKind::Comma) {
                                break;
                            }
                            if self.check(&TokenKind::RBrace) {
                                break;
                            }
                        }
                    }
                    let end = self.current_span();
                    self.expect(&TokenKind::RBrace)?;
                    Ok(Pattern::Struct(name, fields, has_rest, span.merge(end)))
                }
                // Constructor pattern: Variant(args)
                else if self.eat(&TokenKind::LParen) {
                    let mut args = Vec::new();
                    if !self.check(&TokenKind::RParen) {
                        args.push(self.parse_pattern()?);
                        while self.eat(&TokenKind::Comma) {
                            args.push(self.parse_pattern()?);
                        }
                    }
                    let end = self.current_span();
                    self.expect(&TokenKind::RParen)?;
                    Ok(Pattern::Constructor(name, args, span.merge(end)))
                } else {
                    Ok(Pattern::Constructor(name, Vec::new(), span))
                }
            }
            TokenKind::LParen => {
                let start = self.current_span();
                self.advance();
                let mut pats = Vec::new();
                if !self.check(&TokenKind::RParen) {
                    pats.push(self.parse_pattern()?);
                    while self.eat(&TokenKind::Comma) {
                        pats.push(self.parse_pattern()?);
                    }
                }
                let end = self.current_span();
                self.expect(&TokenKind::RParen)?;
                Ok(Pattern::Tuple(pats, start.merge(end)))
            }
            _ => Err(ParseError {
                message: format!("expected pattern, found {:?}", self.peek()),
                span: self.current_span(),
            }),
        }
    }
}
