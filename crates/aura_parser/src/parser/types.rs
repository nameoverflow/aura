use crate::ast::*;
use aura_lexer::token::TokenKind;

use super::{ParseError, Parser};

impl Parser {
    // ── Type expression parsing ──

    pub(crate) fn parse_type_expr(&mut self) -> Result<TypeExpr, ParseError> {
        if self.check(&TokenKind::Forall) {
            self.parse_type_forall()
        } else {
            self.parse_type_function()
        }
    }

    fn parse_type_forall(&mut self) -> Result<TypeExpr, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::Forall)?;
        self.expect(&TokenKind::LParen)?;

        let mut constraints = Vec::new();
        if !self.check(&TokenKind::RParen) {
            loop {
                let cstart = self.current_span();
                let (concept, _) = self.any_ident()?;
                let (ty_var, vspan) = self.expect_ident()?;
                constraints.push(ConceptConstraint {
                    concept,
                    ty_var,
                    span: cstart.merge(vspan),
                });
                if !self.eat(&TokenKind::Comma) {
                    break;
                }
            }
        }

        self.expect(&TokenKind::RParen)?;
        self.expect(&TokenKind::Dot)?;
        let body = self.parse_type_expr()?;
        let span = start.merge(body.span());
        Ok(TypeExpr::Forall(constraints, Box::new(body), span))
    }

    fn parse_type_function(&mut self) -> Result<TypeExpr, ParseError> {
        let first = self.parse_type_product()?;

        if self.eat(&TokenKind::Arrow) {
            let ret = self.parse_type_function()?;
            let effects = if self.check(&TokenKind::LBracket) {
                Some(self.parse_effect_list()?)
            } else {
                None
            };
            let end_span = effects
                .as_ref()
                .and_then(|e| e.last().map(|x| x.span))
                .unwrap_or_else(|| ret.span());
            let span = first.span().merge(end_span);
            // If first is a product, its elements are the params
            let params = match first {
                TypeExpr::Product(ts, _) => ts,
                other => vec![other],
            };
            Ok(TypeExpr::Function(params, Box::new(ret), effects, span))
        } else {
            Ok(first)
        }
    }

    fn parse_type_product(&mut self) -> Result<TypeExpr, ParseError> {
        let first = self.parse_type_app()?;

        if self.check(&TokenKind::Star) {
            let mut types = vec![first];
            while self.eat(&TokenKind::Star) {
                types.push(self.parse_type_app()?);
            }
            let span = types
                .first()
                .unwrap()
                .span()
                .merge(types.last().unwrap().span());
            Ok(TypeExpr::Product(types, span))
        } else {
            Ok(first)
        }
    }

    fn parse_type_app(&mut self) -> Result<TypeExpr, ParseError> {
        let base = self.parse_type_atom()?;

        // Haskell-style type application: `List Int`, `Result User Error`
        // Only apply if base is a named type and next token starts a type
        match &base {
            TypeExpr::Named(_, _) => {
                let mut args = Vec::new();
                while self.is_type_atom_start() {
                    args.push(self.parse_type_atom()?);
                }
                if args.is_empty() {
                    Ok(base)
                } else {
                    let span = base.span().merge(args.last().unwrap().span());
                    Ok(TypeExpr::App(Box::new(base), args, span))
                }
            }
            _ => Ok(base),
        }
    }

    pub(crate) fn is_type_atom_start(&self) -> bool {
        match self.peek() {
            TokenKind::UpperIdent(_) => true,
            TokenKind::Ident(name) => {
                // Lowercase identifiers in type position are type variables.
                name.chars()
                    .next()
                    .map(|c| c.is_ascii_lowercase())
                    .unwrap_or(false)
            }
            TokenKind::LParen => true,
            _ => false,
        }
    }

    pub(crate) fn parse_type_atom(&mut self) -> Result<TypeExpr, ParseError> {
        match self.peek().clone() {
            TokenKind::UpperIdent(_) | TokenKind::Ident(_) => {
                let span = self.current_span();
                let (mut name, mut end) = self.any_ident()?;
                while self.eat(&TokenKind::Dot) {
                    let (seg, seg_span) = self.any_ident()?;
                    name.push('.');
                    name.push_str(&seg);
                    end = seg_span;
                }
                Ok(TypeExpr::Named(name, span.merge(end)))
            }
            TokenKind::LParen => {
                self.advance();
                if self.check(&TokenKind::RParen) {
                    let span = self.current_span();
                    self.advance();
                    return Ok(TypeExpr::Unit(span));
                }
                let inner = self.parse_type_expr()?;
                self.expect(&TokenKind::RParen)?;
                Ok(inner)
            }
            _ => Err(ParseError {
                message: format!("expected type, found {:?}", self.peek()),
                span: self.current_span(),
            }),
        }
    }
}
