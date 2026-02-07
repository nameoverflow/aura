use crate::ast::*;
use aura_common::Span;
use aura_lexer::token::TokenKind;

use super::operators::{infix_bp, pattern_span, postfix_bp, token_to_binop, PREFIX_BP};
use super::{ParseError, Parser};

impl Parser {
    // ── Expression parsing (Pratt) ──

    pub(crate) fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_expr_bp(0)
    }

    fn parse_expr_bp(&mut self, min_bp: u8) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_prefix()?;

        loop {
            // Postfix operators
            match self.peek() {
                TokenKind::Question => {
                    let (_, r_bp) = postfix_bp(&TokenKind::Question);
                    if r_bp < min_bp {
                        break;
                    }
                    self.advance();
                    let span = lhs.span().merge(self.current_span());
                    lhs = Expr::Try(Box::new(lhs), span);
                    continue;
                }
                TokenKind::Dot => {
                    let (_, r_bp) = postfix_bp(&TokenKind::Dot);
                    if r_bp < min_bp {
                        break;
                    }
                    self.advance();
                    let (field, fspan) = self.any_ident()?;
                    if self.check(&TokenKind::LParen) {
                        // Method call
                        self.advance();
                        let args = self.parse_arg_list()?;
                        self.expect(&TokenKind::RParen)?;
                        let span = lhs.span().merge(self.current_span());
                        lhs = Expr::MethodCall(Box::new(lhs), field, args, span);
                    } else {
                        let span = lhs.span().merge(fspan);
                        lhs = Expr::FieldAccess(Box::new(lhs), field, span);
                    }
                    continue;
                }
                TokenKind::LParen => {
                    // Function call — only if high enough precedence
                    let (_, r_bp) = postfix_bp(&TokenKind::LParen);
                    if r_bp < min_bp {
                        break;
                    }
                    self.advance();
                    let args = self.parse_arg_list()?;
                    self.expect(&TokenKind::RParen)?;
                    let span = lhs.span().merge(self.current_span());
                    lhs = Expr::Call(Box::new(lhs), args, span);
                    continue;
                }
                _ => {}
            }

            // Infix operators
            if let Some((l_bp, r_bp)) = infix_bp(self.peek()) {
                if l_bp < min_bp {
                    break;
                }

                let op_token = self.peek().clone();
                self.advance();

                // value |> .method(args)  ==> value.method(args)
                if matches!(op_token, TokenKind::Pipeline) && self.eat(&TokenKind::Dot) {
                    let (method, mspan) = self.any_ident()?;
                    let args = if self.eat(&TokenKind::LParen) {
                        let args = self.parse_arg_list()?;
                        self.expect(&TokenKind::RParen)?;
                        args
                    } else {
                        Vec::new()
                    };
                    let span = lhs.span().merge(self.current_span().merge(mspan));
                    lhs = Expr::MethodCall(Box::new(lhs), method, args, span);
                    continue;
                }

                // Handle `with` as special infix
                if matches!(op_token, TokenKind::With) {
                    self.expect(&TokenKind::LBrace)?;
                    let fields = self.parse_struct_field_list()?;
                    self.expect(&TokenKind::RBrace)?;
                    let span = lhs.span().merge(self.current_span());
                    lhs = Expr::With(Box::new(lhs), fields, span);
                    continue;
                }

                let rhs = self.parse_expr_bp(r_bp)?;
                let span = lhs.span().merge(rhs.span());

                match &op_token {
                    TokenKind::Pipeline => {
                        lhs = Expr::Pipeline(Box::new(lhs), Box::new(rhs), span);
                    }
                    TokenKind::DotDot => {
                        lhs = Expr::Range(Box::new(lhs), Box::new(rhs), false, span);
                    }
                    TokenKind::DotDotEq => {
                        lhs = Expr::Range(Box::new(lhs), Box::new(rhs), true, span);
                    }
                    TokenKind::Assign => {
                        lhs = Expr::Assign(Box::new(lhs), Box::new(rhs), span);
                    }
                    _ => {
                        let op = token_to_binop(&op_token).ok_or_else(|| ParseError {
                            message: format!("unknown binary operator {:?}", op_token),
                            span,
                        })?;
                        lhs = Expr::Binary(Box::new(lhs), op, Box::new(rhs), span);
                    }
                }
                continue;
            }

            break;
        }

        Ok(lhs)
    }

    fn parse_prefix(&mut self) -> Result<Expr, ParseError> {
        match self.peek().clone() {
            // Unary minus
            TokenKind::Minus => {
                let start = self.current_span();
                self.advance();
                let expr = self.parse_expr_bp(PREFIX_BP)?;
                let span = start.merge(expr.span());
                Ok(Expr::Unary(UnaryOp::Neg, Box::new(expr), span))
            }
            // Unary not
            TokenKind::Not => {
                let start = self.current_span();
                self.advance();
                let expr = self.parse_expr_bp(PREFIX_BP)?;
                let span = start.merge(expr.span());
                Ok(Expr::Unary(UnaryOp::Not, Box::new(expr), span))
            }
            _ => self.parse_atom(),
        }
    }

    fn parse_atom(&mut self) -> Result<Expr, ParseError> {
        match self.peek().clone() {
            TokenKind::IntLit(n) => {
                let span = self.current_span();
                self.advance();
                Ok(Expr::IntLit(n, span))
            }
            TokenKind::FloatLit(n) => {
                let span = self.current_span();
                self.advance();
                Ok(Expr::FloatLit(n, span))
            }
            TokenKind::StringLit(s) => {
                let span = self.current_span();
                self.advance();
                Ok(Expr::StringLit(s, span))
            }
            TokenKind::StringStart(s) => self.parse_string_interp(s),
            TokenKind::True => {
                let span = self.current_span();
                self.advance();
                Ok(Expr::BoolLit(true, span))
            }
            TokenKind::False => {
                let span = self.current_span();
                self.advance();
                Ok(Expr::BoolLit(false, span))
            }
            TokenKind::Ident(name) => {
                let span = self.current_span();
                self.advance();
                Ok(Expr::Ident(name, span))
            }
            TokenKind::UpperIdent(name) => self.parse_upper_ident_atom(name),
            TokenKind::LParen => self.parse_paren_expr(),
            TokenKind::LBrace => self.parse_block(),
            TokenKind::LBracket => {
                let start = self.current_span();
                self.advance();
                let mut elems = Vec::new();
                if !self.check(&TokenKind::RBracket) {
                    elems.push(self.parse_expr()?);
                    while self.eat(&TokenKind::Comma) {
                        if self.check(&TokenKind::RBracket) {
                            break;
                        }
                        elems.push(self.parse_expr()?);
                    }
                }
                let end = self.current_span();
                self.expect(&TokenKind::RBracket)?;
                Ok(Expr::ListLit(elems, start.merge(end)))
            }
            TokenKind::If => self.parse_if(),
            TokenKind::Match => self.parse_match(),
            TokenKind::For => self.parse_for(),
            TokenKind::While => self.parse_while(),
            TokenKind::Let => self.parse_let(),
            TokenKind::Return => {
                let start = self.current_span();
                self.advance();
                let val = if self.is_expr_start() {
                    Some(Box::new(self.parse_expr()?))
                } else {
                    None
                };
                let end = val.as_ref().map(|v| v.span()).unwrap_or(start);
                Ok(Expr::Return(val, start.merge(end)))
            }
            TokenKind::Break => {
                let span = self.current_span();
                self.advance();
                Ok(Expr::Break(span))
            }
            TokenKind::Continue => {
                let span = self.current_span();
                self.advance();
                Ok(Expr::Continue(span))
            }
            _ => Err(ParseError {
                message: format!("expected expression, found {:?}", self.peek()),
                span: self.current_span(),
            }),
        }
    }

    fn parse_upper_ident_atom(&mut self, name: String) -> Result<Expr, ParseError> {
        let span = self.current_span();
        self.advance();
        // Check for qualified: Type.Variant
        if self.check(&TokenKind::Dot)
            && self.pos + 1 < self.tokens.len()
            && matches!(self.tokens[self.pos + 1].kind, TokenKind::UpperIdent(_))
        {
            self.advance(); // consume '.'
            if let TokenKind::UpperIdent(variant) = self.peek().clone() {
                let vspan = self.current_span();
                self.advance();
                return Ok(Expr::QualifiedIdent(name, variant, span.merge(vspan)));
            }
        }
        // Could be struct literal: TypeName { fields }
        if self.check(&TokenKind::LBrace) {
            // Peek ahead to distinguish struct lit from block
            // Struct lit: UpperIdent { ident: expr, ... }
            if self.is_struct_lit_ahead() {
                self.advance(); // consume {
                let fields = self.parse_struct_field_list()?;
                let end = self.current_span();
                self.expect(&TokenKind::RBrace)?;
                return Ok(Expr::StructLit(name, fields, span.merge(end)));
            }
        }
        Ok(Expr::Ident(name, span))
    }

    fn parse_paren_expr(&mut self) -> Result<Expr, ParseError> {
        let start = self.current_span();
        self.advance();

        // Empty parens = Unit
        if self.check(&TokenKind::RParen) {
            let end = self.current_span();
            self.advance();
            return Ok(Expr::Unit(start.merge(end)));
        }

        // Could be lambda: (params) -> body
        // Or tuple: (a, b, c)
        // Or grouped expr: (expr)
        if self.is_lambda_ahead() {
            return self.parse_lambda(start);
        }

        let first = self.parse_expr()?;

        if self.eat(&TokenKind::Comma) {
            // Tuple
            let mut elems = vec![first];
            if !self.check(&TokenKind::RParen) {
                elems.push(self.parse_expr()?);
                while self.eat(&TokenKind::Comma) {
                    if self.check(&TokenKind::RParen) {
                        break;
                    }
                    elems.push(self.parse_expr()?);
                }
            }
            let end = self.current_span();
            self.expect(&TokenKind::RParen)?;
            return Ok(Expr::TupleLit(elems, start.merge(end)));
        }

        self.expect(&TokenKind::RParen)?;
        Ok(first)
    }

    pub(crate) fn parse_string_interp(&mut self, first_text: String) -> Result<Expr, ParseError> {
        let start = self.current_span();
        self.advance(); // consume StringStart

        let mut parts = Vec::new();
        parts.push(StringPart {
            kind: StringPartKind::Literal(first_text),
            span: start,
        });

        // Parse the interpolated expression
        let expr = self.parse_expr()?;
        parts.push(StringPart {
            kind: StringPartKind::Expr(expr),
            span: self.current_span(),
        });

        // Continue with StringMid/StringEnd
        loop {
            match self.peek().clone() {
                TokenKind::StringMid(text) => {
                    let span = self.current_span();
                    self.advance();
                    parts.push(StringPart {
                        kind: StringPartKind::Literal(text),
                        span,
                    });
                    let expr = self.parse_expr()?;
                    parts.push(StringPart {
                        kind: StringPartKind::Expr(expr),
                        span: self.current_span(),
                    });
                }
                TokenKind::StringEnd(text) => {
                    let span = self.current_span();
                    self.advance();
                    parts.push(StringPart {
                        kind: StringPartKind::Literal(text),
                        span,
                    });
                    break;
                }
                _ => {
                    return Err(ParseError {
                        message: "unterminated string interpolation".into(),
                        span: self.current_span(),
                    });
                }
            }
        }

        let end = self.current_span();
        Ok(Expr::StringInterp(parts, start.merge(end)))
    }

    pub(crate) fn parse_block(&mut self) -> Result<Expr, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::LBrace)?;

        let mut exprs = Vec::new();
        while !self.check(&TokenKind::RBrace) && !self.at_eof() {
            exprs.push(self.parse_expr()?);
            // Optional semicolons between expressions
            self.eat(&TokenKind::Semicolon);
        }

        let end = self.current_span();
        self.expect(&TokenKind::RBrace)?;
        Ok(Expr::Block(exprs, start.merge(end)))
    }

    fn parse_if(&mut self) -> Result<Expr, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::If)?;
        let cond = self.parse_expr_bp(0)?;
        let then_branch = self.parse_block()?;
        let else_branch = if self.eat(&TokenKind::Else) {
            if self.check(&TokenKind::If) {
                Some(Box::new(self.parse_if()?))
            } else {
                Some(Box::new(self.parse_block()?))
            }
        } else {
            None
        };
        let end = else_branch
            .as_ref()
            .map(|e| e.span())
            .unwrap_or_else(|| then_branch.span());
        Ok(Expr::If(
            Box::new(cond),
            Box::new(then_branch),
            else_branch,
            start.merge(end),
        ))
    }

    fn parse_match(&mut self) -> Result<Expr, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::Match)?;
        let scrutinee = self.parse_expr_bp(0)?;
        self.expect(&TokenKind::LBrace)?;

        let mut arms = Vec::new();
        while !self.check(&TokenKind::RBrace) && !self.at_eof() {
            arms.push(self.parse_match_arm()?);
            self.eat(&TokenKind::Comma);
        }

        let end = self.current_span();
        self.expect(&TokenKind::RBrace)?;
        Ok(Expr::Match(Box::new(scrutinee), arms, start.merge(end)))
    }

    fn parse_match_arm(&mut self) -> Result<MatchArm, ParseError> {
        let start = self.current_span();
        let pattern = self.parse_pattern()?;
        let guard = if self.eat(&TokenKind::If) {
            Some(self.parse_expr()?)
        } else {
            None
        };
        self.expect(&TokenKind::FatArrow)?;
        let body = self.parse_expr()?;
        let end = body.span();
        Ok(MatchArm {
            pattern,
            guard,
            body,
            span: start.merge(end),
        })
    }

    fn parse_for(&mut self) -> Result<Expr, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::For)?;
        let pattern = self.parse_pattern()?;
        self.expect(&TokenKind::In)?;
        let iter = self.parse_expr_bp(0)?;
        let body = self.parse_block()?;
        let end = body.span();
        if let Pattern::Ident(var, _) = pattern {
            Ok(Expr::For(
                var,
                Box::new(iter),
                Box::new(body),
                start.merge(end),
            ))
        } else {
            Ok(Expr::ForPattern(
                pattern,
                Box::new(iter),
                Box::new(body),
                start.merge(end),
            ))
        }
    }

    fn parse_while(&mut self) -> Result<Expr, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::While)?;
        let cond = self.parse_expr_bp(0)?;
        let body = self.parse_block()?;
        let end = body.span();
        Ok(Expr::While(
            Box::new(cond),
            Box::new(body),
            start.merge(end),
        ))
    }

    fn parse_let(&mut self) -> Result<Expr, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::Let)?;
        let is_mut = self.eat(&TokenKind::Mut);
        let pattern = self.parse_pattern()?;

        let ty = if self.eat(&TokenKind::Colon) {
            Some(self.parse_type_expr()?)
        } else {
            None
        };

        self.expect(&TokenKind::Assign)?;
        let value = self.parse_expr()?;
        let end = value.span();
        if is_mut {
            if let Pattern::Ident(name, _) = pattern {
                Ok(Expr::Let(
                    name,
                    is_mut,
                    ty,
                    Box::new(value),
                    start.merge(end),
                ))
            } else {
                Err(ParseError {
                    message: "mutable let bindings require an identifier pattern".into(),
                    span: pattern_span(&pattern),
                })
            }
        } else if let Pattern::Ident(name, _) = pattern {
            Ok(Expr::Let(
                name,
                is_mut,
                ty,
                Box::new(value),
                start.merge(end),
            ))
        } else {
            Ok(Expr::LetPattern(
                pattern,
                is_mut,
                ty,
                Box::new(value),
                start.merge(end),
            ))
        }
    }

    pub(crate) fn parse_lambda(&mut self, start: Span) -> Result<Expr, ParseError> {
        let params = self.parse_param_list()?;
        self.expect(&TokenKind::RParen)?;
        self.expect(&TokenKind::Arrow)?;
        let body = self.parse_expr()?;
        let end = body.span();
        Ok(Expr::Lambda(params, None, Box::new(body), start.merge(end)))
    }

    pub(crate) fn parse_arg_list(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut args = Vec::new();
        if self.check(&TokenKind::RParen) {
            return Ok(args);
        }
        args.push(self.parse_expr()?);
        while self.eat(&TokenKind::Comma) {
            if self.check(&TokenKind::RParen) {
                break;
            }
            args.push(self.parse_expr()?);
        }
        Ok(args)
    }

    pub(crate) fn parse_struct_field_list(&mut self) -> Result<Vec<(String, Expr)>, ParseError> {
        let mut fields = Vec::new();
        if self.check(&TokenKind::RBrace) {
            return Ok(fields);
        }
        loop {
            let (name, _) = self.expect_ident()?;
            self.expect(&TokenKind::Colon)?;
            let value = self.parse_expr()?;
            fields.push((name, value));
            if !self.eat(&TokenKind::Comma) {
                break;
            }
            if self.check(&TokenKind::RBrace) {
                break;
            }
        }
        Ok(fields)
    }

    pub(crate) fn parse_effect_list(&mut self) -> Result<Vec<EffectRef>, ParseError> {
        self.expect(&TokenKind::LBracket)?;
        let mut effects = Vec::new();
        if !self.check(&TokenKind::RBracket) {
            loop {
                effects.push(self.parse_effect_ref()?);
                if !self.eat(&TokenKind::Comma) {
                    break;
                }
                if self.check(&TokenKind::RBracket) {
                    break;
                }
            }
        }
        self.expect(&TokenKind::RBracket)?;
        Ok(effects)
    }

    fn parse_effect_ref(&mut self) -> Result<EffectRef, ParseError> {
        let start = self.current_span();
        let (mut name, mut end) = self.any_ident()?;
        while self.eat(&TokenKind::Dot) {
            let (seg, seg_span) = self.any_ident()?;
            name.push('.');
            name.push_str(&seg);
            end = seg_span;
        }
        Ok(EffectRef {
            name,
            span: start.merge(end),
        })
    }

    pub(crate) fn parse_contract_expr(&mut self) -> Result<Expr, ParseError> {
        // Contracts are single expressions terminated by the next clause keyword or '='.
        // Start at precedence 2 so top-level assignment (`=`) is not consumed.
        self.parse_expr_bp(2)
    }
}
