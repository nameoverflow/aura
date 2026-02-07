use crate::ast::*;
use aura_common::Span;
use aura_lexer::token::{Token, TokenKind};

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    errors: Vec<ParseError>,
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

    fn peek(&self) -> &TokenKind {
        self.tokens
            .get(self.pos)
            .map(|t| &t.kind)
            .unwrap_or(&TokenKind::Eof)
    }

    fn current_span(&self) -> Span {
        self.tokens
            .get(self.pos)
            .map(|t| t.span)
            .unwrap_or(Span::dummy())
    }

    fn advance(&mut self) -> &Token {
        let tok = &self.tokens[self.pos];
        if self.pos < self.tokens.len() - 1 {
            self.pos += 1;
        }
        tok
    }

    fn at_eof(&self) -> bool {
        matches!(self.peek(), TokenKind::Eof)
    }

    fn expect(&mut self, kind: &TokenKind) -> Result<Span, ParseError> {
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

    fn expect_ident(&mut self) -> Result<(String, Span), ParseError> {
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

    fn expect_upper_ident(&mut self) -> Result<(String, Span), ParseError> {
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

    fn any_ident(&mut self) -> Result<(String, Span), ParseError> {
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

    fn check(&self, kind: &TokenKind) -> bool {
        std::mem::discriminant(self.peek()) == std::mem::discriminant(kind)
    }

    fn eat(&mut self, kind: &TokenKind) -> bool {
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

    // ── Item parsing ──

    fn parse_item(&mut self) -> Result<Item, ParseError> {
        let is_pub = self.eat(&TokenKind::Pub);

        match self.peek() {
            TokenKind::Def => self.parse_fn_def(is_pub, false),
            TokenKind::Async => {
                self.advance();
                self.parse_fn_def(is_pub, true)
            }
            TokenKind::Type => self.parse_type_def(is_pub),
            TokenKind::Concept => self.parse_concept_def(is_pub),
            TokenKind::Instance => self.parse_instance_def(is_pub),
            TokenKind::Use => self.parse_use_decl(),
            TokenKind::Module => self.parse_module_decl(),
            TokenKind::Ident(_) => {
                // Could be a standalone type annotation: name: Type
                self.parse_type_annotation_or_error()
            }
            _ => Err(ParseError {
                message: format!(
                    "expected item (def, type, concept, instance, use, module), found {:?}",
                    self.peek()
                ),
                span: self.current_span(),
            }),
        }
    }

    fn parse_fn_def(&mut self, is_pub: bool, is_async: bool) -> Result<Item, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::Def)?;
        let (name, _) = self.expect_ident()?;
        self.expect(&TokenKind::LParen)?;

        let params = self.parse_param_list()?;
        self.expect(&TokenKind::RParen)?;

        let return_type = if self.eat(&TokenKind::Arrow) {
            Some(self.parse_type_expr()?)
        } else {
            None
        };

        let effects = if self.check(&TokenKind::LBracket) {
            self.parse_effect_list()?
        } else {
            Vec::new()
        };

        let mut requires = Vec::new();
        while self.eat(&TokenKind::Requires) {
            requires.push(self.parse_contract_expr()?);
        }

        let mut ensures = Vec::new();
        while self.eat(&TokenKind::Ensures) {
            ensures.push(self.parse_contract_expr()?);
        }

        self.expect(&TokenKind::Assign)?;
        let body = self.parse_expr()?;

        let end = body.span();
        Ok(Item::Function(FnDef {
            name,
            params,
            return_type,
            effects,
            requires,
            ensures,
            body,
            is_pub,
            is_async,
            span: start.merge(end),
        }))
    }

    fn parse_param_list(&mut self) -> Result<Vec<Param>, ParseError> {
        let mut params = Vec::new();
        if self.check(&TokenKind::RParen) {
            return Ok(params);
        }

        params.push(self.parse_param()?);
        while self.eat(&TokenKind::Comma) {
            if self.check(&TokenKind::RParen) {
                break; // trailing comma
            }
            params.push(self.parse_param()?);
        }
        Ok(params)
    }

    fn parse_param(&mut self) -> Result<Param, ParseError> {
        let (name, span) = self.expect_ident()?;
        let ty = if self.eat(&TokenKind::Colon) {
            Some(self.parse_type_expr()?)
        } else {
            None
        };
        let end = ty.as_ref().map(|t| t.span()).unwrap_or(span);
        Ok(Param {
            name,
            ty,
            span: span.merge(end),
        })
    }

    fn parse_type_def(&mut self, is_pub: bool) -> Result<Item, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::Type)?;
        let (name, _) = self.expect_upper_ident()?;

        // Optional type parameters (lowercase identifiers before =)
        let mut type_params = Vec::new();
        while let TokenKind::Ident(param) = self.peek() {
            type_params.push(param.clone());
            self.advance();
        }

        self.expect(&TokenKind::Assign)?;

        // Check for struct vs sum type vs refined type
        let kind = if self.is_refined_type_def_ahead() {
            let base = self.parse_type_expr()?;
            self.expect(&TokenKind::Where)?;
            let constraint = self.parse_expr()?;
            TypeDefKind::Refined {
                base_type: base,
                constraint,
            }
        } else if self.check(&TokenKind::LBrace) {
            // Struct type
            self.advance();
            let mut fields = Vec::new();
            while !self.check(&TokenKind::RBrace) && !self.at_eof() {
                let (fname, fspan) = self.expect_ident()?;
                self.expect(&TokenKind::Colon)?;
                let fty = self.parse_type_expr()?;
                let fend = fty.span();
                fields.push(Field {
                    name: fname,
                    ty: fty,
                    span: fspan.merge(fend),
                });
                if !self.eat(&TokenKind::Comma) {
                    break;
                }
            }
            self.expect(&TokenKind::RBrace)?;
            TypeDefKind::Struct(fields)
        } else if let TokenKind::UpperIdent(_) = self.peek() {
            if self.has_pipe_in_type_def_ahead() || !self.looks_like_type_alias_ahead() {
                // Sum type: Variant1 | Variant2 payload | ...
                let mut variants = Vec::new();
                loop {
                    let (vname, vspan) = self.expect_upper_ident()?;
                    let mut fields = Vec::new();
                    // Variant payloads: each field is a type atom (use parens for complex types)
                    while self.is_type_atom_start() && !self.check(&TokenKind::Pipe) {
                        if self.is_top_level_type_annotation_start() {
                            break;
                        }
                        fields.push(self.parse_type_atom()?);
                    }
                    let vend = fields.last().map(|f| f.span()).unwrap_or(vspan);
                    variants.push(Variant {
                        name: vname,
                        fields,
                        span: vspan.merge(vend),
                    });
                    if !self.eat(&TokenKind::Pipe) {
                        break;
                    }
                }
                TypeDefKind::Sum(variants)
            } else {
                // Type alias: `type Name = ExistingType`
                let ty = self.parse_type_expr()?;
                TypeDefKind::Alias(ty)
            }
        } else {
            // Type expression starting with lowercase, parens, etc.
            let base = self.parse_type_expr()?;
            if self.eat(&TokenKind::Where) {
                let constraint = self.parse_expr()?;
                TypeDefKind::Refined {
                    base_type: base,
                    constraint,
                }
            } else {
                // Type alias to non-uppercase-starting type expression
                TypeDefKind::Alias(base)
            }
        };

        let end = self.current_span();
        Ok(Item::TypeDef(TypeDef {
            name,
            type_params,
            kind,
            is_pub,
            span: start.merge(end),
        }))
    }

    fn parse_concept_def(&mut self, is_pub: bool) -> Result<Item, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::Concept)?;
        let (name, _) = self.any_ident()?;

        // Optional concept header params/defaults: concept Add (rhs = Self) { ... }
        if self.eat(&TokenKind::LParen) {
            let mut depth = 1u32;
            while depth > 0 && !self.at_eof() {
                match self.peek() {
                    TokenKind::LParen => {
                        depth += 1;
                        self.advance();
                    }
                    TokenKind::RParen => {
                        depth -= 1;
                        self.advance();
                    }
                    _ => {
                        self.advance();
                    }
                }
            }
        }

        let mut supers = Vec::new();
        if self.eat(&TokenKind::Colon) {
            let (sup, _) = self.any_ident()?;
            supers.push(sup);
            while self.eat(&TokenKind::Comma) {
                let (sup, _) = self.any_ident()?;
                supers.push(sup);
            }
        }

        self.expect(&TokenKind::LBrace)?;
        let mut assoc_types = Vec::new();
        let mut methods = Vec::new();
        while !self.check(&TokenKind::RBrace) && !self.at_eof() {
            match self.peek() {
                TokenKind::Type => assoc_types.push(self.parse_assoc_type_decl()?),
                TokenKind::Def => methods.push(self.parse_concept_method_sig()?),
                TokenKind::Semicolon | TokenKind::Comma => {
                    self.advance();
                }
                _ => {
                    return Err(ParseError {
                        message: format!(
                            "expected concept item ('type' or 'def'), found {:?}",
                            self.peek()
                        ),
                        span: self.current_span(),
                    });
                }
            }
        }
        let end = self.current_span();
        self.expect(&TokenKind::RBrace)?;

        Ok(Item::ConceptDef(ConceptDef {
            name,
            supers,
            assoc_types,
            methods,
            is_pub,
            span: start.merge(end),
        }))
    }

    fn parse_instance_def(&mut self, is_pub: bool) -> Result<Item, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::Instance)?;

        let kind = if self.is_concept_instance_ahead() {
            let (concept, cspan) = self.any_ident()?;
            let _ = cspan;
            self.expect(&TokenKind::For)?;
            let for_type = self.parse_type_expr()?;
            InstanceKind::Concept { concept, for_type }
        } else {
            let target = self.parse_type_expr()?;
            InstanceKind::Inherent(target)
        };

        self.expect(&TokenKind::LBrace)?;
        let mut assoc_types = Vec::new();
        let mut methods = Vec::new();
        while !self.check(&TokenKind::RBrace) && !self.at_eof() {
            match self.peek() {
                TokenKind::Type => assoc_types.push(self.parse_assoc_type_binding()?),
                TokenKind::Def => methods.push(self.parse_method_def()?),
                TokenKind::Semicolon | TokenKind::Comma => {
                    self.advance();
                }
                _ => {
                    return Err(ParseError {
                        message: format!(
                            "expected instance item ('type' or 'def'), found {:?}",
                            self.peek()
                        ),
                        span: self.current_span(),
                    });
                }
            }
        }
        let end = self.current_span();
        self.expect(&TokenKind::RBrace)?;

        Ok(Item::InstanceDef(InstanceDef {
            kind,
            assoc_types,
            methods,
            is_pub,
            span: start.merge(end),
        }))
    }

    fn parse_assoc_type_decl(&mut self) -> Result<AssocTypeDecl, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::Type)?;
        let (name, _) = self.any_ident()?;
        let default = if self.eat(&TokenKind::Assign) {
            Some(self.parse_type_expr()?)
        } else {
            None
        };
        let end = default.as_ref().map(|t| t.span()).unwrap_or(start);
        Ok(AssocTypeDecl {
            name,
            default,
            span: start.merge(end),
        })
    }

    fn parse_assoc_type_binding(&mut self) -> Result<AssocTypeBinding, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::Type)?;
        let (name, _) = self.any_ident()?;
        self.expect(&TokenKind::Assign)?;
        let ty = self.parse_type_expr()?;
        let end = ty.span();
        Ok(AssocTypeBinding {
            name,
            ty,
            span: start.merge(end),
        })
    }

    fn parse_concept_method_sig(&mut self) -> Result<ConceptMethodSig, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::Def)?;
        let (name, _) = self.expect_ident()?;
        self.expect(&TokenKind::LParen)?;
        let params = self.parse_param_list()?;
        self.expect(&TokenKind::RParen)?;

        let return_type = if self.eat(&TokenKind::Arrow) {
            Some(self.parse_type_expr()?)
        } else {
            None
        };

        let default_body = if self.eat(&TokenKind::Assign) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        let end = default_body
            .as_ref()
            .map(|e| e.span())
            .or_else(|| return_type.as_ref().map(|t| t.span()))
            .unwrap_or(start);

        Ok(ConceptMethodSig {
            name,
            params,
            return_type,
            default_body,
            span: start.merge(end),
        })
    }

    fn parse_method_def(&mut self) -> Result<MethodDef, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::Def)?;
        let (name, _) = self.expect_ident()?;
        self.expect(&TokenKind::LParen)?;
        let params = self.parse_param_list()?;
        self.expect(&TokenKind::RParen)?;

        let return_type = if self.eat(&TokenKind::Arrow) {
            Some(self.parse_type_expr()?)
        } else {
            None
        };

        self.expect(&TokenKind::Assign)?;
        let body = self.parse_expr()?;
        let end = body.span();

        Ok(MethodDef {
            name,
            params,
            return_type,
            body,
            span: start.merge(end),
        })
    }

    fn is_concept_instance_ahead(&self) -> bool {
        // instance Concept for Type { ... }
        // scan until '{' and see whether top-level `for` appears first.
        let mut i = self.pos;
        while i < self.tokens.len() {
            match self.tokens[i].kind {
                TokenKind::For => return true,
                TokenKind::LBrace | TokenKind::Eof => return false,
                _ => i += 1,
            }
        }
        false
    }

    fn parse_use_decl(&mut self) -> Result<Item, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::Use)?;

        let mut path = Vec::new();
        let (first, _) = self.any_ident()?;
        path.push(first);

        while self.eat(&TokenKind::ColonColon) {
            if self.check(&TokenKind::LBrace) {
                // Grouped import: use std::collections::{HashMap, HashSet}
                self.advance();
                let mut items = Vec::new();
                loop {
                    let (item, _) = self.any_ident()?;
                    items.push(item);
                    if !self.eat(&TokenKind::Comma) {
                        break;
                    }
                }
                self.expect(&TokenKind::RBrace)?;
                let end = self.current_span();
                return Ok(Item::Use(UseDecl {
                    path,
                    items: Some(items),
                    span: start.merge(end),
                }));
            }
            let (seg, _) = self.any_ident()?;
            path.push(seg);
        }

        let end = self.current_span();
        Ok(Item::Use(UseDecl {
            path,
            items: None,
            span: start.merge(end),
        }))
    }

    fn parse_module_decl(&mut self) -> Result<Item, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::Module)?;
        let (name, _) = self.expect_ident()?;
        let end = self.current_span();
        Ok(Item::ModuleDecl(ModuleDecl {
            name,
            span: start.merge(end),
        }))
    }

    fn parse_type_annotation_or_error(&mut self) -> Result<Item, ParseError> {
        // Check if this is `name: Type` (standalone type annotation)
        let start = self.current_span();
        let (name, _) = self.expect_ident()?;
        if self.eat(&TokenKind::Colon) {
            let ty = self.parse_type_expr()?;
            let end = ty.span();
            return Ok(Item::TypeAnnotation(TypeAnnotation {
                name,
                ty,
                span: start.merge(end),
            }));
        }
        Err(ParseError {
            message: format!("expected ':' after '{name}' for type annotation"),
            span: self.current_span(),
        })
    }

    // ── Type expression parsing ──

    fn parse_type_expr(&mut self) -> Result<TypeExpr, ParseError> {
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

    fn is_type_atom_start(&self) -> bool {
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

    fn parse_type_atom(&mut self) -> Result<TypeExpr, ParseError> {
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

    // ── Expression parsing (Pratt) ──

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
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
            TokenKind::UpperIdent(name) => {
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
            TokenKind::LParen => {
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

    fn parse_string_interp(&mut self, first_text: String) -> Result<Expr, ParseError> {
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

    fn parse_block(&mut self) -> Result<Expr, ParseError> {
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

    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
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

    fn parse_lambda(&mut self, start: Span) -> Result<Expr, ParseError> {
        let params = self.parse_param_list()?;
        self.expect(&TokenKind::RParen)?;
        self.expect(&TokenKind::Arrow)?;
        let body = self.parse_expr()?;
        let end = body.span();
        Ok(Expr::Lambda(params, None, Box::new(body), start.merge(end)))
    }

    fn parse_arg_list(&mut self) -> Result<Vec<Expr>, ParseError> {
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

    fn parse_struct_field_list(&mut self) -> Result<Vec<(String, Expr)>, ParseError> {
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

    fn parse_effect_list(&mut self) -> Result<Vec<EffectRef>, ParseError> {
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

    fn parse_contract_expr(&mut self) -> Result<Expr, ParseError> {
        // Contracts are single expressions terminated by the next clause keyword or '='.
        // Start at precedence 2 so top-level assignment (`=`) is not consumed.
        self.parse_expr_bp(2)
    }

    /// Lookahead: does the type definition RHS contain a `|` at the top level?
    /// If yes, it's a sum type. If no, it's a type alias.
    fn has_pipe_in_type_def_ahead(&self) -> bool {
        let mut i = self.pos;
        let mut paren_depth = 0i32;
        while i < self.tokens.len() {
            match &self.tokens[i].kind {
                TokenKind::LParen => paren_depth += 1,
                TokenKind::RParen => paren_depth = (paren_depth - 1).max(0),
                TokenKind::Pipe if paren_depth == 0 => return true,
                TokenKind::Where if paren_depth == 0 => return false,
                TokenKind::Def
                | TokenKind::Type
                | TokenKind::Concept
                | TokenKind::Instance
                | TokenKind::Use
                | TokenKind::Module
                | TokenKind::Pub
                | TokenKind::Eof
                    if paren_depth == 0 =>
                {
                    return false;
                }
                _ => {}
            }
            i += 1;
        }
        false
    }

    /// Lookahead: does the type def RHS look like a type alias (as opposed to
    /// a sum type with variants)?  Returns true when the RHS starts with a
    /// known primitive type name or when `*` / `->` appear at top level
    /// *before* any end-of-definition boundary (indicating a product/function
    /// type expression, not variant syntax).
    fn looks_like_type_alias_ahead(&self) -> bool {
        // Check if the first UpperIdent is a known primitive type name.
        if let TokenKind::UpperIdent(name) = self.peek() {
            let known = matches!(
                name.as_str(),
                "Int" | "Int8" | "Int16" | "Int32" | "Int64"
                    | "UInt" | "UInt8" | "UInt16" | "UInt32" | "UInt64"
                    | "Float32" | "Float64" | "Decimal" | "BigDecimal"
                    | "Bool" | "Char" | "String" | "Unit"
            );
            if known {
                return true;
            }
        }

        // Scan ahead for `*` or `->` at the top level (paren-depth 0)
        // within this type definition only (stop at item boundaries).
        let mut i = self.pos;
        let mut paren_depth = 0i32;
        while i < self.tokens.len() {
            match &self.tokens[i].kind {
                TokenKind::LParen => paren_depth += 1,
                TokenKind::RParen => paren_depth = (paren_depth - 1).max(0),
                TokenKind::Star | TokenKind::Arrow if paren_depth == 0 => return true,
                TokenKind::Pipe if paren_depth == 0 => return false,
                // Stop scanning at any item-boundary keyword.
                TokenKind::Def
                | TokenKind::Type
                | TokenKind::Concept
                | TokenKind::Instance
                | TokenKind::Use
                | TokenKind::Module
                | TokenKind::Pub
                | TokenKind::Eof
                    if paren_depth == 0 =>
                {
                    return false;
                }
                // A standalone type annotation (`name: Type`) means we hit the
                // next definition — stop.
                TokenKind::Colon if paren_depth == 0 => return false,
                _ => {}
            }
            i += 1;
        }
        false
    }

    fn is_refined_type_def_ahead(&self) -> bool {
        let mut i = self.pos;
        let mut paren_depth = 0i32;
        while i < self.tokens.len() {
            match &self.tokens[i].kind {
                TokenKind::LParen => paren_depth += 1,
                TokenKind::RParen => paren_depth = (paren_depth - 1).max(0),
                TokenKind::Where if paren_depth == 0 => return true,
                TokenKind::Pipe if paren_depth == 0 => return false,
                TokenKind::Def
                | TokenKind::Type
                | TokenKind::Concept
                | TokenKind::Instance
                | TokenKind::Use
                | TokenKind::Module
                | TokenKind::Pub
                | TokenKind::Eof
                    if paren_depth == 0 =>
                {
                    return false;
                }
                _ => {}
            }
            i += 1;
        }
        false
    }

    fn is_top_level_type_annotation_start(&self) -> bool {
        matches!(self.peek(), TokenKind::Ident(_))
            && self.pos + 1 < self.tokens.len()
            && matches!(self.tokens[self.pos + 1].kind, TokenKind::Colon)
    }

    fn is_lambda_ahead(&self) -> bool {
        // Look ahead to determine if this is a lambda: (params) -> ...
        // Heuristic: if after matching parens we see ->, it's a lambda
        let mut depth = 1u32;
        let mut i = self.pos;
        while i < self.tokens.len() {
            match &self.tokens[i].kind {
                TokenKind::LParen => depth += 1,
                TokenKind::RParen => {
                    depth -= 1;
                    if depth == 0 {
                        // Check if next token is ->
                        return i + 1 < self.tokens.len()
                            && matches!(self.tokens[i + 1].kind, TokenKind::Arrow);
                    }
                }
                TokenKind::Eof => return false,
                _ => {}
            }
            i += 1;
        }
        false
    }

    fn is_struct_lit_ahead(&self) -> bool {
        // Look ahead: UpperIdent { ident: ... }
        // vs UpperIdent { expr } (which would be a variant + block)
        let mut i = self.pos + 1; // skip {
        if i >= self.tokens.len() {
            return false;
        }
        match &self.tokens[i].kind {
            TokenKind::Ident(_) => {
                i += 1;
                if i >= self.tokens.len() {
                    return false;
                }
                matches!(self.tokens[i].kind, TokenKind::Colon)
            }
            TokenKind::RBrace => true, // empty struct lit
            _ => false,
        }
    }

    fn is_expr_start(&self) -> bool {
        matches!(
            self.peek(),
            TokenKind::IntLit(_)
                | TokenKind::FloatLit(_)
                | TokenKind::StringLit(_)
                | TokenKind::StringStart(_)
                | TokenKind::True
                | TokenKind::False
                | TokenKind::Ident(_)
                | TokenKind::UpperIdent(_)
                | TokenKind::LParen
                | TokenKind::LBrace
                | TokenKind::LBracket
                | TokenKind::If
                | TokenKind::Match
                | TokenKind::For
                | TokenKind::While
                | TokenKind::Let
                | TokenKind::Return
                | TokenKind::Break
                | TokenKind::Continue
                | TokenKind::Minus
                | TokenKind::Not
        )
    }
}

// Operator precedence (binding power)
// Higher = tighter binding

const PREFIX_BP: u8 = 17; // unary -, not

fn infix_bp(token: &TokenKind) -> Option<(u8, u8)> {
    match token {
        // |> pipeline (lowest)
        TokenKind::Pipeline => Some((1, 2)),
        // or
        TokenKind::Or => Some((3, 4)),
        // and
        TokenKind::And => Some((5, 6)),
        // comparison (non-associative, use same l and r)
        TokenKind::Eq
        | TokenKind::NotEq
        | TokenKind::Lt
        | TokenKind::Gt
        | TokenKind::LtEq
        | TokenKind::GtEq => Some((7, 8)),
        // range
        TokenKind::DotDot | TokenKind::DotDotEq => Some((9, 10)),
        // + -
        TokenKind::Plus | TokenKind::Minus => Some((11, 12)),
        // * / %
        TokenKind::Star | TokenKind::Slash | TokenKind::Percent => Some((13, 14)),
        // with (functional update)
        TokenKind::With => Some((1, 2)),
        // assignment
        TokenKind::Assign => Some((1, 2)),
        _ => None,
    }
}

fn postfix_bp(token: &TokenKind) -> (u8, u8) {
    match token {
        TokenKind::Question => (19, 19),
        TokenKind::Dot => (21, 21),
        TokenKind::LParen => (21, 21), // function call
        _ => (0, 0),
    }
}

fn token_to_binop(token: &TokenKind) -> Option<BinOp> {
    match token {
        TokenKind::Plus => Some(BinOp::Add),
        TokenKind::Minus => Some(BinOp::Sub),
        TokenKind::Star => Some(BinOp::Mul),
        TokenKind::Slash => Some(BinOp::Div),
        TokenKind::Percent => Some(BinOp::Mod),
        TokenKind::Eq => Some(BinOp::Eq),
        TokenKind::NotEq => Some(BinOp::NotEq),
        TokenKind::Lt => Some(BinOp::Lt),
        TokenKind::Gt => Some(BinOp::Gt),
        TokenKind::LtEq => Some(BinOp::LtEq),
        TokenKind::GtEq => Some(BinOp::GtEq),
        TokenKind::And => Some(BinOp::And),
        TokenKind::Or => Some(BinOp::Or),
        _ => None,
    }
}

fn pattern_span(pattern: &Pattern) -> Span {
    match pattern {
        Pattern::Wildcard(span)
        | Pattern::Ident(_, span)
        | Pattern::Literal(_, span)
        | Pattern::Constructor(_, _, span)
        | Pattern::Tuple(_, span)
        | Pattern::Struct(_, _, _, span)
        | Pattern::Or(_, span) => *span,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use aura_lexer::Lexer;

    fn parse_expr_str(input: &str) -> Expr {
        // Wrap in a function for the parser
        let wrapped = format!("def __test__() -> Int = {input}");
        let mut lexer2 = Lexer::new(&wrapped, 0);
        let tokens2 = lexer2.tokenize();
        let mut parser = Parser::new(tokens2);
        let module = parser.parse_module().unwrap();
        match &module.items[0] {
            Item::Function(f) => f.body.clone(),
            _ => panic!("expected function"),
        }
    }

    fn parse_module_str(input: &str) -> Module {
        let mut lexer = Lexer::new(input, 0);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        parser.parse_module().unwrap()
    }

    #[test]
    fn test_parse_int_literal() {
        let expr = parse_expr_str("42");
        assert!(matches!(expr, Expr::IntLit(42, _)));
    }

    #[test]
    fn test_parse_binary_ops() {
        let expr = parse_expr_str("1 + 2 * 3");
        // Should parse as 1 + (2 * 3) due to precedence
        match expr {
            Expr::Binary(lhs, BinOp::Add, rhs, _) => {
                assert!(matches!(*lhs, Expr::IntLit(1, _)));
                match *rhs {
                    Expr::Binary(l, BinOp::Mul, r, _) => {
                        assert!(matches!(*l, Expr::IntLit(2, _)));
                        assert!(matches!(*r, Expr::IntLit(3, _)));
                    }
                    _ => panic!("expected mul"),
                }
            }
            _ => panic!("expected add"),
        }
    }

    #[test]
    fn test_parse_function_def() {
        let module = parse_module_str("def add(a: Int, b: Int) -> Int = a + b");
        assert_eq!(module.items.len(), 1);
        match &module.items[0] {
            Item::Function(f) => {
                assert_eq!(f.name, "add");
                assert_eq!(f.params.len(), 2);
                assert_eq!(f.params[0].name, "a");
                assert_eq!(f.params[1].name, "b");
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_if_expr() {
        let module = parse_module_str("def test() -> Int = if x { 1 } else { 2 }");
        match &module.items[0] {
            Item::Function(f) => {
                assert!(matches!(&f.body, Expr::If(_, _, Some(_), _)));
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_let_binding() {
        let module = parse_module_str("def test() -> Int = { let x = 42; x }");
        match &module.items[0] {
            Item::Function(f) => match &f.body {
                Expr::Block(exprs, _) => {
                    assert_eq!(exprs.len(), 2);
                    assert!(matches!(&exprs[0], Expr::Let(name, false, None, _, _) if name == "x"));
                }
                _ => panic!("expected block"),
            },
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_let_mut() {
        let module = parse_module_str("def test() -> Int = { let mut x = 0; x }");
        match &module.items[0] {
            Item::Function(f) => match &f.body {
                Expr::Block(exprs, _) => {
                    assert!(matches!(&exprs[0], Expr::Let(_, true, None, _, _)));
                }
                _ => panic!("expected block"),
            },
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_type_def_sum() {
        let module =
            parse_module_str("type Status = Pending | Processing | Complete | Failed String");
        match &module.items[0] {
            Item::TypeDef(td) => {
                assert_eq!(td.name, "Status");
                match &td.kind {
                    TypeDefKind::Sum(variants) => {
                        assert_eq!(variants.len(), 4);
                        assert_eq!(variants[0].name, "Pending");
                        assert_eq!(variants[3].name, "Failed");
                        assert_eq!(variants[3].fields.len(), 1);
                    }
                    _ => panic!("expected sum type"),
                }
            }
            _ => panic!("expected type def"),
        }
    }

    #[test]
    fn test_parse_type_def_struct() {
        let module = parse_module_str("type User = { name: String, age: Int }");
        match &module.items[0] {
            Item::TypeDef(td) => {
                assert_eq!(td.name, "User");
                match &td.kind {
                    TypeDefKind::Struct(fields) => {
                        assert_eq!(fields.len(), 2);
                        assert_eq!(fields[0].name, "name");
                        assert_eq!(fields[1].name, "age");
                    }
                    _ => panic!("expected struct type"),
                }
            }
            _ => panic!("expected type def"),
        }
    }

    #[test]
    fn test_parse_use() {
        let module = parse_module_str("use std::collections::HashMap");
        match &module.items[0] {
            Item::Use(u) => {
                assert_eq!(u.path, vec!["std", "collections", "HashMap"]);
            }
            _ => panic!("expected use"),
        }
    }

    #[test]
    fn test_parse_match() {
        let module = parse_module_str("def test(x: Int) -> Int = match x { 1 => 10, _ => 0 }");
        match &module.items[0] {
            Item::Function(f) => match &f.body {
                Expr::Match(_, arms, _) => {
                    assert_eq!(arms.len(), 2);
                }
                _ => panic!("expected match"),
            },
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_or_pattern() {
        let module = parse_module_str("def test(x: Option Int) -> Int = match x { None | Some(0) => 0, Some(n) => n }");
        match &module.items[0] {
            Item::Function(f) => match &f.body {
                Expr::Match(_, arms, _) => {
                    assert_eq!(arms.len(), 2);
                    assert!(matches!(arms[0].pattern, Pattern::Or(_, _)));
                }
                _ => panic!("expected match"),
            },
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_struct_pattern_with_rest() {
        let module = parse_module_str(
            "def test(u: User) -> String = match u { User { name, .. } => name }",
        );
        match &module.items[0] {
            Item::Function(f) => match &f.body {
                Expr::Match(_, arms, _) => match &arms[0].pattern {
                    Pattern::Struct(name, fields, has_rest, _) => {
                        assert_eq!(name, "User");
                        assert_eq!(fields.len(), 1);
                        assert_eq!(fields[0].name, "name");
                        assert!(*has_rest);
                    }
                    _ => panic!("expected struct pattern"),
                },
                _ => panic!("expected match"),
            },
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_for_loop() {
        let module = parse_module_str("def test() -> Int = for i in 0..10 { i }");
        match &module.items[0] {
            Item::Function(f) => {
                assert!(matches!(&f.body, Expr::For(var, _, _, _) if var == "i"));
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_for_destructuring_loop() {
        let module = parse_module_str("def test() -> Int = for (a, b) in pairs { a }");
        match &module.items[0] {
            Item::Function(f) => {
                assert!(matches!(&f.body, Expr::ForPattern(Pattern::Tuple(_, _), _, _, _)));
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_lambda() {
        let module = parse_module_str("def test() -> Int = (x) -> x + 1");
        match &module.items[0] {
            Item::Function(f) => {
                assert!(matches!(&f.body, Expr::Lambda(_, _, _, _)));
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_pipeline() {
        let module = parse_module_str("def test(x: Int) -> Int = x |> add(1)");
        match &module.items[0] {
            Item::Function(f) => {
                assert!(matches!(&f.body, Expr::Pipeline(_, _, _)));
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_method_call() {
        let module = parse_module_str("def test(x: Int) -> Int = x.to_string()");
        match &module.items[0] {
            Item::Function(f) => {
                assert!(matches!(&f.body, Expr::MethodCall(_, name, _, _) if name == "to_string"));
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_field_access() {
        let module = parse_module_str("def test(x: Int) -> Int = user.name");
        match &module.items[0] {
            Item::Function(f) => {
                assert!(matches!(&f.body, Expr::FieldAccess(_, name, _) if name == "name"));
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_list_literal() {
        let module = parse_module_str("def test() -> Int = [1, 2, 3]");
        match &module.items[0] {
            Item::Function(f) => match &f.body {
                Expr::ListLit(elems, _) => assert_eq!(elems.len(), 3),
                _ => panic!("expected list"),
            },
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_type_app() {
        let module = parse_module_str("def test(x: List Int) -> Int = 0");
        match &module.items[0] {
            Item::Function(f) => match &f.params[0].ty {
                Some(TypeExpr::App(base, args, _)) => {
                    assert!(matches!(base.as_ref(), TypeExpr::Named(n, _) if n == "List"));
                    assert_eq!(args.len(), 1);
                }
                _ => panic!("expected type app"),
            },
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_type_app_with_multi_char_type_var() {
        let module = parse_module_str("def test(x: Result value error) -> Int = 0");
        match &module.items[0] {
            Item::Function(f) => match &f.params[0].ty {
                Some(TypeExpr::App(base, args, _)) => {
                    assert!(matches!(base.as_ref(), TypeExpr::Named(n, _) if n == "Result"));
                    assert_eq!(args.len(), 2);
                    assert!(matches!(&args[0], TypeExpr::Named(n, _) if n == "value"));
                    assert!(matches!(&args[1], TypeExpr::Named(n, _) if n == "error"));
                }
                _ => panic!("expected type app"),
            },
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_product_type() {
        let module = parse_module_str("def test(x: Int * String) -> Int = 0");
        match &module.items[0] {
            Item::Function(f) => {
                assert!(matches!(&f.params[0].ty, Some(TypeExpr::Product(_, _))));
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_nested_blocks() {
        let module = parse_module_str("def test() -> Int = { let x = { 1 + 2 }; x }");
        match &module.items[0] {
            Item::Function(f) => match &f.body {
                Expr::Block(exprs, _) => {
                    assert_eq!(exprs.len(), 2);
                }
                _ => panic!("expected block"),
            },
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_let_pattern_binding() {
        let module = parse_module_str("def test() -> Int = { let (a, b) = pair; a }");
        match &module.items[0] {
            Item::Function(f) => match &f.body {
                Expr::Block(exprs, _) => {
                    assert!(matches!(&exprs[0], Expr::LetPattern(Pattern::Tuple(_, _), false, None, _, _)));
                }
                _ => panic!("expected block"),
            },
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_unary() {
        let module = parse_module_str("def test() -> Int = -42");
        match &module.items[0] {
            Item::Function(f) => {
                assert!(matches!(&f.body, Expr::Unary(UnaryOp::Neg, _, _)));
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_forall_type() {
        let module = parse_module_str("id: forall (Ord a). a -> a\ndef id(x) = x");
        match &module.items[0] {
            Item::TypeAnnotation(ta) => {
                assert!(matches!(ta.ty, TypeExpr::Forall(_, _, _)));
            }
            _ => panic!("expected type annotation"),
        }
    }

    #[test]
    fn test_parse_function_type_effects() {
        let module = parse_module_str("map: List a * (a -> b [e]) -> List b [e]\ndef map(xs, f) = xs");
        match &module.items[0] {
            Item::TypeAnnotation(ta) => match &ta.ty {
                TypeExpr::Function(_, _, Some(effects), _) => {
                    assert_eq!(effects.len(), 1);
                    assert_eq!(effects[0].name, "e");
                }
                _ => panic!("expected function type with effects"),
            },
            _ => panic!("expected type annotation"),
        }
    }

    #[test]
    fn test_parse_fn_effects_and_contracts() {
        let module = parse_module_str(
            "def f(x: Int) -> Int [Net, Log] requires x > 0 ensures result > 0 = x",
        );
        match &module.items[0] {
            Item::Function(f) => {
                assert_eq!(f.effects.len(), 2);
                assert_eq!(f.effects[0].name, "Net");
                assert_eq!(f.effects[1].name, "Log");
                assert_eq!(f.requires.len(), 1);
                assert_eq!(f.ensures.len(), 1);
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_concept_def() {
        let module = parse_module_str(
            "concept Display {\n\
               type Output\n\
               def display(self) -> String\n\
             }",
        );
        match &module.items[0] {
            Item::ConceptDef(c) => {
                assert_eq!(c.name, "Display");
                assert_eq!(c.assoc_types.len(), 1);
                assert_eq!(c.methods.len(), 1);
            }
            _ => panic!("expected concept def"),
        }
    }

    #[test]
    fn test_parse_instance_def() {
        let module = parse_module_str(
            "instance Display for User {\n\
               type Output = String\n\
               def display(self) -> String = self.name\n\
             }",
        );
        match &module.items[0] {
            Item::InstanceDef(inst) => {
                assert!(matches!(&inst.kind, InstanceKind::Concept { concept, .. } if concept == "Display"));
                assert_eq!(inst.assoc_types.len(), 1);
                assert_eq!(inst.methods.len(), 1);
            }
            _ => panic!("expected instance def"),
        }
    }

    #[test]
    fn test_parse_pipeline_method_sugar() {
        let module = parse_module_str("def test(x: Int) -> Int = x |> .to_string()");
        match &module.items[0] {
            Item::Function(f) => {
                assert!(matches!(&f.body, Expr::MethodCall(_, name, _, _) if name == "to_string"));
            }
            _ => panic!("expected function"),
        }
    }
}
