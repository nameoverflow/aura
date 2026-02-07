use crate::ast::*;
use aura_lexer::token::TokenKind;

use super::{ParseError, Parser};

impl Parser {
    pub(crate) fn parse_item(&mut self) -> Result<Item, ParseError> {
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

    pub(crate) fn parse_param_list(&mut self) -> Result<Vec<Param>, ParseError> {
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
            self.parse_struct_body()?
        } else if let TokenKind::UpperIdent(_) = self.peek() {
            if self.has_pipe_in_type_def_ahead() || !self.looks_like_type_alias_ahead() {
                self.parse_sum_variants()?
            } else {
                // Type alias: `type Name = ExistingType`
                let ty = self.parse_type_expr()?;
                TypeDefKind::Alias(ty)
            }
        } else {
            self.parse_alias_or_refined()?
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

    fn parse_struct_body(&mut self) -> Result<TypeDefKind, ParseError> {
        self.advance(); // consume {
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
        Ok(TypeDefKind::Struct(fields))
    }

    fn parse_sum_variants(&mut self) -> Result<TypeDefKind, ParseError> {
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
        Ok(TypeDefKind::Sum(variants))
    }

    fn parse_alias_or_refined(&mut self) -> Result<TypeDefKind, ParseError> {
        // Type expression starting with lowercase, parens, etc.
        let base = self.parse_type_expr()?;
        if self.eat(&TokenKind::Where) {
            let constraint = self.parse_expr()?;
            Ok(TypeDefKind::Refined {
                base_type: base,
                constraint,
            })
        } else {
            // Type alias to non-uppercase-starting type expression
            Ok(TypeDefKind::Alias(base))
        }
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
}
