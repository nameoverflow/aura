use aura_parser::ast::*;

use super::{DefInfo, DefKind, ResolveError, Resolver};

impl Resolver {
    // Pass 2: Resolve references
    pub(crate) fn resolve_item(&mut self, item: &Item) {
        match item {
            Item::Function(f) => {
                self.scope.push();
                // Bind parameters
                for param in &f.params {
                    let id = self.fresh_id();
                    self.scope.define(param.name.clone(), id);
                    self.defs.insert(
                        id,
                        DefInfo {
                            id,
                            name: param.name.clone(),
                            kind: DefKind::Parameter,
                            span: param.span,
                            is_pub: false,
                        },
                    );
                    if let Some(ty) = &param.ty {
                        self.resolve_type_expr(ty);
                    }
                }
                if let Some(ret) = &f.return_type {
                    self.resolve_type_expr(ret);
                }
                for req in &f.requires {
                    self.resolve_expr(req);
                }
                self.resolve_expr(&f.body);
                if !f.ensures.is_empty() {
                    let result_id = self.fresh_id();
                    self.scope.define("result".into(), result_id);
                    self.defs.insert(
                        result_id,
                        DefInfo {
                            id: result_id,
                            name: "result".into(),
                            kind: DefKind::Variable,
                            span: f.span,
                            is_pub: false,
                        },
                    );
                    for ens in &f.ensures {
                        self.resolve_expr(ens);
                    }
                }
                self.scope.pop();
            }
            Item::TypeDef(td) => match &td.kind {
                TypeDefKind::Sum(variants) => {
                    for variant in variants {
                        for field in &variant.fields {
                            self.resolve_type_expr(field);
                        }
                    }
                }
                TypeDefKind::Struct(fields) => {
                    for field in fields {
                        self.resolve_type_expr(&field.ty);
                    }
                }
                TypeDefKind::Refined {
                    base_type,
                    constraint,
                } => {
                    self.resolve_type_expr(base_type);
                    self.scope.push();
                    let self_id = self.fresh_id();
                    self.scope.define("self".into(), self_id);
                    self.defs.insert(
                        self_id,
                        DefInfo {
                            id: self_id,
                            name: "self".into(),
                            kind: DefKind::Variable,
                            span: td.span,
                            is_pub: false,
                        },
                    );
                    self.resolve_expr(constraint);
                    self.scope.pop();
                }
                TypeDefKind::Alias(ty) => {
                    self.resolve_type_expr(ty);
                }
            },
            Item::ConceptDef(cd) => self.resolve_concept_def(cd),
            Item::InstanceDef(inst) => self.resolve_instance_def(inst),
            Item::Use(_) | Item::ModuleDecl(_) | Item::TypeAnnotation(_) => {}
        }
    }

    pub(crate) fn resolve_concept_def(&mut self, cd: &ConceptDef) {
        for sup in &cd.supers {
            if self.scope.lookup(sup).is_none() {
                self.errors.push(ResolveError {
                    message: format!("undefined concept '{}'", sup),
                    span: cd.span,
                });
            }
        }

        self.scope.push();
        // Self is valid in concept method signatures.
        let self_id = self.fresh_id();
        self.scope.define("Self".into(), self_id);
        self.defs.insert(
            self_id,
            DefInfo {
                id: self_id,
                name: "Self".into(),
                kind: DefKind::Type,
                span: cd.span,
                is_pub: false,
            },
        );

        for assoc in &cd.assoc_types {
            let id = self.fresh_id();
            self.scope.define(assoc.name.clone(), id);
            self.defs.insert(
                id,
                DefInfo {
                    id,
                    name: assoc.name.clone(),
                    kind: DefKind::AssocType,
                    span: assoc.span,
                    is_pub: false,
                },
            );
            if let Some(default) = &assoc.default {
                self.resolve_type_expr(default);
            }
        }

        for method in &cd.methods {
            self.scope.push();
            for param in &method.params {
                let id = self.fresh_id();
                self.scope.define(param.name.clone(), id);
                self.defs.insert(
                    id,
                    DefInfo {
                        id,
                        name: param.name.clone(),
                        kind: DefKind::Parameter,
                        span: param.span,
                        is_pub: false,
                    },
                );
                if let Some(ty) = &param.ty {
                    self.resolve_type_expr(ty);
                }
            }
            if let Some(ret) = &method.return_type {
                self.resolve_type_expr(ret);
            }
            if let Some(body) = &method.default_body {
                self.resolve_expr(body);
            }
            self.scope.pop();
        }

        self.scope.pop();
    }

    pub(crate) fn resolve_instance_def(&mut self, inst: &InstanceDef) {
        match &inst.kind {
            InstanceKind::Inherent(target) => self.resolve_type_expr(target),
            InstanceKind::Concept { concept, for_type } => {
                if self.scope.lookup(concept).is_none() {
                    self.errors.push(ResolveError {
                        message: format!("undefined concept '{}'", concept),
                        span: inst.span,
                    });
                }
                self.resolve_type_expr(for_type);
            }
        }

        self.scope.push();
        let self_id = self.fresh_id();
        self.scope.define("Self".into(), self_id);
        self.defs.insert(
            self_id,
            DefInfo {
                id: self_id,
                name: "Self".into(),
                kind: DefKind::Type,
                span: inst.span,
                is_pub: false,
            },
        );

        for assoc in &inst.assoc_types {
            self.resolve_type_expr(&assoc.ty);
            let id = self.fresh_id();
            self.scope.define(assoc.name.clone(), id);
            self.defs.insert(
                id,
                DefInfo {
                    id,
                    name: assoc.name.clone(),
                    kind: DefKind::AssocType,
                    span: assoc.span,
                    is_pub: false,
                },
            );
        }

        for method in &inst.methods {
            self.scope.push();
            for param in &method.params {
                let id = self.fresh_id();
                self.scope.define(param.name.clone(), id);
                self.defs.insert(
                    id,
                    DefInfo {
                        id,
                        name: param.name.clone(),
                        kind: DefKind::Parameter,
                        span: param.span,
                        is_pub: false,
                    },
                );
                if let Some(ty) = &param.ty {
                    self.resolve_type_expr(ty);
                }
            }
            if let Some(ret) = &method.return_type {
                self.resolve_type_expr(ret);
            }
            self.resolve_expr(&method.body);
            self.scope.pop();
        }

        self.scope.pop();
    }
}
