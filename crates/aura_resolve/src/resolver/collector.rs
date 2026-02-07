use aura_parser::ast::*;

use super::{DefInfo, DefKind, ResolveError, Resolver};

impl Resolver {
    // Pass 1: Collect all top-level names
    pub(crate) fn collect_top_level(&mut self, module: &Module) {
        for item in &module.items {
            match item {
                Item::Function(f) => {
                    let id = self.fresh_id();
                    let prev = self.scope.define(f.name.clone(), id);
                    if prev.is_some() {
                        self.errors.push(ResolveError {
                            message: format!("duplicate definition '{}'", f.name),
                            span: f.span,
                        });
                    }
                    self.defs.insert(
                        id,
                        DefInfo {
                            id,
                            name: f.name.clone(),
                            kind: DefKind::Function,
                            span: f.span,
                            is_pub: f.is_pub,
                        },
                    );
                }
                Item::TypeDef(td) => {
                    let id = self.fresh_id();
                    self.scope.define(td.name.clone(), id);
                    self.defs.insert(
                        id,
                        DefInfo {
                            id,
                            name: td.name.clone(),
                            kind: DefKind::Type,
                            span: td.span,
                            is_pub: td.is_pub,
                        },
                    );
                    self.type_defs.insert(td.name.clone(), id);

                    // Register variants for sum types
                    if let TypeDefKind::Sum(variants) = &td.kind {
                        for variant in variants {
                            let vid = self.fresh_id();
                            self.scope.define(variant.name.clone(), vid);
                            self.defs.insert(
                                vid,
                                DefInfo {
                                    id: vid,
                                    name: variant.name.clone(),
                                    kind: DefKind::Variant {
                                        parent_type: td.name.clone(),
                                    },
                                    span: variant.span,
                                    is_pub: td.is_pub,
                                },
                            );
                            self.variant_types
                                .insert(variant.name.clone(), td.name.clone());
                        }
                    }
                }
                Item::ConceptDef(cd) => {
                    let id = self.fresh_id();
                    let prev = self.scope.define(cd.name.clone(), id);
                    if prev.is_some() {
                        self.errors.push(ResolveError {
                            message: format!("duplicate definition '{}'", cd.name),
                            span: cd.span,
                        });
                    }
                    self.defs.insert(
                        id,
                        DefInfo {
                            id,
                            name: cd.name.clone(),
                            kind: DefKind::Concept,
                            span: cd.span,
                            is_pub: cd.is_pub,
                        },
                    );
                }
                Item::InstanceDef(_) => {
                    // Instances do not introduce top-level names.
                }
                Item::TypeAnnotation(_) => {
                    // Type annotations are handled when their matching def is resolved
                }
                Item::Use(_) | Item::ModuleDecl(_) => {}
            }
        }
    }
}
