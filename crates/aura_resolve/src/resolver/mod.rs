mod collector;
mod exprs;
mod items;
mod patterns;
mod types;

#[cfg(test)]
mod tests;

use crate::scope::Scope;
use aura_common::Span;
use aura_parser::ast::*;
use std::collections::HashMap;

pub use crate::scope::DefId;

#[derive(Debug, Clone)]
pub struct ResolveError {
    pub message: String,
    pub span: Span,
}

impl std::fmt::Display for ResolveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "resolve error at {}..{}: {}",
            self.span.start, self.span.end, self.message
        )
    }
}

#[derive(Debug, Clone)]
pub enum DefKind {
    Function,
    Concept,
    Method,
    Variable,
    Parameter,
    Type,
    Variant { parent_type: String },
    AssocType,
}

#[derive(Debug, Clone)]
pub struct DefInfo {
    pub id: DefId,
    pub name: String,
    pub kind: DefKind,
    pub span: Span,
    pub is_pub: bool,
}

/// Result of name resolution on a module.
#[derive(Debug)]
pub struct ResolvedModule {
    pub module: Module,
    /// Map from DefId to definition info
    pub defs: HashMap<DefId, DefInfo>,
    /// Map from expression span to the DefId it refers to
    pub references: HashMap<(u32, u32), DefId>,
    /// Type definitions (name -> DefId)
    pub type_defs: HashMap<String, DefId>,
    /// Variant to parent type mapping
    pub variant_types: HashMap<String, String>,
}

pub struct Resolver {
    pub(crate) scope: Scope,
    pub(crate) next_id: u32,
    pub(crate) defs: HashMap<DefId, DefInfo>,
    pub(crate) references: HashMap<(u32, u32), DefId>,
    pub(crate) type_defs: HashMap<String, DefId>,
    pub(crate) variant_types: HashMap<String, String>,
    pub(crate) errors: Vec<ResolveError>,
}

impl Resolver {
    pub fn new() -> Self {
        let mut resolver = Self {
            scope: Scope::new(),
            next_id: 0,
            defs: HashMap::new(),
            references: HashMap::new(),
            type_defs: HashMap::new(),
            variant_types: HashMap::new(),
            errors: Vec::new(),
        };
        // Register built-in types in the prelude scope
        for ty in [
            "Int",
            "Int8",
            "Int16",
            "Int32",
            "Int64",
            "UInt",
            "UInt8",
            "UInt16",
            "UInt32",
            "UInt64",
            "Float32",
            "Float64",
            "Decimal",
            "BigDecimal",
            "Bool",
            "Char",
            "String",
            "Unit",
            "Option",
            "Result",
            "List",
            "Map",
            "Set",
            "ConstraintError",
            "Duration",
            "Timeout",
            "Runtime",
        ] {
            let id = resolver.fresh_id();
            resolver.scope.define(ty.to_string(), id);
            resolver.defs.insert(
                id,
                DefInfo {
                    id,
                    name: ty.to_string(),
                    kind: DefKind::Type,
                    span: Span::dummy(),
                    is_pub: true,
                },
            );
            resolver.type_defs.insert(ty.to_string(), id);
        }
        // Built-in variants: Some, None, Ok, Err
        for (variant, parent) in [
            ("Some", "Option"),
            ("None", "Option"),
            ("Ok", "Result"),
            ("Err", "Result"),
        ] {
            let id = resolver.fresh_id();
            resolver.scope.define(variant.to_string(), id);
            resolver.defs.insert(
                id,
                DefInfo {
                    id,
                    name: variant.to_string(),
                    kind: DefKind::Variant {
                        parent_type: parent.to_string(),
                    },
                    span: Span::dummy(),
                    is_pub: true,
                },
            );
            resolver
                .variant_types
                .insert(variant.to_string(), parent.to_string());
        }
        // Built-in functions
        for func in ["print", "println"] {
            let id = resolver.fresh_id();
            resolver.scope.define(func.to_string(), id);
            resolver.defs.insert(
                id,
                DefInfo {
                    id,
                    name: func.to_string(),
                    kind: DefKind::Function,
                    span: Span::dummy(),
                    is_pub: true,
                },
            );
        }
        resolver
    }

    pub(crate) fn fresh_id(&mut self) -> DefId {
        let id = DefId(self.next_id);
        self.next_id += 1;
        id
    }

    pub fn resolve(mut self, module: Module) -> Result<ResolvedModule, Vec<ResolveError>> {
        // Pass 1: collect all top-level definitions
        self.collect_top_level(&module);

        // Pass 2: resolve all references
        for item in &module.items {
            self.resolve_item(item);
        }

        if self.errors.is_empty() {
            Ok(ResolvedModule {
                module,
                defs: self.defs,
                references: self.references,
                type_defs: self.type_defs,
                variant_types: self.variant_types,
            })
        } else {
            Err(self.errors)
        }
    }
}

impl Default for Resolver {
    fn default() -> Self {
        Self::new()
    }
}
