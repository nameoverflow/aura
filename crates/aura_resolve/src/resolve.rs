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
    scope: Scope,
    next_id: u32,
    defs: HashMap<DefId, DefInfo>,
    references: HashMap<(u32, u32), DefId>,
    type_defs: HashMap<String, DefId>,
    variant_types: HashMap<String, String>,
    errors: Vec<ResolveError>,
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

    fn fresh_id(&mut self) -> DefId {
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

    // Pass 1: Collect all top-level names
    fn collect_top_level(&mut self, module: &Module) {
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

    // Pass 2: Resolve references
    fn resolve_item(&mut self, item: &Item) {
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
            },
            Item::ConceptDef(cd) => self.resolve_concept_def(cd),
            Item::InstanceDef(inst) => self.resolve_instance_def(inst),
            Item::Use(_) | Item::ModuleDecl(_) | Item::TypeAnnotation(_) => {}
        }
    }

    fn resolve_concept_def(&mut self, cd: &ConceptDef) {
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

    fn resolve_instance_def(&mut self, inst: &InstanceDef) {
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

    fn resolve_type_expr(&mut self, ty: &TypeExpr) {
        match ty {
            TypeExpr::Named(name, span) => {
                if let Some(id) = self.scope.lookup(name) {
                    self.references.insert((span.start, span.end), id);
                } else if name == "Self" || name.starts_with("Self.") {
                    // `Self` and `Self.Assoc` are valid in concept/instance contexts.
                } else if name
                    .chars()
                    .next()
                    .map(|c| c.is_ascii_lowercase())
                    .unwrap_or(false)
                {
                    // Type variable — no resolution needed
                } else {
                    self.errors.push(ResolveError {
                        message: format!("undefined type '{name}'"),
                        span: *span,
                    });
                }
            }
            TypeExpr::App(base, args, _) => {
                self.resolve_type_expr(base);
                for arg in args {
                    self.resolve_type_expr(arg);
                }
            }
            TypeExpr::Product(types, _) => {
                for t in types {
                    self.resolve_type_expr(t);
                }
            }
            TypeExpr::Function(params, ret, _, _) => {
                for p in params {
                    self.resolve_type_expr(p);
                }
                self.resolve_type_expr(ret);
            }
            TypeExpr::Forall(bounds, body, _) => {
                for bound in bounds {
                    if self.scope.lookup(&bound.concept).is_none() {
                        self.errors.push(ResolveError {
                            message: format!("undefined concept '{}'", bound.concept),
                            span: bound.span,
                        });
                    }
                }
                self.resolve_type_expr(body);
            }
            TypeExpr::Unit(_) => {}
        }
    }

    fn resolve_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Ident(name, span) => {
                if let Some(id) = self.scope.lookup(name) {
                    self.references.insert((span.start, span.end), id);
                } else {
                    self.errors.push(ResolveError {
                        message: format!("undefined variable '{name}'"),
                        span: *span,
                    });
                }
            }
            Expr::QualifiedIdent(type_name, variant_name, span) => {
                if let Some(type_id) = self.scope.lookup(type_name) {
                    self.references.insert((span.start, span.end), type_id);
                    // Also try to resolve the variant
                    let qualified = format!("{type_name}.{variant_name}");
                    if let Some(vid) = self.scope.lookup(variant_name) {
                        self.references.insert((span.start, span.end), vid);
                    }
                    let _ = qualified;
                } else {
                    self.errors.push(ResolveError {
                        message: format!("undefined type '{type_name}'"),
                        span: *span,
                    });
                }
            }
            Expr::Binary(lhs, _, rhs, _) => {
                self.resolve_expr(lhs);
                self.resolve_expr(rhs);
            }
            Expr::Unary(_, expr, _) => self.resolve_expr(expr),
            Expr::Pipeline(lhs, rhs, _) => {
                self.resolve_expr(lhs);
                self.resolve_expr(rhs);
            }
            Expr::Block(exprs, _) => {
                self.scope.push();
                for e in exprs {
                    self.resolve_expr(e);
                }
                self.scope.pop();
            }
            Expr::If(cond, then_branch, else_branch, _) => {
                self.resolve_expr(cond);
                self.resolve_expr(then_branch);
                if let Some(e) = else_branch {
                    self.resolve_expr(e);
                }
            }
            Expr::Match(scrutinee, arms, _) => {
                self.resolve_expr(scrutinee);
                for arm in arms {
                    self.scope.push();
                    self.resolve_pattern(&arm.pattern);
                    if let Some(guard) = &arm.guard {
                        self.resolve_expr(guard);
                    }
                    self.resolve_expr(&arm.body);
                    self.scope.pop();
                }
            }
            Expr::For(var, iter, body, span) => {
                self.resolve_expr(iter);
                self.scope.push();
                let id = self.fresh_id();
                self.scope.define(var.clone(), id);
                self.defs.insert(
                    id,
                    DefInfo {
                        id,
                        name: var.clone(),
                        kind: DefKind::Variable,
                        span: *span,
                        is_pub: false,
                    },
                );
                self.resolve_expr(body);
                self.scope.pop();
            }
            Expr::ForPattern(pattern, iter, body, _) => {
                self.resolve_expr(iter);
                self.scope.push();
                self.resolve_pattern(pattern);
                self.resolve_expr(body);
                self.scope.pop();
            }
            Expr::While(cond, body, _) => {
                self.resolve_expr(cond);
                self.resolve_expr(body);
            }
            Expr::Let(name, _, ty, value, span) => {
                self.resolve_expr(value);
                if let Some(t) = ty {
                    self.resolve_type_expr(t);
                }
                let id = self.fresh_id();
                self.scope.define(name.clone(), id);
                self.defs.insert(
                    id,
                    DefInfo {
                        id,
                        name: name.clone(),
                        kind: DefKind::Variable,
                        span: *span,
                        is_pub: false,
                    },
                );
            }
            Expr::LetPattern(pattern, _, ty, value, _) => {
                self.resolve_expr(value);
                if let Some(t) = ty {
                    self.resolve_type_expr(t);
                }
                self.resolve_pattern(pattern);
            }
            Expr::Assign(target, value, _) => {
                self.resolve_expr(target);
                self.resolve_expr(value);
            }
            Expr::Return(val, _) => {
                if let Some(v) = val {
                    self.resolve_expr(v);
                }
            }
            Expr::Break(_) | Expr::Continue(_) | Expr::Unit(_) => {}
            Expr::Call(callee, args, _) => {
                self.resolve_expr(callee);
                for arg in args {
                    self.resolve_expr(arg);
                }
            }
            Expr::MethodCall(receiver, _, args, _) => {
                self.resolve_expr(receiver);
                for arg in args {
                    self.resolve_expr(arg);
                }
            }
            Expr::FieldAccess(expr, _, _) => self.resolve_expr(expr),
            Expr::Lambda(params, _, body, _) => {
                self.scope.push();
                for param in params {
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
                }
                self.resolve_expr(body);
                self.scope.pop();
            }
            Expr::StructLit(name, fields, span) => {
                if let Some(id) = self.scope.lookup(name) {
                    self.references.insert((span.start, span.end), id);
                } else {
                    self.errors.push(ResolveError {
                        message: format!("undefined type '{name}'"),
                        span: *span,
                    });
                }
                for (_, val) in fields {
                    self.resolve_expr(val);
                }
            }
            Expr::With(base, fields, _) => {
                self.resolve_expr(base);
                for (_, val) in fields {
                    self.resolve_expr(val);
                }
            }
            Expr::ListLit(elems, _) | Expr::TupleLit(elems, _) => {
                for e in elems {
                    self.resolve_expr(e);
                }
            }
            Expr::Try(expr, _) => self.resolve_expr(expr),
            Expr::Range(start, end, _, _) => {
                self.resolve_expr(start);
                self.resolve_expr(end);
            }
            Expr::StringInterp(parts, _) => {
                for part in parts {
                    if let StringPartKind::Expr(e) = &part.kind {
                        self.resolve_expr(e);
                    }
                }
            }
            Expr::IntLit(_, _)
            | Expr::FloatLit(_, _)
            | Expr::StringLit(_, _)
            | Expr::BoolLit(_, _) => {}
        }
    }

    fn resolve_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Wildcard(_) => {}
            Pattern::Ident(name, span) => {
                let id = self.fresh_id();
                self.scope.define(name.clone(), id);
                self.defs.insert(
                    id,
                    DefInfo {
                        id,
                        name: name.clone(),
                        kind: DefKind::Variable,
                        span: *span,
                        is_pub: false,
                    },
                );
            }
            Pattern::Literal(_, _) => {}
            Pattern::Constructor(name, args, span) => {
                // Resolve the constructor name
                if let Some(id) = self.scope.lookup(name) {
                    self.references.insert((span.start, span.end), id);
                }
                // Constructor names that are uppercase and not in scope
                // will be caught during type checking
                for arg in args {
                    self.resolve_pattern(arg);
                }
            }
            Pattern::Tuple(pats, _) => {
                for p in pats {
                    self.resolve_pattern(p);
                }
            }
            Pattern::Struct(name, fields, _, span) => {
                if let Some(id) = self.scope.lookup(name) {
                    self.references.insert((span.start, span.end), id);
                }
                for field in fields {
                    self.resolve_pattern(&field.pattern);
                }
            }
            Pattern::Or(patterns, _) => {
                if let Some(first) = patterns.first() {
                    self.resolve_pattern(first);
                }
                for pat in patterns.iter().skip(1) {
                    self.scope.push();
                    self.resolve_pattern(pat);
                    self.scope.pop();
                }
            }
        }
    }
}

impl Default for Resolver {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use aura_lexer::Lexer;
    use aura_parser::Parser;

    fn resolve_str(input: &str) -> Result<ResolvedModule, Vec<ResolveError>> {
        let mut lexer = Lexer::new(input, 0);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let module = parser.parse_module().unwrap();
        let resolver = Resolver::new();
        resolver.resolve(module)
    }

    #[test]
    fn test_resolve_simple_function() {
        let result = resolve_str("def add(a: Int, b: Int) -> Int = a + b");
        assert!(result.is_ok());
    }

    #[test]
    fn test_resolve_undefined_variable() {
        let result = resolve_str("def test() -> Int = x");
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(errors[0].message.contains("undefined variable 'x'"));
    }

    #[test]
    fn test_resolve_let_binding() {
        let result = resolve_str("def test() -> Int = { let x = 42; x }");
        assert!(result.is_ok());
    }

    #[test]
    fn test_resolve_nested_scope() {
        let result = resolve_str("def test() -> Int = { let x = 1; { let y = x; y } }");
        assert!(result.is_ok());
    }

    #[test]
    fn test_resolve_function_call() {
        let result = resolve_str(
            "def add(a: Int, b: Int) -> Int = a + b\n\
             def main() -> Int = add(1, 2)",
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_resolve_type_def() {
        let result = resolve_str(
            "type Status = Pending | Complete\n\
             def test() -> Int = 0",
        );
        assert!(result.is_ok());
        let resolved = result.unwrap();
        assert!(resolved.type_defs.contains_key("Status"));
        assert!(resolved.variant_types.contains_key("Pending"));
        assert!(resolved.variant_types.contains_key("Complete"));
    }

    #[test]
    fn test_resolve_match_binding() {
        let result = resolve_str("def test(x: Int) -> Int = match x { n => n }");
        assert!(result.is_ok());
    }

    #[test]
    fn test_resolve_lambda() {
        let result = resolve_str("def test() -> Int = { let f = (x) -> x + 1; f(10) }");
        assert!(result.is_ok());
    }

    #[test]
    fn test_resolve_for_loop_binding() {
        let result = resolve_str("def test() -> Int = for i in 0..10 { i }");
        assert!(result.is_ok());
    }

    #[test]
    fn test_resolve_builtin_types() {
        let result = resolve_str("def test(x: Int, y: String, z: Bool) -> Float64 = 0.0");
        assert!(result.is_ok());
    }

    #[test]
    fn test_resolve_multi_char_type_vars() {
        let result = resolve_str(
            "id: value -> value\n\
             def id(x) = x\n\
             def test(x: Result value error) -> Int = 0",
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_resolve_print() {
        let result = resolve_str("def main() -> Int = { println(42); 0 }");
        assert!(result.is_ok());
    }

    #[test]
    fn test_resolve_duplicate_def() {
        let result = resolve_str("def foo() -> Int = 1\ndef foo() -> Int = 2");
        assert!(result.is_err());
        assert!(result.unwrap_err()[0].message.contains("duplicate"));
    }

    #[test]
    fn test_resolve_option_variants() {
        let result = resolve_str("def test(x: Int) -> Int = match x { 0 => 0, n => n }");
        assert!(result.is_ok());
    }

    #[test]
    fn test_resolve_struct_type() {
        let result = resolve_str(
            "type User = { name: String, age: Int }\n\
             def test() -> Int = 0",
        );
        assert!(result.is_ok());
        let resolved = result.unwrap();
        assert!(resolved.type_defs.contains_key("User"));
    }

    #[test]
    fn test_resolve_struct_pattern() {
        let result = resolve_str(
            "type User = { name: String, age: Int }\n\
             def test(u: User) -> String = match u { User { name, .. } => name }",
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_resolve_or_pattern() {
        let result = resolve_str(
            "type OptionInt = Some Int | None\n\
             def test(x: OptionInt) -> Int = match x { Some(n) | None => 0, Some(m) => m }",
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_resolve_let_pattern() {
        let result = resolve_str("def test() -> Int = { let (a, b) = (1, 2); a }");
        assert!(result.is_ok());
    }

    #[test]
    fn test_resolve_for_pattern() {
        let result = resolve_str("def test() -> Int = { for _ in 0..10 { 0 }; 1 }");
        assert!(result.is_ok());
    }

    #[test]
    fn test_resolve_refined_type_self_constraint() {
        let result = resolve_str(
            "type NonZero = Int where self != 0\n\
             def test(x: Int) -> Int = x",
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_resolve_function_contracts() {
        let result = resolve_str(
            "def f(x: Int) -> Int requires x > 0 ensures result > 0 = x",
        );
        assert!(result.is_ok());
    }
}
