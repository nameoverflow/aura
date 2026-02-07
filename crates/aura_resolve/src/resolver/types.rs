use aura_parser::ast::*;

use super::{ResolveError, Resolver};

impl Resolver {
    pub(crate) fn resolve_type_expr(&mut self, ty: &TypeExpr) {
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
}
