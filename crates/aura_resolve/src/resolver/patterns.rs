use aura_parser::ast::*;

use super::{DefInfo, DefKind, Resolver};

impl Resolver {
    pub(crate) fn resolve_pattern(&mut self, pattern: &Pattern) {
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
