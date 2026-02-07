use aura_parser::ast::*;

use super::{DefInfo, DefKind, ResolveError, Resolver};

impl Resolver {
    pub(crate) fn resolve_expr(&mut self, expr: &Expr) {
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
            Expr::Parallel(body, _) => match body {
                ParallelBody::ForYield {
                    pattern,
                    iter,
                    body,
                    ..
                } => {
                    self.resolve_expr(iter);
                    self.scope.push();
                    self.resolve_pattern(pattern);
                    self.resolve_expr(body);
                    self.scope.pop();
                }
                ParallelBody::FixedYield(values) => {
                    for value in values {
                        self.resolve_expr(value);
                    }
                }
            },
            Expr::Race(arms, _) => {
                for arm in arms {
                    self.resolve_expr(arm);
                }
            }
            Expr::Timeout(duration, body, _) => {
                self.resolve_expr(duration);
                self.resolve_expr(body);
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
}
