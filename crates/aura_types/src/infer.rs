use std::collections::HashMap;
use aura_common::Span;
use aura_parser::ast::*;
use aura_resolve::{ResolvedModule, DefId, DefKind};
use crate::types::{Type, TypeVarId};

#[derive(Debug, Clone)]
pub struct TypeError {
    pub message: String,
    pub span: Span,
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "type error at {}..{}: {}", self.span.start, self.span.end, self.message)
    }
}

/// Typed module — the output of type checking.
pub struct TypedModule {
    pub module: Module,
    pub expr_types: HashMap<(u32, u32), Type>,
    pub def_types: HashMap<DefId, Type>,
    pub struct_fields: HashMap<String, Vec<(String, Type)>>,
    pub variant_info: HashMap<String, VariantTypeInfo>,
}

#[derive(Debug, Clone)]
pub struct VariantTypeInfo {
    pub parent_type: String,
    pub field_types: Vec<Type>,
}

pub struct TypeChecker {
    next_var: u32,
    substitution: HashMap<TypeVarId, Type>,
    expr_types: HashMap<(u32, u32), Type>,
    def_types: HashMap<DefId, Type>,
    struct_fields: HashMap<String, Vec<(String, Type)>>,
    variant_info: HashMap<String, VariantTypeInfo>,
    errors: Vec<TypeError>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            next_var: 0,
            substitution: HashMap::new(),
            expr_types: HashMap::new(),
            def_types: HashMap::new(),
            struct_fields: HashMap::new(),
            variant_info: HashMap::new(),
            errors: Vec::new(),
        }
    }

    fn fresh_var(&mut self) -> Type {
        let id = TypeVarId(self.next_var);
        self.next_var += 1;
        Type::Var(id)
    }

    fn record_type(&mut self, span: Span, ty: Type) {
        self.expr_types.insert((span.start, span.end), ty);
    }

    pub fn check(mut self, resolved: &ResolvedModule) -> Result<TypedModule, Vec<TypeError>> {
        // First pass: collect type definitions (structs and sum types)
        for item in &resolved.module.items {
            if let Item::TypeDef(td) = item {
                self.collect_type_def(td);
            }
        }

        // Second pass: collect function signatures (from annotations and defs)
        let mut annotations: HashMap<String, TypeExpr> = HashMap::new();
        for item in &resolved.module.items {
            if let Item::TypeAnnotation(ta) = item {
                annotations.insert(ta.name.clone(), ta.ty.clone());
            }
        }

        // Register function types
        for item in &resolved.module.items {
            if let Item::Function(f) = item {
                let fn_type = self.function_signature(f, annotations.get(&f.name));
                if let Some(&def_id) = resolved.references.get(&(f.span.start, f.span.end)) {
                    self.def_types.insert(def_id, fn_type);
                }
                // Also store by name lookup via the defs map
                for (id, info) in &resolved.defs {
                    if info.name == f.name && matches!(info.kind, DefKind::Function) {
                        let sig = self.function_signature(f, annotations.get(&f.name));
                        self.def_types.insert(*id, sig);
                        break;
                    }
                }
            }
        }

        // Third pass: type check function bodies
        for item in &resolved.module.items {
            if let Item::Function(f) = item {
                self.check_function(f, resolved);
            }
        }

        if self.errors.is_empty() {
            Ok(TypedModule {
                module: resolved.module.clone(),
                expr_types: self.expr_types,
                def_types: self.def_types,
                struct_fields: self.struct_fields,
                variant_info: self.variant_info,
            })
        } else {
            Err(self.errors)
        }
    }

    fn collect_type_def(&mut self, td: &TypeDef) {
        match &td.kind {
            TypeDefKind::Struct(fields) => {
                let field_types: Vec<(String, Type)> = fields
                    .iter()
                    .map(|f| (f.name.clone(), self.resolve_type_expr(&f.ty)))
                    .collect();
                self.struct_fields.insert(td.name.clone(), field_types);
            }
            TypeDefKind::Sum(variants) => {
                for variant in variants {
                    let field_types: Vec<Type> = variant
                        .fields
                        .iter()
                        .map(|f| self.resolve_type_expr(f))
                        .collect();
                    self.variant_info.insert(variant.name.clone(), VariantTypeInfo {
                        parent_type: td.name.clone(),
                        field_types,
                    });
                }
            }
            TypeDefKind::Refined { .. } => {
                // Refined types deferred to P2
            }
        }
    }

    fn function_signature(&mut self, f: &FnDef, _annotation: Option<&TypeExpr>) -> Type {
        let param_types: Vec<Type> = f.params.iter().map(|p| {
            match &p.ty {
                Some(te) => self.resolve_type_expr(te),
                None => self.fresh_var(),
            }
        }).collect();

        let ret_type = match &f.return_type {
            Some(te) => self.resolve_type_expr(te),
            None => self.fresh_var(),
        };

        Type::Function(param_types, Box::new(ret_type))
    }

    fn check_function(&mut self, f: &FnDef, resolved: &ResolvedModule) {
        // Build local env: parameter name -> type
        let mut env: HashMap<String, Type> = HashMap::new();

        for param in &f.params {
            let ty = match &param.ty {
                Some(te) => self.resolve_type_expr(te),
                None => self.fresh_var(),
            };
            // Find the DefId for this parameter
            for (id, info) in &resolved.defs {
                if info.name == param.name && matches!(info.kind, DefKind::Parameter) && info.span == param.span {
                    self.def_types.insert(*id, ty.clone());
                    break;
                }
            }
            env.insert(param.name.clone(), ty);
        }

        let expected_ret = match &f.return_type {
            Some(te) => self.resolve_type_expr(te),
            None => self.fresh_var(),
        };

        let actual_ret = self.infer_expr(&f.body, &mut env, resolved);
        self.unify(&expected_ret, &actual_ret, f.body.span());
    }

    fn infer_expr(&mut self, expr: &Expr, env: &mut HashMap<String, Type>, resolved: &ResolvedModule) -> Type {
        let ty = match expr {
            Expr::IntLit(_, span) => {
                self.record_type(*span, Type::Int);
                Type::Int
            }
            Expr::FloatLit(_, span) => {
                self.record_type(*span, Type::Float64);
                Type::Float64
            }
            Expr::StringLit(_, span) => {
                self.record_type(*span, Type::String);
                Type::String
            }
            Expr::StringInterp(_, span) => {
                self.record_type(*span, Type::String);
                Type::String
            }
            Expr::BoolLit(_, span) => {
                self.record_type(*span, Type::Bool);
                Type::Bool
            }
            Expr::Unit(span) => {
                self.record_type(*span, Type::Unit);
                Type::Unit
            }
            Expr::Ident(name, span) => {
                if let Some(ty) = env.get(name) {
                    let ty = ty.clone();
                    self.record_type(*span, ty.clone());
                    ty
                } else if let Some(id) = resolved.references.get(&(span.start, span.end)) {
                    if let Some(ty) = self.def_types.get(id) {
                        let ty = ty.clone();
                        self.record_type(*span, ty.clone());
                        ty
                    } else {
                        let info = resolved.defs.get(id);
                        if let Some(info) = info {
                            if let DefKind::Variant { parent_type } = &info.kind {
                                // Variant used as value (no args)
                                let ty = Type::Named(parent_type.clone(), Vec::new());
                                self.record_type(*span, ty.clone());
                                return ty;
                            }
                        }
                        let ty = self.fresh_var();
                        self.record_type(*span, ty.clone());
                        ty
                    }
                } else {
                    let ty = self.fresh_var();
                    self.record_type(*span, ty.clone());
                    ty
                }
            }
            Expr::QualifiedIdent(_, _, span) => {
                // Qualified ident like Status.Pending
                let ty = self.fresh_var();
                self.record_type(*span, ty.clone());
                ty
            }
            Expr::Binary(lhs, op, rhs, span) => {
                let lhs_ty = self.infer_expr(lhs, env, resolved);
                let rhs_ty = self.infer_expr(rhs, env, resolved);
                let result_ty = match op {
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                        self.unify(&lhs_ty, &rhs_ty, *span);
                        self.apply(&lhs_ty)
                    }
                    BinOp::Eq | BinOp::NotEq | BinOp::Lt | BinOp::Gt |
                    BinOp::LtEq | BinOp::GtEq => {
                        self.unify(&lhs_ty, &rhs_ty, *span);
                        Type::Bool
                    }
                    BinOp::And | BinOp::Or => {
                        self.unify(&lhs_ty, &Type::Bool, *span);
                        self.unify(&rhs_ty, &Type::Bool, *span);
                        Type::Bool
                    }
                };
                self.record_type(*span, result_ty.clone());
                result_ty
            }
            Expr::Unary(op, inner, span) => {
                let inner_ty = self.infer_expr(inner, env, resolved);
                let result_ty = match op {
                    UnaryOp::Neg => {
                        // Must be numeric
                        self.record_type(*span, inner_ty.clone());
                        inner_ty
                    }
                    UnaryOp::Not => {
                        self.unify(&inner_ty, &Type::Bool, *span);
                        Type::Bool
                    }
                };
                self.record_type(*span, result_ty.clone());
                result_ty
            }
            Expr::Pipeline(lhs, rhs, span) => {
                // a |> f(args) desugars to f(a, args)
                let _lhs_ty = self.infer_expr(lhs, env, resolved);
                let result = self.infer_expr(rhs, env, resolved);
                self.record_type(*span, result.clone());
                result
            }
            Expr::Block(exprs, span) => {
                let mut result = Type::Unit;
                for e in exprs {
                    result = self.infer_expr(e, env, resolved);
                }
                self.record_type(*span, result.clone());
                result
            }
            Expr::If(cond, then_branch, else_branch, span) => {
                let cond_ty = self.infer_expr(cond, env, resolved);
                self.unify(&cond_ty, &Type::Bool, cond.span());

                let then_ty = self.infer_expr(then_branch, env, resolved);
                let result = if let Some(else_br) = else_branch {
                    let else_ty = self.infer_expr(else_br, env, resolved);
                    self.unify(&then_ty, &else_ty, *span);
                    self.apply(&then_ty)
                } else {
                    // No else branch: result is Unit
                    Type::Unit
                };
                self.record_type(*span, result.clone());
                result
            }
            Expr::Match(scrutinee, arms, span) => {
                let _scrut_ty = self.infer_expr(scrutinee, env, resolved);
                let result_ty = self.fresh_var();

                for arm in arms {
                    // For each arm, infer the pattern bindings then check the body
                    let mut arm_env = env.clone();
                    self.bind_pattern(&arm.pattern, &_scrut_ty, &mut arm_env);
                    if let Some(guard) = &arm.guard {
                        let guard_ty = self.infer_expr(guard, &mut arm_env, resolved);
                        self.unify(&guard_ty, &Type::Bool, guard.span());
                    }
                    let body_ty = self.infer_expr(&arm.body, &mut arm_env, resolved);
                    self.unify(&result_ty, &body_ty, arm.body.span());
                }

                let result = self.apply(&result_ty);
                self.record_type(*span, result.clone());
                result
            }
            Expr::For(var, iter, body, span) => {
                let _iter_ty = self.infer_expr(iter, env, resolved);
                let mut body_env = env.clone();
                // For now, loop var type is Int (for range loops)
                body_env.insert(var.clone(), Type::Int);
                self.infer_expr(body, &mut body_env, resolved);
                self.record_type(*span, Type::Unit);
                Type::Unit
            }
            Expr::While(cond, body, span) => {
                let cond_ty = self.infer_expr(cond, env, resolved);
                self.unify(&cond_ty, &Type::Bool, cond.span());
                self.infer_expr(body, env, resolved);
                self.record_type(*span, Type::Unit);
                Type::Unit
            }
            Expr::Let(name, _is_mut, type_ann, value, span) => {
                let val_ty = self.infer_expr(value, env, resolved);
                let ty = if let Some(te) = type_ann {
                    let ann_ty = self.resolve_type_expr(te);
                    self.unify(&ann_ty, &val_ty, *span);
                    self.apply(&ann_ty)
                } else {
                    self.apply(&val_ty)
                };
                env.insert(name.clone(), ty.clone());
                self.record_type(*span, Type::Unit);
                Type::Unit
            }
            Expr::Assign(target, value, span) => {
                let target_ty = self.infer_expr(target, env, resolved);
                let val_ty = self.infer_expr(value, env, resolved);
                self.unify(&target_ty, &val_ty, *span);
                self.record_type(*span, Type::Unit);
                Type::Unit
            }
            Expr::Return(val, span) => {
                if let Some(v) = val {
                    self.infer_expr(v, env, resolved);
                }
                // Return type checking deferred (would need function context)
                let ty = self.fresh_var();
                self.record_type(*span, ty.clone());
                ty
            }
            Expr::Break(span) | Expr::Continue(span) => {
                self.record_type(*span, Type::Unit);
                Type::Unit
            }
            Expr::Call(callee, args, span) => {
                // Check if this is a variant constructor call (e.g., Some(42))
                if let Expr::Ident(name, _) = callee.as_ref() {
                    if let Some(vinfo) = self.variant_info.get(name).cloned() {
                        // Variant constructor: check payload arg types
                        let arg_types: Vec<Type> = args.iter()
                            .map(|a| self.infer_expr(a, env, resolved))
                            .collect();
                        for (i, (arg_ty, expected_ty)) in arg_types.iter().zip(&vinfo.field_types).enumerate() {
                            self.unify(arg_ty, expected_ty, *span);
                            let _ = i;
                        }
                        let result = Type::Named(vinfo.parent_type.clone(), Vec::new());
                        self.record_type(*span, result.clone());
                        return result;
                    }
                }

                let callee_ty = self.infer_expr(callee, env, resolved);
                let arg_types: Vec<Type> = args.iter()
                    .map(|a| self.infer_expr(a, env, resolved))
                    .collect();

                let ret_ty = self.fresh_var();
                let expected_fn = Type::Function(arg_types.clone(), Box::new(ret_ty.clone()));
                self.unify(&callee_ty, &expected_fn, *span);

                let result = self.apply(&ret_ty);
                self.record_type(*span, result.clone());
                result
            }
            Expr::MethodCall(receiver, _method, args, span) => {
                self.infer_expr(receiver, env, resolved);
                for arg in args {
                    self.infer_expr(arg, env, resolved);
                }
                let ty = self.fresh_var();
                self.record_type(*span, ty.clone());
                ty
            }
            Expr::FieldAccess(receiver, field_name, span) => {
                let recv_ty = self.infer_expr(receiver, env, resolved);
                let recv_ty = self.apply(&recv_ty);

                let ty = match &recv_ty {
                    Type::Named(name, _) => {
                        if let Some(fields) = self.struct_fields.get(name) {
                            let found = fields.iter().find(|(n, _)| n == field_name);
                            if let Some((_, ft)) = found {
                                ft.clone()
                            } else {
                                self.errors.push(TypeError {
                                    message: format!("type '{name}' has no field '{field_name}'"),
                                    span: *span,
                                });
                                Type::Error
                            }
                        } else {
                            self.fresh_var()
                        }
                    }
                    _ => self.fresh_var(),
                };
                self.record_type(*span, ty.clone());
                ty
            }
            Expr::Lambda(params, _, body, span) => {
                let mut lambda_env = env.clone();
                let param_types: Vec<Type> = params.iter().map(|p| {
                    let ty = match &p.ty {
                        Some(te) => self.resolve_type_expr(te),
                        None => self.fresh_var(),
                    };
                    lambda_env.insert(p.name.clone(), ty.clone());
                    ty
                }).collect();
                let body_ty = self.infer_expr(body, &mut lambda_env, resolved);
                let fn_ty = Type::Function(param_types, Box::new(body_ty));
                self.record_type(*span, fn_ty.clone());
                fn_ty
            }
            Expr::StructLit(name, fields, span) => {
                for (_, val) in fields {
                    self.infer_expr(val, env, resolved);
                }
                let ty = Type::Named(name.clone(), Vec::new());
                self.record_type(*span, ty.clone());
                ty
            }
            Expr::With(base, fields, span) => {
                let base_ty = self.infer_expr(base, env, resolved);
                for (_, val) in fields {
                    self.infer_expr(val, env, resolved);
                }
                self.record_type(*span, base_ty.clone());
                base_ty
            }
            Expr::ListLit(elems, span) => {
                let elem_ty = self.fresh_var();
                for e in elems {
                    let et = self.infer_expr(e, env, resolved);
                    self.unify(&elem_ty, &et, e.span());
                }
                let list_ty = Type::Named("List".into(), vec![self.apply(&elem_ty)]);
                self.record_type(*span, list_ty.clone());
                list_ty
            }
            Expr::TupleLit(elems, span) => {
                let types: Vec<Type> = elems.iter()
                    .map(|e| self.infer_expr(e, env, resolved))
                    .collect();
                let product = Type::Product(types);
                self.record_type(*span, product.clone());
                product
            }
            Expr::Try(inner, span) => {
                let _inner_ty = self.infer_expr(inner, env, resolved);
                // Try operator extracts value from Result
                let ty = self.fresh_var();
                self.record_type(*span, ty.clone());
                ty
            }
            Expr::Range(start, end, _, span) => {
                let start_ty = self.infer_expr(start, env, resolved);
                let end_ty = self.infer_expr(end, env, resolved);
                self.unify(&start_ty, &end_ty, *span);
                let range_ty = Type::Named("Range".into(), vec![self.apply(&start_ty)]);
                self.record_type(*span, range_ty.clone());
                range_ty
            }
        };
        ty
    }

    fn bind_pattern(&mut self, pattern: &Pattern, expected: &Type, env: &mut HashMap<String, Type>) {
        match pattern {
            Pattern::Wildcard(_) => {}
            Pattern::Ident(name, _) => {
                env.insert(name.clone(), expected.clone());
            }
            Pattern::Literal(_, _) => {}
            Pattern::Constructor(name, args, _) => {
                if let Some(info) = self.variant_info.get(name).cloned() {
                    for (i, arg_pat) in args.iter().enumerate() {
                        let field_ty = info.field_types.get(i).cloned().unwrap_or(Type::Error);
                        self.bind_pattern(arg_pat, &field_ty, env);
                    }
                } else {
                    // Unknown variant - bind args as fresh vars
                    for arg_pat in args {
                        let ty = self.fresh_var();
                        self.bind_pattern(arg_pat, &ty, env);
                    }
                }
            }
            Pattern::Tuple(pats, _) => {
                for (i, pat) in pats.iter().enumerate() {
                    let ty = match expected {
                        Type::Product(types) => types.get(i).cloned().unwrap_or(Type::Error),
                        _ => self.fresh_var(),
                    };
                    self.bind_pattern(pat, &ty, env);
                }
            }
        }
    }

    fn resolve_type_expr(&mut self, te: &TypeExpr) -> Type {
        match te {
            TypeExpr::Named(name, _) => {
                if let Some(ty) = Type::from_name(name) {
                    ty
                } else {
                    Type::Named(name.clone(), Vec::new())
                }
            }
            TypeExpr::App(base, args, _) => {
                let base_name = match base.as_ref() {
                    TypeExpr::Named(name, _) => name.clone(),
                    _ => return Type::Error,
                };
                let arg_types: Vec<Type> = args.iter()
                    .map(|a| self.resolve_type_expr(a))
                    .collect();
                Type::Named(base_name, arg_types)
            }
            TypeExpr::Product(types, _) => {
                let ts: Vec<Type> = types.iter()
                    .map(|t| self.resolve_type_expr(t))
                    .collect();
                Type::Product(ts)
            }
            TypeExpr::Function(params, ret, _) => {
                let param_types: Vec<Type> = params.iter()
                    .map(|p| self.resolve_type_expr(p))
                    .collect();
                let ret_type = self.resolve_type_expr(ret);
                Type::Function(param_types, Box::new(ret_type))
            }
            TypeExpr::Unit(_) => Type::Unit,
        }
    }

    // ── Unification ──

    fn unify(&mut self, a: &Type, b: &Type, span: Span) {
        let a = self.apply(a);
        let b = self.apply(b);

        match (&a, &b) {
            _ if a == b => {} // Same type
            (Type::Var(id), _) => {
                if self.occurs_in(*id, &b) {
                    self.errors.push(TypeError {
                        message: format!("infinite type: {a} occurs in {b}"),
                        span,
                    });
                } else {
                    self.substitution.insert(*id, b);
                }
            }
            (_, Type::Var(id)) => {
                if self.occurs_in(*id, &a) {
                    self.errors.push(TypeError {
                        message: format!("infinite type: {b} occurs in {a}"),
                        span,
                    });
                } else {
                    self.substitution.insert(*id, a);
                }
            }
            (Type::Function(p1, r1), Type::Function(p2, r2)) => {
                if p1.len() != p2.len() {
                    self.errors.push(TypeError {
                        message: format!("function arity mismatch: expected {} args, got {}", p1.len(), p2.len()),
                        span,
                    });
                    return;
                }
                for (a, b) in p1.iter().zip(p2.iter()) {
                    self.unify(a, b, span);
                }
                self.unify(r1, r2, span);
            }
            (Type::Product(ts1), Type::Product(ts2)) => {
                if ts1.len() != ts2.len() {
                    self.errors.push(TypeError {
                        message: format!("product type mismatch: {} vs {} elements", ts1.len(), ts2.len()),
                        span,
                    });
                    return;
                }
                for (a, b) in ts1.iter().zip(ts2.iter()) {
                    self.unify(a, b, span);
                }
            }
            (Type::Named(n1, a1), Type::Named(n2, a2)) => {
                if n1 != n2 {
                    self.errors.push(TypeError {
                        message: format!("type mismatch: expected {a}, got {b}"),
                        span,
                    });
                    return;
                }
                if a1.len() != a2.len() {
                    self.errors.push(TypeError {
                        message: format!("type argument mismatch for '{n1}'"),
                        span,
                    });
                    return;
                }
                for (x, y) in a1.iter().zip(a2.iter()) {
                    self.unify(x, y, span);
                }
            }
            (Type::Error, _) | (_, Type::Error) => {} // Error recovery
            _ => {
                self.errors.push(TypeError {
                    message: format!("type mismatch: expected {a}, got {b}"),
                    span,
                });
            }
        }
    }

    fn occurs_in(&self, id: TypeVarId, ty: &Type) -> bool {
        match ty {
            Type::Var(other) => *other == id,
            Type::Function(params, ret) => {
                params.iter().any(|p| self.occurs_in(id, p)) || self.occurs_in(id, ret)
            }
            Type::Product(types) => types.iter().any(|t| self.occurs_in(id, t)),
            Type::Named(_, args) => args.iter().any(|a| self.occurs_in(id, a)),
            _ => false,
        }
    }

    fn apply(&self, ty: &Type) -> Type {
        match ty {
            Type::Var(id) => {
                if let Some(resolved) = self.substitution.get(id) {
                    self.apply(resolved)
                } else {
                    ty.clone()
                }
            }
            Type::Function(params, ret) => {
                let params: Vec<Type> = params.iter().map(|p| self.apply(p)).collect();
                let ret = self.apply(ret);
                Type::Function(params, Box::new(ret))
            }
            Type::Product(types) => {
                let types: Vec<Type> = types.iter().map(|t| self.apply(t)).collect();
                Type::Product(types)
            }
            Type::Named(name, args) => {
                let args: Vec<Type> = args.iter().map(|a| self.apply(a)).collect();
                Type::Named(name.clone(), args)
            }
            _ => ty.clone(),
        }
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use aura_lexer::Lexer;
    use aura_parser::Parser;
    use aura_resolve::Resolver;

    fn typecheck_str(input: &str) -> Result<TypedModule, Vec<TypeError>> {
        let mut lexer = Lexer::new(input, 0);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let module = parser.parse_module().unwrap();
        let resolver = Resolver::new();
        let resolved = resolver.resolve(module).unwrap();
        let checker = TypeChecker::new();
        checker.check(&resolved)
    }

    #[test]
    fn test_int_literal() {
        let result = typecheck_str("def test() -> Int = 42");
        assert!(result.is_ok());
    }

    #[test]
    fn test_float_literal() {
        let result = typecheck_str("def test() -> Float64 = 3.14");
        assert!(result.is_ok());
    }

    #[test]
    fn test_string_literal() {
        let result = typecheck_str("def test() -> String = \"hello\"");
        assert!(result.is_ok());
    }

    #[test]
    fn test_bool_literal() {
        let result = typecheck_str("def test() -> Bool = true");
        assert!(result.is_ok());
    }

    #[test]
    fn test_arithmetic() {
        let result = typecheck_str("def test(a: Int, b: Int) -> Int = a + b");
        assert!(result.is_ok());
    }

    #[test]
    fn test_comparison() {
        let result = typecheck_str("def test(a: Int, b: Int) -> Bool = a < b");
        assert!(result.is_ok());
    }

    #[test]
    fn test_if_expression() {
        let result = typecheck_str("def test(x: Bool) -> Int = if x { 1 } else { 2 }");
        assert!(result.is_ok());
    }

    #[test]
    fn test_type_mismatch() {
        let result = typecheck_str("def test() -> Int = true");
        assert!(result.is_err());
    }

    #[test]
    fn test_let_binding_infer() {
        let result = typecheck_str("def test() -> Int = { let x = 42; x }");
        assert!(result.is_ok());
    }

    #[test]
    fn test_function_call() {
        let result = typecheck_str(
            "def add(a: Int, b: Int) -> Int = a + b\n\
             def main() -> Int = add(1, 2)"
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_struct_field_access() {
        let result = typecheck_str(
            "type User = { name: String, age: Int }\n\
             def test(u: User) -> String = u.name"
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_lambda_type() {
        let result = typecheck_str(
            "def test() -> Int = { let f = (x: Int) -> x + 1; f(10) }"
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_if_branch_mismatch() {
        let result = typecheck_str(
            "def test(x: Bool) -> Int = if x { 1 } else { \"hello\" }"
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_logical_operators() {
        let result = typecheck_str(
            "def test(a: Bool, b: Bool) -> Bool = a and b or not a"
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_while_loop() {
        let result = typecheck_str(
            "def test(x: Bool) -> Int = { while x { 0 }; 1 }"
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_match_expression() {
        let result = typecheck_str(
            "def test(x: Int) -> Int = match x { 1 => 10, _ => 0 }"
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_list_literal() {
        let result = typecheck_str("def test() -> Int = { let xs = [1, 2, 3]; 0 }");
        assert!(result.is_ok());
    }

    #[test]
    fn test_tuple_literal() {
        let result = typecheck_str("def test() -> Int = { let t = (1, \"hello\"); 0 }");
        assert!(result.is_ok());
    }

    #[test]
    fn test_nested_function_calls() {
        let result = typecheck_str(
            "def double(x: Int) -> Int = x * 2\n\
             def main() -> Int = double(double(5))"
        );
        assert!(result.is_ok());
    }
}
