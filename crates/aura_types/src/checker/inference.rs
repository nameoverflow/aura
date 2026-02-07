use std::collections::{HashMap, HashSet};

use aura_parser::ast::*;
use aura_resolve::{DefKind, ResolvedModule};

use crate::types::Type;

use super::{TypeChecker, TypeEnv, TypeError, TypeScheme};

impl TypeChecker {
    pub(crate) fn infer_expr(&mut self, expr: &Expr, env: &mut TypeEnv, resolved: &ResolvedModule) -> Type {
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
                if let Some(scheme) = env.get(name).cloned() {
                    let ty = self.instantiate_scheme(&scheme);
                    self.record_type(*span, ty.clone());
                    ty
                } else if let Some(id) = resolved.references.get(&(span.start, span.end)) {
                    let info = resolved.defs.get(id);

                    if matches!(info.map(|i| &i.kind), Some(DefKind::Function)) {
                        if let Some(scheme) = self.def_schemes.get(id).cloned() {
                            let ty = self.instantiate_scheme(&scheme);
                            self.record_type(*span, ty.clone());
                            ty
                        } else {
                            let ty = self.fresh_var();
                            self.record_type(*span, ty.clone());
                            ty
                        }
                    } else if matches!(info.map(|i| &i.kind), Some(DefKind::Variant { .. })) {
                        self.variant_value_type(name, *span)
                    } else if let Some(ty) = self.def_types.get(id).cloned() {
                        self.record_type(*span, ty.clone());
                        ty
                    } else {
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
            Expr::QualifiedIdent(type_name, variant_name, span) => {
                let ty = if let Some(v) = self.variant_info.get(variant_name).cloned() {
                    let inst = self.instantiate_variant_info(&v);
                    if inst.parent_type != *type_name {
                        self.errors.push(TypeError {
                            message: format!(
                                "variant '{}' does not belong to type '{}'",
                                variant_name, type_name
                            ),
                            span: *span,
                        });
                        Type::Error
                    } else if inst.field_types.is_empty() {
                        Type::Named(inst.parent_type, inst.parent_args)
                    } else {
                        self.errors.push(TypeError {
                            message: format!(
                                "variant '{}' requires {} payload argument(s)",
                                variant_name,
                                inst.field_types.len()
                            ),
                            span: *span,
                        });
                        Type::Named(inst.parent_type, inst.parent_args)
                    }
                } else {
                    self.fresh_var()
                };

                self.record_type(*span, ty.clone());
                ty
            }
            Expr::Binary(lhs, op, rhs, span) => {
                self.infer_binary_op(lhs, op, rhs, *span, env, resolved)
            }
            Expr::Unary(op, inner, span) => {
                let inner_ty = self.infer_expr(inner, env, resolved);
                let result_ty = match op {
                    UnaryOp::Neg => {
                        let unified = self.apply(&inner_ty);
                        if !matches!(unified, Type::Var(_) | Type::Error) && !unified.is_numeric() {
                            self.errors.push(TypeError {
                                message: format!("unary '-' expects numeric type, got {}", unified),
                                span: *span,
                            });
                        }
                        unified
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
                self.infer_pipeline(lhs, rhs, *span, env, resolved)
            }
            Expr::Block(exprs, span) => {
                let mut block_env = env.clone();
                let mut result = Type::Unit;
                for e in exprs {
                    result = self.infer_expr(e, &mut block_env, resolved);
                }
                self.record_type(*span, result.clone());
                result
            }
            Expr::If(cond, then_branch, else_branch, span) => {
                let cond_ty = self.infer_expr(cond, env, resolved);
                self.unify(&cond_ty, &Type::Bool, cond.span());

                let mut then_env = env.clone();
                let then_ty = self.infer_expr(then_branch, &mut then_env, resolved);

                let result = if let Some(else_br) = else_branch {
                    let mut else_env = env.clone();
                    let else_ty = self.infer_expr(else_br, &mut else_env, resolved);
                    self.unify(&then_ty, &else_ty, *span);
                    self.apply(&then_ty)
                } else {
                    Type::Unit
                };

                self.record_type(*span, result.clone());
                result
            }
            Expr::Match(scrutinee, arms, span) => {
                let scrut_ty = self.infer_expr(scrutinee, env, resolved);
                let result_ty = self.fresh_var();

                for arm in arms {
                    let mut arm_env = env.clone();
                    self.bind_pattern(&arm.pattern, &scrut_ty, &mut arm_env, arm.span);

                    if let Some(guard) = &arm.guard {
                        let guard_ty = self.infer_expr(guard, &mut arm_env, resolved);
                        self.unify(&guard_ty, &Type::Bool, guard.span());
                    }

                    let body_ty = self.infer_expr(&arm.body, &mut arm_env, resolved);
                    self.unify(&result_ty, &body_ty, arm.body.span());
                }

                self.check_match_exhaustiveness(&scrut_ty, arms, *span);
                self.check_match_redundancy(&scrut_ty, arms);

                let result = self.apply(&result_ty);
                self.record_type(*span, result.clone());
                result
            }
            Expr::For(var, iter, body, span) => {
                let iter_ty = self.infer_expr(iter, env, resolved);

                let elem_ty = match self.apply(&iter_ty) {
                    Type::Named(name, args) if name == "Range" && args.len() == 1 => {
                        args[0].clone()
                    }
                    _ => Type::Int,
                };

                let mut body_env = env.clone();
                body_env.insert(var.clone(), TypeScheme::monomorphic(elem_ty));
                self.infer_expr(body, &mut body_env, resolved);

                self.record_type(*span, Type::Unit);
                Type::Unit
            }
            Expr::ForPattern(pattern, iter, body, span) => {
                let iter_ty = self.infer_expr(iter, env, resolved);

                let elem_ty = match self.apply(&iter_ty) {
                    Type::Named(name, args) if name == "Range" && args.len() == 1 => {
                        args[0].clone()
                    }
                    _ => Type::Int,
                };

                let mut body_env = env.clone();
                self.bind_pattern(pattern, &elem_ty, &mut body_env, *span);
                self.infer_expr(body, &mut body_env, resolved);

                self.record_type(*span, Type::Unit);
                Type::Unit
            }
            Expr::While(cond, body, span) => {
                let cond_ty = self.infer_expr(cond, env, resolved);
                self.unify(&cond_ty, &Type::Bool, cond.span());

                let mut body_env = env.clone();
                self.infer_expr(body, &mut body_env, resolved);

                self.record_type(*span, Type::Unit);
                Type::Unit
            }
            Expr::Let(name, is_mut, type_ann, value, span) => {
                let val_ty = self.infer_expr(value, env, resolved);
                let ty = if let Some(te) = type_ann {
                    let mut local_tvars = HashMap::new();
                    let ann_ty = self.resolve_type_expr_with_vars(te, &mut local_tvars);
                    self.unify(&ann_ty, &val_ty, *span);
                    self.apply(&ann_ty)
                } else {
                    self.apply(&val_ty)
                };

                let binding = if *is_mut {
                    TypeScheme::monomorphic(ty)
                } else {
                    self.generalize(&ty, env)
                };

                env.insert(name.clone(), binding);
                self.record_type(*span, Type::Unit);
                Type::Unit
            }
            Expr::LetPattern(pattern, is_mut, type_ann, value, span) => {
                let val_ty = self.infer_expr(value, env, resolved);
                let ty = if let Some(te) = type_ann {
                    let mut local_tvars = HashMap::new();
                    let ann_ty = self.resolve_type_expr_with_vars(te, &mut local_tvars);
                    self.unify(&ann_ty, &val_ty, *span);
                    self.apply(&ann_ty)
                } else {
                    self.apply(&val_ty)
                };

                let mut pat_env: TypeEnv = HashMap::new();
                self.bind_pattern(pattern, &ty, &mut pat_env, *span);
                for (name, scheme) in pat_env {
                    let bound_ty = self.apply(&scheme.ty);
                    let binding = if *is_mut {
                        TypeScheme::monomorphic(bound_ty)
                    } else {
                        self.generalize(&bound_ty, env)
                    };
                    env.insert(name, binding);
                }

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
                let expected = self
                    .return_type_stack
                    .last()
                    .cloned()
                    .unwrap_or_else(|| self.fresh_var());
                if let Some(v) = val {
                    let vt = self.infer_expr(v, env, resolved);
                    self.unify(&expected, &vt, *span);
                } else {
                    self.unify(&expected, &Type::Unit, *span);
                }
                let result = self.apply(&expected);
                self.record_type(*span, result.clone());
                result
            }
            Expr::Break(span) | Expr::Continue(span) => {
                self.record_type(*span, Type::Unit);
                Type::Unit
            }
            Expr::Call(callee, args, span) => {
                self.infer_call(callee, args, *span, env, resolved)
            }
            Expr::MethodCall(receiver, method, args, span) => {
                self.infer_method_call(receiver, method, args, *span, env, resolved)
            }
            Expr::FieldAccess(receiver, field_name, span) => {
                self.infer_field_access(receiver, field_name, *span, env, resolved)
            }
            Expr::Lambda(params, _, body, span) => {
                let mut lambda_env = env.clone();
                let mut local_tvars = HashMap::new();
                let param_types: Vec<Type> = params
                    .iter()
                    .map(|p| {
                        let ty = match &p.ty {
                            Some(te) => self.resolve_type_expr_with_vars(te, &mut local_tvars),
                            None => self.fresh_var(),
                        };
                        lambda_env.insert(p.name.clone(), TypeScheme::monomorphic(ty.clone()));
                        ty
                    })
                    .collect();

                self.effect_usage_stack.push(HashSet::new());
                self.suspend_effect_checks += 1;
                let body_ty = self.infer_expr(body, &mut lambda_env, resolved);
                self.suspend_effect_checks = self.suspend_effect_checks.saturating_sub(1);
                let lambda_effects = self.effect_usage_stack.pop().unwrap_or_default();
                self.lambda_effects
                    .insert((span.start, span.end), lambda_effects);
                let fn_ty = Type::Function(param_types, Box::new(body_ty));
                self.record_type(*span, fn_ty.clone());
                fn_ty
            }
            Expr::StructLit(name, fields, span) => {
                let ty = self.check_struct_literal(name, fields, env, resolved, *span);
                self.record_type(*span, ty.clone());
                ty
            }
            Expr::With(base, fields, span) => {
                let base_ty = self.infer_expr(base, env, resolved);
                let result = self.check_with_expr(&base_ty, fields, env, resolved, *span);
                self.record_type(*span, result.clone());
                result
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
                let types: Vec<Type> = elems
                    .iter()
                    .map(|e| self.infer_expr(e, env, resolved))
                    .collect();
                let product = Type::Product(types);
                self.record_type(*span, product.clone());
                product
            }
            Expr::Try(inner, span) => {
                self.infer_try(inner, *span, env, resolved)
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

    fn infer_binary_op(
        &mut self,
        lhs: &Expr,
        op: &BinOp,
        rhs: &Expr,
        span: aura_common::Span,
        env: &mut TypeEnv,
        resolved: &ResolvedModule,
    ) -> Type {
        let lhs_ty = self.infer_expr(lhs, env, resolved);
        let rhs_ty = self.infer_expr(rhs, env, resolved);

        let result_ty = match op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                self.unify(&lhs_ty, &rhs_ty, span);
                let unified = self.apply(&lhs_ty);
                if !matches!(unified, Type::Var(_) | Type::Error) && !unified.is_numeric() {
                    let (concept, method) = match op {
                        BinOp::Add => ("Add", "add"),
                        BinOp::Sub => ("Sub", "sub"),
                        BinOp::Mul => ("Mul", "mul"),
                        BinOp::Div => ("Div", "div"),
                        BinOp::Mod => ("Rem", "rem"),
                        _ => unreachable!(),
                    };
                    if let Some(overloaded_ret) =
                        self.resolve_binary_concept_method(concept, method, &lhs_ty, &rhs_ty, span)
                    {
                        overloaded_ret
                    } else {
                        self.errors.push(TypeError {
                            message: format!(
                                "arithmetic operator expects numeric type or '{}' instance, got {}",
                                concept, unified
                            ),
                            span,
                        });
                        unified
                    }
                } else {
                    unified
                }
            }
            BinOp::Eq
            | BinOp::NotEq
            | BinOp::Lt
            | BinOp::Gt
            | BinOp::LtEq
            | BinOp::GtEq => {
                self.unify(&lhs_ty, &rhs_ty, span);
                let unified = self.apply(&lhs_ty);
                if matches!(op, BinOp::Eq | BinOp::NotEq) {
                    if !matches!(unified, Type::Var(_) | Type::Error)
                        && !self.type_implements_concept(&unified, "Eq")
                    {
                        self.errors.push(TypeError {
                            message: format!(
                                "comparison operator requires 'Eq', got {}",
                                unified
                            ),
                            span,
                        });
                    }
                } else if !matches!(unified, Type::Var(_) | Type::Error)
                    && !self.type_implements_concept(&unified, "Ord")
                {
                    self.errors.push(TypeError {
                        message: format!(
                            "ordering operator requires 'Ord', got {}",
                            unified
                        ),
                        span,
                    });
                }
                Type::Bool
            }
            BinOp::And | BinOp::Or => {
                self.unify(&lhs_ty, &Type::Bool, span);
                self.unify(&rhs_ty, &Type::Bool, span);
                Type::Bool
            }
        };

        self.record_type(span, result_ty.clone());
        result_ty
    }

    fn infer_pipeline(
        &mut self,
        lhs: &Expr,
        rhs: &Expr,
        span: aura_common::Span,
        env: &mut TypeEnv,
        resolved: &ResolvedModule,
    ) -> Type {
        let lhs_ty = self.infer_expr(lhs, env, resolved);

        let result = match rhs {
            Expr::Call(callee, args, _) => {
                let callee_ty = self.infer_expr(callee, env, resolved);
                let mut arg_types = Vec::with_capacity(args.len() + 1);
                arg_types.push(lhs_ty);
                for a in args {
                    arg_types.push(self.infer_expr(a, env, resolved));
                }
                let ret_ty = self.fresh_var();
                self.unify(
                    &callee_ty,
                    &Type::Function(arg_types, Box::new(ret_ty.clone())),
                    span,
                );

                if let Expr::Ident(_, cspan) = callee.as_ref() {
                    if let Some(def_id) =
                        resolved.references.get(&(cspan.start, cspan.end))
                    {
                        let mut full_args = Vec::with_capacity(args.len() + 1);
                        full_args.push((*lhs).clone());
                        full_args.extend(args.iter().cloned());
                        self.infer_call_required_effects(*def_id, &full_args, resolved, span);
                    }
                }
                self.apply(&ret_ty)
            }
            _ => {
                let rhs_ty = self.infer_expr(rhs, env, resolved);
                let ret_ty = self.fresh_var();
                self.unify(
                    &rhs_ty,
                    &Type::Function(vec![lhs_ty], Box::new(ret_ty.clone())),
                    span,
                );
                self.apply(&ret_ty)
            }
        };

        self.record_type(span, result.clone());
        result
    }

    fn infer_call(
        &mut self,
        callee: &Expr,
        args: &[Expr],
        span: aura_common::Span,
        env: &mut TypeEnv,
        resolved: &ResolvedModule,
    ) -> Type {
        if let Expr::Ident(name, _) = callee {
            if let Some(vinfo) = self.variant_info.get(name).cloned() {
                let result = self.check_variant_constructor_call(
                    name, &vinfo, args, env, resolved, span,
                );
                self.record_type(span, result.clone());
                return result;
            }
        }

        if let Expr::Ident(_, cspan) = callee {
            if let Some(def_id) = resolved.references.get(&(cspan.start, cspan.end)) {
                if let Some(scheme) = self.def_schemes.get(def_id).cloned() {
                    let (fn_ty, bounds) = self.instantiate_scheme_with_bounds(&scheme);
                    if let Type::Function(params, ret) = fn_ty {
                        if params.len() != args.len() {
                            self.errors.push(TypeError {
                                message: format!(
                                    "function expects {} argument(s), got {}",
                                    params.len(),
                                    args.len()
                                ),
                                span,
                            });
                        }

                        for (arg, param_ty) in args.iter().zip(params.iter()) {
                            let arg_ty = self.infer_expr(arg, env, resolved);
                            self.unify(&arg_ty, param_ty, arg.span());
                        }

                        self.check_instantiated_bounds(&bounds, span);
                        self.infer_call_required_effects(*def_id, args, resolved, span);
                        let result = self.apply(&ret);
                        self.record_type(span, result.clone());
                        return result;
                    }
                }
            }
        }

        let callee_ty = self.infer_expr(callee, env, resolved);
        let arg_types: Vec<Type> = args
            .iter()
            .map(|a| self.infer_expr(a, env, resolved))
            .collect();

        let ret_ty = self.fresh_var();
        self.unify(
            &callee_ty,
            &Type::Function(arg_types, Box::new(ret_ty.clone())),
            span,
        );

        let result = self.apply(&ret_ty);
        self.record_type(span, result.clone());
        result
    }

    fn infer_method_call(
        &mut self,
        receiver: &Expr,
        method: &str,
        args: &[Expr],
        span: aura_common::Span,
        env: &mut TypeEnv,
        resolved: &ResolvedModule,
    ) -> Type {
        // Explicit concept disambiguation: Concept.method(value, ...)
        if let Expr::Ident(concept_name, recv_span) = receiver {
            if let Some(id) = resolved.references.get(&(recv_span.start, recv_span.end)) {
                if let Some(info) = resolved.defs.get(id) {
                    if matches!(info.kind, DefKind::Concept) {
                        return self.infer_concept_dispatch_call(concept_name, method, args, span, env, resolved);
                    }
                }
            }
        }

        // Associated function call: TypeName.method(args)
        if let Expr::Ident(type_name, recv_span) = receiver {
            if let Some(id) = resolved.references.get(&(recv_span.start, recv_span.end)) {
                if let Some(info) = resolved.defs.get(id) {
                    if matches!(info.kind, DefKind::Type) {
                        return self.infer_associated_function_call(type_name, method, args, span, env, resolved);
                    }
                }
            }
        }

        let recv_ty = self.infer_expr(receiver, env, resolved);
        let recv_ty = self.apply(&recv_ty);
        if let Some(recv_name) = self.type_name_key(&recv_ty) {
            // Try inherent methods first
            if let Some(result) = self.try_infer_inherent_method_call(&recv_ty, &recv_name, method, args, span, env, resolved) {
                return result;
            }

            // Try concept methods
            if let Some(result) = self.try_infer_concept_method_call(&recv_ty, &recv_name, method, args, span, env, resolved) {
                return result;
            }

            self.errors.push(TypeError {
                message: format!("unknown method '{}' for type '{}'", method, recv_name),
                span,
            });
            self.record_type(span, Type::Error);
            Type::Error
        } else if let Type::Var(v) = recv_ty {
            self.infer_type_var_method_call(v, method, args, span, env, resolved)
        } else {
            self.errors.push(TypeError {
                message: format!("type '{}' has no methods", recv_ty),
                span,
            });
            self.record_type(span, Type::Error);
            Type::Error
        }
    }

    fn infer_concept_dispatch_call(
        &mut self,
        concept_name: &str,
        method: &str,
        args: &[Expr],
        span: aura_common::Span,
        env: &mut TypeEnv,
        resolved: &ResolvedModule,
    ) -> Type {
        if args.is_empty() {
            self.errors.push(TypeError {
                message: format!(
                    "explicit concept method '{}.{}' requires a receiver argument",
                    concept_name, method
                ),
                span,
            });
            self.record_type(span, Type::Error);
            return Type::Error;
        }

        let recv_arg_ty = self.infer_expr(&args[0], env, resolved);
        let scheme = if let Some(recv_name) = self.type_name_key(&recv_arg_ty) {
            self.concept_instances
                .get(&(concept_name.to_string(), recv_name))
                .and_then(|inst| inst.methods.get(method))
                .cloned()
        } else if let Type::Var(v) = self.apply(&recv_arg_ty) {
            if self.has_assumed_bound(v, concept_name) {
                self.concepts.get(concept_name).cloned().and_then(|cinfo| {
                    cinfo.methods.get(method).map(|sig| {
                        let mut assoc_map = HashMap::new();
                        for assoc in &cinfo.assoc_types {
                            assoc_map.insert(assoc.clone(), self.fresh_var());
                        }
                        self.instantiate_concept_method_for_type(
                            &cinfo,
                            sig,
                            &Type::Var(v),
                            &assoc_map,
                        )
                    })
                })
            } else {
                None
            }
        } else {
            None
        };

        let Some(scheme) = scheme else {
            self.errors.push(TypeError {
                message: format!(
                    "type '{}' does not implement concept '{}' method '{}'",
                    self.apply(&recv_arg_ty),
                    concept_name,
                    method
                ),
                span,
            });
            self.record_type(span, Type::Error);
            return Type::Error;
        };

        let (fn_ty, bounds) = self.instantiate_scheme_with_bounds(&scheme);
        if let Type::Function(params, ret) = fn_ty {
            if params.len() != args.len() {
                self.errors.push(TypeError {
                    message: format!(
                        "concept method '{}.{}' expects {} argument(s), got {}",
                        concept_name,
                        method,
                        params.len(),
                        args.len()
                    ),
                    span,
                });
            }
            for (arg, expected) in args.iter().zip(params.iter()) {
                let arg_ty = self.infer_expr(arg, env, resolved);
                self.unify(&arg_ty, expected, arg.span());
            }
            self.check_instantiated_bounds(&bounds, span);
            let result = self.apply(&ret);
            self.record_type(span, result.clone());
            return result;
        }

        self.record_type(span, Type::Error);
        Type::Error
    }

    fn infer_associated_function_call(
        &mut self,
        type_name: &str,
        method: &str,
        args: &[Expr],
        span: aura_common::Span,
        env: &mut TypeEnv,
        resolved: &ResolvedModule,
    ) -> Type {
        if let Some(methods) = self.associated_functions.get(type_name) {
            if let Some(scheme) = methods.get(method).cloned() {
                let (fn_ty, bounds) =
                    self.instantiate_scheme_with_bounds(&scheme);
                if let Type::Function(params, ret) = fn_ty {
                    if params.len() != args.len() {
                        self.errors.push(TypeError {
                            message: format!(
                                "associated function '{}.{}' expects {} argument(s), got {}",
                                type_name,
                                method,
                                params.len(),
                                args.len()
                            ),
                            span,
                        });
                    }
                    for (arg, expected) in args.iter().zip(params.iter()) {
                        let arg_ty = self.infer_expr(arg, env, resolved);
                        self.unify(&arg_ty, expected, arg.span());
                    }

                    if method == "new" && args.len() == 1 {
                        if let Some(info) = self.refined_info.get(type_name) {
                            if let Some(ok) = self.check_refined_constraint_on_literal(
                                type_name,
                                &args[0],
                                info,
                            ) {
                                if !ok {
                                    self.errors.push(TypeError {
                                        message: format!(
                                            "refined constructor '{}.new' failed constraint",
                                            type_name
                                        ),
                                        span: args[0].span(),
                                    });
                                }
                            }
                        }
                    }
                    self.check_instantiated_bounds(&bounds, span);
                    let result = self.apply(&ret);
                    self.record_type(span, result.clone());
                    return result;
                }
            }
        }
        self.errors.push(TypeError {
            message: format!(
                "unknown associated function '{}.{}'",
                type_name, method
            ),
            span,
        });
        self.record_type(span, Type::Error);
        Type::Error
    }

    fn try_infer_inherent_method_call(
        &mut self,
        recv_ty: &Type,
        recv_name: &str,
        method: &str,
        args: &[Expr],
        span: aura_common::Span,
        env: &mut TypeEnv,
        resolved: &ResolvedModule,
    ) -> Option<Type> {
        let methods = self.inherent_methods.get(recv_name)?;
        let scheme = methods.get(method).cloned()?;
        let (fn_ty, bounds) = self.instantiate_scheme_with_bounds(&scheme);
        if let Type::Function(params, ret) = fn_ty {
            if params.is_empty() {
                self.errors.push(TypeError {
                    message: format!(
                        "method '{}' on '{}' has no receiver parameter",
                        method, recv_name
                    ),
                    span,
                });
                self.record_type(span, Type::Error);
                return Some(Type::Error);
            }
            self.unify(recv_ty, &params[0], span);
            if params.len() - 1 != args.len() {
                self.errors.push(TypeError {
                    message: format!(
                        "method '{}.{}' expects {} argument(s), got {}",
                        recv_name,
                        method,
                        params.len() - 1,
                        args.len()
                    ),
                    span,
                });
            }
            for (arg, expected) in args.iter().zip(params.iter().skip(1)) {
                let arg_ty = self.infer_expr(arg, env, resolved);
                self.unify(&arg_ty, expected, arg.span());
            }
            self.check_instantiated_bounds(&bounds, span);
            let result = self.apply(&ret);
            self.record_type(span, result.clone());
            Some(result)
        } else {
            None
        }
    }

    fn try_infer_concept_method_call(
        &mut self,
        recv_ty: &Type,
        recv_name: &str,
        method: &str,
        args: &[Expr],
        span: aura_common::Span,
        env: &mut TypeEnv,
        resolved: &ResolvedModule,
    ) -> Option<Type> {
        let mut concept_candidates: Vec<(String, super::TypeScheme)> = Vec::new();
        for inst in self.concept_instances.values() {
            if inst.target_name == recv_name {
                if let Some(s) = inst.methods.get(method) {
                    concept_candidates.push((inst.concept.clone(), s.clone()));
                }
            }
        }

        if concept_candidates.len() > 1 {
            let names = concept_candidates
                .iter()
                .map(|(c, _)| c.clone())
                .collect::<Vec<_>>()
                .join(", ");
            self.errors.push(TypeError {
                message: format!(
                    "ambiguous method '{}': multiple concept candidates ({})",
                    method, names
                ),
                span,
            });
            self.record_type(span, Type::Error);
            return Some(Type::Error);
        }

        let (_, scheme) = concept_candidates.into_iter().next()?;
        let (fn_ty, bounds) = self.instantiate_scheme_with_bounds(&scheme);
        if let Type::Function(params, ret) = fn_ty {
            if params.is_empty() {
                self.errors.push(TypeError {
                    message: format!(
                        "concept method '{}' on '{}' has no receiver parameter",
                        method, recv_name
                    ),
                    span,
                });
                self.record_type(span, Type::Error);
                return Some(Type::Error);
            }
            self.unify(recv_ty, &params[0], span);
            if params.len() - 1 != args.len() {
                self.errors.push(TypeError {
                    message: format!(
                        "method '{}.{}' expects {} argument(s), got {}",
                        recv_name,
                        method,
                        params.len() - 1,
                        args.len()
                    ),
                    span,
                });
            }
            for (arg, expected) in args.iter().zip(params.iter().skip(1)) {
                let arg_ty = self.infer_expr(arg, env, resolved);
                self.unify(&arg_ty, expected, arg.span());
            }
            self.check_instantiated_bounds(&bounds, span);
            let result = self.apply(&ret);
            self.record_type(span, result.clone());
            Some(result)
        } else {
            None
        }
    }

    fn infer_type_var_method_call(
        &mut self,
        v: crate::types::TypeVarId,
        method: &str,
        args: &[Expr],
        span: aura_common::Span,
        env: &mut TypeEnv,
        resolved: &ResolvedModule,
    ) -> Type {
        let assumed_concepts = self.assumed_concepts_for_var(v);
        let mut candidates: Vec<(String, super::TypeScheme)> = Vec::new();
        for concept_name in assumed_concepts {
            if let Some(cinfo) = self.concepts.get(&concept_name).cloned() {
                if let Some(sig) = cinfo.methods.get(method) {
                    let mut assoc_map = HashMap::new();
                    for assoc in &cinfo.assoc_types {
                        assoc_map.insert(assoc.clone(), self.fresh_var());
                    }
                    let specialized = self.instantiate_concept_method_for_type(
                        &cinfo,
                        sig,
                        &Type::Var(v),
                        &assoc_map,
                    );
                    candidates.push((concept_name.clone(), specialized));
                }
            }
        }

        if candidates.len() > 1 {
            let names = candidates
                .iter()
                .map(|(c, _)| c.clone())
                .collect::<Vec<_>>()
                .join(", ");
            self.errors.push(TypeError {
                message: format!(
                    "ambiguous method '{}': bounded by multiple concepts ({})",
                    method, names
                ),
                span,
            });
            self.record_type(span, Type::Error);
            return Type::Error;
        }

        if let Some((_, scheme)) = candidates.into_iter().next() {
            let (fn_ty, bounds) = self.instantiate_scheme_with_bounds(&scheme);
            if let Type::Function(params, ret) = fn_ty {
                if params.is_empty() {
                    self.record_type(span, Type::Error);
                    return Type::Error;
                }
                self.unify(&Type::Var(v), &params[0], span);
                if params.len() - 1 != args.len() {
                    self.errors.push(TypeError {
                        message: format!(
                            "method '{}' expects {} argument(s), got {}",
                            method,
                            params.len() - 1,
                            args.len()
                        ),
                        span,
                    });
                }
                for (arg, expected) in args.iter().zip(params.iter().skip(1)) {
                    let arg_ty = self.infer_expr(arg, env, resolved);
                    self.unify(&arg_ty, expected, arg.span());
                }
                self.check_instantiated_bounds(&bounds, span);
                let result = self.apply(&ret);
                self.record_type(span, result.clone());
                return result;
            }
        }

        self.errors.push(TypeError {
            message: format!("unknown method '{}' for type variable {}", method, v.0),
            span,
        });
        self.record_type(span, Type::Error);
        Type::Error
    }

    fn infer_field_access(
        &mut self,
        receiver: &Expr,
        field_name: &str,
        span: aura_common::Span,
        env: &mut TypeEnv,
        resolved: &ResolvedModule,
    ) -> Type {
        let recv_ty = self.infer_expr(receiver, env, resolved);
        let recv_ty = self.apply(&recv_ty);

        let ty = match recv_ty {
            Type::Named(name, args) => {
                if let Some(fields) = self.instantiate_struct_fields_for_args(&name, &args)
                {
                    if let Some((_, ft)) =
                        fields.iter().find(|(fname, _)| fname == field_name)
                    {
                        ft.clone()
                    } else {
                        self.errors.push(TypeError {
                            message: format!(
                                "type '{}' has no field '{}'",
                                name, field_name
                            ),
                            span,
                        });
                        Type::Error
                    }
                } else {
                    if field_name == "value" {
                        if let Some(base) =
                            self.instantiate_refined_base_for_args(&name, &args)
                        {
                            base
                        } else {
                            self.errors.push(TypeError {
                                message: format!(
                                    "type '{}' does not support field access",
                                    name
                                ),
                                span,
                            });
                            Type::Error
                        }
                    } else {
                        self.errors.push(TypeError {
                            message: format!("type '{}' does not support field access", name),
                            span,
                        });
                        Type::Error
                    }
                }
            }
            other => {
                self.errors.push(TypeError {
                    message: format!(
                        "cannot access field '{}' on type {}",
                        field_name, other
                    ),
                    span,
                });
                Type::Error
            }
        };

        self.record_type(span, ty.clone());
        ty
    }

    fn infer_try(
        &mut self,
        inner: &Expr,
        span: aura_common::Span,
        env: &mut TypeEnv,
        resolved: &ResolvedModule,
    ) -> Type {
        let inner_ty = self.infer_expr(inner, env, resolved);
        let inner_ty = self.apply(&inner_ty);
        let expected_ret = self
            .return_type_stack
            .last()
            .cloned()
            .unwrap_or(Type::Error);

        let result = match inner_ty {
            Type::Named(name, args) if name == "Result" && args.len() == 2 => {
                let ok_ty = args[0].clone();
                let src_err = args[1].clone();
                match self.apply(&expected_ret) {
                    Type::Named(rname, rargs) if rname == "Result" && rargs.len() == 2 => {
                        let dst_err = rargs[1].clone();
                        if !self.can_convert_error_type(&src_err, &dst_err) {
                            self.errors.push(TypeError {
                                message: format!(
                                    "cannot use '?': no conversion from '{}' to '{}'",
                                    src_err, dst_err
                                ),
                                span,
                            });
                        }
                    }
                    other => {
                        self.errors.push(TypeError {
                            message: format!(
                                "cannot use '?' on Result in function returning {}",
                                other
                            ),
                            span,
                        });
                    }
                }
                ok_ty
            }
            Type::Named(name, args) if name == "Option" && args.len() == 1 => {
                match self.apply(&expected_ret) {
                    Type::Named(rname, rargs) if rname == "Option" && rargs.len() == 1 => {}
                    other => {
                        self.errors.push(TypeError {
                            message: format!(
                                "cannot use '?' on Option in function returning {}",
                                other
                            ),
                            span,
                        });
                    }
                }
                args[0].clone()
            }
            other => {
                self.errors.push(TypeError {
                    message: format!("'?' expects Result or Option, got {}", other),
                    span,
                });
                Type::Error
            }
        };

        self.record_type(span, result.clone());
        result
    }
}
