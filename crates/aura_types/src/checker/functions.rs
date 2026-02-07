use std::collections::{HashMap, HashSet};

use aura_common::Span;
use aura_parser::ast::*;
use aura_resolve::{DefKind, ResolvedModule};

use crate::types::Type;

use super::{
    ConceptBound, ConceptInfo, EffectContext, EffectSpec, FunctionEffectScheme, TypeChecker,
    TypeEnv, TypeError, TypeScheme,
};

impl TypeChecker {
    pub(crate) fn method_scheme_in_context(
        &mut self,
        method: &MethodDef,
        self_ty: &Type,
        assoc_map: &HashMap<String, Type>,
    ) -> TypeScheme {
        let mut tvars = HashMap::new();
        let param_types: Vec<Type> = method
            .params
            .iter()
            .map(|p| {
                if p.name == "self" && p.ty.is_none() {
                    self_ty.clone()
                } else {
                    let base = match &p.ty {
                        Some(te) => self.resolve_type_expr_with_vars(te, &mut tvars),
                        None => self.fresh_var(),
                    };
                    self.replace_special_type_names(base, self_ty, assoc_map)
                }
            })
            .collect();
        let ret_type = match &method.return_type {
            Some(te) => {
                let base = self.resolve_type_expr_with_vars(te, &mut tvars);
                self.replace_special_type_names(base, self_ty, assoc_map)
            }
            None => self.fresh_var(),
        };

        let fn_ty = Type::Function(param_types, Box::new(ret_type));
        let quantified = self.free_type_vars(&fn_ty).into_iter().collect();
        TypeScheme {
            quantified,
            bounds: Vec::new(),
            ty: self.apply(&fn_ty),
        }
    }

    pub(crate) fn instantiate_concept_method_for_type(
        &self,
        concept: &ConceptInfo,
        method_sig: &TypeScheme,
        target_ty: &Type,
        assoc_map: &HashMap<String, Type>,
    ) -> TypeScheme {
        let mut subst = HashMap::new();
        subst.insert(concept.self_var, target_ty.clone());
        for (name, id) in &concept.assoc_var_ids {
            if let Some(assoc_ty) = assoc_map.get(name) {
                subst.insert(*id, assoc_ty.clone());
            }
        }

        let ty = self.substitute_type(&method_sig.ty, &subst);
        TypeScheme {
            quantified: method_sig.quantified.clone(),
            bounds: method_sig.bounds.clone(),
            ty,
        }
    }

    pub(crate) fn replace_special_type_names(
        &self,
        ty: Type,
        self_ty: &Type,
        assoc_map: &HashMap<String, Type>,
    ) -> Type {
        match ty {
            Type::Named(name, args) => {
                if name == "Self" {
                    self_ty.clone()
                } else if let Some(suffix) = name.strip_prefix("Self.") {
                    assoc_map.get(suffix).cloned().unwrap_or(Type::Error)
                } else if let Some(assoc_ty) = assoc_map.get(&name) {
                    assoc_ty.clone()
                } else {
                    Type::Named(
                        name,
                        args.into_iter()
                            .map(|a| self.replace_special_type_names(a, self_ty, assoc_map))
                            .collect(),
                    )
                }
            }
            Type::Function(params, ret) => Type::Function(
                params
                    .into_iter()
                    .map(|p| self.replace_special_type_names(p, self_ty, assoc_map))
                    .collect(),
                Box::new(self.replace_special_type_names(*ret, self_ty, assoc_map)),
            ),
            Type::Product(types) => Type::Product(
                types
                    .into_iter()
                    .map(|t| self.replace_special_type_names(t, self_ty, assoc_map))
                    .collect(),
            ),
            other => other,
        }
    }

    pub(crate) fn type_name_key(&self, ty: &Type) -> Option<String> {
        match self.apply(ty) {
            Type::Named(name, _) => Some(name),
            Type::Int => Some("Int".into()),
            Type::Int8 => Some("Int8".into()),
            Type::Int16 => Some("Int16".into()),
            Type::Int32 => Some("Int32".into()),
            Type::Int64 => Some("Int64".into()),
            Type::UInt => Some("UInt".into()),
            Type::UInt8 => Some("UInt8".into()),
            Type::UInt16 => Some("UInt16".into()),
            Type::UInt32 => Some("UInt32".into()),
            Type::UInt64 => Some("UInt64".into()),
            Type::Float32 => Some("Float32".into()),
            Type::Float64 => Some("Float64".into()),
            Type::Decimal => Some("Decimal".into()),
            Type::BigDecimal => Some("BigDecimal".into()),
            Type::Bool => Some("Bool".into()),
            Type::Char => Some("Char".into()),
            Type::String => Some("String".into()),
            Type::Unit => Some("Unit".into()),
            _ => None,
        }
    }

    pub(crate) fn resolve_binary_concept_method(
        &mut self,
        concept: &str,
        method: &str,
        lhs_ty: &Type,
        rhs_ty: &Type,
        span: Span,
    ) -> Option<Type> {
        let lhs_name = self.type_name_key(lhs_ty)?;
        let scheme = self
            .concept_instances
            .get(&(concept.to_string(), lhs_name))
            .and_then(|inst| inst.methods.get(method))
            .cloned()?;

        let (fn_ty, bounds) = self.instantiate_scheme_with_bounds(&scheme);
        if let Type::Function(params, ret) = fn_ty {
            if params.len() >= 2 {
                self.unify(lhs_ty, &params[0], span);
                self.unify(rhs_ty, &params[1], span);
                self.check_instantiated_bounds(&bounds, span);
                return Some(self.apply(&ret));
            }
        }

        None
    }

    pub(crate) fn function_scheme(
        &mut self,
        f: &FnDef,
        annotation: Option<&TypeExpr>,
    ) -> TypeScheme {
        let mut tvars = HashMap::new();
        let mut ann_bounds = Vec::new();

        let declared_params: Vec<Type> = f
            .params
            .iter()
            .map(|p| match &p.ty {
                Some(te) => self.resolve_type_expr_with_vars(te, &mut tvars),
                None => self.fresh_var(),
            })
            .collect();

        let declared_ret = match &f.return_type {
            Some(te) => self.resolve_type_expr_with_vars(te, &mut tvars),
            None => self.fresh_var(),
        };

        let mut fn_ty = Type::Function(declared_params, Box::new(declared_ret));

        if let Some(ann) = annotation {
            let ann_ty = if let TypeExpr::Forall(bounds, body, _) = ann {
                for b in bounds {
                    tvars
                        .entry(b.ty_var.clone())
                        .or_insert_with(|| self.fresh_var());
                }
                for b in bounds {
                    if let Some(Type::Var(v)) = tvars.get(&b.ty_var) {
                        ann_bounds.push(ConceptBound {
                            concept: b.concept.clone(),
                            type_var: *v,
                        });
                    }
                }
                self.resolve_type_expr_with_vars(body, &mut tvars)
            } else {
                self.resolve_type_expr_with_vars(ann, &mut tvars)
            };

            if let Type::Function(ann_params, ann_ret) = ann_ty {
                if let Type::Function(decl_params, decl_ret) = &fn_ty {
                    if ann_params.len() != decl_params.len() {
                        self.errors.push(TypeError {
                            message: format!(
                                "annotation for '{}' has {} params but definition has {}",
                                f.name,
                                ann_params.len(),
                                decl_params.len()
                            ),
                            span: f.span,
                        });
                    }
                    for (ap, dp) in ann_params.iter().zip(decl_params.iter()) {
                        self.unify(ap, dp, f.span);
                    }
                    self.unify(&ann_ret, decl_ret, f.span);
                }
                fn_ty = Type::Function(ann_params, ann_ret);
            } else {
                self.errors.push(TypeError {
                    message: format!("annotation for '{}' must be a function type", f.name),
                    span: ann.span(),
                });
            }
        }

        let quantified = self
            .free_type_vars(&self.apply(&fn_ty))
            .into_iter()
            .collect();
        let mut normalized_bounds = Vec::new();
        for b in ann_bounds {
            if let Type::Var(v) = self.apply(&Type::Var(b.type_var)) {
                normalized_bounds.push(ConceptBound {
                    concept: b.concept,
                    type_var: v,
                });
            }
        }

        TypeScheme {
            quantified,
            bounds: normalized_bounds,
            ty: self.apply(&fn_ty),
        }
    }

    pub(crate) fn function_effect_scheme(
        &mut self,
        f: &FnDef,
        annotation: Option<&TypeExpr>,
    ) -> FunctionEffectScheme {
        let declared = if !f.effects.is_empty() {
            self.effect_spec_from_refs(&f.effects)
        } else if let Some(TypeExpr::Function(_, _, effects, _)) = annotation {
            effects
                .as_ref()
                .map(|e| self.effect_spec_from_refs(e))
                .unwrap_or_default()
        } else if let Some(TypeExpr::Forall(_, body, _)) = annotation {
            if let TypeExpr::Function(_, _, effects, _) = body.as_ref() {
                effects
                    .as_ref()
                    .map(|e| self.effect_spec_from_refs(e))
                    .unwrap_or_default()
            } else {
                EffectSpec::default()
            }
        } else {
            EffectSpec::default()
        };

        let ann_param_types: Vec<TypeExpr> = match annotation {
            Some(TypeExpr::Function(params, _, _, _)) => params.clone(),
            Some(TypeExpr::Forall(_, body, _)) => {
                if let TypeExpr::Function(params, _, _, _) = body.as_ref() {
                    params.clone()
                } else {
                    Vec::new()
                }
            }
            _ => Vec::new(),
        };

        let mut param_effects = Vec::new();
        for (i, p) in f.params.iter().enumerate() {
            let src = p.ty.as_ref().or_else(|| ann_param_types.get(i));
            let spec = match src {
                Some(TypeExpr::Function(_, _, effects, _)) => {
                    effects.as_ref().map(|e| self.effect_spec_from_refs(e))
                }
                _ => None,
            };
            param_effects.push(spec);
        }

        FunctionEffectScheme {
            declared,
            param_effects,
        }
    }

    pub(crate) fn check_function(&mut self, f: &FnDef, resolved: &ResolvedModule) {
        let Some(func_id) = self.lookup_function_def_id(&f.name, resolved) else {
            return;
        };

        let Some(sig_scheme) = self.def_schemes.get(&func_id).cloned() else {
            return;
        };

        let (instantiated, inst_bounds) = self.instantiate_scheme_with_bounds(&sig_scheme);
        let Type::Function(param_types, ret_ty) = instantiated else {
            self.errors.push(TypeError {
                message: format!("'{}' does not have a function type", f.name),
                span: f.span,
            });
            return;
        };

        let effect_scheme =
            self.fn_effects
                .get(&func_id)
                .cloned()
                .unwrap_or(FunctionEffectScheme {
                    declared: EffectSpec::default(),
                    param_effects: vec![None; f.params.len()],
                });
        let allowed_effects = Self::effect_concretes(&effect_scheme.declared);
        let effect_vars = Self::effect_vars(&effect_scheme.declared);
        self.effect_context_stack.push(EffectContext {
            allowed: allowed_effects.clone(),
            vars: effect_vars.clone(),
        });
        self.effect_usage_stack.push(HashSet::new());
        self.async_context_stack.push(f.is_async);

        let assumptions = self.bounds_to_assumptions(&inst_bounds);
        self.bound_assumptions.push(assumptions);

        let mut env: TypeEnv = HashMap::new();
        for (param, pty) in f.params.iter().zip(param_types.iter()) {
            env.insert(param.name.clone(), TypeScheme::monomorphic(pty.clone()));

            for (id, info) in &resolved.defs {
                if info.name == param.name
                    && matches!(info.kind, DefKind::Parameter)
                    && info.span == param.span
                {
                    self.def_types.insert(*id, pty.clone());
                    break;
                }
            }
        }

        for req in &f.requires {
            let req_ty = self.infer_expr(req, &mut env, resolved);
            self.unify(&req_ty, &Type::Bool, req.span());
        }

        self.return_type_stack.push((*ret_ty).clone());
        let actual_ret = self.infer_expr(&f.body, &mut env, resolved);
        self.return_type_stack.pop();
        self.bound_assumptions.pop();

        self.unify(&ret_ty, &actual_ret, f.body.span());

        if !f.ensures.is_empty() {
            let mut ensures_env = env.clone();
            ensures_env.insert(
                "result".into(),
                TypeScheme::monomorphic(self.apply(&ret_ty)),
            );
            for ens in &f.ensures {
                let ens_ty = self.infer_expr(ens, &mut ensures_env, resolved);
                self.unify(&ens_ty, &Type::Bool, ens.span());
            }
        }

        let used_effects = self.effect_usage_stack.pop().unwrap_or_default();
        self.effect_context_stack.pop();
        self.async_context_stack.pop();
        if effect_vars.is_empty() {
            let allowed = self.with_implied_effects(&allowed_effects);
            let missing = used_effects
                .difference(&allowed)
                .copied()
                .collect::<HashSet<_>>();
            if !missing.is_empty() {
                self.errors.push(TypeError {
                    message: format!(
                        "function '{}' declares {} but uses {}",
                        f.name,
                        self.effect_set_to_string(&allowed),
                        self.effect_set_to_string(&used_effects)
                    ),
                    span: f.span,
                });
            }
        }
    }
}
