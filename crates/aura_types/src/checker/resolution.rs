use std::collections::{HashMap, HashSet};

use aura_parser::ast::*;

use crate::types::{Type, TypeVarId};

use super::{TypeChecker, TypeError};

impl TypeChecker {
    pub(crate) fn resolve_type_expr_with_vars(
        &mut self,
        te: &TypeExpr,
        tvars: &mut HashMap<String, Type>,
    ) -> Type {
        match te {
            TypeExpr::Named(name, span) => {
                if let Some(ty) = tvars.get(name) {
                    return ty.clone();
                }

                if let Some(ty) = Type::from_name(name) {
                    return ty;
                }

                if Self::is_type_var_name(name) {
                    let v = self.fresh_var();
                    tvars.insert(name.clone(), v.clone());
                    return v;
                }

                // Check for type alias
                if let Some((params, alias_ty)) = self.type_aliases.get(name).cloned() {
                    if params.is_empty() {
                        return alias_ty;
                    } else {
                        self.errors.push(TypeError {
                            message: format!(
                                "type alias '{}' expects {} argument(s), got 0",
                                name,
                                params.len()
                            ),
                            span: *span,
                        });
                        return Type::Error;
                    }
                }

                if let Some(arity) = self.type_arity.get(name) {
                    if *arity > 0 {
                        self.errors.push(TypeError {
                            message: format!(
                                "type '{}' expects {} argument(s), got 0",
                                name, arity
                            ),
                            span: *span,
                        });
                    }
                }

                Type::Named(name.clone(), Vec::new())
            }
            TypeExpr::App(base, args, span) => {
                let base_name = match base.as_ref() {
                    TypeExpr::Named(name, _) => name.clone(),
                    _ => {
                        self.errors.push(TypeError {
                            message: "type application base must be a named type".into(),
                            span: *span,
                        });
                        return Type::Error;
                    }
                };

                let arg_types: Vec<Type> = args
                    .iter()
                    .map(|a| self.resolve_type_expr_with_vars(a, tvars))
                    .collect();

                if Type::from_name(&base_name).is_some() {
                    self.errors.push(TypeError {
                        message: format!("primitive type '{}' cannot be applied", base_name),
                        span: *span,
                    });
                }

                // Check for type alias with arguments
                if let Some((params, alias_ty)) = self.type_aliases.get(&base_name).cloned() {
                    if params.len() != arg_types.len() {
                        self.errors.push(TypeError {
                            message: format!(
                                "type alias '{}' expects {} argument(s), got {}",
                                base_name,
                                params.len(),
                                arg_types.len()
                            ),
                            span: *span,
                        });
                        return Type::Error;
                    }
                    // Substitute type parameters in the alias body
                    return self.substitute_alias_params(&alias_ty, &params, &arg_types);
                }

                if let Some(expected_arity) = self.type_arity.get(&base_name) {
                    if *expected_arity != arg_types.len() {
                        self.errors.push(TypeError {
                            message: format!(
                                "type '{}' expects {} argument(s), got {}",
                                base_name,
                                expected_arity,
                                arg_types.len()
                            ),
                            span: *span,
                        });
                    }
                }

                Type::Named(base_name, arg_types)
            }
            TypeExpr::Product(types, _) => {
                let ts: Vec<Type> = types
                    .iter()
                    .map(|t| self.resolve_type_expr_with_vars(t, tvars))
                    .collect();
                Type::Product(ts)
            }
            TypeExpr::Function(params, ret, _, _) => {
                let param_types: Vec<Type> = params
                    .iter()
                    .map(|p| self.resolve_type_expr_with_vars(p, tvars))
                    .collect();
                let ret_type = self.resolve_type_expr_with_vars(ret, tvars);
                Type::Function(param_types, Box::new(ret_type))
            }
            TypeExpr::Forall(bounds, body, _) => {
                let mut scoped = tvars.clone();
                for b in bounds {
                    scoped
                        .entry(b.ty_var.clone())
                        .or_insert_with(|| self.fresh_var());
                }
                self.resolve_type_expr_with_vars(body, &mut scoped)
            }
            TypeExpr::Unit(_) => Type::Unit,
        }
    }

    pub(crate) fn is_type_var_name(name: &str) -> bool {
        name.chars()
            .next()
            .map(|c| c.is_ascii_lowercase())
            .unwrap_or(false)
    }

    /// Substitute type parameters in an alias body with concrete type arguments.
    /// The alias body was resolved with fresh type vars for each param; this
    /// method replaces those vars with the supplied arguments.
    pub(crate) fn substitute_alias_params(
        &self,
        alias_ty: &Type,
        _params: &[String],
        args: &[Type],
    ) -> Type {
        // During collect_type_def for Alias, each param was mapped to a fresh Var.
        // We need to find those vars in alias_ty and replace them with args.
        // Collect the type vars that appear in the alias type positionally.
        let vars = self.collect_alias_vars(alias_ty);
        if vars.len() != args.len() {
            return alias_ty.clone();
        }
        let mut subst: HashMap<TypeVarId, Type> = HashMap::new();
        for (var_id, arg) in vars.iter().zip(args.iter()) {
            subst.insert(*var_id, arg.clone());
        }
        self.apply_subst(alias_ty, &subst)
    }

    /// Collect unique type variable IDs from a type in order of first appearance.
    pub(crate) fn collect_alias_vars(&self, ty: &Type) -> Vec<TypeVarId> {
        let mut vars = Vec::new();
        let mut seen = HashSet::new();
        self.collect_alias_vars_inner(ty, &mut vars, &mut seen);
        vars
    }

    fn collect_alias_vars_inner(
        &self,
        ty: &Type,
        vars: &mut Vec<TypeVarId>,
        seen: &mut HashSet<TypeVarId>,
    ) {
        match ty {
            Type::Var(id) => {
                let resolved = self.substitution.get(id);
                if let Some(resolved_ty) = resolved {
                    self.collect_alias_vars_inner(resolved_ty, vars, seen);
                } else if seen.insert(*id) {
                    vars.push(*id);
                }
            }
            Type::Named(_, args) => {
                for a in args {
                    self.collect_alias_vars_inner(a, vars, seen);
                }
            }
            Type::Product(ts) => {
                for t in ts {
                    self.collect_alias_vars_inner(t, vars, seen);
                }
            }
            Type::Function(params, ret) => {
                for p in params {
                    self.collect_alias_vars_inner(p, vars, seen);
                }
                self.collect_alias_vars_inner(ret, vars, seen);
            }
            _ => {}
        }
    }
}
