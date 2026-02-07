use std::collections::{HashMap, HashSet};

use aura_common::Span;

use crate::types::{Type, TypeVarId};

use super::{InstantiatedBound, TypeChecker, TypeEnv, TypeError, TypeScheme};

impl TypeChecker {
    /// Apply a specific substitution map (not the global one) to a type.
    pub(crate) fn apply_subst(&self, ty: &Type, subst: &HashMap<TypeVarId, Type>) -> Type {
        match ty {
            Type::Var(id) => {
                if let Some(replacement) = subst.get(id) {
                    replacement.clone()
                } else if let Some(resolved) = self.substitution.get(id) {
                    self.apply_subst(resolved, subst)
                } else {
                    ty.clone()
                }
            }
            Type::Named(name, args) => {
                let new_args = args.iter().map(|a| self.apply_subst(a, subst)).collect();
                Type::Named(name.clone(), new_args)
            }
            Type::Product(ts) => {
                Type::Product(ts.iter().map(|t| self.apply_subst(t, subst)).collect())
            }
            Type::Function(params, ret) => Type::Function(
                params.iter().map(|p| self.apply_subst(p, subst)).collect(),
                Box::new(self.apply_subst(ret, subst)),
            ),
            _ => ty.clone(),
        }
    }

    pub(crate) fn generalize(&self, ty: &Type, env: &TypeEnv) -> TypeScheme {
        let ty = self.apply(ty);
        let ty_fv = self.free_type_vars(&ty);
        let env_fv = self.free_type_vars_env(env);

        let quantified = ty_fv.difference(&env_fv).copied().collect::<Vec<_>>();

        TypeScheme {
            quantified,
            bounds: Vec::new(),
            ty,
        }
    }

    pub(crate) fn instantiate_scheme(&mut self, scheme: &TypeScheme) -> Type {
        self.instantiate_scheme_with_bounds(scheme).0
    }

    pub(crate) fn instantiate_scheme_with_bounds(
        &mut self,
        scheme: &TypeScheme,
    ) -> (Type, Vec<InstantiatedBound>) {
        if scheme.quantified.is_empty() {
            let bounds = scheme
                .bounds
                .iter()
                .map(|b| InstantiatedBound {
                    concept: b.concept.clone(),
                    ty: Type::Var(b.type_var),
                })
                .collect();
            return (scheme.ty.clone(), bounds);
        }

        let mut subst = HashMap::new();
        for v in &scheme.quantified {
            subst.insert(*v, self.fresh_var());
        }

        let ty = self.substitute_type(&scheme.ty, &subst);
        let bounds = scheme
            .bounds
            .iter()
            .map(|b| InstantiatedBound {
                concept: b.concept.clone(),
                ty: self.substitute_type(&Type::Var(b.type_var), &subst),
            })
            .collect();
        (ty, bounds)
    }

    pub(crate) fn check_instantiated_bounds(&mut self, bounds: &[InstantiatedBound], span: Span) {
        for b in bounds {
            if !self.type_implements_concept(&b.ty, &b.concept) {
                self.errors.push(TypeError {
                    message: format!(
                        "type '{}' does not implement concept '{}'",
                        self.apply(&b.ty),
                        b.concept
                    ),
                    span,
                });
            }
        }
    }

    pub(crate) fn bounds_to_assumptions(
        &self,
        bounds: &[InstantiatedBound],
    ) -> HashMap<TypeVarId, HashSet<String>> {
        let mut out: HashMap<TypeVarId, HashSet<String>> = HashMap::new();
        for b in bounds {
            if let Type::Var(v) = self.apply(&b.ty) {
                out.entry(v).or_default().insert(b.concept.clone());
            }
        }
        out
    }

    pub(crate) fn has_assumed_bound(&self, var: TypeVarId, concept: &str) -> bool {
        self.bound_assumptions.iter().rev().any(|scope| {
            scope
                .get(&var)
                .map(|set| set.contains(concept))
                .unwrap_or(false)
        })
    }

    pub(crate) fn assumed_concepts_for_var(&self, var: TypeVarId) -> HashSet<String> {
        let mut out = HashSet::new();
        for scope in &self.bound_assumptions {
            if let Some(set) = scope.get(&var) {
                out.extend(set.iter().cloned());
            }
        }
        out
    }

    pub(crate) fn type_implements_concept(&self, ty: &Type, concept: &str) -> bool {
        let normalized = self.apply(ty);
        if let Type::Var(v) = normalized {
            return self.has_assumed_bound(v, concept);
        }
        let Some(type_name) = self.type_name_key(&normalized) else {
            return false;
        };

        if self
            .concept_instances
            .contains_key(&(concept.to_string(), type_name.clone()))
        {
            return true;
        }

        // Built-in primitive concept support used by operators and bounded polymorphism.
        match concept {
            "Eq" => matches!(
                normalized,
                Type::Int
                    | Type::Int8
                    | Type::Int16
                    | Type::Int32
                    | Type::Int64
                    | Type::UInt
                    | Type::UInt8
                    | Type::UInt16
                    | Type::UInt32
                    | Type::UInt64
                    | Type::Float32
                    | Type::Float64
                    | Type::Decimal
                    | Type::BigDecimal
                    | Type::Bool
                    | Type::Char
                    | Type::String
            ),
            "Ord" => matches!(
                normalized,
                Type::Int
                    | Type::Int8
                    | Type::Int16
                    | Type::Int32
                    | Type::Int64
                    | Type::UInt
                    | Type::UInt8
                    | Type::UInt16
                    | Type::UInt32
                    | Type::UInt64
                    | Type::Float32
                    | Type::Float64
                    | Type::Decimal
                    | Type::BigDecimal
                    | Type::Char
                    | Type::String
            ),
            "Add" | "Sub" | "Mul" | "Div" | "Rem" => normalized.is_numeric(),
            _ => false,
        }
    }

    pub(crate) fn free_type_vars_env(&self, env: &TypeEnv) -> HashSet<TypeVarId> {
        let mut out = HashSet::new();
        for scheme in env.values() {
            let mut fv = self.free_type_vars(&scheme.ty);
            for q in &scheme.quantified {
                fv.remove(q);
            }
            out.extend(fv);
        }
        out
    }

    pub(crate) fn free_type_vars(&self, ty: &Type) -> HashSet<TypeVarId> {
        let mut out = HashSet::new();
        self.collect_free_type_vars(ty, &mut out);
        out
    }

    pub(crate) fn collect_free_type_vars(&self, ty: &Type, out: &mut HashSet<TypeVarId>) {
        match self.apply(ty) {
            Type::Var(id) => {
                out.insert(id);
            }
            Type::Function(params, ret) => {
                for p in params {
                    self.collect_free_type_vars(&p, out);
                }
                self.collect_free_type_vars(&ret, out);
            }
            Type::Product(types) => {
                for t in types {
                    self.collect_free_type_vars(&t, out);
                }
            }
            Type::Named(_, args) => {
                for a in args {
                    self.collect_free_type_vars(&a, out);
                }
            }
            _ => {}
        }
    }

    pub(crate) fn collect_and_freshen_type_vars(
        &mut self,
        tys: &[Type],
        map: &mut HashMap<TypeVarId, Type>,
    ) {
        for ty in tys {
            self.collect_and_freshen_type_var(ty, map);
        }
    }

    pub(crate) fn collect_and_freshen_type_var(
        &mut self,
        ty: &Type,
        map: &mut HashMap<TypeVarId, Type>,
    ) {
        match ty {
            Type::Var(id) => {
                map.entry(*id).or_insert_with(|| self.fresh_var());
            }
            Type::Function(params, ret) => {
                for p in params {
                    self.collect_and_freshen_type_var(p, map);
                }
                self.collect_and_freshen_type_var(ret, map);
            }
            Type::Product(types) => {
                for t in types {
                    self.collect_and_freshen_type_var(t, map);
                }
            }
            Type::Named(_, args) => {
                for a in args {
                    self.collect_and_freshen_type_var(a, map);
                }
            }
            _ => {}
        }
    }

    pub(crate) fn substitute_type(&self, ty: &Type, subst: &HashMap<TypeVarId, Type>) -> Type {
        match ty {
            Type::Var(id) => subst.get(id).cloned().unwrap_or_else(|| ty.clone()),
            Type::Function(params, ret) => Type::Function(
                params
                    .iter()
                    .map(|p| self.substitute_type(p, subst))
                    .collect(),
                Box::new(self.substitute_type(ret, subst)),
            ),
            Type::Product(types) => Type::Product(
                types
                    .iter()
                    .map(|t| self.substitute_type(t, subst))
                    .collect(),
            ),
            Type::Named(name, args) => Type::Named(
                name.clone(),
                args.iter()
                    .map(|a| self.substitute_type(a, subst))
                    .collect(),
            ),
            _ => ty.clone(),
        }
    }

    // ── Unification ──

    pub(crate) fn unify(&mut self, a: &Type, b: &Type, span: Span) {
        let a = self.apply(a);
        let b = self.apply(b);

        match (&a, &b) {
            _ if a == b => {}
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
                        message: format!(
                            "function arity mismatch: expected {} args, got {}",
                            p1.len(),
                            p2.len()
                        ),
                        span,
                    });
                    return;
                }
                for (x, y) in p1.iter().zip(p2.iter()) {
                    self.unify(x, y, span);
                }
                self.unify(r1, r2, span);
            }
            (Type::Product(ts1), Type::Product(ts2)) => {
                if ts1.len() != ts2.len() {
                    self.errors.push(TypeError {
                        message: format!(
                            "product type mismatch: {} vs {} elements",
                            ts1.len(),
                            ts2.len()
                        ),
                        span,
                    });
                    return;
                }
                for (x, y) in ts1.iter().zip(ts2.iter()) {
                    self.unify(x, y, span);
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
                        message: format!("type argument mismatch for '{}'", n1),
                        span,
                    });
                    return;
                }
                for (x, y) in a1.iter().zip(a2.iter()) {
                    self.unify(x, y, span);
                }
            }
            (Type::Error, _) | (_, Type::Error) => {}
            _ => {
                self.errors.push(TypeError {
                    message: format!("type mismatch: expected {a}, got {b}"),
                    span,
                });
            }
        }
    }

    pub(crate) fn occurs_in(&self, id: TypeVarId, ty: &Type) -> bool {
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

    pub(crate) fn apply(&self, ty: &Type) -> Type {
        match ty {
            Type::Var(id) => {
                if let Some(resolved) = self.substitution.get(id) {
                    self.apply(resolved)
                } else {
                    ty.clone()
                }
            }
            Type::Function(params, ret) => {
                let params = params.iter().map(|p| self.apply(p)).collect();
                let ret = self.apply(ret);
                Type::Function(params, Box::new(ret))
            }
            Type::Product(types) => {
                let types = types.iter().map(|t| self.apply(t)).collect();
                Type::Product(types)
            }
            Type::Named(name, args) => {
                let args = args.iter().map(|a| self.apply(a)).collect();
                Type::Named(name.clone(), args)
            }
            _ => ty.clone(),
        }
    }
}
