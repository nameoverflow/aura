use std::collections::{HashMap, HashSet};

use aura_common::Span;
use aura_parser::ast::*;

use crate::types::Type;

use super::{TypeChecker, TypeEnv, TypeError, TypeScheme};

impl TypeChecker {
    pub(crate) fn bind_pattern(
        &mut self,
        pattern: &Pattern,
        expected: &Type,
        env: &mut TypeEnv,
        span: Span,
    ) {
        match pattern {
            Pattern::Wildcard(_) => {}
            Pattern::Ident(name, _) => {
                env.insert(name.clone(), TypeScheme::monomorphic(self.apply(expected)));
            }
            Pattern::Literal(lit, lspan) => {
                let lit_ty = self.lit_pattern_type(lit);
                self.unify(expected, &lit_ty, *lspan);
            }
            Pattern::Constructor(name, args, pspan) => {
                if let Some(info) = self.variant_info.get(name).cloned() {
                    let inst = self.instantiate_variant_info(&info);
                    let parent_ty = Type::Named(inst.parent_type.clone(), inst.parent_args.clone());
                    self.unify(expected, &parent_ty, *pspan);

                    if args.len() != inst.field_types.len() {
                        self.errors.push(TypeError {
                            message: format!(
                                "pattern '{}' expects {} payload argument(s), got {}",
                                name,
                                inst.field_types.len(),
                                args.len()
                            ),
                            span: *pspan,
                        });
                    }

                    for (arg_pat, field_ty) in args.iter().zip(inst.field_types.iter()) {
                        self.bind_pattern(arg_pat, field_ty, env, span);
                    }
                } else {
                    self.errors.push(TypeError {
                        message: format!("unknown pattern constructor '{}'", name),
                        span: *pspan,
                    });

                    for arg_pat in args {
                        let ty = self.fresh_var();
                        self.bind_pattern(arg_pat, &ty, env, span);
                    }
                }
            }
            Pattern::Tuple(pats, pspan) => {
                let resolved_expected = self.apply(expected);
                match resolved_expected {
                    Type::Product(types) => {
                        if types.len() != pats.len() {
                            self.errors.push(TypeError {
                                message: format!(
                                    "tuple pattern has {} elements, but expected {}",
                                    pats.len(),
                                    types.len()
                                ),
                                span: *pspan,
                            });
                        }

                        for (i, pat) in pats.iter().enumerate() {
                            let ty = types.get(i).cloned().unwrap_or(Type::Error);
                            self.bind_pattern(pat, &ty, env, span);
                        }
                    }
                    Type::Var(_) => {
                        let elem_types: Vec<Type> =
                            (0..pats.len()).map(|_| self.fresh_var()).collect();
                        let product_ty = Type::Product(elem_types.clone());
                        self.unify(expected, &product_ty, *pspan);
                        for (pat, ty) in pats.iter().zip(elem_types.iter()) {
                            self.bind_pattern(pat, ty, env, span);
                        }
                    }
                    _ => {
                        self.errors.push(TypeError {
                            message: format!(
                                "tuple pattern does not match type {}",
                                resolved_expected
                            ),
                            span: *pspan,
                        });
                        for pat in pats {
                            let ty = self.fresh_var();
                            self.bind_pattern(pat, &ty, env, span);
                        }
                    }
                }
            }
            Pattern::Struct(name, fields, has_rest, pspan) => {
                if let Some(info) = self.struct_info.get(name).cloned() {
                    let (type_args, instantiated_fields) = self.instantiate_struct_info(&info);
                    let struct_ty = Type::Named(name.clone(), type_args);
                    self.unify(expected, &struct_ty, *pspan);

                    let mut field_map: HashMap<String, Type> = HashMap::new();
                    for (fname, fty) in instantiated_fields {
                        field_map.insert(fname, fty);
                    }

                    let mut seen = HashSet::new();
                    for field in fields {
                        if !seen.insert(field.name.clone()) {
                            self.errors.push(TypeError {
                                message: format!(
                                    "duplicate field '{}' in struct pattern '{}'",
                                    field.name, name
                                ),
                                span: field.span,
                            });
                            continue;
                        }

                        if let Some(field_ty) = field_map.get(&field.name) {
                            self.bind_pattern(&field.pattern, field_ty, env, field.span);
                        } else {
                            self.errors.push(TypeError {
                                message: format!(
                                    "type '{}' has no field '{}' in pattern",
                                    name, field.name
                                ),
                                span: field.span,
                            });
                        }
                    }

                    if !*has_rest {
                        let missing: Vec<String> = field_map
                            .keys()
                            .filter(|fname| !seen.contains(*fname))
                            .cloned()
                            .collect();
                        if !missing.is_empty() {
                            self.errors.push(TypeError {
                                message: format!(
                                    "struct pattern '{}' is missing fields: {} (use '..' to ignore)",
                                    name,
                                    missing.join(", ")
                                ),
                                span: *pspan,
                            });
                        }
                    }
                } else {
                    self.errors.push(TypeError {
                        message: format!("unknown struct pattern type '{}'", name),
                        span: *pspan,
                    });
                    for field in fields {
                        let ty = self.fresh_var();
                        self.bind_pattern(&field.pattern, &ty, env, field.span);
                    }
                }
            }
            Pattern::Or(patterns, pspan) => {
                if patterns.is_empty() {
                    return;
                }

                let mut branch_bindings: Vec<HashMap<String, Type>> = Vec::new();
                for pat in patterns {
                    let mut branch_env: TypeEnv = HashMap::new();
                    self.bind_pattern(pat, expected, &mut branch_env, span);
                    let mut bindings = HashMap::new();
                    for (name, scheme) in branch_env {
                        bindings.insert(name, scheme.ty);
                    }
                    branch_bindings.push(bindings);
                }

                let expected_names: HashSet<String> = branch_bindings
                    .first()
                    .map(|b| b.keys().cloned().collect())
                    .unwrap_or_default();

                for (idx, bindings) in branch_bindings.iter().enumerate().skip(1) {
                    let names: HashSet<String> = bindings.keys().cloned().collect();
                    if names != expected_names {
                        self.errors.push(TypeError {
                            message: format!(
                                "or-pattern branch {} binds different names (expected {{{}}}, found {{{}}})",
                                idx + 1,
                                expected_names.iter().cloned().collect::<Vec<_>>().join(", "),
                                names.iter().cloned().collect::<Vec<_>>().join(", ")
                            ),
                            span: *pspan,
                        });
                    }
                }

                for name in expected_names {
                    let mut merged: Option<Type> = None;
                    for bindings in &branch_bindings {
                        if let Some(ty) = bindings.get(&name) {
                            if let Some(existing) = &merged {
                                self.unify(existing, ty, *pspan);
                            } else {
                                merged = Some(ty.clone());
                            }
                        }
                    }
                    if let Some(ty) = merged {
                        env.insert(name, TypeScheme::monomorphic(self.apply(&ty)));
                    }
                }
            }
        }
    }

    pub(crate) fn lit_pattern_type(&self, lit: &LitPattern) -> Type {
        match lit {
            LitPattern::Int(_) => Type::Int,
            LitPattern::Float(_) => Type::Float64,
            LitPattern::String(_) => Type::String,
            LitPattern::Bool(_) => Type::Bool,
        }
    }

    pub(crate) fn check_match_exhaustiveness(
        &mut self,
        scrut_ty: &Type,
        arms: &[MatchArm],
        span: Span,
    ) {
        let scrut_ty = self.apply(scrut_ty);

        let has_catch_all = arms
            .iter()
            .any(|arm| arm.guard.is_none() && self.pattern_has_catch_all(&arm.pattern));

        if has_catch_all {
            return;
        }

        match scrut_ty {
            Type::Named(parent, _) => {
                if let Some(variants) = self.variants_by_parent.get(&parent).cloned() {
                    let mut covered = HashSet::new();

                    for arm in arms {
                        if arm.guard.is_some() {
                            continue;
                        }
                        self.pattern_constructors(&arm.pattern, &mut covered);
                    }

                    let missing: Vec<String> = variants
                        .into_iter()
                        .filter(|v| !covered.contains(v))
                        .collect();

                    if !missing.is_empty() {
                        self.errors.push(TypeError {
                            message: format!(
                                "non-exhaustive match for '{}', missing patterns: {}",
                                parent,
                                missing.join(", ")
                            ),
                            span,
                        });
                    }
                }
            }
            Type::Bool => {
                let mut has_true = false;
                let mut has_false = false;
                for arm in arms {
                    if arm.guard.is_some() {
                        continue;
                    }
                    let mut bools = HashSet::new();
                    self.pattern_bool_literals(&arm.pattern, &mut bools);
                    if bools.contains(&true) {
                        has_true = true;
                    }
                    if bools.contains(&false) {
                        has_false = true;
                    }
                }

                if !(has_true && has_false) {
                    self.errors.push(TypeError {
                        message: "non-exhaustive match for Bool".into(),
                        span,
                    });
                }
            }
            _ => {}
        }
    }

    /// Check for unreachable (redundant) match arms. An arm is unreachable if
    /// all patterns it covers are already covered by previous unguarded arms.
    pub(crate) fn check_match_redundancy(&mut self, _scrut_ty: &Type, arms: &[MatchArm]) {
        // Track whether a catch-all has been seen among unguarded arms.
        let mut catch_all_seen = false;
        // For sum types: constructors fully covered (all sub-patterns are catch-all).
        let mut fully_covered_constructors: HashSet<String> = HashSet::new();
        // For Bool: which literals have been covered.
        let mut covered_bools: HashSet<bool> = HashSet::new();
        // For literal patterns: which literal values have been covered.
        let mut covered_int_lits: HashSet<i64> = HashSet::new();
        let mut covered_string_lits: HashSet<String> = HashSet::new();

        for (i, arm) in arms.iter().enumerate() {
            // Skip the first arm — it's never redundant.
            if i > 0 {
                let is_redundant = self.arm_is_redundant(
                    &arm.pattern,
                    catch_all_seen,
                    &fully_covered_constructors,
                    &covered_bools,
                    &covered_int_lits,
                    &covered_string_lits,
                );
                if is_redundant {
                    self.errors.push(TypeError {
                        message: "unreachable match arm".into(),
                        span: arm.span,
                    });
                }
            }

            // Only unguarded arms contribute to coverage.
            if arm.guard.is_none() {
                if self.pattern_has_catch_all(&arm.pattern) {
                    catch_all_seen = true;
                }
                self.collect_full_coverage(
                    &arm.pattern,
                    &mut fully_covered_constructors,
                    &mut covered_bools,
                    &mut covered_int_lits,
                    &mut covered_string_lits,
                );
            }
        }
    }

    /// Returns true if this arm's pattern is fully subsumed by already-covered patterns.
    fn arm_is_redundant(
        &self,
        pattern: &Pattern,
        catch_all_seen: bool,
        fully_covered_constructors: &HashSet<String>,
        covered_bools: &HashSet<bool>,
        covered_int_lits: &HashSet<i64>,
        covered_string_lits: &HashSet<String>,
    ) -> bool {
        // If a previous catch-all covers everything, this arm is unreachable.
        if catch_all_seen {
            return true;
        }

        match pattern {
            Pattern::Wildcard(_) | Pattern::Ident(_, _) => false,
            Pattern::Constructor(name, sub_pats, _) => {
                // Only redundant if the constructor was fully covered
                // (previous arm had same constructor with catch-all sub-patterns).
                if fully_covered_constructors.contains(name) {
                    return true;
                }
                // A constructor with all catch-all sub-patterns after full coverage
                // is handled. A constructor with specific sub-patterns isn't redundant
                // unless we also tracked the sub-pattern space (not done here).
                let _ = sub_pats;
                false
            }
            Pattern::Literal(lit, _) => match lit {
                LitPattern::Bool(b) => covered_bools.contains(b),
                LitPattern::Int(n) => covered_int_lits.contains(n),
                LitPattern::String(s) => covered_string_lits.contains(s),
                LitPattern::Float(_) => false,
            },
            Pattern::Or(pats, _) => pats.iter().all(|p| {
                self.arm_is_redundant(
                    p,
                    catch_all_seen,
                    fully_covered_constructors,
                    covered_bools,
                    covered_int_lits,
                    covered_string_lits,
                )
            }),
            Pattern::Tuple(_, _) | Pattern::Struct(_, _, _, _) => false,
        }
    }

    /// Collect patterns that are fully covered (constructor with all catch-all sub-patterns,
    /// or exact literals).
    fn collect_full_coverage(
        &self,
        pattern: &Pattern,
        constructors: &mut HashSet<String>,
        bools: &mut HashSet<bool>,
        int_lits: &mut HashSet<i64>,
        string_lits: &mut HashSet<String>,
    ) {
        match pattern {
            Pattern::Constructor(name, sub_pats, _) => {
                // Only mark as fully covered if all sub-patterns are catch-all.
                let all_catch_all = sub_pats.iter().all(|p| self.pattern_has_catch_all(p));
                if all_catch_all {
                    constructors.insert(name.clone());
                }
            }
            Pattern::Literal(lit, _) => match lit {
                LitPattern::Bool(b) => {
                    bools.insert(*b);
                }
                LitPattern::Int(n) => {
                    int_lits.insert(*n);
                }
                LitPattern::String(s) => {
                    string_lits.insert(s.clone());
                }
                _ => {}
            },
            Pattern::Or(pats, _) => {
                for p in pats {
                    self.collect_full_coverage(p, constructors, bools, int_lits, string_lits);
                }
            }
            _ => {}
        }
    }

    pub(crate) fn pattern_has_catch_all(&self, pattern: &Pattern) -> bool {
        match pattern {
            Pattern::Wildcard(_) | Pattern::Ident(_, _) => true,
            Pattern::Or(patterns, _) => patterns.iter().any(|p| self.pattern_has_catch_all(p)),
            _ => false,
        }
    }

    pub(crate) fn pattern_constructors(&self, pattern: &Pattern, out: &mut HashSet<String>) {
        match pattern {
            Pattern::Constructor(name, _, _) => {
                out.insert(name.clone());
            }
            Pattern::Or(patterns, _) => {
                for p in patterns {
                    self.pattern_constructors(p, out);
                }
            }
            _ => {}
        }
    }

    pub(crate) fn pattern_bool_literals(&self, pattern: &Pattern, out: &mut HashSet<bool>) {
        match pattern {
            Pattern::Literal(LitPattern::Bool(b), _) => {
                out.insert(*b);
            }
            Pattern::Or(patterns, _) => {
                for p in patterns {
                    self.pattern_bool_literals(p, out);
                }
            }
            _ => {}
        }
    }
}
