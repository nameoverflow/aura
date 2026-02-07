use std::collections::{HashMap, HashSet};

use aura_parser::ast::*;
use aura_resolve::ResolvedModule;

use crate::types::Type;

use super::{ConceptInfo, ConceptInstanceInfo, TypeChecker, TypeEnv, TypeError, TypeScheme};

impl TypeChecker {
    pub(crate) fn collect_type_def(&mut self, td: &TypeDef) {
        self.type_arity
            .insert(td.name.clone(), td.type_params.len());

        let mut tvars = HashMap::new();
        for tp in &td.type_params {
            tvars.insert(tp.clone(), self.fresh_var());
        }

        match &td.kind {
            TypeDefKind::Struct(fields) => {
                let field_types: Vec<(String, Type)> = fields
                    .iter()
                    .map(|f| {
                        (
                            f.name.clone(),
                            self.resolve_type_expr_with_vars(&f.ty, &mut tvars),
                        )
                    })
                    .collect();

                let type_args = td
                    .type_params
                    .iter()
                    .filter_map(|p| tvars.get(p).cloned())
                    .collect();

                self.struct_fields
                    .insert(td.name.clone(), field_types.clone());
                self.struct_info.insert(
                    td.name.clone(),
                    super::StructTypeInfo {
                        type_args,
                        fields: field_types,
                    },
                );
            }
            TypeDefKind::Sum(variants) => {
                let parent_args: Vec<Type> = td
                    .type_params
                    .iter()
                    .filter_map(|p| tvars.get(p).cloned())
                    .collect();

                let variant_names: Vec<String> = variants.iter().map(|v| v.name.clone()).collect();
                self.variants_by_parent
                    .entry(td.name.clone())
                    .or_default()
                    .extend(variant_names);

                for variant in variants {
                    let field_types: Vec<Type> = variant
                        .fields
                        .iter()
                        .map(|f| self.resolve_type_expr_with_vars(f, &mut tvars))
                        .collect();

                    self.variant_info.insert(
                        variant.name.clone(),
                        super::VariantTypeInfo {
                            parent_type: td.name.clone(),
                            parent_args: parent_args.clone(),
                            field_types,
                        },
                    );
                }

                // Auto-derive From payload -> parent for unique single-payload variants.
                let mut payload_to_variant: HashMap<String, Option<String>> = HashMap::new();
                for variant in variants {
                    if variant.fields.len() != 1 {
                        continue;
                    }
                    let mut local = HashMap::new();
                    let payload = self.resolve_type_expr_with_vars(&variant.fields[0], &mut local);
                    if let Some(payload_name) = self.type_name_key(&payload) {
                        payload_to_variant
                            .entry(payload_name)
                            .and_modify(|slot| *slot = None)
                            .or_insert_with(|| Some(variant.name.clone()));
                    }
                }
                for (payload, variant_opt) in payload_to_variant {
                    if let Some(variant_name) = variant_opt {
                        self.auto_from_variants
                            .insert((payload, td.name.clone()), variant_name);
                    }
                }
            }
            TypeDefKind::Refined { .. } => {
                if let TypeDefKind::Refined {
                    base_type,
                    constraint,
                } = &td.kind
                {
                    let mut base_tvars = HashMap::new();
                    let base = self.resolve_type_expr_with_vars(base_type, &mut base_tvars);

                    // Validate constraint grammar against the restricted P2 subset.
                    self.validate_refined_constraint(constraint, td.span);

                    let type_args = td
                        .type_params
                        .iter()
                        .filter_map(|p| tvars.get(p).cloned())
                        .collect::<Vec<_>>();

                    self.refined_info.insert(
                        td.name.clone(),
                        super::RefinedTypeInfo {
                            type_args: type_args.clone(),
                            base_type: base.clone(),
                            constraint: constraint.clone(),
                        },
                    );

                    // Auto-generate associated constructor:
                    // Name.new(base) -> Result Name ConstraintError
                    let refined_ty = Type::Named(td.name.clone(), type_args.clone());
                    let ret = Type::Named(
                        "Result".into(),
                        vec![refined_ty, Type::Named("ConstraintError".into(), vec![])],
                    );
                    let ctor_sig =
                        TypeScheme::monomorphic(Type::Function(vec![base], Box::new(ret)));
                    self.associated_functions
                        .entry(td.name.clone())
                        .or_default()
                        .insert("new".into(), ctor_sig);
                }
            }
            TypeDefKind::Alias(ty) => {
                let resolved = self.resolve_type_expr_with_vars(ty, &mut tvars);
                self.type_aliases
                    .insert(td.name.clone(), (td.type_params.clone(), resolved));
            }
        }
    }

    pub(crate) fn collect_concept_def(&mut self, cd: &ConceptDef) {
        if self.concepts.contains_key(&cd.name) {
            self.errors.push(TypeError {
                message: format!("duplicate concept '{}'", cd.name),
                span: cd.span,
            });
            return;
        }

        let self_ty = self.fresh_var();
        let self_var = match self_ty {
            Type::Var(v) => v,
            _ => unreachable!(),
        };

        let mut assoc_placeholders: HashMap<String, Type> = HashMap::new();
        let mut assoc_var_ids: HashMap<String, super::TypeVarId> = HashMap::new();
        for assoc in &cd.assoc_types {
            let v = self.fresh_var();
            let vid = match v {
                Type::Var(id) => id,
                _ => unreachable!(),
            };
            assoc_placeholders.insert(assoc.name.clone(), Type::Var(vid));
            assoc_var_ids.insert(assoc.name.clone(), vid);
        }
        let mut assoc_defaults = HashMap::new();
        for assoc in &cd.assoc_types {
            if let Some(default) = &assoc.default {
                let mut local_tvars = HashMap::new();
                let base = self.resolve_type_expr_with_vars(default, &mut local_tvars);
                let ty = self.replace_special_type_names(base, &self_ty, &assoc_placeholders);
                assoc_defaults.insert(assoc.name.clone(), ty);
            }
        }

        let mut methods = HashMap::new();
        let mut methods_with_default = HashSet::new();
        for method in &cd.methods {
            let scheme = self.concept_method_scheme(
                method,
                &self_ty,
                &assoc_placeholders,
                self_var,
                &assoc_var_ids,
            );
            if methods.insert(method.name.clone(), scheme).is_some() {
                self.errors.push(TypeError {
                    message: format!(
                        "duplicate method '{}' in concept '{}'",
                        method.name, cd.name
                    ),
                    span: method.span,
                });
            }
            if method.default_body.is_some() {
                methods_with_default.insert(method.name.clone());
            }
        }

        self.concepts.insert(
            cd.name.clone(),
            ConceptInfo {
                supers: cd.supers.clone(),
                assoc_types: cd.assoc_types.iter().map(|a| a.name.clone()).collect(),
                assoc_defaults,
                self_var,
                assoc_var_ids,
                methods,
                methods_with_default,
            },
        );
    }

    pub(crate) fn concept_method_scheme(
        &mut self,
        method: &ConceptMethodSig,
        self_ty: &Type,
        assoc_map: &HashMap<String, Type>,
        self_var: super::TypeVarId,
        assoc_var_ids: &HashMap<String, super::TypeVarId>,
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
        let mut fixed = HashSet::new();
        fixed.insert(self_var);
        fixed.extend(assoc_var_ids.values().copied());

        let quantified = self
            .free_type_vars(&fn_ty)
            .difference(&fixed)
            .copied()
            .collect::<Vec<_>>();

        TypeScheme {
            quantified,
            bounds: Vec::new(),
            ty: self.apply(&fn_ty),
        }
    }

    /// Type-check default method bodies in a concept definition.
    /// The body is checked with `self` bound to the abstract Self type variable.
    pub(crate) fn check_concept_default_bodies(
        &mut self,
        cd: &ConceptDef,
        resolved: &ResolvedModule,
    ) {
        let concept_info = match self.concepts.get(&cd.name) {
            Some(info) => info.clone(),
            None => return,
        };
        let self_ty = Type::Var(concept_info.self_var);

        for method in &cd.methods {
            let Some(body) = &method.default_body else {
                continue;
            };

            // Build env with self and method params
            let mut env: TypeEnv = HashMap::new();
            let mut tvars = HashMap::new();

            for param in &method.params {
                if param.name == "self" {
                    env.insert("self".into(), TypeScheme::monomorphic(self_ty.clone()));
                } else {
                    let ty = match &param.ty {
                        Some(te) => {
                            let base = self.resolve_type_expr_with_vars(te, &mut tvars);
                            self.replace_special_type_names(
                                base,
                                &self_ty,
                                &concept_info
                                    .assoc_var_ids
                                    .iter()
                                    .map(|(k, v)| (k.clone(), Type::Var(*v)))
                                    .collect(),
                            )
                        }
                        None => self.fresh_var(),
                    };
                    env.insert(param.name.clone(), TypeScheme::monomorphic(ty));
                }
            }

            // Push assumption that Self implements this concept (and its supers)
            // so method calls on self can resolve concept methods.
            let mut assumed: HashMap<super::TypeVarId, HashSet<String>> = HashMap::new();
            let mut concept_set = HashSet::new();
            concept_set.insert(cd.name.clone());
            // Also add super-concepts transitively
            let mut stack: Vec<String> = concept_info.supers.clone();
            while let Some(sup) = stack.pop() {
                if concept_set.insert(sup.clone()) {
                    if let Some(sup_info) = self.concepts.get(&sup) {
                        stack.extend(sup_info.supers.clone());
                    }
                }
            }
            assumed.insert(concept_info.self_var, concept_set);
            self.bound_assumptions.push(assumed);

            // Infer body type
            let body_ty = self.infer_expr(body, &mut env, resolved);

            self.bound_assumptions.pop();

            // Check against declared return type
            if let Some(ret_te) = &method.return_type {
                let assoc_map: HashMap<String, Type> = concept_info
                    .assoc_var_ids
                    .iter()
                    .map(|(k, v)| (k.clone(), Type::Var(*v)))
                    .collect();
                let expected = self.resolve_type_expr_with_vars(ret_te, &mut tvars);
                let expected = self.replace_special_type_names(expected, &self_ty, &assoc_map);
                self.unify(&body_ty, &expected, body.span());
            }
        }
    }

    pub(crate) fn collect_instance_def(&mut self, inst: &InstanceDef) {
        let mut tvars = HashMap::new();
        let (target_ty, target_name, concept_name_opt) = match &inst.kind {
            InstanceKind::Inherent(target) => {
                let target_ty = self.resolve_type_expr_with_vars(target, &mut tvars);
                let target_name = self
                    .type_name_key(&target_ty)
                    .unwrap_or_else(|| "<unknown>".into());
                (target_ty, target_name, None)
            }
            InstanceKind::Concept { concept, for_type } => {
                let target_ty = self.resolve_type_expr_with_vars(for_type, &mut tvars);
                let target_name = self
                    .type_name_key(&target_ty)
                    .unwrap_or_else(|| "<unknown>".into());
                (target_ty, target_name, Some(concept.clone()))
            }
        };

        let mut assoc_map = HashMap::new();
        for assoc in &inst.assoc_types {
            let mut assoc_tvars = HashMap::new();
            let base_ty = self.resolve_type_expr_with_vars(&assoc.ty, &mut assoc_tvars);
            let ty = self.replace_special_type_names(base_ty, &target_ty, &HashMap::new());
            assoc_map.insert(assoc.name.clone(), ty);
        }

        if let Some(concept_name) = &concept_name_opt {
            let Some(concept_info) = self.concepts.get(concept_name).cloned() else {
                self.errors.push(TypeError {
                    message: format!("unknown concept '{}'", concept_name),
                    span: inst.span,
                });
                return;
            };

            for assoc_name in &concept_info.assoc_types {
                if assoc_map.contains_key(assoc_name) {
                    continue;
                }
                if let Some(default_ty) = concept_info.assoc_defaults.get(assoc_name) {
                    let mut subst = HashMap::new();
                    subst.insert(concept_info.self_var, target_ty.clone());
                    for (name, id) in &concept_info.assoc_var_ids {
                        let replacement = assoc_map
                            .get(name)
                            .cloned()
                            .unwrap_or_else(|| self.fresh_var());
                        subst.insert(*id, replacement);
                    }
                    let resolved_default = self.substitute_type(default_ty, &subst);
                    assoc_map.insert(assoc_name.clone(), resolved_default);
                } else {
                    assoc_map.insert(assoc_name.clone(), self.fresh_var());
                }
            }
            for assoc in assoc_map.keys() {
                if !concept_info.assoc_types.contains(assoc) {
                    self.errors.push(TypeError {
                        message: format!(
                            "associated type '{}' not declared in concept '{}'",
                            assoc, concept_name
                        ),
                        span: inst.span,
                    });
                }
            }

            let mut methods = HashMap::new();
            for method in &inst.methods {
                if !concept_info.methods.contains_key(&method.name) {
                    self.errors.push(TypeError {
                        message: format!(
                            "method '{}' is not part of concept '{}'",
                            method.name, concept_name
                        ),
                        span: method.span,
                    });
                }

                let scheme = self.method_scheme_in_context(method, &target_ty, &assoc_map);
                methods.insert(method.name.clone(), scheme);
            }

            for (name, concept_sig) in &concept_info.methods {
                if !methods.contains_key(name) {
                    if concept_info.methods_with_default.contains(name) {
                        let sig = self.instantiate_concept_method_for_type(
                            &concept_info,
                            concept_sig,
                            &target_ty,
                            &assoc_map,
                        );
                        methods.insert(name.clone(), sig);
                    } else {
                        self.errors.push(TypeError {
                            message: format!(
                                "instance of '{}' for '{}' is missing method '{}'",
                                concept_name, target_name, name
                            ),
                            span: inst.span,
                        });
                    }
                }
            }

            let key = (concept_name.clone(), target_name.clone());
            if self.concept_instances.contains_key(&key) {
                self.errors.push(TypeError {
                    message: format!(
                        "duplicate instance '{}' for type '{}'",
                        concept_name, target_name
                    ),
                    span: inst.span,
                });
                return;
            }

            self.concept_instances.insert(
                key,
                ConceptInstanceInfo {
                    concept: concept_name.clone(),
                    target_name,
                    assoc_types: assoc_map,
                    methods,
                    span: inst.span,
                },
            );
        } else {
            for method in &inst.methods {
                let scheme = self.method_scheme_in_context(method, &target_ty, &assoc_map);
                let has_self = method
                    .params
                    .first()
                    .map(|p| p.name == "self")
                    .unwrap_or(false);

                if has_self {
                    let type_methods = self
                        .inherent_methods
                        .entry(target_name.clone())
                        .or_default();
                    if type_methods.insert(method.name.clone(), scheme).is_some() {
                        self.errors.push(TypeError {
                            message: format!(
                                "duplicate inherent method '{}' for type '{}'",
                                method.name, target_name
                            ),
                            span: method.span,
                        });
                    }
                } else {
                    let type_funcs = self
                        .associated_functions
                        .entry(target_name.clone())
                        .or_default();
                    if type_funcs.insert(method.name.clone(), scheme).is_some() {
                        self.errors.push(TypeError {
                            message: format!(
                                "duplicate associated function '{}' for type '{}'",
                                method.name, target_name
                            ),
                            span: method.span,
                        });
                    }
                }
            }
        }
    }

    pub(crate) fn check_instance_coherence(&mut self) {
        for inst in self.concept_instances.values() {
            let Some(concept) = self.concepts.get(&inst.concept) else {
                continue;
            };
            for sup in &concept.supers {
                if !self
                    .concept_instances
                    .contains_key(&(sup.clone(), inst.target_name.clone()))
                {
                    self.errors.push(TypeError {
                        message: format!(
                            "instance '{}' for '{}' requires superclass instance '{}'",
                            inst.concept, inst.target_name, sup
                        ),
                        span: inst.span,
                    });
                }
            }
        }
    }

    pub(crate) fn check_instance_methods(&mut self, inst: &InstanceDef, resolved: &ResolvedModule) {
        let mut tvars = HashMap::new();
        let (target_ty, target_name, concept_name_opt) = match &inst.kind {
            InstanceKind::Inherent(target) => {
                let target_ty = self.resolve_type_expr_with_vars(target, &mut tvars);
                let target_name = self
                    .type_name_key(&target_ty)
                    .unwrap_or_else(|| "<unknown>".into());
                (target_ty, target_name, None)
            }
            InstanceKind::Concept { concept, for_type } => {
                let target_ty = self.resolve_type_expr_with_vars(for_type, &mut tvars);
                let target_name = self
                    .type_name_key(&target_ty)
                    .unwrap_or_else(|| "<unknown>".into());
                (target_ty, target_name, Some(concept.clone()))
            }
        };

        let _ = target_ty;

        for method in &inst.methods {
            let scheme = if let Some(concept_name) = &concept_name_opt {
                self.concept_instances
                    .get(&(concept_name.clone(), target_name.clone()))
                    .and_then(|ci| ci.methods.get(&method.name))
                    .cloned()
            } else if method
                .params
                .first()
                .map(|p| p.name == "self")
                .unwrap_or(false)
            {
                self.inherent_methods
                    .get(&target_name)
                    .and_then(|m| m.get(&method.name))
                    .cloned()
            } else {
                self.associated_functions
                    .get(&target_name)
                    .and_then(|m| m.get(&method.name))
                    .cloned()
            };

            if let Some(scheme) = scheme {
                self.check_method_body(method, &scheme, resolved);
            }
        }
    }

    pub(crate) fn check_method_body(
        &mut self,
        method: &MethodDef,
        scheme: &TypeScheme,
        resolved: &ResolvedModule,
    ) {
        let (inst_ty, inst_bounds) = self.instantiate_scheme_with_bounds(scheme);
        let Type::Function(param_types, ret_ty) = inst_ty else {
            self.errors.push(TypeError {
                message: format!("method '{}' does not have a function type", method.name),
                span: method.span,
            });
            return;
        };
        let assumptions = self.bounds_to_assumptions(&inst_bounds);
        self.bound_assumptions.push(assumptions);

        if param_types.len() != method.params.len() {
            self.errors.push(TypeError {
                message: format!(
                    "method '{}' parameter count mismatch ({} vs {})",
                    method.name,
                    method.params.len(),
                    param_types.len()
                ),
                span: method.span,
            });
            return;
        }

        let mut env: TypeEnv = HashMap::new();
        for (param, pty) in method.params.iter().zip(param_types.iter()) {
            env.insert(param.name.clone(), TypeScheme::monomorphic(pty.clone()));
        }

        self.return_type_stack.push((*ret_ty).clone());
        let actual_ret = self.infer_expr(&method.body, &mut env, resolved);
        self.return_type_stack.pop();
        self.bound_assumptions.pop();
        self.unify(&ret_ty, &actual_ret, method.body.span());
    }
}
