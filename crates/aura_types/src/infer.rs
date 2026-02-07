use std::collections::{HashMap, HashSet};

use aura_common::Span;
use aura_parser::ast::*;
use aura_resolve::{DefId, DefKind, ResolvedModule};

use crate::types::{Type, TypeVarId};

#[derive(Debug, Clone)]
pub struct TypeError {
    pub message: String,
    pub span: Span,
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "type error at {}..{}: {}",
            self.span.start, self.span.end, self.message
        )
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
    /// Parent type arguments (for generic parents) in declaration order.
    pub parent_args: Vec<Type>,
    pub field_types: Vec<Type>,
}

#[derive(Debug, Clone)]
struct StructTypeInfo {
    type_args: Vec<Type>,
    fields: Vec<(String, Type)>,
}

#[derive(Debug, Clone)]
struct TypeScheme {
    quantified: Vec<TypeVarId>,
    bounds: Vec<ConceptBound>,
    ty: Type,
}

#[derive(Debug, Clone)]
struct ConceptBound {
    concept: String,
    type_var: TypeVarId,
}

#[derive(Debug, Clone)]
struct InstantiatedBound {
    concept: String,
    ty: Type,
}

impl TypeScheme {
    fn monomorphic(ty: Type) -> Self {
        Self {
            quantified: Vec::new(),
            bounds: Vec::new(),
            ty,
        }
    }
}

type TypeEnv = HashMap<String, TypeScheme>;

#[derive(Debug, Clone)]
struct ConceptInfo {
    supers: Vec<String>,
    assoc_types: Vec<String>,
    assoc_defaults: HashMap<String, Type>,
    self_var: TypeVarId,
    assoc_var_ids: HashMap<String, TypeVarId>,
    methods: HashMap<String, TypeScheme>,
    methods_with_default: HashSet<String>,
}

#[derive(Debug, Clone)]
struct ConceptInstanceInfo {
    concept: String,
    target_name: String,
    assoc_types: HashMap<String, Type>,
    methods: HashMap<String, TypeScheme>,
    span: Span,
}

#[derive(Debug, Clone)]
struct RefinedTypeInfo {
    type_args: Vec<Type>,
    base_type: Type,
    constraint: Expr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Effect {
    DbRead,
    DbWrite,
    Net,
    FsRead,
    FsWrite,
    Log,
    Time,
    Random,
    Env,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum EffectTerm {
    Concrete(Effect),
    Var(String),
}

#[derive(Debug, Clone, Default)]
struct EffectSpec {
    terms: Vec<EffectTerm>,
}

#[derive(Debug, Clone)]
struct FunctionEffectScheme {
    declared: EffectSpec,
    param_effects: Vec<Option<EffectSpec>>,
}

#[derive(Debug, Clone, Default)]
struct EffectContext {
    allowed: HashSet<Effect>,
    vars: HashSet<String>,
}

pub struct TypeChecker {
    next_var: u32,
    substitution: HashMap<TypeVarId, Type>,
    expr_types: HashMap<(u32, u32), Type>,
    def_types: HashMap<DefId, Type>,

    // Internal type metadata
    type_arity: HashMap<String, usize>,
    def_schemes: HashMap<DefId, TypeScheme>,
    struct_info: HashMap<String, StructTypeInfo>,
    variant_info: HashMap<String, VariantTypeInfo>,
    variants_by_parent: HashMap<String, Vec<String>>,
    refined_info: HashMap<String, RefinedTypeInfo>,
    auto_from_variants: HashMap<(String, String), String>,
    concepts: HashMap<String, ConceptInfo>,
    inherent_methods: HashMap<String, HashMap<String, TypeScheme>>,
    associated_functions: HashMap<String, HashMap<String, TypeScheme>>,
    concept_instances: HashMap<(String, String), ConceptInstanceInfo>,
    fn_effects: HashMap<DefId, FunctionEffectScheme>,

    // Kept for typed output compatibility
    struct_fields: HashMap<String, Vec<(String, Type)>>,

    return_type_stack: Vec<Type>,
    bound_assumptions: Vec<HashMap<TypeVarId, HashSet<String>>>,
    effect_context_stack: Vec<EffectContext>,
    effect_usage_stack: Vec<HashSet<Effect>>,
    lambda_effects: HashMap<(u32, u32), HashSet<Effect>>,
    suspend_effect_checks: usize,
    errors: Vec<TypeError>,
}

impl TypeChecker {
    pub fn new() -> Self {
        let mut checker = Self {
            next_var: 0,
            substitution: HashMap::new(),
            expr_types: HashMap::new(),
            def_types: HashMap::new(),

            type_arity: HashMap::new(),
            def_schemes: HashMap::new(),
            struct_info: HashMap::new(),
            variant_info: HashMap::new(),
            variants_by_parent: HashMap::new(),
            refined_info: HashMap::new(),
            auto_from_variants: HashMap::new(),
            concepts: HashMap::new(),
            inherent_methods: HashMap::new(),
            associated_functions: HashMap::new(),
            concept_instances: HashMap::new(),
            fn_effects: HashMap::new(),

            struct_fields: HashMap::new(),

            return_type_stack: Vec::new(),
            bound_assumptions: Vec::new(),
            effect_context_stack: Vec::new(),
            effect_usage_stack: Vec::new(),
            lambda_effects: HashMap::new(),
            suspend_effect_checks: 0,
            errors: Vec::new(),
        };

        checker.register_builtin_type_arity();
        checker.register_builtin_variants();
        checker
    }

    fn register_builtin_type_arity(&mut self) {
        self.type_arity.insert("Option".into(), 1);
        self.type_arity.insert("Result".into(), 2);
        self.type_arity.insert("List".into(), 1);
        self.type_arity.insert("Map".into(), 2);
        self.type_arity.insert("Set".into(), 1);
        self.type_arity.insert("Range".into(), 1);
        self.type_arity.insert("ConstraintError".into(), 0);
    }

    fn register_builtin_variants(&mut self) {
        let a = self.fresh_var();
        let e = self.fresh_var();

        self.variant_info.insert(
            "Some".into(),
            VariantTypeInfo {
                parent_type: "Option".into(),
                parent_args: vec![a.clone()],
                field_types: vec![a.clone()],
            },
        );
        self.variant_info.insert(
            "None".into(),
            VariantTypeInfo {
                parent_type: "Option".into(),
                parent_args: vec![a.clone()],
                field_types: Vec::new(),
            },
        );

        self.variant_info.insert(
            "Ok".into(),
            VariantTypeInfo {
                parent_type: "Result".into(),
                parent_args: vec![a.clone(), e.clone()],
                field_types: vec![a],
            },
        );

        let a2 = self.fresh_var();
        let e2 = self.fresh_var();
        self.variant_info.insert(
            "Err".into(),
            VariantTypeInfo {
                parent_type: "Result".into(),
                parent_args: vec![a2, e2.clone()],
                field_types: vec![e2],
            },
        );

        self.variants_by_parent
            .insert("Option".into(), vec!["Some".into(), "None".into()]);
        self.variants_by_parent
            .insert("Result".into(), vec!["Ok".into(), "Err".into()]);
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
        // Pass 1: collect type definitions.
        for item in &resolved.module.items {
            if let Item::TypeDef(td) = item {
                self.collect_type_def(td);
            }
        }

        // Pass 2: collect concept definitions.
        for item in &resolved.module.items {
            if let Item::ConceptDef(cd) = item {
                self.collect_concept_def(cd);
            }
        }

        // Pass 3: collect instance definitions and build method tables.
        for item in &resolved.module.items {
            if let Item::InstanceDef(inst) = item {
                self.collect_instance_def(inst);
            }
        }
        self.check_instance_coherence();

        // Pass 4: collect standalone type annotations.
        let mut annotations: HashMap<String, TypeExpr> = HashMap::new();
        for item in &resolved.module.items {
            if let Item::TypeAnnotation(ta) = item {
                annotations.insert(ta.name.clone(), ta.ty.clone());
            }
        }

        // Pass 5: build function schemes.
        for item in &resolved.module.items {
            if let Item::Function(f) = item {
                let Some(def_id) = self.lookup_function_def_id(&f.name, resolved) else {
                    continue;
                };

                let scheme = self.function_scheme(f, annotations.get(&f.name));
                self.def_types.insert(def_id, scheme.ty.clone());
                self.def_schemes.insert(def_id, scheme);

                let effect_scheme = self.function_effect_scheme(f, annotations.get(&f.name));
                self.fn_effects.insert(def_id, effect_scheme);
            }
        }

        // Pass 6: type-check instance method bodies.
        for item in &resolved.module.items {
            if let Item::InstanceDef(inst) = item {
                self.check_instance_methods(inst, resolved);
            }
        }

        // Pass 7: type-check function bodies.
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

    fn lookup_function_def_id(&self, name: &str, resolved: &ResolvedModule) -> Option<DefId> {
        for (id, info) in &resolved.defs {
            if info.name == name && matches!(info.kind, DefKind::Function) {
                return Some(*id);
            }
        }
        None
    }

    fn collect_type_def(&mut self, td: &TypeDef) {
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
                    StructTypeInfo {
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
                        VariantTypeInfo {
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
                        RefinedTypeInfo {
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
                    let ctor_sig = TypeScheme::monomorphic(Type::Function(vec![base], Box::new(ret)));
                    self.associated_functions
                        .entry(td.name.clone())
                        .or_default()
                        .insert("new".into(), ctor_sig);
                }
            }
        }
    }

    fn collect_concept_def(&mut self, cd: &ConceptDef) {
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
        let mut assoc_var_ids: HashMap<String, TypeVarId> = HashMap::new();
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
            let scheme =
                self.concept_method_scheme(method, &self_ty, &assoc_placeholders, self_var, &assoc_var_ids);
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

    fn concept_method_scheme(
        &mut self,
        method: &ConceptMethodSig,
        self_ty: &Type,
        assoc_map: &HashMap<String, Type>,
        self_var: TypeVarId,
        assoc_var_ids: &HashMap<String, TypeVarId>,
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

    fn collect_instance_def(&mut self, inst: &InstanceDef) {
        let mut tvars = HashMap::new();
        let (target_ty, target_name, concept_name_opt) = match &inst.kind {
            InstanceKind::Inherent(target) => {
                let target_ty = self.resolve_type_expr_with_vars(target, &mut tvars);
                let target_name = self.type_name_key(&target_ty).unwrap_or_else(|| "<unknown>".into());
                (target_ty, target_name, None)
            }
            InstanceKind::Concept { concept, for_type } => {
                let target_ty = self.resolve_type_expr_with_vars(for_type, &mut tvars);
                let target_name = self.type_name_key(&target_ty).unwrap_or_else(|| "<unknown>".into());
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
                        let replacement = assoc_map.get(name).cloned().unwrap_or_else(|| self.fresh_var());
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

    fn check_instance_coherence(&mut self) {
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

    fn check_instance_methods(&mut self, inst: &InstanceDef, resolved: &ResolvedModule) {
        let mut tvars = HashMap::new();
        let (target_ty, target_name, concept_name_opt) = match &inst.kind {
            InstanceKind::Inherent(target) => {
                let target_ty = self.resolve_type_expr_with_vars(target, &mut tvars);
                let target_name = self.type_name_key(&target_ty).unwrap_or_else(|| "<unknown>".into());
                (target_ty, target_name, None)
            }
            InstanceKind::Concept { concept, for_type } => {
                let target_ty = self.resolve_type_expr_with_vars(for_type, &mut tvars);
                let target_name = self.type_name_key(&target_ty).unwrap_or_else(|| "<unknown>".into());
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
            } else if method.params.first().map(|p| p.name == "self").unwrap_or(false) {
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

    fn check_method_body(&mut self, method: &MethodDef, scheme: &TypeScheme, resolved: &ResolvedModule) {
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

    fn method_scheme_in_context(
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

    fn instantiate_concept_method_for_type(
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

    fn replace_special_type_names(
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

    fn type_name_key(&self, ty: &Type) -> Option<String> {
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

    fn resolve_binary_concept_method(
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

    fn function_scheme(&mut self, f: &FnDef, annotation: Option<&TypeExpr>) -> TypeScheme {
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
                    tvars.entry(b.ty_var.clone()).or_insert_with(|| self.fresh_var());
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

    fn function_effect_scheme(&mut self, f: &FnDef, annotation: Option<&TypeExpr>) -> FunctionEffectScheme {
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
                Some(TypeExpr::Function(_, _, effects, _)) => effects
                    .as_ref()
                    .map(|e| self.effect_spec_from_refs(e)),
                _ => None,
            };
            param_effects.push(spec);
        }

        FunctionEffectScheme {
            declared,
            param_effects,
        }
    }

    fn effect_spec_from_refs(&mut self, refs: &[EffectRef]) -> EffectSpec {
        let mut out = EffectSpec::default();
        for ef in refs {
            let term = self.effect_term_from_name(&ef.name, ef.span);
            out.terms.push(term);
        }
        out
    }

    fn effect_term_from_name(&mut self, name: &str, span: Span) -> EffectTerm {
        if name
            .chars()
            .next()
            .map(|c| c.is_ascii_lowercase())
            .unwrap_or(false)
        {
            return EffectTerm::Var(name.to_string());
        }

        if let Some(effect) = Self::effect_from_name(name) {
            EffectTerm::Concrete(effect)
        } else {
            self.errors.push(TypeError {
                message: format!("unknown effect '{}'", name),
                span,
            });
            EffectTerm::Var(name.to_string())
        }
    }

    fn effect_from_name(name: &str) -> Option<Effect> {
        match name {
            "Db.Read" => Some(Effect::DbRead),
            "Db.Write" => Some(Effect::DbWrite),
            "Net" => Some(Effect::Net),
            "Fs.Read" => Some(Effect::FsRead),
            "Fs.Write" => Some(Effect::FsWrite),
            "Log" => Some(Effect::Log),
            "Time" => Some(Effect::Time),
            "Random" => Some(Effect::Random),
            "Env" => Some(Effect::Env),
            _ => None,
        }
    }

    fn effect_implied(effect: Effect) -> Option<Effect> {
        match effect {
            Effect::DbWrite => Some(Effect::DbRead),
            Effect::FsWrite => Some(Effect::FsRead),
            _ => None,
        }
    }

    fn with_implied_effects(&self, effects: &HashSet<Effect>) -> HashSet<Effect> {
        let mut out = effects.clone();
        let mut changed = true;
        while changed {
            changed = false;
            let current: Vec<Effect> = out.iter().copied().collect();
            for e in current {
                if let Some(implied) = Self::effect_implied(e) {
                    if out.insert(implied) {
                        changed = true;
                    }
                }
            }
        }
        out
    }

    fn effect_vars(spec: &EffectSpec) -> HashSet<String> {
        let mut vars = HashSet::new();
        for t in &spec.terms {
            if let EffectTerm::Var(v) = t {
                vars.insert(v.clone());
            }
        }
        vars
    }

    fn effect_concretes(spec: &EffectSpec) -> HashSet<Effect> {
        let mut out = HashSet::new();
        for t in &spec.terms {
            if let EffectTerm::Concrete(e) = t {
                out.insert(*e);
            }
        }
        out
    }

    fn instantiate_effect_spec(
        &self,
        spec: &EffectSpec,
        bindings: &HashMap<String, HashSet<Effect>>,
    ) -> HashSet<Effect> {
        let mut out = HashSet::new();
        for t in &spec.terms {
            match t {
                EffectTerm::Concrete(e) => {
                    out.insert(*e);
                }
                EffectTerm::Var(v) => {
                    if let Some(bound) = bindings.get(v) {
                        out.extend(bound.iter().copied());
                    }
                }
            }
        }
        self.with_implied_effects(&out)
    }

    fn effect_set_to_string(&self, effects: &HashSet<Effect>) -> String {
        let mut names = effects
            .iter()
            .map(|e| match e {
                Effect::DbRead => "Db.Read",
                Effect::DbWrite => "Db.Write",
                Effect::Net => "Net",
                Effect::FsRead => "Fs.Read",
                Effect::FsWrite => "Fs.Write",
                Effect::Log => "Log",
                Effect::Time => "Time",
                Effect::Random => "Random",
                Effect::Env => "Env",
            })
            .collect::<Vec<_>>();
        names.sort();
        format!("[{}]", names.join(", "))
    }

    fn record_effect_usage(&mut self, effects: &HashSet<Effect>) {
        if let Some(top) = self.effect_usage_stack.last_mut() {
            top.extend(effects.iter().copied());
        }
    }

    fn callable_effects_from_expr(
        &self,
        expr: &Expr,
        resolved: &ResolvedModule,
    ) -> Option<HashSet<Effect>> {
        match expr {
            Expr::Lambda(_, _, _, span) => self.lambda_effects.get(&(span.start, span.end)).cloned(),
            Expr::Ident(_, span) => {
                let id = resolved.references.get(&(span.start, span.end))?;
                let scheme = self.fn_effects.get(id)?;
                Some(self.with_implied_effects(&Self::effect_concretes(&scheme.declared)))
            }
            _ => None,
        }
    }

    fn infer_call_required_effects(
        &mut self,
        callee_id: DefId,
        args: &[Expr],
        resolved: &ResolvedModule,
        span: Span,
    ) -> HashSet<Effect> {
        let Some(scheme) = self.fn_effects.get(&callee_id).cloned() else {
            return HashSet::new();
        };

        let mut bindings: HashMap<String, HashSet<Effect>> = HashMap::new();
        for (arg, param_spec_opt) in args.iter().zip(scheme.param_effects.iter()) {
            let Some(param_spec) = param_spec_opt else {
                continue;
            };
            let Some(arg_effects) = self.callable_effects_from_expr(arg, resolved) else {
                continue;
            };

            let required = self.with_implied_effects(&Self::effect_concretes(param_spec));
            if !required.is_subset(&arg_effects) {
                self.errors.push(TypeError {
                    message: format!(
                        "call argument callback effects {} do not satisfy required {}",
                        self.effect_set_to_string(&arg_effects),
                        self.effect_set_to_string(&required)
                    ),
                    span: arg.span(),
                });
            }

            let vars = Self::effect_vars(param_spec).into_iter().collect::<Vec<_>>();
            let extra = arg_effects
                .difference(&required)
                .copied()
                .collect::<HashSet<_>>();

            if vars.is_empty() && !extra.is_empty() {
                self.errors.push(TypeError {
                    message: format!(
                        "effectful callback {} is not allowed by parameter effect annotation",
                        self.effect_set_to_string(&arg_effects)
                    ),
                    span: arg.span(),
                });
            }

            for v in vars {
                bindings.entry(v).or_default().extend(extra.iter().copied());
            }
        }

        let required = self.instantiate_effect_spec(&scheme.declared, &bindings);
        self.check_call_effects(&required, span);
        required
    }

    fn check_call_effects(&mut self, required: &HashSet<Effect>, span: Span) {
        self.record_effect_usage(required);

        if self.suspend_effect_checks > 0 {
            return;
        }

        let Some(ctx) = self.effect_context_stack.last() else {
            return;
        };
        let available = self.with_implied_effects(&ctx.allowed);
        let missing = required
            .difference(&available)
            .copied()
            .collect::<HashSet<_>>();

        if !missing.is_empty() && ctx.vars.is_empty() {
            self.errors.push(TypeError {
                message: format!(
                    "missing capabilities: required {}, available {}",
                    self.effect_set_to_string(required),
                    self.effect_set_to_string(&available)
                ),
                span,
            });
        }
    }

    fn can_convert_error_type(&mut self, source: &Type, target: &Type) -> bool {
        if self.apply(source) == self.apply(target) {
            return true;
        }

        let Some(source_name) = self.type_name_key(source) else {
            return false;
        };
        let Some(target_name) = self.type_name_key(target) else {
            return false;
        };

        if self
            .auto_from_variants
            .contains_key(&(source_name.clone(), target_name.clone()))
        {
            return true;
        }

        if let Some(inst) = self
            .concept_instances
            .get(&("From".to_string(), target_name.clone()))
        {
            if let Some(scheme) = inst.methods.get("from").cloned() {
                let (fn_ty, _) = self.instantiate_scheme_with_bounds(&scheme);
                if let Type::Function(params, ret) = fn_ty {
                    if params.len() == 1
                        && self.apply(&params[0]) == self.apply(source)
                        && self.apply(&ret) == self.apply(target)
                    {
                        return true;
                    }
                }
            }
        }

        false
    }

    fn check_refined_constraint_on_literal(
        &self,
        refined: &str,
        base_expr: &Expr,
        info: &RefinedTypeInfo,
    ) -> Option<bool> {
        self.eval_refined_constraint(refined, base_expr, &info.constraint)
    }

    fn eval_refined_constraint(&self, refined: &str, value: &Expr, expr: &Expr) -> Option<bool> {
        let _ = refined;
        match expr {
            Expr::Binary(lhs, BinOp::And, rhs, _) => {
                Some(self.eval_refined_constraint(refined, value, lhs)? && self.eval_refined_constraint(refined, value, rhs)?)
            }
            Expr::Binary(lhs, BinOp::Or, rhs, _) => {
                Some(self.eval_refined_constraint(refined, value, lhs)? || self.eval_refined_constraint(refined, value, rhs)?)
            }
            Expr::Unary(UnaryOp::Not, inner, _) => Some(!self.eval_refined_constraint(refined, value, inner)?),
            Expr::Binary(lhs, op, rhs, _) => {
                let lv = self.eval_constraint_numeric(value, lhs)?;
                let rv = self.eval_constraint_numeric(value, rhs)?;
                match op {
                    BinOp::Eq => Some((lv - rv).abs() < f64::EPSILON),
                    BinOp::NotEq => Some((lv - rv).abs() >= f64::EPSILON),
                    BinOp::Lt => Some(lv < rv),
                    BinOp::Gt => Some(lv > rv),
                    BinOp::LtEq => Some(lv <= rv),
                    BinOp::GtEq => Some(lv >= rv),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn eval_constraint_numeric(&self, self_value: &Expr, expr: &Expr) -> Option<f64> {
        match expr {
            Expr::Ident(name, _) if name == "self" => match self_value {
                Expr::IntLit(n, _) => Some(*n as f64),
                Expr::FloatLit(n, _) => Some(*n),
                _ => None,
            },
            Expr::IntLit(n, _) => Some(*n as f64),
            Expr::FloatLit(n, _) => Some(*n),
            Expr::Binary(lhs, BinOp::Add, rhs, _) => {
                Some(self.eval_constraint_numeric(self_value, lhs)? + self.eval_constraint_numeric(self_value, rhs)?)
            }
            Expr::Binary(lhs, BinOp::Sub, rhs, _) => {
                Some(self.eval_constraint_numeric(self_value, lhs)? - self.eval_constraint_numeric(self_value, rhs)?)
            }
            Expr::Binary(lhs, BinOp::Mul, rhs, _) => {
                Some(self.eval_constraint_numeric(self_value, lhs)? * self.eval_constraint_numeric(self_value, rhs)?)
            }
            Expr::Binary(lhs, BinOp::Div, rhs, _) => {
                Some(self.eval_constraint_numeric(self_value, lhs)? / self.eval_constraint_numeric(self_value, rhs)?)
            }
            Expr::Binary(lhs, BinOp::Mod, rhs, _) => {
                Some(self.eval_constraint_numeric(self_value, lhs)? % self.eval_constraint_numeric(self_value, rhs)?)
            }
            _ => None,
        }
    }

    fn validate_refined_constraint(&mut self, expr: &Expr, span: Span) {
        if !self.is_valid_refined_constraint_expr(expr) {
            self.errors.push(TypeError {
                message: "invalid refined constraint: only comparisons, boolean operators, arithmetic, self/field access, and limited built-in methods are allowed".into(),
                span,
            });
        }
    }

    fn is_valid_refined_constraint_expr(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Ident(name, _) => {
                name == "self"
                    || name
                        .chars()
                        .next()
                        .map(|c| c.is_ascii_uppercase())
                        .unwrap_or(false)
            }
            Expr::IntLit(_, _)
            | Expr::FloatLit(_, _)
            | Expr::StringLit(_, _)
            | Expr::BoolLit(_, _) => true,
            Expr::Unary(UnaryOp::Not | UnaryOp::Neg, inner, _) => {
                self.is_valid_refined_constraint_expr(inner)
            }
            Expr::Binary(lhs, op, rhs, _) => matches!(
                op,
                BinOp::Eq
                    | BinOp::NotEq
                    | BinOp::Lt
                    | BinOp::Gt
                    | BinOp::LtEq
                    | BinOp::GtEq
                    | BinOp::And
                    | BinOp::Or
                    | BinOp::Add
                    | BinOp::Sub
                    | BinOp::Mul
                    | BinOp::Div
                    | BinOp::Mod
            ) && self.is_valid_refined_constraint_expr(lhs)
                && self.is_valid_refined_constraint_expr(rhs),
            Expr::FieldAccess(base, _, _) => self.is_valid_refined_constraint_expr(base),
            Expr::MethodCall(base, name, args, _) => {
                matches!(name.as_str(), "len" | "matches" | "contains")
                    && self.is_valid_refined_constraint_expr(base)
                    && args.iter().all(|a| self.is_valid_refined_constraint_expr(a))
            }
            _ => false,
        }
    }

    fn check_function(&mut self, f: &FnDef, resolved: &ResolvedModule) {
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

        let effect_scheme = self
            .fn_effects
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

    fn infer_expr(&mut self, expr: &Expr, env: &mut TypeEnv, resolved: &ResolvedModule) -> Type {
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
                let lhs_ty = self.infer_expr(lhs, env, resolved);
                let rhs_ty = self.infer_expr(rhs, env, resolved);

                let result_ty = match op {
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                        self.unify(&lhs_ty, &rhs_ty, *span);
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
                                self.resolve_binary_concept_method(concept, method, &lhs_ty, &rhs_ty, *span)
                            {
                                overloaded_ret
                            } else {
                                self.errors.push(TypeError {
                                    message: format!(
                                        "arithmetic operator expects numeric type or '{}' instance, got {}",
                                        concept, unified
                                    ),
                                    span: *span,
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
                        self.unify(&lhs_ty, &rhs_ty, *span);
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
                                    span: *span,
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
                                span: *span,
                            });
                        }
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
                let lhs_ty = self.infer_expr(lhs, env, resolved);

                let result = match rhs.as_ref() {
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
                            *span,
                        );

                        if let Expr::Ident(_, cspan) = callee.as_ref() {
                            if let Some(def_id) =
                                resolved.references.get(&(cspan.start, cspan.end))
                            {
                                let mut full_args = Vec::with_capacity(args.len() + 1);
                                full_args.push((**lhs).clone());
                                full_args.extend(args.iter().cloned());
                                self.infer_call_required_effects(*def_id, &full_args, resolved, *span);
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
                            *span,
                        );
                        self.apply(&ret_ty)
                    }
                };

                self.record_type(*span, result.clone());
                result
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
                if let Expr::Ident(name, _) = callee.as_ref() {
                    if let Some(vinfo) = self.variant_info.get(name).cloned() {
                        let result = self.check_variant_constructor_call(
                            name, &vinfo, args, env, resolved, *span,
                        );
                        self.record_type(*span, result.clone());
                        return result;
                    }
                }

                if let Expr::Ident(_, cspan) = callee.as_ref() {
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
                                        span: *span,
                                    });
                                }

                                for (arg, param_ty) in args.iter().zip(params.iter()) {
                                    let arg_ty = self.infer_expr(arg, env, resolved);
                                    self.unify(&arg_ty, param_ty, arg.span());
                                }

                                self.check_instantiated_bounds(&bounds, *span);
                                self.infer_call_required_effects(*def_id, args, resolved, *span);
                                let result = self.apply(&ret);
                                self.record_type(*span, result.clone());
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
                    *span,
                );

                let result = self.apply(&ret_ty);
                self.record_type(*span, result.clone());
                result
            }
            Expr::MethodCall(receiver, method, args, span) => {
                // Explicit concept disambiguation: Concept.method(value, ...)
                if let Expr::Ident(concept_name, recv_span) = receiver.as_ref() {
                    if let Some(id) = resolved.references.get(&(recv_span.start, recv_span.end)) {
                        if let Some(info) = resolved.defs.get(id) {
                            if matches!(info.kind, DefKind::Concept) {
                                if args.is_empty() {
                                    self.errors.push(TypeError {
                                        message: format!(
                                            "explicit concept method '{}.{}' requires a receiver argument",
                                            concept_name, method
                                        ),
                                        span: *span,
                                    });
                                    self.record_type(*span, Type::Error);
                                    return Type::Error;
                                }

                                let recv_arg_ty = self.infer_expr(&args[0], env, resolved);
                                let scheme = if let Some(recv_name) = self.type_name_key(&recv_arg_ty) {
                                    self.concept_instances
                                        .get(&(concept_name.clone(), recv_name))
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
                                        span: *span,
                                    });
                                    self.record_type(*span, Type::Error);
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
                                            span: *span,
                                        });
                                    }
                                    for (arg, expected) in args.iter().zip(params.iter()) {
                                        let arg_ty = self.infer_expr(arg, env, resolved);
                                        self.unify(&arg_ty, expected, arg.span());
                                    }
                                    self.check_instantiated_bounds(&bounds, *span);
                                    let result = self.apply(&ret);
                                    self.record_type(*span, result.clone());
                                    return result;
                                }
                            }
                        }
                    }
                }

                // Associated function call: TypeName.method(args)
                if let Expr::Ident(type_name, recv_span) = receiver.as_ref() {
                    if let Some(id) = resolved.references.get(&(recv_span.start, recv_span.end)) {
                        if let Some(info) = resolved.defs.get(id) {
                            if matches!(info.kind, DefKind::Type) {
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
                                                    span: *span,
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
                                            self.check_instantiated_bounds(&bounds, *span);
                                            let result = self.apply(&ret);
                                            self.record_type(*span, result.clone());
                                            return result;
                                        }
                                    }
                                }
                                self.errors.push(TypeError {
                                    message: format!(
                                        "unknown associated function '{}.{}'",
                                        type_name, method
                                    ),
                                    span: *span,
                                });
                                self.record_type(*span, Type::Error);
                                return Type::Error;
                            }
                        }
                    }
                }

                let recv_ty = self.infer_expr(receiver, env, resolved);
                let recv_ty = self.apply(&recv_ty);
                if let Some(recv_name) = self.type_name_key(&recv_ty) {
                    if let Some(methods) = self.inherent_methods.get(&recv_name) {
                        if let Some(scheme) = methods.get(method).cloned() {
                            let (fn_ty, bounds) = self.instantiate_scheme_with_bounds(&scheme);
                            if let Type::Function(params, ret) = fn_ty {
                                if params.is_empty() {
                                    self.errors.push(TypeError {
                                        message: format!(
                                            "method '{}' on '{}' has no receiver parameter",
                                            method, recv_name
                                        ),
                                        span: *span,
                                    });
                                    self.record_type(*span, Type::Error);
                                    return Type::Error;
                                }
                                self.unify(&recv_ty, &params[0], *span);
                                if params.len() - 1 != args.len() {
                                    self.errors.push(TypeError {
                                        message: format!(
                                            "method '{}.{}' expects {} argument(s), got {}",
                                            recv_name,
                                            method,
                                            params.len() - 1,
                                            args.len()
                                        ),
                                        span: *span,
                                    });
                                }
                                for (arg, expected) in args.iter().zip(params.iter().skip(1)) {
                                    let arg_ty = self.infer_expr(arg, env, resolved);
                                    self.unify(&arg_ty, expected, arg.span());
                                }
                                self.check_instantiated_bounds(&bounds, *span);
                                let result = self.apply(&ret);
                                self.record_type(*span, result.clone());
                                return result;
                            }
                        }
                    }

                    let mut concept_candidates: Vec<(String, TypeScheme)> = Vec::new();
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
                            span: *span,
                        });
                        self.record_type(*span, Type::Error);
                        return Type::Error;
                    }

                    if let Some((_, scheme)) = concept_candidates.into_iter().next() {
                        let (fn_ty, bounds) = self.instantiate_scheme_with_bounds(&scheme);
                        if let Type::Function(params, ret) = fn_ty {
                            if params.is_empty() {
                                self.errors.push(TypeError {
                                    message: format!(
                                        "concept method '{}' on '{}' has no receiver parameter",
                                        method, recv_name
                                    ),
                                    span: *span,
                                });
                                self.record_type(*span, Type::Error);
                                return Type::Error;
                            }
                            self.unify(&recv_ty, &params[0], *span);
                            if params.len() - 1 != args.len() {
                                self.errors.push(TypeError {
                                    message: format!(
                                        "method '{}.{}' expects {} argument(s), got {}",
                                        recv_name,
                                        method,
                                        params.len() - 1,
                                        args.len()
                                    ),
                                    span: *span,
                                });
                            }
                            for (arg, expected) in args.iter().zip(params.iter().skip(1)) {
                                let arg_ty = self.infer_expr(arg, env, resolved);
                                self.unify(&arg_ty, expected, arg.span());
                            }
                            self.check_instantiated_bounds(&bounds, *span);
                            let result = self.apply(&ret);
                            self.record_type(*span, result.clone());
                            return result;
                        }
                    }

                    self.errors.push(TypeError {
                        message: format!("unknown method '{}' for type '{}'", method, recv_name),
                        span: *span,
                    });
                    self.record_type(*span, Type::Error);
                    Type::Error
                } else if let Type::Var(v) = recv_ty {
                    let assumed_concepts = self.assumed_concepts_for_var(v);
                    let mut candidates: Vec<(String, TypeScheme)> = Vec::new();
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
                            span: *span,
                        });
                        self.record_type(*span, Type::Error);
                        return Type::Error;
                    }

                    if let Some((_, scheme)) = candidates.into_iter().next() {
                        let (fn_ty, bounds) = self.instantiate_scheme_with_bounds(&scheme);
                        if let Type::Function(params, ret) = fn_ty {
                            if params.is_empty() {
                                self.record_type(*span, Type::Error);
                                return Type::Error;
                            }
                            self.unify(&Type::Var(v), &params[0], *span);
                            if params.len() - 1 != args.len() {
                                self.errors.push(TypeError {
                                    message: format!(
                                        "method '{}' expects {} argument(s), got {}",
                                        method,
                                        params.len() - 1,
                                        args.len()
                                    ),
                                    span: *span,
                                });
                            }
                            for (arg, expected) in args.iter().zip(params.iter().skip(1)) {
                                let arg_ty = self.infer_expr(arg, env, resolved);
                                self.unify(&arg_ty, expected, arg.span());
                            }
                            self.check_instantiated_bounds(&bounds, *span);
                            let result = self.apply(&ret);
                            self.record_type(*span, result.clone());
                            return result;
                        }
                    }

                    self.errors.push(TypeError {
                        message: format!("unknown method '{}' for type variable {}", method, v.0),
                        span: *span,
                    });
                    self.record_type(*span, Type::Error);
                    Type::Error
                } else {
                    self.errors.push(TypeError {
                        message: format!("type '{}' has no methods", recv_ty),
                        span: *span,
                    });
                    self.record_type(*span, Type::Error);
                    Type::Error
                }
            }
            Expr::FieldAccess(receiver, field_name, span) => {
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
                                    span: *span,
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
                                        span: *span,
                                    });
                                    Type::Error
                                }
                            } else {
                                self.errors.push(TypeError {
                                    message: format!("type '{}' does not support field access", name),
                                    span: *span,
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
                            span: *span,
                        });
                        Type::Error
                    }
                };

                self.record_type(*span, ty.clone());
                ty
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
                                        span: *span,
                                    });
                                }
                            }
                            other => {
                                self.errors.push(TypeError {
                                    message: format!(
                                        "cannot use '?' on Result in function returning {}",
                                        other
                                    ),
                                    span: *span,
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
                                    span: *span,
                                });
                            }
                        }
                        args[0].clone()
                    }
                    other => {
                        self.errors.push(TypeError {
                            message: format!("'?' expects Result or Option, got {}", other),
                            span: *span,
                        });
                        Type::Error
                    }
                };

                self.record_type(*span, result.clone());
                result
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

    fn variant_value_type(&mut self, name: &str, span: Span) -> Type {
        let Some(info) = self.variant_info.get(name).cloned() else {
            let ty = self.fresh_var();
            self.record_type(span, ty.clone());
            return ty;
        };

        let inst = self.instantiate_variant_info(&info);
        if inst.field_types.is_empty() {
            let ty = Type::Named(inst.parent_type, inst.parent_args);
            self.record_type(span, ty.clone());
            ty
        } else {
            self.errors.push(TypeError {
                message: format!(
                    "variant '{}' requires {} payload argument(s)",
                    name,
                    inst.field_types.len()
                ),
                span,
            });
            let ty = Type::Named(inst.parent_type, inst.parent_args);
            self.record_type(span, ty.clone());
            ty
        }
    }

    fn check_variant_constructor_call(
        &mut self,
        name: &str,
        template: &VariantTypeInfo,
        args: &[Expr],
        env: &mut TypeEnv,
        resolved: &ResolvedModule,
        span: Span,
    ) -> Type {
        let inst = self.instantiate_variant_info(template);

        if args.len() != inst.field_types.len() {
            self.errors.push(TypeError {
                message: format!(
                    "variant '{}' expects {} payload argument(s), got {}",
                    name,
                    inst.field_types.len(),
                    args.len()
                ),
                span,
            });
        }

        for (arg, expected_ty) in args.iter().zip(inst.field_types.iter()) {
            let arg_ty = self.infer_expr(arg, env, resolved);
            self.unify(&arg_ty, expected_ty, arg.span());
        }

        Type::Named(inst.parent_type, inst.parent_args)
    }

    fn check_struct_literal(
        &mut self,
        name: &str,
        fields: &[(String, Expr)],
        env: &mut TypeEnv,
        resolved: &ResolvedModule,
        span: Span,
    ) -> Type {
        let Some(info) = self.struct_info.get(name).cloned() else {
            self.errors.push(TypeError {
                message: format!("unknown struct type '{}'", name),
                span,
            });
            for (_, expr) in fields {
                self.infer_expr(expr, env, resolved);
            }
            return Type::Error;
        };

        let (fresh_args, fresh_fields) = self.instantiate_struct_info(&info);
        let mut expected_by_name: HashMap<&str, Type> = HashMap::new();
        for (fname, fty) in &fresh_fields {
            expected_by_name.insert(fname, fty.clone());
        }

        let mut seen = HashSet::new();

        for (fname, fexpr) in fields {
            if !seen.insert(fname.as_str()) {
                self.errors.push(TypeError {
                    message: format!("duplicate field '{}' in struct literal", fname),
                    span: fexpr.span(),
                });
            }

            let actual = self.infer_expr(fexpr, env, resolved);
            if let Some(expected) = expected_by_name.get(fname.as_str()) {
                self.unify(&actual, expected, fexpr.span());
            } else {
                self.errors.push(TypeError {
                    message: format!("struct '{}' has no field '{}'", name, fname),
                    span: fexpr.span(),
                });
            }
        }

        for (fname, _) in &fresh_fields {
            if !seen.contains(fname.as_str()) {
                self.errors.push(TypeError {
                    message: format!("missing field '{}' in struct '{}' literal", fname, name),
                    span,
                });
            }
        }

        Type::Named(
            name.to_string(),
            fresh_args.into_iter().map(|t| self.apply(&t)).collect(),
        )
    }

    fn check_with_expr(
        &mut self,
        base_ty: &Type,
        fields: &[(String, Expr)],
        env: &mut TypeEnv,
        resolved: &ResolvedModule,
        span: Span,
    ) -> Type {
        let base_ty = self.apply(base_ty);
        let (name, args) = match &base_ty {
            Type::Named(name, args) => (name.clone(), args.clone()),
            other => {
                self.errors.push(TypeError {
                    message: format!("'with' expects a struct value, got {}", other),
                    span,
                });
                for (_, expr) in fields {
                    self.infer_expr(expr, env, resolved);
                }
                return Type::Error;
            }
        };

        let Some(instantiated_fields) = self.instantiate_struct_fields_for_args(&name, &args)
        else {
            self.errors.push(TypeError {
                message: format!("'with' is only valid on struct types, got {}", name),
                span,
            });
            for (_, expr) in fields {
                self.infer_expr(expr, env, resolved);
            }
            return Type::Error;
        };

        let mut expected_by_name: HashMap<&str, Type> = HashMap::new();
        for (fname, fty) in &instantiated_fields {
            expected_by_name.insert(fname, fty.clone());
        }

        let mut seen = HashSet::new();
        for (fname, fexpr) in fields {
            if !seen.insert(fname.as_str()) {
                self.errors.push(TypeError {
                    message: format!("duplicate field '{}' in with-expression", fname),
                    span: fexpr.span(),
                });
            }

            let actual = self.infer_expr(fexpr, env, resolved);
            if let Some(expected) = expected_by_name.get(fname.as_str()) {
                self.unify(&actual, expected, fexpr.span());
            } else {
                self.errors.push(TypeError {
                    message: format!("type '{}' has no field '{}'", name, fname),
                    span: fexpr.span(),
                });
            }
        }

        base_ty
    }

    fn bind_pattern(&mut self, pattern: &Pattern, expected: &Type, env: &mut TypeEnv, span: Span) {
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

    fn lit_pattern_type(&self, lit: &LitPattern) -> Type {
        match lit {
            LitPattern::Int(_) => Type::Int,
            LitPattern::Float(_) => Type::Float64,
            LitPattern::String(_) => Type::String,
            LitPattern::Bool(_) => Type::Bool,
        }
    }

    fn check_match_exhaustiveness(&mut self, scrut_ty: &Type, arms: &[MatchArm], span: Span) {
        let scrut_ty = self.apply(scrut_ty);

        let has_catch_all = arms.iter().any(|arm| {
            arm.guard.is_none() && self.pattern_has_catch_all(&arm.pattern)
        });

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

    fn pattern_has_catch_all(&self, pattern: &Pattern) -> bool {
        match pattern {
            Pattern::Wildcard(_) | Pattern::Ident(_, _) => true,
            Pattern::Or(patterns, _) => patterns.iter().any(|p| self.pattern_has_catch_all(p)),
            _ => false,
        }
    }

    fn pattern_constructors(&self, pattern: &Pattern, out: &mut HashSet<String>) {
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

    fn pattern_bool_literals(&self, pattern: &Pattern, out: &mut HashSet<bool>) {
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

    fn resolve_type_expr_with_vars(
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

    fn is_type_var_name(name: &str) -> bool {
        name.chars()
            .next()
            .map(|c| c.is_ascii_lowercase())
            .unwrap_or(false)
    }

    fn generalize(&self, ty: &Type, env: &TypeEnv) -> TypeScheme {
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

    fn instantiate_scheme(&mut self, scheme: &TypeScheme) -> Type {
        self.instantiate_scheme_with_bounds(scheme).0
    }

    fn instantiate_scheme_with_bounds(
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
                ty: self
                    .substitute_type(&Type::Var(b.type_var), &subst),
            })
            .collect();
        (ty, bounds)
    }

    fn check_instantiated_bounds(&mut self, bounds: &[InstantiatedBound], span: Span) {
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

    fn bounds_to_assumptions(
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

    fn has_assumed_bound(&self, var: TypeVarId, concept: &str) -> bool {
        self.bound_assumptions.iter().rev().any(|scope| {
            scope
                .get(&var)
                .map(|set| set.contains(concept))
                .unwrap_or(false)
        })
    }

    fn assumed_concepts_for_var(&self, var: TypeVarId) -> HashSet<String> {
        let mut out = HashSet::new();
        for scope in &self.bound_assumptions {
            if let Some(set) = scope.get(&var) {
                out.extend(set.iter().cloned());
            }
        }
        out
    }

    fn type_implements_concept(&self, ty: &Type, concept: &str) -> bool {
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

    fn free_type_vars_env(&self, env: &TypeEnv) -> HashSet<TypeVarId> {
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

    fn free_type_vars(&self, ty: &Type) -> HashSet<TypeVarId> {
        let mut out = HashSet::new();
        self.collect_free_type_vars(ty, &mut out);
        out
    }

    fn collect_free_type_vars(&self, ty: &Type, out: &mut HashSet<TypeVarId>) {
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

    fn instantiate_variant_info(&mut self, info: &VariantTypeInfo) -> VariantTypeInfo {
        let mut map: HashMap<TypeVarId, Type> = HashMap::new();
        self.collect_and_freshen_type_vars(&info.parent_args, &mut map);
        self.collect_and_freshen_type_vars(&info.field_types, &mut map);

        VariantTypeInfo {
            parent_type: info.parent_type.clone(),
            parent_args: info
                .parent_args
                .iter()
                .map(|t| self.substitute_type(t, &map))
                .collect(),
            field_types: info
                .field_types
                .iter()
                .map(|t| self.substitute_type(t, &map))
                .collect(),
        }
    }

    fn instantiate_struct_info(
        &mut self,
        info: &StructTypeInfo,
    ) -> (Vec<Type>, Vec<(String, Type)>) {
        let mut map: HashMap<TypeVarId, Type> = HashMap::new();
        self.collect_and_freshen_type_vars(&info.type_args, &mut map);
        self.collect_and_freshen_type_vars(
            &info
                .fields
                .iter()
                .map(|(_, t)| t.clone())
                .collect::<Vec<_>>(),
            &mut map,
        );

        let type_args = info
            .type_args
            .iter()
            .map(|t| self.substitute_type(t, &map))
            .collect();

        let fields = info
            .fields
            .iter()
            .map(|(name, ty)| (name.clone(), self.substitute_type(ty, &map)))
            .collect();

        (type_args, fields)
    }

    fn instantiate_struct_fields_for_args(
        &self,
        name: &str,
        args: &[Type],
    ) -> Option<Vec<(String, Type)>> {
        let info = self.struct_info.get(name)?;
        if info.type_args.len() != args.len() {
            return None;
        }

        let mut map: HashMap<TypeVarId, Type> = HashMap::new();
        for (template, actual) in info.type_args.iter().zip(args.iter()) {
            if let Type::Var(v) = template {
                map.insert(*v, actual.clone());
            }
        }

        Some(
            info.fields
                .iter()
                .map(|(fname, fty)| (fname.clone(), self.substitute_type(fty, &map)))
                .collect(),
        )
    }

    fn instantiate_refined_base_for_args(&self, name: &str, args: &[Type]) -> Option<Type> {
        let info = self.refined_info.get(name)?;
        if info.type_args.len() != args.len() {
            return None;
        }

        let mut map: HashMap<TypeVarId, Type> = HashMap::new();
        for (template, actual) in info.type_args.iter().zip(args.iter()) {
            if let Type::Var(v) = template {
                map.insert(*v, actual.clone());
            }
        }
        Some(self.substitute_type(&info.base_type, &map))
    }

    fn collect_and_freshen_type_vars(&mut self, tys: &[Type], map: &mut HashMap<TypeVarId, Type>) {
        for ty in tys {
            self.collect_and_freshen_type_var(ty, map);
        }
    }

    fn collect_and_freshen_type_var(&mut self, ty: &Type, map: &mut HashMap<TypeVarId, Type>) {
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

    fn substitute_type(&self, ty: &Type, subst: &HashMap<TypeVarId, Type>) -> Type {
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

    fn unify(&mut self, a: &Type, b: &Type, span: Span) {
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
             def main() -> Int = add(1, 2)",
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_struct_field_access() {
        let result = typecheck_str(
            "type User = { name: String, age: Int }\n\
             def test(u: User) -> String = u.name",
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_lambda_type() {
        let result = typecheck_str("def test() -> Int = { let f = (x: Int) -> x + 1; f(10) }");
        assert!(result.is_ok());
    }

    #[test]
    fn test_if_branch_mismatch() {
        let result = typecheck_str("def test(x: Bool) -> Int = if x { 1 } else { \"hello\" }");
        assert!(result.is_err());
    }

    #[test]
    fn test_logical_operators() {
        let result = typecheck_str("def test(a: Bool, b: Bool) -> Bool = a and b or not a");
        assert!(result.is_ok());
    }

    #[test]
    fn test_while_loop() {
        let result = typecheck_str("def test(x: Bool) -> Int = { while x { 0 }; 1 }");
        assert!(result.is_ok());
    }

    #[test]
    fn test_match_expression() {
        let result = typecheck_str("def test(x: Int) -> Int = match x { 1 => 10, _ => 0 }");
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
    fn test_let_pattern_destructuring() {
        let result = typecheck_str("def test() -> Int = { let (a, b) = (1, 2); a + b }");
        assert!(result.is_ok(), "{:?}", result.err());
    }

    #[test]
    fn test_nested_function_calls() {
        let result = typecheck_str(
            "def double(x: Int) -> Int = x * 2\n\
             def main() -> Int = double(double(5))",
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_generic_function_annotation() {
        let result = typecheck_str(
            "id: a -> a\n\
             def id(x) = x\n\
             def test() -> Int = id(42)",
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_let_polymorphism() {
        let result = typecheck_str(
            "def test() -> Int = {\n\
                let id = (x) -> x;\n\
                let a = id(1);\n\
                let b = id(2);\n\
                a + b\n\
            }",
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_variant_constructor_arity_error() {
        let result = typecheck_str(
            "type Status = Failed String\n\
             def test() -> Status = Failed()",
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_struct_literal_missing_field_error() {
        let result = typecheck_str(
            "type User = { name: String, age: Int }\n\
             def test() -> User = User { name: \"a\" }",
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_match_non_exhaustive_error() {
        let result = typecheck_str(
            "type OptionInt = Some Int | None\n\
             def test(x: OptionInt) -> Int = match x { Some(n) => n }",
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_type_arity_error() {
        let result = typecheck_str("def test(x: Option Int String) -> Int = 0");
        assert!(result.is_err());
    }

    #[test]
    fn test_with_field_typecheck() {
        let result = typecheck_str(
            "type User = { name: String, age: Int }\n\
             def test(u: User) -> User = u with { age: 42 }",
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_concept_method_call() {
        let result = typecheck_str(
            "type User = { name: String }\n\
             concept Display { def display(self) -> String }\n\
             instance Display for User { def display(self) -> String = self.name }\n\
             def test(u: User) -> String = u.display()",
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_associated_function_call() {
        let result = typecheck_str(
            "type User = { name: String }\n\
             instance User { def origin() -> User = User { name: \"root\" } }\n\
             def test() -> User = User.origin()",
        );
        assert!(result.is_ok(), "{:?}", result.err());
    }

    #[test]
    fn test_inherent_method_priority_over_concept() {
        let result = typecheck_str(
            "type User = { name: String }\n\
             concept Size { def size(self) -> Int }\n\
             instance Size for User { def size(self) -> Int = 1 }\n\
             instance User { def size(self) -> String = self.name }\n\
             def test(u: User) -> String = u.size()",
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_method_ambiguity_between_concepts() {
        let result = typecheck_str(
            "type User = { name: String }\n\
             concept A { def m(self) -> Int }\n\
             concept B { def m(self) -> Int }\n\
             instance A for User { def m(self) -> Int = 1 }\n\
             instance B for User { def m(self) -> Int = 2 }\n\
             def test(u: User) -> Int = u.m()",
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_concept_superclass_requirement() {
        let result = typecheck_str(
            "type User = { name: String }\n\
             concept Eq { def eq(self, other: Self) -> Bool }\n\
             concept Ord: Eq { def lt(self, other: Self) -> Bool }\n\
             instance Ord for User { def lt(self, other: Self) -> Bool = true }\n\
             def test() -> Int = 0",
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_forall_bound_success() {
        let result = typecheck_str(
            "type User = { name: String }\n\
             concept Display { def display(self) -> String }\n\
             instance Display for User { def display(self) -> String = self.name }\n\
             show: forall (Display a). a -> String\n\
             def show(x) = x.display()\n\
             def test(u: User) -> String = show(u)",
        );
        assert!(result.is_ok(), "{:?}", result.err());
    }

    #[test]
    fn test_forall_bound_failure() {
        let result = typecheck_str(
            "concept Display { def display(self) -> String }\n\
             show: forall (Display a). a -> String\n\
             def show(x) = x.display()\n\
             def test() -> String = show(1)",
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_operator_desugar_via_concept() {
        let result = typecheck_str(
            "type Vec2 = { x: Int }\n\
             concept Add { def add(self, other: Self) -> Int }\n\
             instance Add for Vec2 { def add(self, other: Self) -> Int = self.x + other.x }\n\
             def test(a: Vec2, b: Vec2) -> Int = a + b",
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_struct_pattern_with_rest() {
        let result = typecheck_str(
            "type User = { name: String, age: Int }\n\
             def test(u: User) -> String = match u { User { name, .. } => name }",
        );
        assert!(result.is_ok(), "{:?}", result.err());
    }

    #[test]
    fn test_struct_pattern_missing_fields_error() {
        let result = typecheck_str(
            "type User = { name: String, age: Int }\n\
             def test(u: User) -> String = match u { User { name } => name }",
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_or_pattern_exhaustive() {
        let result = typecheck_str(
            "def test(x: Bool) -> Int = match x { true | false => 1 }",
        );
        assert!(result.is_ok(), "{:?}", result.err());
    }

    #[test]
    fn test_or_pattern_binding_mismatch_error() {
        let result = typecheck_str(
            "type OptionInt = Some Int | None\n\
             def test(x: OptionInt) -> Int = match x { Some(n) | None => n }",
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_explicit_concept_disambiguation_call() {
        let result = typecheck_str(
            "type User = { name: String }\n\
             concept A { def m(self) -> Int }\n\
             concept B { def m(self) -> Int }\n\
             instance A for User { def m(self) -> Int = 1 }\n\
             instance B for User { def m(self) -> Int = 2 }\n\
             def test(u: User) -> Int = A.m(u)",
        );
        assert!(result.is_ok(), "{:?}", result.err());
    }

    #[test]
    fn test_concept_assoc_type_default() {
        let result = typecheck_str(
            "type Vec2 = { x: Int }\n\
             concept Add {\n\
               type Output = Int\n\
               def add(self, other: Self) -> Self.Output\n\
             }\n\
             instance Add for Vec2 {\n\
               def add(self, other: Self) -> Int = self.x + other.x\n\
             }\n\
             def test(a: Vec2, b: Vec2) -> Int = a.add(b)",
        );
        assert!(result.is_ok(), "{:?}", result.err());
    }

    #[test]
    fn test_effect_missing_capability_error() {
        let result = typecheck_str(
            "fetch: Int -> Int [Net]\n\
             def fetch(x) = x\n\
             def pure(x: Int) -> Int = fetch(x)",
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_effect_declared_capability_ok() {
        let result = typecheck_str(
            "fetch: Int -> Int [Net]\n\
             def fetch(x) = x\n\
             def caller(x: Int) -> Int [Net] = fetch(x)",
        );
        assert!(result.is_ok(), "{:?}", result.err());
    }

    #[test]
    fn test_effect_hierarchy_write_implies_read() {
        let result = typecheck_str(
            "read_db: Int -> Int [Db.Read]\n\
             def read_db(x) = x\n\
             def write_db(x: Int) -> Int [Db.Write] = read_db(x)",
        );
        assert!(result.is_ok(), "{:?}", result.err());
    }

    #[test]
    fn test_effect_polymorphism_from_callback() {
        let result = typecheck_str(
            "map1: (Int -> Int [e]) * Int -> Int [e]\n\
             def map1(f, x) = f(x)\n\
             fetch: Int -> Int [Net]\n\
             def fetch(x) = x\n\
             def caller(x: Int) -> Int [Net] = map1(fetch, x)",
        );
        assert!(result.is_ok(), "{:?}", result.err());
    }

    #[test]
    fn test_effect_polymorphism_missing_in_caller() {
        let result = typecheck_str(
            "map1: (Int -> Int [e]) * Int -> Int [e]\n\
             def map1(f, x) = f(x)\n\
             fetch: Int -> Int [Net]\n\
             def fetch(x) = x\n\
             def caller(x: Int) -> Int = map1(fetch, x)",
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_contracts_bool_typechecking() {
        let result = typecheck_str(
            "def f(x: Int) -> Int requires x > 0 ensures result > 0 = x",
        );
        assert!(result.is_ok(), "{:?}", result.err());
    }

    #[test]
    fn test_contract_requires_must_be_bool() {
        let result = typecheck_str("def f(x: Int) -> Int requires x + 1 = x");
        assert!(result.is_err());
    }

    #[test]
    fn test_try_option_success() {
        let result = typecheck_str(
            "head: List Int -> Option Int\n\
             def head(xs) = None\n\
             def test(xs: List Int) -> Option Int = { let n = head(xs)?; Some(n) }",
        );
        assert!(result.is_ok(), "{:?}", result.err());
    }

    #[test]
    fn test_try_option_wrong_return_type_error() {
        let result = typecheck_str(
            "head: List Int -> Option Int\n\
             def head(xs) = None\n\
             def test(xs: List Int) -> Int = { let n = head(xs)?; n }",
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_try_result_auto_from_conversion() {
        let result = typecheck_str(
            "type IoError = Failed\n\
             type AppError = Io IoError | Other String\n\
             load: Int -> Result Int IoError\n\
             def load(x) = Err(Failed)\n\
             def run(x: Int) -> Result Int AppError = { let v = load(x)?; Ok(v) }",
        );
        assert!(result.is_ok(), "{:?}", result.err());
    }

    #[test]
    fn test_try_result_missing_conversion_error() {
        let result = typecheck_str(
            "type E1 = A\n\
             type E2 = B\n\
             get: Int -> Result Int E1\n\
             def get(x) = Err(A)\n\
             def run(x: Int) -> Result Int E2 = { let v = get(x)?; Ok(v) }",
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_refined_type_new_and_value() {
        let result = typecheck_str(
            "type NonZero = Int where self != 0\n\
             def div(a: Int, b: NonZero) -> Int = a / b.value\n\
             def mk() -> Result NonZero ConstraintError = NonZero.new(5)",
        );
        assert!(result.is_ok(), "{:?}", result.err());
    }

    #[test]
    fn test_refined_literal_constraint_failure() {
        let result = typecheck_str(
            "type NonZero = Int where self != 0\n\
             def mk() -> Result NonZero ConstraintError = NonZero.new(0)",
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_refined_invalid_constraint_expression_error() {
        let result = typecheck_str(
            "type Bad = Int where ((x) -> x)(self)\n\
             def x() -> Int = 1",
        );
        assert!(result.is_err());
    }
}
