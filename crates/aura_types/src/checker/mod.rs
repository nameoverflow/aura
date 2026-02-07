use std::collections::{HashMap, HashSet};

use aura_common::Span;
use aura_parser::ast::*;
use aura_resolve::{DefId, DefKind, ResolvedModule};

use crate::types::{Type, TypeVarId};

mod collection;
mod constraints;
mod effects;
mod functions;
mod inference;
mod patterns;
mod resolution;
mod unification;
mod variants;

#[cfg(test)]
mod tests;

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
pub(crate) struct StructTypeInfo {
    pub(crate) type_args: Vec<Type>,
    pub(crate) fields: Vec<(String, Type)>,
}

#[derive(Debug, Clone)]
pub(crate) struct TypeScheme {
    pub(crate) quantified: Vec<TypeVarId>,
    pub(crate) bounds: Vec<ConceptBound>,
    pub(crate) ty: Type,
}

#[derive(Debug, Clone)]
pub(crate) struct ConceptBound {
    pub(crate) concept: String,
    pub(crate) type_var: TypeVarId,
}

#[derive(Debug, Clone)]
pub(crate) struct InstantiatedBound {
    pub(crate) concept: String,
    pub(crate) ty: Type,
}

impl TypeScheme {
    pub(crate) fn monomorphic(ty: Type) -> Self {
        Self {
            quantified: Vec::new(),
            bounds: Vec::new(),
            ty,
        }
    }
}

pub(crate) type TypeEnv = HashMap<String, TypeScheme>;

#[derive(Debug, Clone)]
pub(crate) struct ConceptInfo {
    pub(crate) supers: Vec<String>,
    pub(crate) assoc_types: Vec<String>,
    pub(crate) assoc_defaults: HashMap<String, Type>,
    pub(crate) self_var: TypeVarId,
    pub(crate) assoc_var_ids: HashMap<String, TypeVarId>,
    pub(crate) methods: HashMap<String, TypeScheme>,
    pub(crate) methods_with_default: HashSet<String>,
}

#[derive(Debug, Clone)]
#[allow(dead_code)] // assoc_types needed for P1+ associated type resolution in dispatch
pub(crate) struct ConceptInstanceInfo {
    pub(crate) concept: String,
    pub(crate) target_name: String,
    pub(crate) assoc_types: HashMap<String, Type>,
    pub(crate) methods: HashMap<String, TypeScheme>,
    pub(crate) span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct RefinedTypeInfo {
    pub(crate) type_args: Vec<Type>,
    pub(crate) base_type: Type,
    pub(crate) constraint: Expr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum Effect {
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
pub(crate) enum EffectTerm {
    Concrete(Effect),
    Var(String),
}

#[derive(Debug, Clone, Default)]
pub(crate) struct EffectSpec {
    pub(crate) terms: Vec<EffectTerm>,
}

#[derive(Debug, Clone)]
pub(crate) struct FunctionEffectScheme {
    pub(crate) declared: EffectSpec,
    pub(crate) param_effects: Vec<Option<EffectSpec>>,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct EffectContext {
    pub(crate) allowed: HashSet<Effect>,
    pub(crate) vars: HashSet<String>,
}

pub struct TypeChecker {
    pub(crate) next_var: u32,
    pub(crate) substitution: HashMap<TypeVarId, Type>,
    pub(crate) expr_types: HashMap<(u32, u32), Type>,
    pub(crate) def_types: HashMap<DefId, Type>,

    // Internal type metadata
    pub(crate) type_arity: HashMap<String, usize>,
    pub(crate) def_schemes: HashMap<DefId, TypeScheme>,
    pub(crate) struct_info: HashMap<String, StructTypeInfo>,
    pub(crate) variant_info: HashMap<String, VariantTypeInfo>,
    pub(crate) variants_by_parent: HashMap<String, Vec<String>>,
    pub(crate) refined_info: HashMap<String, RefinedTypeInfo>,
    pub(crate) auto_from_variants: HashMap<(String, String), String>,
    pub(crate) concepts: HashMap<String, ConceptInfo>,
    pub(crate) inherent_methods: HashMap<String, HashMap<String, TypeScheme>>,
    pub(crate) associated_functions: HashMap<String, HashMap<String, TypeScheme>>,
    pub(crate) concept_instances: HashMap<(String, String), ConceptInstanceInfo>,
    pub(crate) fn_effects: HashMap<DefId, FunctionEffectScheme>,
    /// Transparent type aliases: name -> (type_params, resolved_type)
    pub(crate) type_aliases: HashMap<String, (Vec<String>, Type)>,

    // Kept for typed output compatibility
    pub(crate) struct_fields: HashMap<String, Vec<(String, Type)>>,

    pub(crate) return_type_stack: Vec<Type>,
    pub(crate) bound_assumptions: Vec<HashMap<TypeVarId, HashSet<String>>>,
    pub(crate) effect_context_stack: Vec<EffectContext>,
    pub(crate) effect_usage_stack: Vec<HashSet<Effect>>,
    pub(crate) lambda_effects: HashMap<(u32, u32), HashSet<Effect>>,
    pub(crate) suspend_effect_checks: usize,
    pub(crate) errors: Vec<TypeError>,
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
            type_aliases: HashMap::new(),

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

    pub(crate) fn fresh_var(&mut self) -> Type {
        let id = TypeVarId(self.next_var);
        self.next_var += 1;
        Type::Var(id)
    }

    pub(crate) fn record_type(&mut self, span: Span, ty: Type) {
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

        // Pass 2b: type-check default concept method bodies.
        for item in &resolved.module.items {
            if let Item::ConceptDef(cd) = item {
                self.check_concept_default_bodies(cd, resolved);
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

    pub(crate) fn lookup_function_def_id(&self, name: &str, resolved: &ResolvedModule) -> Option<DefId> {
        for (id, info) in &resolved.defs {
            if info.name == name && matches!(info.kind, DefKind::Function) {
                return Some(*id);
            }
        }
        None
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}
