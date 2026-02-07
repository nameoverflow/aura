use std::collections::{HashMap, HashSet};

use aura_common::Span;
use aura_parser::ast::*;
use aura_resolve::ResolvedModule;

use crate::types::{Type, TypeVarId};

use super::{StructTypeInfo, TypeChecker, TypeEnv, TypeError, VariantTypeInfo};

impl TypeChecker {
    pub(crate) fn variant_value_type(&mut self, name: &str, span: Span) -> Type {
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

    pub(crate) fn check_variant_constructor_call(
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

    pub(crate) fn check_struct_literal(
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

    pub(crate) fn check_with_expr(
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

    pub(crate) fn instantiate_variant_info(&mut self, info: &VariantTypeInfo) -> VariantTypeInfo {
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

    pub(crate) fn instantiate_struct_info(
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

    pub(crate) fn instantiate_struct_fields_for_args(
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

    pub(crate) fn instantiate_refined_base_for_args(
        &self,
        name: &str,
        args: &[Type],
    ) -> Option<Type> {
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
}
