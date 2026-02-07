use std::collections::HashMap;

use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum};
use inkwell::AddressSpace;

use aura_parser::ast;
use aura_types::types::Type;

use super::{CodeGen, StructInfo, SumTypeInfo, VariantInfo};

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn register_type_def(&mut self, td: &ast::TypeDef) {
        match &td.kind {
            ast::TypeDefKind::Struct(fields) => {
                let field_names: Vec<String> = fields.iter().map(|f| f.name.clone()).collect();
                let field_types: Vec<Type> = fields
                    .iter()
                    .map(|f| self.type_expr_to_type(&f.ty))
                    .collect();
                let llvm_field_types: Vec<BasicTypeEnum<'ctx>> =
                    field_types.iter().map(|t| self.type_to_llvm(t)).collect();

                let struct_type = self.context.struct_type(&llvm_field_types, false);
                self.struct_types.insert(
                    td.name.clone(),
                    StructInfo {
                        llvm_type: struct_type,
                        field_names,
                        field_types,
                    },
                );
            }
            ast::TypeDefKind::Sum(variants) => {
                // Build tagged union: { i64 tag, payload... }
                // Compute the max payload size across all variants.
                let i64_type = self.context.i64_type();
                let mut max_payload_fields: Vec<BasicTypeEnum<'ctx>> = Vec::new();
                let mut variant_map: HashMap<String, (u64, Vec<Type>)> = HashMap::new();

                for (i, variant) in variants.iter().enumerate() {
                    let payload_types: Vec<Type> = variant
                        .fields
                        .iter()
                        .map(|f| self.type_expr_to_type(f))
                        .collect();

                    let payload_llvm: Vec<BasicTypeEnum<'ctx>> =
                        payload_types.iter().map(|t| self.type_to_llvm(t)).collect();

                    // Track the variant with the most payload fields
                    if payload_llvm.len() > max_payload_fields.len() {
                        max_payload_fields = payload_llvm;
                    }

                    self.variant_info.insert(
                        variant.name.clone(),
                        VariantInfo {
                            parent_type: td.name.clone(),
                            tag: i as u64,
                            payload_types: payload_types.clone(),
                        },
                    );

                    variant_map.insert(variant.name.clone(), (i as u64, payload_types));
                }

                // The sum type struct: { i64 tag, payload_field_0, payload_field_1, ... }
                let mut fields: Vec<BasicTypeEnum<'ctx>> = vec![i64_type.into()];
                if !max_payload_fields.is_empty() {
                    fields.extend(max_payload_fields);
                }

                let sum_llvm_type = self.context.struct_type(&fields, false);

                self.sum_types.insert(
                    td.name.clone(),
                    SumTypeInfo {
                        llvm_type: sum_llvm_type,
                        variants: variant_map,
                        name: td.name.clone(),
                    },
                );
            }
            _ => {}
        }
    }

    pub(crate) fn type_to_llvm(&self, ty: &Type) -> BasicTypeEnum<'ctx> {
        match ty {
            Type::Int | Type::Int64 => self.context.i64_type().into(),
            Type::Int8 => self.context.i8_type().into(),
            Type::Int16 => self.context.i16_type().into(),
            Type::Int32 => self.context.i32_type().into(),
            Type::UInt | Type::UInt64 => self.context.i64_type().into(),
            Type::UInt8 => self.context.i8_type().into(),
            Type::UInt16 => self.context.i16_type().into(),
            Type::UInt32 => self.context.i32_type().into(),
            Type::Float32 => self.context.f32_type().into(),
            Type::Float64 | Type::Decimal | Type::BigDecimal => self.context.f64_type().into(),
            Type::Bool => self.context.bool_type().into(),
            Type::Char => self.context.i32_type().into(),
            Type::String => self
                .context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .into(),
            Type::Unit => self.context.i8_type().into(),
            Type::Named(name, _) => {
                if let Some(info) = self.struct_types.get(name) {
                    info.llvm_type.into()
                } else if let Some(info) = self.sum_types.get(name) {
                    info.llvm_type.into()
                } else {
                    self.context.i64_type().into() // Fallback for non-struct named types
                }
            }
            Type::Product(_) => self.context.i64_type().into(), // Placeholder
            Type::Function(_, _) => self
                .context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .into(),
            Type::Var(_) | Type::Error => self.context.i64_type().into(),
        }
    }

    pub(crate) fn type_to_llvm_meta(&self, ty: &Type) -> BasicMetadataTypeEnum<'ctx> {
        self.type_to_llvm(ty).into()
    }

    pub(crate) fn resolve_param_type(&self, param: &ast::Param) -> Type {
        match &param.ty {
            Some(te) => self.type_expr_to_type(te),
            None => Type::Int, // Default to Int for untyped params in P0
        }
    }

    pub(crate) fn type_expr_to_type(&self, te: &ast::TypeExpr) -> Type {
        match te {
            ast::TypeExpr::Named(name, _) => {
                Type::from_name(name).unwrap_or(Type::Named(name.clone(), Vec::new()))
            }
            ast::TypeExpr::App(base, args, _) => {
                let base_name = match base.as_ref() {
                    ast::TypeExpr::Named(n, _) => n.clone(),
                    _ => return Type::Error,
                };
                let arg_types: Vec<Type> = args.iter().map(|a| self.type_expr_to_type(a)).collect();
                Type::Named(base_name, arg_types)
            }
            ast::TypeExpr::Product(types, _) => {
                Type::Product(types.iter().map(|t| self.type_expr_to_type(t)).collect())
            }
            ast::TypeExpr::Function(params, ret, _, _) => {
                let pts: Vec<Type> = params.iter().map(|p| self.type_expr_to_type(p)).collect();
                Type::Function(pts, Box::new(self.type_expr_to_type(ret)))
            }
            ast::TypeExpr::Forall(_, body, _) => self.type_expr_to_type(body),
            ast::TypeExpr::Unit(_) => Type::Unit,
        }
    }
}
