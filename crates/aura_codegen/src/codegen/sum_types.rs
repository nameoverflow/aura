use inkwell::values::BasicValueEnum;

use aura_parser::ast;
use aura_types::types::Type;

use super::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn compile_builtin_variant_construct(
        &mut self,
        expr: &ast::Expr,
        variant_name: &str,
        args: &[ast::Expr],
    ) -> Result<Option<BasicValueEnum<'ctx>>, String> {
        match variant_name {
            "Some" | "Ok" | "Err" => {
                if args.len() != 1 {
                    return Err(format!(
                        "builtin variant '{variant_name}' expects 1 argument, got {}",
                        args.len()
                    ));
                }
                self.compile_expr(&args[0])
            }
            "None" => {
                if !args.is_empty() {
                    return Err(format!(
                        "builtin variant 'None' expects 0 arguments, got {}",
                        args.len()
                    ));
                }

                // Codegen-level encoding for Option/Result builtins currently uses the
                // zero value for the expression's lowered type.
                let value = if let Some(ty) = self.expr_type(expr) {
                    self.zero_value_for_type(self.type_to_llvm(&ty))
                } else {
                    // Fallback to i64 zero when expression typing is unavailable.
                    self.context.i64_type().const_zero().into()
                };
                Ok(Some(value))
            }
            _ => Err(format!("unknown builtin variant '{variant_name}'")),
        }
    }

    pub(crate) fn compile_builtin_variant_call(
        &mut self,
        expr: &ast::Expr,
        variant_name: &str,
        args: &[ast::Expr],
    ) -> Result<Option<BasicValueEnum<'ctx>>, String> {
        // If the typechecker recorded this as Option/Result, keep zero-payload/identity
        // encoding consistent for current backend representation.
        if let Some(Type::Named(name, _)) = self.expr_type(expr) {
            if name == "Option" || name == "Result" {
                return self.compile_builtin_variant_construct(expr, variant_name, args);
            }
        }
        self.compile_builtin_variant_construct(expr, variant_name, args)
    }

    /// Construct a zero-payload variant (e.g., `None`, `Pending`).
    pub(crate) fn compile_variant_construct(
        &mut self,
        variant_name: &str,
        _args: &[ast::Expr],
    ) -> Result<Option<BasicValueEnum<'ctx>>, String> {
        let vinfo = self
            .variant_info
            .get(variant_name)
            .ok_or_else(|| format!("unknown variant '{variant_name}'"))?;
        let parent = vinfo.parent_type.clone();
        let tag = vinfo.tag;

        let sum_info = self
            .sum_types
            .get(&parent)
            .ok_or_else(|| format!("unknown sum type '{parent}'"))?;
        let sum_type = sum_info.llvm_type;

        let function = self.current_function.unwrap();
        let alloca = self.create_entry_alloca(function, "variant_tmp", sum_type.into());

        // Store tag
        let tag_ptr = self.builder.build_struct_gep(alloca, 0, "tag_ptr").unwrap();
        let tag_val = self.context.i64_type().const_int(tag, false);
        self.builder.build_store(tag_ptr, tag_val).unwrap();

        let result = self.builder.build_load(alloca, variant_name).unwrap();
        Ok(Some(result))
    }

    /// Construct a variant with payload (e.g., `Some(42)`, `Failed("msg")`).
    pub(crate) fn compile_variant_call(
        &mut self,
        variant_name: &str,
        args: &[ast::Expr],
    ) -> Result<Option<BasicValueEnum<'ctx>>, String> {
        let vinfo = self
            .variant_info
            .get(variant_name)
            .ok_or_else(|| format!("unknown variant '{variant_name}'"))?;
        let parent = vinfo.parent_type.clone();
        let tag = vinfo.tag;

        let sum_info = self
            .sum_types
            .get(&parent)
            .ok_or_else(|| format!("unknown sum type '{parent}'"))?;
        let sum_type = sum_info.llvm_type;

        let function = self.current_function.unwrap();
        let alloca = self.create_entry_alloca(function, "variant_tmp", sum_type.into());

        // Store tag
        let tag_ptr = self.builder.build_struct_gep(alloca, 0, "tag_ptr").unwrap();
        let tag_val = self.context.i64_type().const_int(tag, false);
        self.builder.build_store(tag_ptr, tag_val).unwrap();

        // Store payload fields (starting at struct index 1)
        for (i, arg) in args.iter().enumerate() {
            let val = self
                .compile_expr(arg)?
                .ok_or_else(|| format!("expected value for variant payload field {i}"))?;
            let field_ptr = self
                .builder
                .build_struct_gep(
                    alloca,
                    (i + 1) as u32,
                    &format!("{variant_name}.payload.{i}"),
                )
                .unwrap();
            self.builder.build_store(field_ptr, val).unwrap();
        }

        let result = self.builder.build_load(alloca, variant_name).unwrap();
        Ok(Some(result))
    }
}
