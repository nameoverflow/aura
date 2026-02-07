use inkwell::values::BasicValueEnum;

use aura_parser::ast;

use super::CodeGen;

impl<'ctx> CodeGen<'ctx> {
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
