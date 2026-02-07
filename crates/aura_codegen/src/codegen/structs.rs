use inkwell::types::AnyType;
use inkwell::values::BasicValueEnum;

use aura_parser::ast;

use super::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn compile_struct_lit(
        &mut self,
        name: &str,
        fields: &[(String, ast::Expr)],
    ) -> Result<Option<BasicValueEnum<'ctx>>, String> {
        let struct_info = self
            .struct_types
            .get(name)
            .ok_or_else(|| format!("undefined struct type '{name}'"))?;
        let struct_type = struct_info.llvm_type;
        let field_names = struct_info.field_names.clone();

        let function = self.current_function.unwrap();
        let alloca = self.create_entry_alloca(function, "struct_tmp", struct_type.into());

        // Store each field value
        for (fname, fexpr) in fields {
            let idx = field_names
                .iter()
                .position(|n| n == fname)
                .ok_or_else(|| format!("struct '{name}' has no field '{fname}'"))?;
            let val = self
                .compile_expr(fexpr)?
                .ok_or_else(|| format!("expected value for field '{fname}'"))?;

            let field_ptr = self
                .builder
                .build_struct_gep(alloca, idx as u32, &format!("{name}.{fname}.ptr"))
                .unwrap();
            self.builder.build_store(field_ptr, val).unwrap();
        }

        let result = self.builder.build_load(alloca, name).unwrap();
        Ok(Some(result))
    }

    pub(crate) fn compile_field_access(
        &mut self,
        receiver: &ast::Expr,
        field_name: &str,
    ) -> Result<Option<BasicValueEnum<'ctx>>, String> {
        let recv_val = self
            .compile_expr(receiver)?
            .ok_or("expected struct value for field access")?;

        // We need the struct type name to look up field index.
        // Try to get it from the variable name or known struct types.
        let struct_name = self.find_struct_type_name(receiver);

        if let Some(sname) = struct_name {
            if let Some(info) = self.struct_types.get(&sname) {
                let idx = info
                    .field_names
                    .iter()
                    .position(|n| n == field_name)
                    .ok_or_else(|| format!("struct '{sname}' has no field '{field_name}'"))?;

                let function = self.current_function.unwrap();
                let alloca = self.create_entry_alloca(function, "recv_tmp", recv_val.get_type());
                self.builder.build_store(alloca, recv_val).unwrap();

                let field_ptr = self
                    .builder
                    .build_struct_gep(alloca, idx as u32, &format!("{sname}.{field_name}.ptr"))
                    .unwrap();
                let val = self.builder.build_load(field_ptr, field_name).unwrap();
                return Ok(Some(val));
            }
        }

        // Fallback: can't determine struct type
        Err(format!(
            "cannot determine struct type for field access '.{field_name}'"
        ))
    }

    pub(crate) fn find_struct_type_name(&self, expr: &ast::Expr) -> Option<String> {
        match expr {
            ast::Expr::Ident(name, _) => {
                // Look at what the variable was assigned from (check struct types)
                // For now, check if any struct type matches the alloca's type
                if let Some(alloca) = self.variables.get(name) {
                    let pointee = alloca.get_type().get_element_type();
                    for (sname, info) in &self.struct_types {
                        if pointee == info.llvm_type.as_any_type_enum() {
                            return Some(sname.clone());
                        }
                    }
                }
                None
            }
            ast::Expr::StructLit(name, _, _) => Some(name.clone()),
            ast::Expr::Call(callee, _, _) => {
                // Could be a function returning a struct
                if let ast::Expr::Ident(fname, _) = callee.as_ref() {
                    // Try to match return type to a struct
                    let _ = fname;
                }
                None
            }
            _ => None,
        }
    }
}
