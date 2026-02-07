use inkwell::types::{BasicMetadataTypeEnum, BasicType};
use inkwell::values::FunctionValue;

use aura_parser::ast;
use aura_types::types::Type;

use super::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn declare_function(
        &mut self,
        f: &ast::FnDef,
    ) -> Result<FunctionValue<'ctx>, String> {
        let param_types: Vec<BasicMetadataTypeEnum<'ctx>> = f
            .params
            .iter()
            .map(|p| self.type_to_llvm_meta(&self.resolve_param_type(p)))
            .collect();

        let ret_type = f
            .return_type
            .as_ref()
            .map(|t| self.type_expr_to_type(t))
            .unwrap_or(Type::Unit);

        let fn_type = if ret_type == Type::Unit {
            self.context.void_type().fn_type(&param_types, false)
        } else {
            let ret_llvm = self.type_to_llvm(&ret_type);
            ret_llvm.fn_type(&param_types, false)
        };

        let fn_name = if f.name == "main" { "main" } else { &f.name };
        let function = self.module.add_function(fn_name, fn_type, None);
        self.functions.insert(f.name.clone(), function);
        Ok(function)
    }

    pub(crate) fn compile_function(&mut self, f: &ast::FnDef) -> Result<(), String> {
        let function = *self
            .functions
            .get(&f.name)
            .ok_or_else(|| format!("function '{}' not declared", f.name))?;

        self.current_function = Some(function);

        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        // Clear variable scope
        self.variables.clear();

        // Bind parameters
        for (i, param) in f.params.iter().enumerate() {
            let arg = function
                .get_nth_param(i as u32)
                .ok_or_else(|| format!("missing param {i}"))?;

            let alloca = self.create_entry_alloca(function, &param.name, arg.get_type());
            self.builder.build_store(alloca, arg).unwrap();
            self.variables.insert(param.name.clone(), alloca);
        }

        // Compile body
        let result = self.compile_expr(&f.body)?;

        let ret_type = f
            .return_type
            .as_ref()
            .map(|t| self.type_expr_to_type(t))
            .unwrap_or(Type::Unit);

        // Only add return if current block has no terminator
        let current_block = self.builder.get_insert_block().unwrap();
        if current_block.get_terminator().is_none() {
            if ret_type == Type::Unit {
                self.builder.build_return(None).unwrap();
            } else if let Some(val) = result {
                self.builder.build_return(Some(&val)).unwrap();
            } else {
                self.builder.build_return(None).unwrap();
            }
        }

        self.current_function = None;
        Ok(())
    }
}
