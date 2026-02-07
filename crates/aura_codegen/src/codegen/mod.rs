mod calls;
mod control_flow;
mod exprs;
mod functions;
mod matching;
mod ops;
mod structs;
mod sum_types;
mod types;

#[cfg(test)]
mod tests;

use std::collections::HashMap;
use std::path::Path;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module as LlvmModule;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::{BasicTypeEnum, StructType};
use inkwell::values::{FunctionValue, PointerValue};
use inkwell::{AddressSpace, OptimizationLevel};

use aura_parser::ast;
use aura_types::types::Type;

/// Info about a struct type: its LLVM type and field names in order.
#[allow(dead_code)] // field_types needed for P1+ generic codegen
pub(crate) struct StructInfo<'ctx> {
    pub(crate) llvm_type: StructType<'ctx>,
    pub(crate) field_names: Vec<String>,
    pub(crate) field_types: Vec<Type>,
}

/// Info about a sum type (tagged union).
#[allow(dead_code)] // variants/name needed for P1+ generic codegen
pub(crate) struct SumTypeInfo<'ctx> {
    /// LLVM struct type: { i64 (tag), payload... }
    pub(crate) llvm_type: StructType<'ctx>,
    /// Variant name → (tag index, payload types)
    pub(crate) variants: HashMap<String, (u64, Vec<Type>)>,
    /// Parent type name
    pub(crate) name: String,
}

/// Info about a single variant, pointing back to its parent sum type.
pub(crate) struct VariantInfo {
    pub(crate) parent_type: String,
    pub(crate) tag: u64,
    pub(crate) payload_types: Vec<Type>,
}

pub struct CodeGen<'ctx> {
    pub(crate) context: &'ctx Context,
    pub(crate) module: LlvmModule<'ctx>,
    pub(crate) builder: Builder<'ctx>,
    pub(crate) variables: HashMap<String, PointerValue<'ctx>>,
    pub(crate) functions: HashMap<String, FunctionValue<'ctx>>,
    pub(crate) current_function: Option<FunctionValue<'ctx>>,
    pub(crate) struct_types: HashMap<String, StructInfo<'ctx>>,
    /// Sum type name → SumTypeInfo
    pub(crate) sum_types: HashMap<String, SumTypeInfo<'ctx>>,
    /// Variant name → VariantInfo (for quick lookup)
    pub(crate) variant_info: HashMap<String, VariantInfo>,
    /// Track which loop's break/continue blocks to jump to.
    pub(crate) loop_exit_stack: Vec<inkwell::basic_block::BasicBlock<'ctx>>,
    pub(crate) loop_continue_stack: Vec<inkwell::basic_block::BasicBlock<'ctx>>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        let mut cg = Self {
            context,
            module,
            builder,
            variables: HashMap::new(),
            functions: HashMap::new(),
            current_function: None,
            struct_types: HashMap::new(),
            sum_types: HashMap::new(),
            variant_info: HashMap::new(),
            loop_exit_stack: Vec::new(),
            loop_continue_stack: Vec::new(),
        };

        // Declare printf and strcmp for print/println and string matching
        cg.declare_printf();
        cg.declare_strcmp();

        cg
    }

    fn declare_printf(&mut self) {
        let i8_ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
        let printf_type = self.context.i32_type().fn_type(&[i8_ptr_type.into()], true);
        let printf = self.module.add_function("printf", printf_type, None);
        self.functions.insert("printf".into(), printf);
    }

    fn declare_strcmp(&mut self) {
        let i8_ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
        let strcmp_type = self
            .context
            .i32_type()
            .fn_type(&[i8_ptr_type.into(), i8_ptr_type.into()], false);
        let strcmp = self.module.add_function("strcmp", strcmp_type, None);
        self.functions.insert("strcmp".into(), strcmp);
    }

    pub fn compile_module(&mut self, module: &ast::Module) -> Result<(), String> {
        // Pass 0: register struct types
        for item in &module.items {
            if let ast::Item::TypeDef(td) = item {
                self.register_type_def(td);
            }
        }

        // Pass 1: declare all functions
        for item in &module.items {
            if let ast::Item::Function(f) = item {
                self.declare_function(f)?;
            }
        }

        // Pass 2: define all function bodies
        for item in &module.items {
            if let ast::Item::Function(f) = item {
                self.compile_function(f)?;
            }
        }

        self.module.verify().map_err(|e| e.to_string())
    }

    pub(crate) fn create_entry_alloca(
        &self,
        function: FunctionValue<'ctx>,
        name: &str,
        ty: BasicTypeEnum<'ctx>,
    ) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();
        let entry = function.get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(inst) => builder.position_before(&inst),
            None => builder.position_at_end(entry),
        }

        builder.build_alloca(ty, name).unwrap()
    }

    // ── Output methods ──

    pub fn print_ir(&self) -> String {
        self.module.print_to_string().to_string()
    }

    pub fn write_object_file(&self, path: &Path) -> Result<(), String> {
        Target::initialize_native(&InitializationConfig::default()).map_err(|e| e.to_string())?;

        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple).map_err(|e| e.to_string())?;

        let machine = target
            .create_target_machine(
                &triple,
                "generic",
                "",
                OptimizationLevel::Default,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .ok_or("failed to create target machine")?;

        machine
            .write_to_file(&self.module, FileType::Object, path)
            .map_err(|e| e.to_string())
    }
}
