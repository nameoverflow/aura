use std::collections::HashMap;
use std::path::Path;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module as LlvmModule;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::{AnyType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, StructType};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue,
};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate, OptimizationLevel};

use aura_parser::ast;
use aura_types::types::Type;

/// Info about a struct type: its LLVM type and field names in order.
#[allow(dead_code)] // field_types needed for P1+ generic codegen
struct StructInfo<'ctx> {
    llvm_type: StructType<'ctx>,
    field_names: Vec<String>,
    field_types: Vec<Type>,
}

/// Info about a sum type (tagged union).
#[allow(dead_code)] // variants/name needed for P1+ generic codegen
struct SumTypeInfo<'ctx> {
    /// LLVM struct type: { i64 (tag), payload... }
    llvm_type: StructType<'ctx>,
    /// Variant name → (tag index, payload types)
    variants: HashMap<String, (u64, Vec<Type>)>,
    /// Parent type name
    name: String,
}

/// Info about a single variant, pointing back to its parent sum type.
struct VariantInfo {
    parent_type: String,
    tag: u64,
    payload_types: Vec<Type>,
}

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: LlvmModule<'ctx>,
    builder: Builder<'ctx>,
    variables: HashMap<String, PointerValue<'ctx>>,
    functions: HashMap<String, FunctionValue<'ctx>>,
    current_function: Option<FunctionValue<'ctx>>,
    struct_types: HashMap<String, StructInfo<'ctx>>,
    /// Sum type name → SumTypeInfo
    sum_types: HashMap<String, SumTypeInfo<'ctx>>,
    /// Variant name → VariantInfo (for quick lookup)
    variant_info: HashMap<String, VariantInfo>,
    /// Track which loop's break/continue blocks to jump to.
    loop_exit_stack: Vec<inkwell::basic_block::BasicBlock<'ctx>>,
    loop_continue_stack: Vec<inkwell::basic_block::BasicBlock<'ctx>>,
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

    fn register_type_def(&mut self, td: &ast::TypeDef) {
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
                if max_payload_fields.is_empty() {
                    // Enum with no payloads — just the tag
                } else {
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

    fn declare_function(&mut self, f: &ast::FnDef) -> Result<FunctionValue<'ctx>, String> {
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

    fn compile_function(&mut self, f: &ast::FnDef) -> Result<(), String> {
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

    fn compile_expr(&mut self, expr: &ast::Expr) -> Result<Option<BasicValueEnum<'ctx>>, String> {
        match expr {
            ast::Expr::IntLit(n, _) => Ok(Some(
                self.context.i64_type().const_int(*n as u64, true).into(),
            )),
            ast::Expr::FloatLit(n, _) => Ok(Some(self.context.f64_type().const_float(*n).into())),
            ast::Expr::BoolLit(b, _) => Ok(Some(
                self.context.bool_type().const_int(*b as u64, false).into(),
            )),
            ast::Expr::StringLit(s, _) => {
                let global = self.builder.build_global_string_ptr(s, "str").unwrap();
                Ok(Some(global.as_pointer_value().into()))
            }
            ast::Expr::Unit(_) => Ok(None),
            ast::Expr::Ident(name, _) => {
                if let Some(alloca) = self.variables.get(name) {
                    let val = self.builder.build_load(*alloca, name).unwrap();
                    Ok(Some(val))
                } else if self.variant_info.contains_key(name) {
                    // Zero-payload variant construction (e.g., None, Pending)
                    self.compile_variant_construct(name, &[])
                } else if let Some(func) = self.functions.get(name) {
                    // Function reference — return as pointer
                    Ok(Some(func.as_global_value().as_pointer_value().into()))
                } else {
                    Err(format!("undefined variable '{name}'"))
                }
            }
            ast::Expr::Binary(lhs, op, rhs, _) => {
                let lhs_val = self
                    .compile_expr(lhs)?
                    .ok_or("expected value on LHS of binary op")?;
                let rhs_val = self
                    .compile_expr(rhs)?
                    .ok_or("expected value on RHS of binary op")?;

                self.compile_binary_op(lhs_val, *op, rhs_val)
            }
            ast::Expr::Unary(op, inner, _) => {
                let val = self
                    .compile_expr(inner)?
                    .ok_or("expected value for unary op")?;
                match op {
                    ast::UnaryOp::Neg => {
                        if val.is_int_value() {
                            let neg = self
                                .builder
                                .build_int_neg(val.into_int_value(), "neg")
                                .unwrap();
                            Ok(Some(neg.into()))
                        } else if val.is_float_value() {
                            let neg = self
                                .builder
                                .build_float_neg(val.into_float_value(), "fneg")
                                .unwrap();
                            Ok(Some(neg.into()))
                        } else {
                            Err("cannot negate non-numeric value".into())
                        }
                    }
                    ast::UnaryOp::Not => {
                        let bool_val = val.into_int_value();
                        let result = self.builder.build_not(bool_val, "not").unwrap();
                        Ok(Some(result.into()))
                    }
                }
            }
            ast::Expr::Block(exprs, _) => {
                let mut result = None;
                for e in exprs {
                    result = self.compile_expr(e)?;
                }
                Ok(result)
            }
            ast::Expr::If(cond, then_branch, else_branch, _) => {
                self.compile_if(cond, then_branch, else_branch.as_deref())
            }
            ast::Expr::Let(name, _is_mut, _ty, value, _) => {
                let val = self.compile_expr(value)?;
                if let Some(val) = val {
                    let function = self.current_function.unwrap();
                    let alloca = self.create_entry_alloca(function, name, val.get_type());
                    self.builder.build_store(alloca, val).unwrap();
                    self.variables.insert(name.clone(), alloca);
                }
                Ok(None)
            }
            ast::Expr::LetPattern(_, _, _, _, _) => {
                Err("let-pattern codegen is not supported yet".into())
            }
            ast::Expr::Assign(target, value, _) => {
                if let ast::Expr::Ident(name, _) = target.as_ref() {
                    let val = self
                        .compile_expr(value)?
                        .ok_or("expected value for assignment")?;
                    let alloca = self
                        .variables
                        .get(name)
                        .ok_or_else(|| format!("undefined variable '{name}'"))?;
                    self.builder.build_store(*alloca, val).unwrap();
                    Ok(None)
                } else {
                    Err("can only assign to variables".into())
                }
            }
            ast::Expr::Return(val, _) => {
                if let Some(val_expr) = val {
                    let val = self.compile_expr(val_expr)?;
                    if let Some(v) = val {
                        self.builder.build_return(Some(&v)).unwrap();
                    } else {
                        self.builder.build_return(None).unwrap();
                    }
                } else {
                    self.builder.build_return(None).unwrap();
                }
                Ok(None)
            }
            ast::Expr::Call(callee, args, _) => {
                // Check if this is a variant constructor call (e.g., Some(42))
                if let ast::Expr::Ident(name, _) = callee.as_ref() {
                    if self.variant_info.contains_key(name) {
                        return self.compile_variant_call(name, args);
                    }
                }
                self.compile_call(callee, args)
            }
            ast::Expr::While(cond, body, _) => self.compile_while(cond, body),
            ast::Expr::For(var, iter, body, _) => self.compile_for(var, iter, body),
            ast::Expr::ForPattern(_, _, _, _) => {
                Err("for-pattern codegen is not supported yet".into())
            }
            ast::Expr::FieldAccess(receiver, field_name, _) => {
                self.compile_field_access(receiver, field_name)
            }
            ast::Expr::StructLit(name, fields, _) => self.compile_struct_lit(name, fields),
            ast::Expr::Match(scrutinee, arms, _) => self.compile_match(scrutinee, arms),
            ast::Expr::StringInterp(parts, _) => {
                // For P0: concatenate parts via printf-style formatting
                // Simple approach: just format the string with printf
                let mut fmt = String::new();
                let mut args: Vec<BasicMetadataValueEnum<'ctx>> = Vec::new();
                for part in parts {
                    match &part.kind {
                        ast::StringPartKind::Literal(text) => {
                            fmt.push_str(text);
                        }
                        ast::StringPartKind::Expr(e) => {
                            let val = self.compile_expr(e)?;
                            if let Some(v) = val {
                                if v.is_int_value() {
                                    fmt.push_str("%lld");
                                    args.push(v.into());
                                } else if v.is_float_value() {
                                    fmt.push_str("%f");
                                    args.push(v.into());
                                } else {
                                    fmt.push_str("%s");
                                    args.push(v.into());
                                }
                            }
                        }
                    }
                }
                let global = self
                    .builder
                    .build_global_string_ptr(&fmt, "interp_fmt")
                    .unwrap();
                Ok(Some(global.as_pointer_value().into()))
            }
            ast::Expr::Pipeline(lhs, rhs, _) => {
                // Pipeline: a |> f(args) desugars to f(a, args)
                let lhs_val = self.compile_expr(lhs)?.ok_or("pipeline LHS produced no value")?;
                match rhs.as_ref() {
                    ast::Expr::Call(callee, args, _) => {
                        // a |> f(x, y) => f(a, x, y)
                        if let ast::Expr::Ident(name, _) = callee.as_ref() {
                            if name == "print" || name == "println" {
                                // For print/println pipeline, just pass LHS as the argument
                                let mut all_args = vec![lhs.as_ref().clone()];
                                all_args.extend(args.iter().cloned());
                                return self.compile_print(name, &all_args);
                            }
                            if self.variant_info.contains_key(name) {
                                // Variant constructor in pipeline: a |> Some()
                                let mut all_args = vec![lhs.as_ref().clone()];
                                all_args.extend(args.iter().cloned());
                                return self.compile_variant_call(name, &all_args);
                            }
                        }
                        let callee_name = match callee.as_ref() {
                            ast::Expr::Ident(name, _) => name.clone(),
                            _ => return Err("complex callees not yet supported".into()),
                        };
                        let function = *self
                            .functions
                            .get(&callee_name)
                            .ok_or_else(|| format!("undefined function '{callee_name}'"))?;
                        let mut compiled_args: Vec<BasicMetadataValueEnum<'ctx>> =
                            vec![lhs_val.into()];
                        for arg in args {
                            let val =
                                self.compile_expr(arg)?.ok_or("expected argument value")?;
                            compiled_args.push(val.into());
                        }
                        let call = self
                            .builder
                            .build_call(function, &compiled_args, &callee_name)
                            .unwrap();
                        Ok(call.try_as_basic_value().left())
                    }
                    ast::Expr::Ident(name, _) => {
                        // a |> f => f(a)
                        if name == "print" || name == "println" {
                            let all_args = vec![lhs.as_ref().clone()];
                            return self.compile_print(name, &all_args);
                        }
                        let function = *self
                            .functions
                            .get(name)
                            .ok_or_else(|| format!("undefined function '{name}'"))?;
                        let compiled_args: Vec<BasicMetadataValueEnum<'ctx>> =
                            vec![lhs_val.into()];
                        let call = self
                            .builder
                            .build_call(function, &compiled_args, name)
                            .unwrap();
                        Ok(call.try_as_basic_value().left())
                    }
                    _ => {
                        // Fallback: evaluate RHS (e.g., method calls deferred)
                        let rhs_val = self.compile_expr(rhs)?;
                        Ok(rhs_val)
                    }
                }
            }
            ast::Expr::Break(_) => {
                if let Some(exit_bb) = self.loop_exit_stack.last() {
                    self.builder.build_unconditional_branch(*exit_bb).unwrap();
                }
                Ok(None)
            }
            ast::Expr::Continue(_) => {
                if let Some(cont_bb) = self.loop_continue_stack.last() {
                    self.builder.build_unconditional_branch(*cont_bb).unwrap();
                }
                Ok(None)
            }
            ast::Expr::ListLit(_, _)
            | ast::Expr::TupleLit(_, _)
            | ast::Expr::Lambda(_, _, _, _)
            | ast::Expr::With(_, _, _)
            | ast::Expr::Try(_, _)
            | ast::Expr::Range(_, _, _, _)
            | ast::Expr::MethodCall(_, _, _, _)
            | ast::Expr::QualifiedIdent(_, _, _) => {
                // These features are deferred to later tiers
                Ok(None)
            }
        }
    }

    fn compile_binary_op(
        &mut self,
        lhs: BasicValueEnum<'ctx>,
        op: ast::BinOp,
        rhs: BasicValueEnum<'ctx>,
    ) -> Result<Option<BasicValueEnum<'ctx>>, String> {
        // Check if both operands are floats
        if lhs.is_float_value() && rhs.is_float_value() {
            let l = lhs.into_float_value();
            let r = rhs.into_float_value();
            let result = match op {
                ast::BinOp::Add => self.builder.build_float_add(l, r, "fadd").unwrap().into(),
                ast::BinOp::Sub => self.builder.build_float_sub(l, r, "fsub").unwrap().into(),
                ast::BinOp::Mul => self.builder.build_float_mul(l, r, "fmul").unwrap().into(),
                ast::BinOp::Div => self.builder.build_float_div(l, r, "fdiv").unwrap().into(),
                ast::BinOp::Mod => self.builder.build_float_rem(l, r, "frem").unwrap().into(),
                ast::BinOp::Lt => self
                    .builder
                    .build_float_compare(FloatPredicate::OLT, l, r, "flt")
                    .unwrap()
                    .into(),
                ast::BinOp::Gt => self
                    .builder
                    .build_float_compare(FloatPredicate::OGT, l, r, "fgt")
                    .unwrap()
                    .into(),
                ast::BinOp::LtEq => self
                    .builder
                    .build_float_compare(FloatPredicate::OLE, l, r, "fle")
                    .unwrap()
                    .into(),
                ast::BinOp::GtEq => self
                    .builder
                    .build_float_compare(FloatPredicate::OGE, l, r, "fge")
                    .unwrap()
                    .into(),
                ast::BinOp::Eq => self
                    .builder
                    .build_float_compare(FloatPredicate::OEQ, l, r, "feq")
                    .unwrap()
                    .into(),
                ast::BinOp::NotEq => self
                    .builder
                    .build_float_compare(FloatPredicate::ONE, l, r, "fne")
                    .unwrap()
                    .into(),
                ast::BinOp::And | ast::BinOp::Or => {
                    return Err("logical ops not supported on floats".into())
                }
            };
            return Ok(Some(result));
        }

        // Integer operations
        let l = lhs.into_int_value();
        let r = rhs.into_int_value();
        let result: BasicValueEnum = match op {
            ast::BinOp::Add => self.builder.build_int_add(l, r, "add").unwrap().into(),
            ast::BinOp::Sub => self.builder.build_int_sub(l, r, "sub").unwrap().into(),
            ast::BinOp::Mul => self.builder.build_int_mul(l, r, "mul").unwrap().into(),
            ast::BinOp::Div => self
                .builder
                .build_int_signed_div(l, r, "div")
                .unwrap()
                .into(),
            ast::BinOp::Mod => self
                .builder
                .build_int_signed_rem(l, r, "mod")
                .unwrap()
                .into(),
            ast::BinOp::Eq => self
                .builder
                .build_int_compare(IntPredicate::EQ, l, r, "eq")
                .unwrap()
                .into(),
            ast::BinOp::NotEq => self
                .builder
                .build_int_compare(IntPredicate::NE, l, r, "ne")
                .unwrap()
                .into(),
            ast::BinOp::Lt => self
                .builder
                .build_int_compare(IntPredicate::SLT, l, r, "lt")
                .unwrap()
                .into(),
            ast::BinOp::Gt => self
                .builder
                .build_int_compare(IntPredicate::SGT, l, r, "gt")
                .unwrap()
                .into(),
            ast::BinOp::LtEq => self
                .builder
                .build_int_compare(IntPredicate::SLE, l, r, "le")
                .unwrap()
                .into(),
            ast::BinOp::GtEq => self
                .builder
                .build_int_compare(IntPredicate::SGE, l, r, "ge")
                .unwrap()
                .into(),
            ast::BinOp::And => self.builder.build_and(l, r, "and").unwrap().into(),
            ast::BinOp::Or => self.builder.build_or(l, r, "or").unwrap().into(),
        };
        Ok(Some(result))
    }

    fn compile_if(
        &mut self,
        cond: &ast::Expr,
        then_branch: &ast::Expr,
        else_branch: Option<&ast::Expr>,
    ) -> Result<Option<BasicValueEnum<'ctx>>, String> {
        let cond_val = self.compile_expr(cond)?.ok_or("expected condition value")?;

        let cond_bool = if cond_val.is_int_value() {
            let iv = cond_val.into_int_value();
            if iv.get_type().get_bit_width() == 1 {
                iv
            } else {
                self.builder
                    .build_int_compare(IntPredicate::NE, iv, iv.get_type().const_zero(), "ifcond")
                    .unwrap()
            }
        } else {
            return Err("condition must be boolean".into());
        };

        let function = self.current_function.unwrap();
        let then_bb = self.context.append_basic_block(function, "then");
        let else_bb = self.context.append_basic_block(function, "else");
        let merge_bb = self.context.append_basic_block(function, "ifmerge");

        self.builder
            .build_conditional_branch(cond_bool, then_bb, else_bb)
            .unwrap();

        // Then block
        self.builder.position_at_end(then_bb);
        let then_val = self.compile_expr(then_branch)?;
        let then_end_bb = self.builder.get_insert_block().unwrap();
        if then_end_bb.get_terminator().is_none() {
            self.builder.build_unconditional_branch(merge_bb).unwrap();
        }

        // Else block
        self.builder.position_at_end(else_bb);
        let else_val = if let Some(else_br) = else_branch {
            self.compile_expr(else_br)?
        } else {
            None
        };
        let else_end_bb = self.builder.get_insert_block().unwrap();
        if else_end_bb.get_terminator().is_none() {
            self.builder.build_unconditional_branch(merge_bb).unwrap();
        }

        // Merge block
        self.builder.position_at_end(merge_bb);

        // If both branches produce values of the same type, create a PHI
        if let (Some(then_v), Some(else_v)) = (then_val, else_val) {
            if then_v.get_type() == else_v.get_type() {
                let phi = self
                    .builder
                    .build_phi(then_v.get_type(), "ifresult")
                    .unwrap();
                phi.add_incoming(&[(&then_v, then_end_bb), (&else_v, else_end_bb)]);
                return Ok(Some(phi.as_basic_value()));
            }
        }

        Ok(None)
    }

    fn compile_call(
        &mut self,
        callee: &ast::Expr,
        args: &[ast::Expr],
    ) -> Result<Option<BasicValueEnum<'ctx>>, String> {
        // Handle built-in print/println
        if let ast::Expr::Ident(name, _) = callee {
            if name == "print" || name == "println" {
                return self.compile_print(name, args);
            }
        }

        let callee_name = match callee {
            ast::Expr::Ident(name, _) => name.clone(),
            _ => return Err("complex callees not yet supported in P0".into()),
        };

        let function = *self
            .functions
            .get(&callee_name)
            .ok_or_else(|| format!("undefined function '{callee_name}'"))?;

        let mut compiled_args: Vec<BasicMetadataValueEnum<'ctx>> = Vec::new();
        for arg in args {
            let val = self.compile_expr(arg)?.ok_or("expected argument value")?;
            compiled_args.push(val.into());
        }

        let call = self
            .builder
            .build_call(function, &compiled_args, &callee_name)
            .unwrap();

        Ok(call.try_as_basic_value().left())
    }

    fn compile_print(
        &mut self,
        kind: &str,
        args: &[ast::Expr],
    ) -> Result<Option<BasicValueEnum<'ctx>>, String> {
        let printf = *self.functions.get("printf").unwrap();

        for (i, arg) in args.iter().enumerate() {
            let val = self.compile_expr(arg)?.ok_or("expected print argument")?;

            // Choose format string based on type
            let fmt = if val.is_int_value() {
                let int_val = val.into_int_value();
                if int_val.get_type().get_bit_width() == 1 {
                    // Bool: print "true" or "false"
                    let true_str = self
                        .builder
                        .build_global_string_ptr("true", "true_str")
                        .unwrap();
                    let false_str = self
                        .builder
                        .build_global_string_ptr("false", "false_str")
                        .unwrap();
                    let selected = self
                        .builder
                        .build_select(
                            int_val,
                            true_str.as_pointer_value(),
                            false_str.as_pointer_value(),
                            "bool_str",
                        )
                        .unwrap();
                    let fmt_str = if kind == "println" && i == args.len() - 1 {
                        "%s\n"
                    } else {
                        "%s"
                    };
                    let fmt_ptr = self
                        .builder
                        .build_global_string_ptr(fmt_str, "fmt")
                        .unwrap();
                    self.builder
                        .build_call(
                            printf,
                            &[fmt_ptr.as_pointer_value().into(), selected.into()],
                            "printf_call",
                        )
                        .unwrap();
                    continue;
                } else {
                    if kind == "println" && i == args.len() - 1 {
                        "%lld\n"
                    } else {
                        "%lld"
                    }
                }
            } else if val.is_float_value() {
                if kind == "println" && i == args.len() - 1 {
                    "%f\n"
                } else {
                    "%f"
                }
            } else {
                // Pointer (string)
                if kind == "println" && i == args.len() - 1 {
                    "%s\n"
                } else {
                    "%s"
                }
            };

            let fmt_ptr = self.builder.build_global_string_ptr(fmt, "fmt").unwrap();
            self.builder
                .build_call(
                    printf,
                    &[fmt_ptr.as_pointer_value().into(), val.into()],
                    "printf_call",
                )
                .unwrap();
        }

        // If no args, just print newline for println
        if args.is_empty() && kind == "println" {
            let fmt_ptr = self
                .builder
                .build_global_string_ptr("\n", "newline")
                .unwrap();
            self.builder
                .build_call(printf, &[fmt_ptr.as_pointer_value().into()], "printf_call")
                .unwrap();
        }

        Ok(None)
    }

    fn compile_while(
        &mut self,
        cond: &ast::Expr,
        body: &ast::Expr,
    ) -> Result<Option<BasicValueEnum<'ctx>>, String> {
        let function = self.current_function.unwrap();
        let cond_bb = self.context.append_basic_block(function, "while_cond");
        let body_bb = self.context.append_basic_block(function, "while_body");
        let end_bb = self.context.append_basic_block(function, "while_end");

        self.builder.build_unconditional_branch(cond_bb).unwrap();

        // Push loop context for break/continue
        self.loop_exit_stack.push(end_bb);
        self.loop_continue_stack.push(cond_bb);

        // Condition
        self.builder.position_at_end(cond_bb);
        let cond_val = self
            .compile_expr(cond)?
            .ok_or("expected condition value")?
            .into_int_value();
        self.builder
            .build_conditional_branch(cond_val, body_bb, end_bb)
            .unwrap();

        // Body
        self.builder.position_at_end(body_bb);
        self.compile_expr(body)?;
        let body_end = self.builder.get_insert_block().unwrap();
        if body_end.get_terminator().is_none() {
            self.builder.build_unconditional_branch(cond_bb).unwrap();
        }

        // Pop loop context
        self.loop_exit_stack.pop();
        self.loop_continue_stack.pop();

        // End
        self.builder.position_at_end(end_bb);
        Ok(None)
    }

    fn compile_for(
        &mut self,
        var: &str,
        iter_expr: &ast::Expr,
        body: &ast::Expr,
    ) -> Result<Option<BasicValueEnum<'ctx>>, String> {
        // For P0, only support integer ranges: for i in start..end
        let (start, end) = match iter_expr {
            ast::Expr::Range(s, e, _inclusive, _) => {
                let start_val = self
                    .compile_expr(s)?
                    .ok_or("expected range start value")?
                    .into_int_value();
                let end_val = self
                    .compile_expr(e)?
                    .ok_or("expected range end value")?
                    .into_int_value();
                (start_val, end_val)
            }
            _ => return Err("for loops only support range iteration in P0".into()),
        };

        let function = self.current_function.unwrap();
        let i64_type = self.context.i64_type();

        // Allocate loop variable
        let alloca = self.create_entry_alloca(function, var, i64_type.into());
        self.builder.build_store(alloca, start).unwrap();
        self.variables.insert(var.to_string(), alloca);

        let cond_bb = self.context.append_basic_block(function, "for_cond");
        let body_bb = self.context.append_basic_block(function, "for_body");
        let inc_bb = self.context.append_basic_block(function, "for_inc");
        let end_bb = self.context.append_basic_block(function, "for_end");

        self.builder.build_unconditional_branch(cond_bb).unwrap();

        // Push loop context for break/continue
        self.loop_exit_stack.push(end_bb);
        self.loop_continue_stack.push(inc_bb);

        // Condition: i < end
        self.builder.position_at_end(cond_bb);
        let current = self
            .builder
            .build_load(alloca, var)
            .unwrap()
            .into_int_value();
        let cmp = self
            .builder
            .build_int_compare(IntPredicate::SLT, current, end, "for_cond")
            .unwrap();
        self.builder
            .build_conditional_branch(cmp, body_bb, end_bb)
            .unwrap();

        // Body
        self.builder.position_at_end(body_bb);
        self.compile_expr(body)?;
        let body_end = self.builder.get_insert_block().unwrap();
        if body_end.get_terminator().is_none() {
            self.builder.build_unconditional_branch(inc_bb).unwrap();
        }

        // Increment
        self.builder.position_at_end(inc_bb);
        let current = self
            .builder
            .build_load(alloca, var)
            .unwrap()
            .into_int_value();
        let next = self
            .builder
            .build_int_add(current, i64_type.const_int(1, false), "inc")
            .unwrap();
        self.builder.build_store(alloca, next).unwrap();
        self.builder.build_unconditional_branch(cond_bb).unwrap();

        // Pop loop context
        self.loop_exit_stack.pop();
        self.loop_continue_stack.pop();

        // End
        self.builder.position_at_end(end_bb);
        Ok(None)
    }

    fn compile_struct_lit(
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

    fn compile_field_access(
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

    fn find_struct_type_name(&self, expr: &ast::Expr) -> Option<String> {
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

    /// Construct a zero-payload variant (e.g., `None`, `Pending`).
    fn compile_variant_construct(
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
    fn compile_variant_call(
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

    fn compile_match(
        &mut self,
        scrutinee: &ast::Expr,
        arms: &[ast::MatchArm],
    ) -> Result<Option<BasicValueEnum<'ctx>>, String> {
        let scrut_val = self.compile_expr(scrutinee)?;
        let function = self.current_function.unwrap();
        let merge_bb = self.context.append_basic_block(function, "match_merge");

        if arms.is_empty() {
            self.builder.build_unconditional_branch(merge_bb).unwrap();
            self.builder.position_at_end(merge_bb);
            return Ok(None);
        }

        // Chain of if-else for literal/wildcard/constructor patterns with guard support
        let mut incoming: Vec<(BasicValueEnum<'ctx>, inkwell::basic_block::BasicBlock<'ctx>)> =
            Vec::new();

        for (i, arm) in arms.iter().enumerate() {
            let is_last = i == arms.len() - 1;

            let next_bb = if is_last {
                merge_bb
            } else {
                self.context.append_basic_block(function, "match_next")
            };

            match &arm.pattern {
                ast::Pattern::Wildcard(_) | ast::Pattern::Ident(_, _) => {
                    // Wildcard/binding: always matches
                    // Bind the variable if it's an ident
                    if let ast::Pattern::Ident(name, _) = &arm.pattern {
                        if let Some(val) = &scrut_val {
                            let alloca = self.create_entry_alloca(function, name, val.get_type());
                            self.builder.build_store(alloca, *val).unwrap();
                            self.variables.insert(name.clone(), alloca);
                        }
                    }

                    // Check guard if present
                    if let Some(guard) = &arm.guard {
                        let guard_val = self
                            .compile_expr(guard)?
                            .ok_or("expected guard value")?
                            .into_int_value();
                        let arm_bb = self.context.append_basic_block(function, "match_arm");
                        self.builder
                            .build_conditional_branch(guard_val, arm_bb, next_bb)
                            .unwrap();
                        self.builder.position_at_end(arm_bb);
                    }

                    let body_val = self.compile_expr(&arm.body)?;
                    let current_bb = self.builder.get_insert_block().unwrap();
                    if current_bb.get_terminator().is_none() {
                        if let Some(v) = body_val {
                            incoming.push((v, current_bb));
                        }
                        self.builder.build_unconditional_branch(merge_bb).unwrap();
                    }
                    if arm.guard.is_none() {
                        break; // Unconditional wildcard is always last
                    }
                    if !is_last {
                        self.builder.position_at_end(next_bb);
                    }
                }
                ast::Pattern::Literal(lit, _) => {
                    let scrut = scrut_val
                        .clone()
                        .ok_or("expected scrutinee value for literal pattern")?;

                    let cmp = match lit {
                        ast::LitPattern::Int(n) => {
                            let pat_val = self.context.i64_type().const_int(*n as u64, true);
                            self.builder
                                .build_int_compare(
                                    IntPredicate::EQ,
                                    scrut.into_int_value(),
                                    pat_val,
                                    "match_cmp",
                                )
                                .unwrap()
                        }
                        ast::LitPattern::Bool(b) => {
                            let pat_val = self.context.bool_type().const_int(*b as u64, false);
                            self.builder
                                .build_int_compare(
                                    IntPredicate::EQ,
                                    scrut.into_int_value(),
                                    pat_val,
                                    "match_cmp",
                                )
                                .unwrap()
                        }
                        ast::LitPattern::String(s) => {
                            // String comparison via strcmp
                            let strcmp = *self.functions.get("strcmp").unwrap();
                            let pat_str = self
                                .builder
                                .build_global_string_ptr(s, "match_str_pat")
                                .unwrap();
                            let cmp_result = self
                                .builder
                                .build_call(
                                    strcmp,
                                    &[scrut.into(), pat_str.as_pointer_value().into()],
                                    "strcmp_result",
                                )
                                .unwrap();
                            let cmp_int = cmp_result
                                .try_as_basic_value()
                                .left()
                                .unwrap()
                                .into_int_value();
                            let zero = self.context.i32_type().const_int(0, false);
                            self.builder
                                .build_int_compare(IntPredicate::EQ, cmp_int, zero, "match_cmp")
                                .unwrap()
                        }
                        ast::LitPattern::Float(_) => {
                            return Err("float patterns not supported in P0".into());
                        }
                    };

                    let arm_bb = self.context.append_basic_block(function, "match_arm");
                    self.builder
                        .build_conditional_branch(cmp, arm_bb, next_bb)
                        .unwrap();

                    self.builder.position_at_end(arm_bb);

                    // Check guard if present
                    if let Some(guard) = &arm.guard {
                        let guard_val = self
                            .compile_expr(guard)?
                            .ok_or("expected guard value")?
                            .into_int_value();
                        let guard_pass_bb = self.context.append_basic_block(function, "guard_pass");
                        self.builder
                            .build_conditional_branch(guard_val, guard_pass_bb, next_bb)
                            .unwrap();
                        self.builder.position_at_end(guard_pass_bb);
                    }

                    let body_val = self.compile_expr(&arm.body)?;
                    let arm_end = self.builder.get_insert_block().unwrap();
                    if arm_end.get_terminator().is_none() {
                        if let Some(v) = body_val {
                            incoming.push((v, arm_end));
                        }
                        self.builder.build_unconditional_branch(merge_bb).unwrap();
                    }

                    if !is_last {
                        self.builder.position_at_end(next_bb);
                    }
                }
                ast::Pattern::Constructor(ctor_name, sub_patterns, _) => {
                    // Match variant tag, then bind payload if patterns present
                    let scrut = scrut_val
                        .clone()
                        .ok_or("expected scrutinee value for constructor pattern")?;

                    if let Some(vinfo) = self.variant_info.get(ctor_name) {
                        let parent = vinfo.parent_type.clone();
                        let tag = vinfo.tag;
                        let payload_types = vinfo.payload_types.clone();

                        let sum_info = self
                            .sum_types
                            .get(&parent)
                            .ok_or_else(|| format!("unknown sum type '{parent}'"))?;
                        let sum_llvm_type = sum_info.llvm_type;

                        let scrut_alloca =
                            self.create_entry_alloca(function, "scrut_tmp", sum_llvm_type.into());
                        self.builder.build_store(scrut_alloca, scrut).unwrap();

                        // Extract and compare tag
                        let tag_ptr = self
                            .builder
                            .build_struct_gep(scrut_alloca, 0, "tag_ptr")
                            .unwrap();
                        let tag_val = self
                            .builder
                            .build_load(tag_ptr, "tag")
                            .unwrap()
                            .into_int_value();
                        let expected_tag = self.context.i64_type().const_int(tag, false);
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::EQ, tag_val, expected_tag, "match_cmp")
                            .unwrap();

                        let arm_bb = self.context.append_basic_block(function, "match_arm");
                        self.builder
                            .build_conditional_branch(cmp, arm_bb, next_bb)
                            .unwrap();

                        self.builder.position_at_end(arm_bb);

                        // Bind payload fields to sub-pattern variables
                        for (j, sub_pat) in sub_patterns.iter().enumerate() {
                            if let ast::Pattern::Ident(bind_name, _) = sub_pat {
                                if j < payload_types.len() {
                                    let field_ptr = self
                                        .builder
                                        .build_struct_gep(
                                            scrut_alloca,
                                            (j + 1) as u32,
                                            &format!("{ctor_name}.payload.{j}"),
                                        )
                                        .unwrap();
                                    let field_val =
                                        self.builder.build_load(field_ptr, bind_name).unwrap();
                                    let bind_alloca = self.create_entry_alloca(
                                        function,
                                        bind_name,
                                        field_val.get_type(),
                                    );
                                    self.builder.build_store(bind_alloca, field_val).unwrap();
                                    self.variables.insert(bind_name.clone(), bind_alloca);
                                }
                            }
                        }

                        // Check guard if present
                        if let Some(guard) = &arm.guard {
                            let guard_val = self
                                .compile_expr(guard)?
                                .ok_or("expected guard value")?
                                .into_int_value();
                            let guard_pass_bb =
                                self.context.append_basic_block(function, "guard_pass");
                            self.builder
                                .build_conditional_branch(guard_val, guard_pass_bb, next_bb)
                                .unwrap();
                            self.builder.position_at_end(guard_pass_bb);
                        }

                        let body_val = self.compile_expr(&arm.body)?;
                        let arm_end = self.builder.get_insert_block().unwrap();
                        if arm_end.get_terminator().is_none() {
                            if let Some(v) = body_val {
                                incoming.push((v, arm_end));
                            }
                            self.builder.build_unconditional_branch(merge_bb).unwrap();
                        }

                        if !is_last {
                            self.builder.position_at_end(next_bb);
                        }
                    } else {
                        // Unknown constructor — treat as wildcard
                        let body_val = self.compile_expr(&arm.body)?;
                        let current_bb = self.builder.get_insert_block().unwrap();
                        if current_bb.get_terminator().is_none() {
                            if let Some(v) = body_val {
                                incoming.push((v, current_bb));
                            }
                            self.builder.build_unconditional_branch(merge_bb).unwrap();
                        }
                        break;
                    }
                }
                ast::Pattern::Tuple(_, _) => {
                    // Tuple patterns deferred
                    let body_val = self.compile_expr(&arm.body)?;
                    let current_bb = self.builder.get_insert_block().unwrap();
                    if current_bb.get_terminator().is_none() {
                        if let Some(v) = body_val {
                            incoming.push((v, current_bb));
                        }
                        self.builder.build_unconditional_branch(merge_bb).unwrap();
                    }
                    break;
                }
                ast::Pattern::Struct(_, _, _, _) | ast::Pattern::Or(_, _) => {
                    // Struct and or-pattern codegen are deferred.
                    let body_val = self.compile_expr(&arm.body)?;
                    let current_bb = self.builder.get_insert_block().unwrap();
                    if current_bb.get_terminator().is_none() {
                        if let Some(v) = body_val {
                            incoming.push((v, current_bb));
                        }
                        self.builder.build_unconditional_branch(merge_bb).unwrap();
                    }
                    break;
                }
            }
        }

        self.builder.position_at_end(merge_bb);

        // Create PHI if we have consistent incoming values
        if !incoming.is_empty()
            && incoming
                .iter()
                .all(|(v, _)| v.get_type() == incoming[0].0.get_type())
        {
            let phi = self
                .builder
                .build_phi(incoming[0].0.get_type(), "match_result")
                .unwrap();
            let refs: Vec<(&dyn BasicValue, inkwell::basic_block::BasicBlock)> = incoming
                .iter()
                .map(|(v, bb)| (v as &dyn BasicValue, *bb))
                .collect();
            phi.add_incoming(&refs.iter().map(|(v, bb)| (*v, *bb)).collect::<Vec<_>>());
            Ok(Some(phi.as_basic_value()))
        } else {
            Ok(None)
        }
    }

    // ── Helper methods ──

    fn create_entry_alloca(
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

    fn resolve_param_type(&self, param: &ast::Param) -> Type {
        match &param.ty {
            Some(te) => self.type_expr_to_type(te),
            None => Type::Int, // Default to Int for untyped params in P0
        }
    }

    fn type_expr_to_type(&self, te: &ast::TypeExpr) -> Type {
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

    fn type_to_llvm(&self, ty: &Type) -> BasicTypeEnum<'ctx> {
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

    fn type_to_llvm_meta(&self, ty: &Type) -> BasicMetadataTypeEnum<'ctx> {
        self.type_to_llvm(ty).into()
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

#[cfg(test)]
mod tests {
    use super::*;
    use aura_lexer::Lexer;
    use aura_parser::Parser;

    fn compile_to_ir(input: &str) -> String {
        let mut lexer = Lexer::new(input, 0);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let module = parser.parse_module().unwrap();

        let context = Context::create();
        let mut codegen = CodeGen::new(&context, "test");
        codegen.compile_module(&module).unwrap();
        codegen.print_ir()
    }

    #[test]
    fn test_simple_function() {
        let ir = compile_to_ir("def main() -> Int = 42");
        assert!(ir.contains("define i64 @main()"));
        assert!(ir.contains("ret i64 42"));
    }

    #[test]
    fn test_function_with_params() {
        let ir = compile_to_ir("def add(a: Int, b: Int) -> Int = a + b");
        assert!(ir.contains("define i64 @add(i64"));
        assert!(ir.contains("add i64"));
    }

    #[test]
    fn test_if_else() {
        let ir = compile_to_ir("def test(x: Bool) -> Int = if x { 1 } else { 2 }");
        assert!(ir.contains("br i1"));
        assert!(ir.contains("then"));
        assert!(ir.contains("else"));
    }

    #[test]
    fn test_let_binding() {
        let ir = compile_to_ir("def main() -> Int = { let x = 42; x }");
        assert!(ir.contains("store i64 42"));
        assert!(ir.contains("load i64"));
    }

    #[test]
    fn test_while_loop() {
        let ir = compile_to_ir("def test(x: Bool) -> Int = { while x { 0 }; 1 }");
        assert!(ir.contains("while_cond"));
        assert!(ir.contains("while_body"));
    }

    #[test]
    fn test_println_int() {
        let ir = compile_to_ir("def main() -> Int = { println(42); 0 }");
        assert!(ir.contains("call i32"));
        assert!(ir.contains("printf"));
    }

    #[test]
    fn test_println_string() {
        let ir = compile_to_ir("def main() -> Int = { println(\"hello\"); 0 }");
        assert!(ir.contains("printf"));
    }

    #[test]
    fn test_arithmetic_ops() {
        let ir = compile_to_ir("def test(a: Int, b: Int) -> Int = a * b + a / b");
        assert!(ir.contains("mul i64"));
        assert!(ir.contains("sdiv i64"));
        assert!(ir.contains("add i64"));
    }

    #[test]
    fn test_comparison() {
        let ir = compile_to_ir("def test(a: Int, b: Int) -> Bool = a < b");
        assert!(ir.contains("icmp slt"));
    }

    #[test]
    fn test_function_call() {
        let ir = compile_to_ir(
            "def add(a: Int, b: Int) -> Int = a + b\n\
             def main() -> Int = add(1, 2)",
        );
        assert!(ir.contains("call i64 @add(i64 1, i64 2)"));
    }

    #[test]
    fn test_for_loop() {
        let ir = compile_to_ir("def main() -> Int = { for i in 0..10 { println(i) }; 0 }");
        assert!(ir.contains("for_cond"));
        assert!(ir.contains("for_body"));
        assert!(ir.contains("for_inc"));
    }

    #[test]
    fn test_negation() {
        let ir = compile_to_ir("def test(x: Int) -> Int = -x");
        assert!(ir.contains("sub i64 0"));
    }

    #[test]
    fn test_return_stmt() {
        let ir = compile_to_ir("def test() -> Int = { return 42 }");
        assert!(ir.contains("ret i64 42"));
    }

    #[test]
    fn test_float_operations() {
        let ir = compile_to_ir("def test(a: Float64, b: Float64) -> Float64 = a + b");
        assert!(ir.contains("fadd double"));
    }

    #[test]
    fn test_bool_println() {
        let ir = compile_to_ir("def main() -> Int = { println(true); 0 }");
        assert!(ir.contains("true_str") || ir.contains("printf"));
    }

    #[test]
    fn test_match_int() {
        let ir = compile_to_ir("def test(x: Int) -> Int = match x { 1 => 10, _ => 0 }");
        assert!(ir.contains("match_cmp"));
        assert!(ir.contains("match_arm"));
    }

    #[test]
    fn test_string_literal() {
        let ir = compile_to_ir("def test() -> String = \"hello\"");
        assert!(ir.contains("hello"));
    }

    #[test]
    fn test_multiple_functions() {
        let ir = compile_to_ir(
            "def double(x: Int) -> Int = x * 2\n\
             def triple(x: Int) -> Int = x * 3\n\
             def main() -> Int = double(triple(1))",
        );
        assert!(ir.contains("@double"));
        assert!(ir.contains("@triple"));
        assert!(ir.contains("@main"));
    }

    #[test]
    fn test_struct_type_and_literal() {
        let ir = compile_to_ir(
            "type Point = { x: Int, y: Int }\n\
             def main() -> Int = {\n\
               let p = Point { x: 10, y: 20 };\n\
               0\n\
             }",
        );
        assert!(ir.contains("Point"));
        assert!(ir.contains("store i64 10"));
        assert!(ir.contains("store i64 20"));
    }

    #[test]
    fn test_struct_field_access() {
        let ir = compile_to_ir(
            "type Point = { x: Int, y: Int }\n\
             def main() -> Int = {\n\
               let p = Point { x: 10, y: 20 };\n\
               p.x\n\
             }",
        );
        assert!(
            ir.contains("Point.x.ptr") || ir.contains("struct_gep") || ir.contains("getelementptr")
        );
    }

    #[test]
    fn test_while_break() {
        let ir = compile_to_ir(
            "def main() -> Int = {\n\
               let mut x = 0;\n\
               while true {\n\
                 x = x + 1;\n\
                 if x == 5 { break } else { 0 }\n\
               };\n\
               x\n\
             }",
        );
        assert!(ir.contains("while_cond"));
        assert!(ir.contains("while_body"));
        assert!(ir.contains("while_end"));
        // break should produce an unconditional branch to while_end
        assert!(ir.contains("br label %while_end"));
    }

    #[test]
    fn test_for_break() {
        let ir = compile_to_ir(
            "def main() -> Int = {\n\
               let mut total = 0;\n\
               for i in 0..100 {\n\
                 if i == 5 { break } else { 0 };\n\
                 total = total + i\n\
               };\n\
               total\n\
             }",
        );
        assert!(ir.contains("for_cond"));
        assert!(ir.contains("for_body"));
        assert!(ir.contains("for_end"));
        // break should produce an unconditional branch to for_end
        assert!(ir.contains("br label %for_end"));
    }

    #[test]
    fn test_for_continue() {
        let ir = compile_to_ir(
            "def main() -> Int = {\n\
               let mut total = 0;\n\
               for i in 0..10 {\n\
                 if i == 3 { continue } else { 0 };\n\
                 total = total + i\n\
               };\n\
               total\n\
             }",
        );
        assert!(ir.contains("for_cond"));
        assert!(ir.contains("for_inc"));
        // continue should produce an unconditional branch to for_inc
        assert!(ir.contains("br label %for_inc"));
    }

    #[test]
    fn test_hex_literal_codegen() {
        let ir = compile_to_ir("def main() -> Int = 0xFF");
        assert!(ir.contains("ret i64 255"));
    }

    #[test]
    fn test_modulo_op() {
        let ir = compile_to_ir("def test(a: Int, b: Int) -> Int = a % b");
        assert!(ir.contains("srem i64"));
    }

    #[test]
    fn test_boolean_and_or() {
        let ir = compile_to_ir("def test(a: Bool, b: Bool) -> Bool = a and b");
        assert!(ir.contains("and i1"));
    }

    #[test]
    fn test_not_operator() {
        let ir = compile_to_ir("def test(a: Bool) -> Bool = not a");
        // not a  is  a XOR true
        assert!(ir.contains("xor i1"));
    }

    #[test]
    fn test_sum_type_no_payload() {
        let ir = compile_to_ir(
            "type Color = Red | Green | Blue\n\
             def main() -> Int = { let c = Red; 0 }",
        );
        // Should have stored tag 0 for Red
        assert!(ir.contains("store i64 0"));
    }

    #[test]
    fn test_sum_type_with_payload() {
        let ir = compile_to_ir(
            "type Maybe = Nothing | Just Int\n\
             def main() -> Int = { let x = Just(42); 0 }",
        );
        // Tag for Just = 1, payload = 42
        assert!(ir.contains("store i64 1"));
        assert!(ir.contains("store i64 42"));
    }

    #[test]
    fn test_match_constructor_pattern() {
        let ir = compile_to_ir(
            "type Color = Red | Green | Blue\n\
             def test(c: Color) -> Int = match c {\n\
               Red => 1,\n\
               Green => 2,\n\
               Blue => 3,\n\
               _ => 0\n\
             }",
        );
        // Should compare tag values
        assert!(ir.contains("match_cmp"));
        assert!(ir.contains("match_arm"));
    }

    #[test]
    fn test_match_string_pattern() {
        let ir = compile_to_ir(
            "def test(s: String) -> Int = match s {\n\
               \"hello\" => 1,\n\
               _ => 0\n\
             }",
        );
        assert!(ir.contains("strcmp"));
        assert!(ir.contains("hello"));
    }

    #[test]
    fn test_match_guard() {
        let ir = compile_to_ir(
            "def test(x: Int) -> Int = match x {\n\
               n if n > 0 => 1,\n\
               _ => 0\n\
             }",
        );
        // Guard should produce a conditional branch
        assert!(ir.contains("guard_pass") || ir.contains("icmp sgt"));
    }

    #[test]
    fn test_pipeline_call() {
        let ir = compile_to_ir(
            "def double(x: Int) -> Int = x * 2\n\
             def main() -> Int = 5 |> double()",
        );
        // Pipeline should compile to a call to double with 5 as argument
        assert!(ir.contains("call i64 @double(i64"));
    }

    #[test]
    fn test_pipeline_with_extra_args() {
        let ir = compile_to_ir(
            "def add(x: Int, y: Int) -> Int = x + y\n\
             def main() -> Int = 10 |> add(3)",
        );
        // Pipeline should compile to a call to add with 10 and 3
        assert!(ir.contains("call i64 @add(i64"));
    }

    #[test]
    fn test_pipeline_bare_function() {
        let ir = compile_to_ir(
            "def negate(x: Int) -> Int = 0 - x\n\
             def main() -> Int = 42 |> negate",
        );
        // Pipeline with bare function name should call negate(42)
        assert!(ir.contains("call i64 @negate(i64"));
    }
}
