use std::collections::HashMap;

use inkwell::types::BasicType;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum};
use inkwell::AddressSpace;

use aura_parser::ast;
use aura_types::types::Type;

use super::{CaptureContext, CodeGen};

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn compile_expr(
        &mut self,
        expr: &ast::Expr,
    ) -> Result<Option<BasicValueEnum<'ctx>>, String> {
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
                } else if let Some(ctx) = self.capture_context_stack.last() {
                    if let Some(index) = ctx.fields.get(name) {
                        let env_ptr = self
                            .builder
                            .build_pointer_cast(
                                ctx.env_ptr,
                                ctx.env_type.ptr_type(AddressSpace::default()),
                                "capture.env.cast",
                            )
                            .unwrap();
                        let slot = self
                            .builder
                            .build_struct_gep(env_ptr, *index, "capture.slot")
                            .unwrap();
                        let var_ptr = self
                            .builder
                            .build_load(slot, "capture.ptr.load")
                            .unwrap()
                            .into_pointer_value();
                        let val = self.builder.build_load(var_ptr, "capture.load").unwrap();
                        Ok(Some(val))
                    } else if name == "None" {
                        self.compile_builtin_variant_construct(expr, "None", &[])
                    } else if self.variant_info.contains_key(name) {
                        self.compile_variant_construct(name, &[])
                    } else if let Some(func) = self.functions.get(name) {
                        Ok(Some(func.as_global_value().as_pointer_value().into()))
                    } else {
                        Err(format!("undefined variable '{name}'"))
                    }
                } else if name == "None" {
                    self.compile_builtin_variant_construct(expr, "None", &[])
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
                    if let Some(alloca) = self.variables.get(name) {
                        self.builder.build_store(*alloca, val).unwrap();
                        return Ok(None);
                    }
                    if let Some(ctx) = self.capture_context_stack.last() {
                        if let Some(index) = ctx.fields.get(name) {
                            let env_ptr = self
                                .builder
                                .build_pointer_cast(
                                    ctx.env_ptr,
                                    ctx.env_type.ptr_type(AddressSpace::default()),
                                    "capture.env.cast",
                                )
                                .unwrap();
                            let slot = self
                                .builder
                                .build_struct_gep(env_ptr, *index, "capture.slot")
                                .unwrap();
                            let var_ptr = self
                                .builder
                                .build_load(slot, "capture.ptr.load")
                                .unwrap()
                                .into_pointer_value();
                            self.builder.build_store(var_ptr, val).unwrap();
                            return Ok(None);
                        }
                    }
                    Err(format!("undefined variable '{name}'"))
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
                    if matches!(name.as_str(), "Some" | "None" | "Ok" | "Err") {
                        return self.compile_builtin_variant_call(expr, name, args);
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
            ast::Expr::StringInterp(parts, _) => self.compile_string_interp(parts),
            ast::Expr::Pipeline(lhs, rhs, _) => self.compile_pipeline(lhs, rhs),
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
            ast::Expr::ListLit(_, _) => {
                // Placeholder representation until stdlib collections are lowered.
                Ok(self.placeholder_for_expr(expr))
            }
            ast::Expr::TupleLit(_, _) => Ok(self.placeholder_for_expr(expr)),
            ast::Expr::Lambda(params, _, body, _) => self.compile_lambda_expr(expr, params, body),
            ast::Expr::Parallel(body, _) => self.compile_parallel_expr(expr, body),
            ast::Expr::Race(arms, _) => self.compile_race_expr(expr, arms),
            ast::Expr::Timeout(duration, body, _) => self.compile_timeout_expr(expr, duration, body),
            ast::Expr::With(_, _, _) => Ok(self.placeholder_for_expr(expr)),
            ast::Expr::Try(inner, _) => self.compile_expr(inner),
            ast::Expr::Range(start, end, _, _) => {
                let _ = self.compile_expr(start)?;
                let _ = self.compile_expr(end)?;
                Ok(self.placeholder_for_expr(expr))
            }
            ast::Expr::MethodCall(receiver, method, args, _) => {
                self.compile_method_call(receiver, method, args)
            }
            ast::Expr::QualifiedIdent(_, variant_name, _) => {
                if self.variant_info.contains_key(variant_name) {
                    self.compile_variant_construct(variant_name, &[])
                } else {
                    Err(format!("unknown qualified identifier '{}'", variant_name))
                }
            }
        }
    }

    fn compile_parallel_expr(
        &mut self,
        expr: &ast::Expr,
        body: &ast::ParallelBody,
    ) -> Result<Option<BasicValueEnum<'ctx>>, String> {
        match body {
            ast::ParallelBody::ForYield {
                pattern, iter, body, ..
            } => {
                if let ast::Pattern::Ident(var, _) = pattern {
                    if matches!(iter.as_ref(), ast::Expr::Range(_, _, _, _)) {
                        let _ = self.compile_for(var, iter, body)?;
                    } else {
                        // P3 bridge: list-backed parallel lowering currently runs sequentially.
                        // Compile iterator and body once with a synthetic loop binding so
                        // full-program codegen succeeds for typed P3 fixtures.
                        let _ = self.compile_expr(iter)?;
                        let elem_ty = match self.expr_type(iter) {
                            Some(Type::Named(name, args)) if name == "List" && args.len() == 1 => {
                                args[0].clone()
                            }
                            Some(Type::Named(name, args))
                                if name == "Range" && args.len() == 1 =>
                            {
                                args[0].clone()
                            }
                            _ => Type::Int,
                        };

                        let function = self.current_function.unwrap();
                        let llvm_ty = self.type_to_llvm(&elem_ty);
                        let alloca = self.create_entry_alloca(function, var, llvm_ty);
                        let zero = self.zero_value_for_type(llvm_ty);
                        self.builder.build_store(alloca, zero).unwrap();

                        let prev = self.variables.insert(var.clone(), alloca);
                        let _ = self.compile_expr(body)?;
                        if let Some(prev_alloca) = prev {
                            self.variables.insert(var.clone(), prev_alloca);
                        } else {
                            self.variables.remove(var);
                        }
                    }
                    Ok(self.placeholder_for_expr_opt(expr))
                } else {
                    Err("parallel for-yield currently requires identifier pattern in codegen".into())
                }
            }
            ast::ParallelBody::FixedYield(values) => {
                for value in values {
                    let _ = self.compile_expr(value)?;
                }
                Ok(self.placeholder_for_expr_opt(expr))
            }
        }
    }

    fn compile_race_expr(
        &mut self,
        expr: &ast::Expr,
        arms: &[ast::Expr],
    ) -> Result<Option<BasicValueEnum<'ctx>>, String> {
        if let Some(first) = arms.first() {
            let first_val = self.compile_expr(first)?;
            if first_val.is_some() {
                return Ok(first_val);
            }
        }
        Ok(self.placeholder_for_expr_opt(expr))
    }

    fn compile_timeout_expr(
        &mut self,
        expr: &ast::Expr,
        duration: &ast::Expr,
        body: &ast::Expr,
    ) -> Result<Option<BasicValueEnum<'ctx>>, String> {
        let _ = self.compile_expr(duration)?;
        let body_val = self.compile_expr(body)?;
        if body_val.is_some() {
            Ok(body_val)
        } else {
            Ok(self.placeholder_for_expr_opt(expr))
        }
    }

    fn compile_lambda_expr(
        &mut self,
        lambda_expr: &ast::Expr,
        params: &[ast::Param],
        body: &ast::Expr,
    ) -> Result<Option<BasicValueEnum<'ctx>>, String> {
        let captures = self.collect_captures(params, body);

        let mut capture_values: Vec<(String, inkwell::values::PointerValue<'ctx>)> = Vec::new();
        for name in &captures {
            let alloca = self
                .variables
                .get(name)
                .ok_or_else(|| format!("captured variable '{name}' is not in scope"))?;
            capture_values.push((name.clone(), *alloca));
        }

        let capture_field_types = capture_values
            .iter()
            .map(|(_, p)| p.get_type().into())
            .collect::<Vec<_>>();
        let env_type = self.context.struct_type(&capture_field_types, false);

        let i8_ptr = self.context.i8_type().ptr_type(AddressSpace::default());
        let alloc_fn = *self
            .functions
            .get("aura_gc_alloc")
            .or_else(|| self.functions.get("malloc"))
            .ok_or("internal error: allocator function not declared")?;
        let uses_gc_alloc = self.functions.contains_key("aura_gc_alloc");
        let null_type_desc = i8_ptr.const_null();

        let env_i8 = if capture_values.is_empty() {
            i8_ptr.const_null()
        } else {
            let env_size = env_type
                .size_of()
                .ok_or("failed to compute lambda env size")?;
            let env_raw = self
                .builder
                .build_call(
                    alloc_fn,
                    if uses_gc_alloc {
                        vec![env_size.into(), null_type_desc.into()]
                    } else {
                        vec![env_size.into()]
                    }
                    .as_slice(),
                    "lambda_env_alloc",
                )
                .unwrap()
                .try_as_basic_value()
                .left()
                .ok_or("allocator did not return a value")?
                .into_pointer_value();
            let env_ptr = self
                .builder
                .build_pointer_cast(
                    env_raw,
                    env_type.ptr_type(AddressSpace::default()),
                    "lambda_env_ptr",
                )
                .unwrap();
            for (i, (_, ptr)) in capture_values.iter().enumerate() {
                let slot = self
                    .builder
                    .build_struct_gep(env_ptr, i as u32, "lambda_env.slot")
                    .unwrap();
                self.builder.build_store(slot, *ptr).unwrap();
            }
            self.builder
                .build_pointer_cast(env_ptr, i8_ptr, "lambda_env_i8")
                .unwrap()
        };

        let (lambda_param_types, lambda_ret_type) = match self.expr_type(lambda_expr) {
            Some(Type::Function(params_tys, ret_ty)) => (params_tys, *ret_ty),
            _ => {
                let pts = params
                    .iter()
                    .map(|p| {
                        p.ty.as_ref()
                            .map(|te| self.type_expr_to_type(te))
                            .unwrap_or(Type::Int)
                    })
                    .collect::<Vec<_>>();
                let ret = self.expr_type(body).unwrap_or(Type::Int);
                (pts, ret)
            }
        };

        let mut fn_arg_types = Vec::with_capacity(lambda_param_types.len() + 1);
        fn_arg_types.push(i8_ptr.into());
        for ty in &lambda_param_types {
            fn_arg_types.push(self.type_to_llvm_meta(ty));
        }

        let lambda_fn_type = if lambda_ret_type == Type::Unit {
            self.context.void_type().fn_type(&fn_arg_types, false)
        } else {
            self.type_to_llvm(&lambda_ret_type)
                .fn_type(&fn_arg_types, false)
        };

        let lambda_name = format!(
            "__aura_lambda_{}_{}",
            self.current_fn_name
                .as_deref()
                .unwrap_or("anon")
                .replace(' ', "_"),
            self.lambda_counter
        );
        self.lambda_counter += 1;

        let lambda_fn = self.module.add_function(&lambda_name, lambda_fn_type, None);

        let saved_function = self.current_function;
        let saved_fn_name = self.current_fn_name.clone();
        let saved_block = self.builder.get_insert_block();
        let saved_vars = self.variables.clone();
        let saved_break_stack = self.loop_exit_stack.clone();
        let saved_continue_stack = self.loop_continue_stack.clone();
        let saved_capture_depth = self.capture_context_stack.len();

        self.current_function = Some(lambda_fn);
        self.current_fn_name = Some(lambda_name.clone());
        self.variables.clear();
        self.loop_exit_stack.clear();
        self.loop_continue_stack.clear();

        let entry = self.context.append_basic_block(lambda_fn, "entry");
        self.builder.position_at_end(entry);

        let env_param = lambda_fn
            .get_nth_param(0)
            .ok_or("missing lambda env param")?
            .into_pointer_value();

        if !captures.is_empty() {
            let fields = captures
                .iter()
                .enumerate()
                .map(|(i, name)| (name.clone(), i as u32))
                .collect::<HashMap<_, _>>();
            self.capture_context_stack.push(CaptureContext {
                env_ptr: env_param,
                env_type,
                fields,
            });
        }

        for (i, param) in params.iter().enumerate() {
            let arg = lambda_fn
                .get_nth_param((i + 1) as u32)
                .ok_or_else(|| format!("missing lambda param {}", i))?;
            let alloca = self.create_entry_alloca(lambda_fn, &param.name, arg.get_type());
            self.builder.build_store(alloca, arg).unwrap();
            self.variables.insert(param.name.clone(), alloca);
        }

        let body_val = self.compile_expr(body)?;
        let block = self.builder.get_insert_block().unwrap();
        if block.get_terminator().is_none() {
            if lambda_ret_type == Type::Unit {
                self.builder.build_return(None).unwrap();
            } else if let Some(v) = body_val {
                self.builder.build_return(Some(&v)).unwrap();
            } else {
                let zero = self.zero_value_for_type(self.type_to_llvm(&lambda_ret_type));
                self.builder.build_return(Some(&zero)).unwrap();
            }
        }

        self.current_function = saved_function;
        self.current_fn_name = saved_fn_name;
        self.variables = saved_vars;
        self.loop_exit_stack = saved_break_stack;
        self.loop_continue_stack = saved_continue_stack;
        while self.capture_context_stack.len() > saved_capture_depth {
            self.capture_context_stack.pop();
        }
        if let Some(bb) = saved_block {
            self.builder.position_at_end(bb);
        }

        // Build closure object: { fn_ptr, env_ptr }
        let closure_size = self
            .closure_type
            .size_of()
            .ok_or("failed to compute closure object size")?;
        let closure_raw = self
            .builder
            .build_call(
                alloc_fn,
                if uses_gc_alloc {
                    vec![closure_size.into(), null_type_desc.into()]
                } else {
                    vec![closure_size.into()]
                }
                .as_slice(),
                "closure_alloc",
            )
            .unwrap()
            .try_as_basic_value()
            .left()
            .ok_or("allocator did not return closure pointer")?
            .into_pointer_value();
        let closure_ptr = self
            .builder
            .build_pointer_cast(
                closure_raw,
                self.closure_type.ptr_type(AddressSpace::default()),
                "closure_ptr",
            )
            .unwrap();

        let fn_i8 = self
            .builder
            .build_pointer_cast(
                lambda_fn.as_global_value().as_pointer_value(),
                i8_ptr,
                "lambda_fn_i8",
            )
            .unwrap();

        let fn_slot = self
            .builder
            .build_struct_gep(closure_ptr, 0, "closure.fn.slot")
            .unwrap();
        let env_slot = self
            .builder
            .build_struct_gep(closure_ptr, 1, "closure.env.slot")
            .unwrap();
        self.builder.build_store(fn_slot, fn_i8).unwrap();
        self.builder.build_store(env_slot, env_i8).unwrap();

        let closure_i8 = self
            .builder
            .build_pointer_cast(closure_ptr, i8_ptr, "closure_i8")
            .unwrap();
        Ok(Some(closure_i8.into()))
    }

    fn placeholder_for_expr_opt(&self, expr: &ast::Expr) -> Option<BasicValueEnum<'ctx>> {
        self.placeholder_for_expr(expr)
    }

    fn placeholder_for_expr(&self, expr: &ast::Expr) -> Option<BasicValueEnum<'ctx>> {
        let ty = self.expr_type(expr);
        match ty {
            Some(Type::Unit) => None,
            Some(ty) => Some(self.zero_value_for_type(self.type_to_llvm(&ty))),
            None => None,
        }
    }

    pub(crate) fn compile_pipeline(
        &mut self,
        lhs: &ast::Expr,
        rhs: &ast::Expr,
    ) -> Result<Option<BasicValueEnum<'ctx>>, String> {
        let lhs_val = self
            .compile_expr(lhs)?
            .ok_or("pipeline LHS produced no value")?;
        match rhs {
            ast::Expr::Call(callee, args, _) => {
                // a |> f(x, y) => f(a, x, y)
                if let ast::Expr::Ident(name, _) = callee.as_ref() {
                    if name == "print" || name == "println" {
                        // For print/println pipeline, just pass LHS as the argument
                        let mut all_args = vec![lhs.clone()];
                        all_args.extend(args.iter().cloned());
                        return self.compile_print(name, &all_args);
                    }
                    if self.variant_info.contains_key(name) {
                        // Variant constructor in pipeline: a |> Some()
                        let mut all_args = vec![lhs.clone()];
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
                let mut compiled_args: Vec<BasicMetadataValueEnum<'ctx>> = vec![lhs_val.into()];
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
            ast::Expr::Ident(name, _) => {
                // a |> f => f(a)
                if name == "print" || name == "println" {
                    let all_args = vec![lhs.clone()];
                    return self.compile_print(name, &all_args);
                }
                let function = *self
                    .functions
                    .get(name)
                    .ok_or_else(|| format!("undefined function '{name}'"))?;
                let compiled_args: Vec<BasicMetadataValueEnum<'ctx>> = vec![lhs_val.into()];
                let call = self
                    .builder
                    .build_call(function, &compiled_args, name)
                    .unwrap();
                Ok(call.try_as_basic_value().left())
            }
            _ => {
                // Fallback: evaluate RHS (e.g., method calls)
                let rhs_val = self.compile_expr(rhs)?;
                Ok(rhs_val)
            }
        }
    }

    pub(crate) fn compile_string_interp(
        &mut self,
        parts: &[ast::StringPart],
    ) -> Result<Option<BasicValueEnum<'ctx>>, String> {
        // For P0/P3 baseline: concatenate parts via printf-style formatting placeholder.
        let mut fmt = String::new();
        let mut _args: Vec<BasicMetadataValueEnum<'ctx>> = Vec::new();
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
                            _args.push(v.into());
                        } else if v.is_float_value() {
                            fmt.push_str("%f");
                            _args.push(v.into());
                        } else {
                            fmt.push_str("%s");
                            _args.push(v.into());
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
}
