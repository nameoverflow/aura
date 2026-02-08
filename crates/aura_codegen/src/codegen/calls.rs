use std::collections::HashSet;
use std::convert::TryFrom;

use inkwell::types::AnyTypeEnum;
use inkwell::types::BasicType;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, CallableValue};
use inkwell::AddressSpace;

use aura_parser::ast;
use aura_types::types::Type;

use super::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn compile_call(
        &mut self,
        callee: &ast::Expr,
        args: &[ast::Expr],
    ) -> Result<Option<BasicValueEnum<'ctx>>, String> {
        // Handle built-in print/println
        if let ast::Expr::Ident(name, _) = callee {
            if name == "print" || name == "println" {
                return self.compile_print(name, args);
            }

            // If this identifier is a local value, treat as first-class callable.
            if self.variables.contains_key(name) {
                return self.compile_callable_value_call(callee, args);
            }

            // Direct top-level function call path.
            if let Some(function) = self.functions.get(name).copied() {
                let mut compiled_args: Vec<BasicMetadataValueEnum<'ctx>> = Vec::new();
                for arg in args {
                    let val = self.compile_expr(arg)?.ok_or("expected argument value")?;
                    compiled_args.push(val.into());
                }

                let call = self
                    .builder
                    .build_call(function, &compiled_args, name)
                    .unwrap();
                return Ok(call.try_as_basic_value().left());
            }
        }

        self.compile_callable_value_call(callee, args)
    }

    pub(crate) fn compile_method_call(
        &mut self,
        receiver: &ast::Expr,
        method: &str,
        args: &[ast::Expr],
    ) -> Result<Option<BasicValueEnum<'ctx>>, String> {
        // P3 bridge: Runtime.block_on(expr) lowers to direct evaluation.
        if matches!(receiver, ast::Expr::Ident(name, _) if name == "Runtime") && method == "block_on"
        {
            if args.len() != 1 {
                return Err(format!(
                    "Runtime.block_on expects 1 argument, got {}",
                    args.len()
                ));
            }
            return self.compile_expr(&args[0]);
        }

        // Best-effort method lowering: value.method(a, b) => method(value, a, b)
        if self.functions.contains_key(method) {
            let mut lowered = Vec::with_capacity(args.len() + 1);
            lowered.push(receiver.clone());
            lowered.extend(args.iter().cloned());
            return self.compile_call(&ast::Expr::Ident(method.into(), receiver.span()), &lowered);
        }

        Err(format!("method call codegen is not implemented for '.{method}'"))
    }

    fn compile_callable_value_call(
        &mut self,
        callee: &ast::Expr,
        args: &[ast::Expr],
    ) -> Result<Option<BasicValueEnum<'ctx>>, String> {
        let callee_val = self
            .compile_expr(callee)?
            .ok_or("expected callable value")?;

        // Function pointer path (plain function values held in variables).
        if let BasicValueEnum::PointerValue(ptr) = callee_val {
            if let AnyTypeEnum::FunctionType(_fn_ty) = ptr.get_type().get_element_type() {
                let mut compiled_args: Vec<BasicMetadataValueEnum<'ctx>> = Vec::new();
                for arg in args {
                    let val = self.compile_expr(arg)?.ok_or("expected argument value")?;
                    compiled_args.push(val.into());
                }
                let callable =
                    CallableValue::try_from(ptr).map_err(|_| "invalid function pointer value")?;
                let call = self
                    .builder
                    .build_call(callable, &compiled_args, "fn_ptr_call")
                    .unwrap();
                return Ok(call.try_as_basic_value().left());
            }
        }

        // Closure object path: value is i8* to { i8* fn, i8* env }.
        let (param_tys, ret_ty) = match self.expr_type(callee) {
            Some(Type::Function(params, ret)) => (params, *ret),
            _ => (
                args.iter().map(|_| Type::Int).collect::<Vec<_>>(),
                Type::Int,
            ),
        };

        let closure_i8_ptr = callee_val.into_pointer_value();
        let closure_ptr = self
            .builder
            .build_pointer_cast(
                closure_i8_ptr,
                self.closure_type.ptr_type(AddressSpace::default()),
                "closure_ptr",
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

        let fn_i8 = self.builder.build_load(fn_slot, "closure.fn").unwrap();
        let env_i8 = self.builder.build_load(env_slot, "closure.env").unwrap();

        let mut fn_arg_types = Vec::with_capacity(param_tys.len() + 1);
        fn_arg_types.push(
            self.context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .into(),
        );
        for ty in &param_tys {
            fn_arg_types.push(self.type_to_llvm_meta(ty));
        }

        let fn_type = if ret_ty == Type::Unit {
            self.context.void_type().fn_type(&fn_arg_types, false)
        } else {
            self.type_to_llvm(&ret_ty).fn_type(&fn_arg_types, false)
        };

        let fn_ptr = self
            .builder
            .build_pointer_cast(
                fn_i8.into_pointer_value(),
                fn_type.ptr_type(AddressSpace::default()),
                "closure.fn.cast",
            )
            .unwrap();
        let callable =
            CallableValue::try_from(fn_ptr).map_err(|_| "invalid closure function pointer")?;

        let mut call_args: Vec<BasicMetadataValueEnum<'ctx>> = Vec::with_capacity(args.len() + 1);
        call_args.push(env_i8.into());
        for arg in args {
            let v = self.compile_expr(arg)?.ok_or("expected argument value")?;
            call_args.push(v.into());
        }

        let call = self
            .builder
            .build_call(callable, &call_args, "closure_call")
            .unwrap();
        Ok(call.try_as_basic_value().left())
    }

    pub(crate) fn collect_captures(&self, params: &[ast::Param], body: &ast::Expr) -> Vec<String> {
        let mut captures = HashSet::new();
        let mut scope_stack: Vec<HashSet<String>> =
            vec![params.iter().map(|p| p.name.clone()).collect()];
        self.collect_captures_inner(body, &mut scope_stack, &mut captures, true);

        let mut out = captures
            .into_iter()
            .filter(|name| self.variables.contains_key(name))
            .collect::<Vec<_>>();
        out.sort();
        out
    }

    fn collect_captures_inner(
        &self,
        expr: &ast::Expr,
        scope_stack: &mut Vec<HashSet<String>>,
        captures: &mut HashSet<String>,
        top_level: bool,
    ) {
        match expr {
            ast::Expr::Ident(name, _) => {
                let defined = scope_stack.iter().rev().any(|scope| scope.contains(name));
                if !defined && self.variables.contains_key(name) {
                    captures.insert(name.clone());
                }
            }
            ast::Expr::Lambda(_, _, _, _) => {
                if top_level {
                    // Skip nested lambda internals; they capture from this lambda, not directly
                    // from the outer function's current frame.
                }
            }
            ast::Expr::Block(exprs, _) => {
                scope_stack.push(HashSet::new());
                for e in exprs {
                    self.collect_captures_inner(e, scope_stack, captures, false);
                    if let ast::Expr::Let(name, _, _, _, _) = e {
                        if let Some(scope) = scope_stack.last_mut() {
                            scope.insert(name.clone());
                        }
                    }
                }
                scope_stack.pop();
            }
            ast::Expr::Let(name, _, _, value, _) => {
                self.collect_captures_inner(value, scope_stack, captures, false);
                if let Some(scope) = scope_stack.last_mut() {
                    scope.insert(name.clone());
                }
            }
            ast::Expr::LetPattern(_, _, _, value, _) => {
                self.collect_captures_inner(value, scope_stack, captures, false);
            }
            ast::Expr::Assign(lhs, rhs, _)
            | ast::Expr::Binary(lhs, _, rhs, _)
            | ast::Expr::Pipeline(lhs, rhs, _)
            | ast::Expr::Range(lhs, rhs, _, _) => {
                self.collect_captures_inner(lhs, scope_stack, captures, false);
                self.collect_captures_inner(rhs, scope_stack, captures, false);
            }
            ast::Expr::Unary(_, inner, _)
            | ast::Expr::FieldAccess(inner, _, _)
            | ast::Expr::Try(inner, _) => {
                self.collect_captures_inner(inner, scope_stack, captures, false);
            }
            ast::Expr::If(cond, then_branch, else_branch, _) => {
                self.collect_captures_inner(cond, scope_stack, captures, false);
                self.collect_captures_inner(then_branch, scope_stack, captures, false);
                if let Some(e) = else_branch {
                    self.collect_captures_inner(e, scope_stack, captures, false);
                }
            }
            ast::Expr::Match(scrut, arms, _) => {
                self.collect_captures_inner(scrut, scope_stack, captures, false);
                for arm in arms {
                    if let Some(g) = &arm.guard {
                        self.collect_captures_inner(g, scope_stack, captures, false);
                    }
                    self.collect_captures_inner(&arm.body, scope_stack, captures, false);
                }
            }
            ast::Expr::For(_, iter, body, _)
            | ast::Expr::ForPattern(_, iter, body, _)
            | ast::Expr::While(iter, body, _) => {
                self.collect_captures_inner(iter, scope_stack, captures, false);
                self.collect_captures_inner(body, scope_stack, captures, false);
            }
            ast::Expr::Call(callee, args, _) | ast::Expr::MethodCall(callee, _, args, _) => {
                self.collect_captures_inner(callee, scope_stack, captures, false);
                for arg in args {
                    self.collect_captures_inner(arg, scope_stack, captures, false);
                }
            }
            ast::Expr::StructLit(_, fields, _) => {
                for (_, e) in fields {
                    self.collect_captures_inner(e, scope_stack, captures, false);
                }
            }
            ast::Expr::With(base, fields, _) => {
                self.collect_captures_inner(base, scope_stack, captures, false);
                for (_, e) in fields {
                    self.collect_captures_inner(e, scope_stack, captures, false);
                }
            }
            ast::Expr::ListLit(values, _)
            | ast::Expr::TupleLit(values, _)
            | ast::Expr::Race(values, _) => {
                for v in values {
                    self.collect_captures_inner(v, scope_stack, captures, false);
                }
            }
            ast::Expr::Parallel(body, _) => match body {
                ast::ParallelBody::ForYield { iter, body, .. } => {
                    self.collect_captures_inner(iter, scope_stack, captures, false);
                    self.collect_captures_inner(body, scope_stack, captures, false);
                }
                ast::ParallelBody::FixedYield(values) => {
                    for v in values {
                        self.collect_captures_inner(v, scope_stack, captures, false);
                    }
                }
            },
            ast::Expr::Timeout(duration, body, _) => {
                self.collect_captures_inner(duration, scope_stack, captures, false);
                self.collect_captures_inner(body, scope_stack, captures, false);
            }
            ast::Expr::StringInterp(parts, _) => {
                for part in parts {
                    if let ast::StringPartKind::Expr(e) = &part.kind {
                        self.collect_captures_inner(e, scope_stack, captures, false);
                    }
                }
            }
            ast::Expr::Return(Some(e), _) => {
                self.collect_captures_inner(e, scope_stack, captures, false);
            }
            ast::Expr::IntLit(_, _)
            | ast::Expr::FloatLit(_, _)
            | ast::Expr::StringLit(_, _)
            | ast::Expr::BoolLit(_, _)
            | ast::Expr::QualifiedIdent(_, _, _)
            | ast::Expr::Return(None, _)
            | ast::Expr::Break(_)
            | ast::Expr::Continue(_)
            | ast::Expr::Unit(_) => {}
        }
    }

    pub(crate) fn compile_print(
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
                } else if kind == "println" && i == args.len() - 1 {
                    "%lld\n"
                } else {
                    "%lld"
                }
            } else if val.is_float_value() {
                if kind == "println" && i == args.len() - 1 {
                    "%f\n"
                } else {
                    "%f"
                }
            } else if kind == "println" && i == args.len() - 1 {
                "%s\n"
            } else {
                "%s"
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
}
