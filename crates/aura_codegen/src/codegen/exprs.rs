use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum};

use aura_parser::ast;

use super::CodeGen;

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

    pub(crate) fn compile_pipeline(
        &mut self,
        lhs: &ast::Expr,
        rhs: &ast::Expr,
    ) -> Result<Option<BasicValueEnum<'ctx>>, String> {
        let lhs_val = self.compile_expr(lhs)?.ok_or("pipeline LHS produced no value")?;
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
                // Fallback: evaluate RHS (e.g., method calls deferred)
                let rhs_val = self.compile_expr(rhs)?;
                Ok(rhs_val)
            }
        }
    }

    pub(crate) fn compile_string_interp(
        &mut self,
        parts: &[ast::StringPart],
    ) -> Result<Option<BasicValueEnum<'ctx>>, String> {
        // For P0: concatenate parts via printf-style formatting
        // Simple approach: just format the string with printf
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
