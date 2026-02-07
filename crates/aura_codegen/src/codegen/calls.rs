use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum};

use aura_parser::ast;

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
}
