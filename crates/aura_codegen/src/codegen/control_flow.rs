use inkwell::values::BasicValueEnum;
use inkwell::IntPredicate;

use aura_parser::ast;

use super::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn compile_if(
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

    pub(crate) fn compile_while(
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

    pub(crate) fn compile_for(
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
}
