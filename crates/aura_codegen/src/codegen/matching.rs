use inkwell::values::{BasicValue, BasicValueEnum};
use inkwell::IntPredicate;

use aura_parser::ast;

use super::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn compile_match(
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
                    let should_break = self.compile_match_wildcard_arm(
                        arm,
                        &scrut_val,
                        function,
                        merge_bb,
                        next_bb,
                        is_last,
                        &mut incoming,
                    )?;
                    if should_break {
                        break;
                    }
                }
                ast::Pattern::Literal(lit, _) => {
                    self.compile_match_literal_arm(
                        arm,
                        lit,
                        &scrut_val,
                        function,
                        merge_bb,
                        next_bb,
                        is_last,
                        &mut incoming,
                    )?;
                }
                ast::Pattern::Constructor(ctor_name, sub_patterns, _) => {
                    let should_break = self.compile_match_constructor_arm(
                        arm,
                        ctor_name,
                        sub_patterns,
                        &scrut_val,
                        function,
                        merge_bb,
                        next_bb,
                        is_last,
                        &mut incoming,
                    )?;
                    if should_break {
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

        self.build_match_phi(&incoming)
    }

    fn compile_match_wildcard_arm(
        &mut self,
        arm: &ast::MatchArm,
        scrut_val: &Option<BasicValueEnum<'ctx>>,
        function: inkwell::values::FunctionValue<'ctx>,
        merge_bb: inkwell::basic_block::BasicBlock<'ctx>,
        next_bb: inkwell::basic_block::BasicBlock<'ctx>,
        is_last: bool,
        incoming: &mut Vec<(BasicValueEnum<'ctx>, inkwell::basic_block::BasicBlock<'ctx>)>,
    ) -> Result<bool, String> {
        // Wildcard/binding: always matches
        // Bind the variable if it's an ident
        if let ast::Pattern::Ident(name, _) = &arm.pattern {
            if let Some(val) = scrut_val {
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
            return Ok(true); // Unconditional wildcard is always last
        }
        if !is_last {
            self.builder.position_at_end(next_bb);
        }
        Ok(false)
    }

    fn compile_match_literal_arm(
        &mut self,
        arm: &ast::MatchArm,
        lit: &ast::LitPattern,
        scrut_val: &Option<BasicValueEnum<'ctx>>,
        function: inkwell::values::FunctionValue<'ctx>,
        merge_bb: inkwell::basic_block::BasicBlock<'ctx>,
        next_bb: inkwell::basic_block::BasicBlock<'ctx>,
        is_last: bool,
        incoming: &mut Vec<(BasicValueEnum<'ctx>, inkwell::basic_block::BasicBlock<'ctx>)>,
    ) -> Result<(), String> {
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
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn compile_match_constructor_arm(
        &mut self,
        arm: &ast::MatchArm,
        ctor_name: &str,
        sub_patterns: &[ast::Pattern],
        scrut_val: &Option<BasicValueEnum<'ctx>>,
        function: inkwell::values::FunctionValue<'ctx>,
        merge_bb: inkwell::basic_block::BasicBlock<'ctx>,
        next_bb: inkwell::basic_block::BasicBlock<'ctx>,
        is_last: bool,
        incoming: &mut Vec<(BasicValueEnum<'ctx>, inkwell::basic_block::BasicBlock<'ctx>)>,
    ) -> Result<bool, String> {
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
                        let field_val = self.builder.build_load(field_ptr, bind_name).unwrap();
                        let bind_alloca =
                            self.create_entry_alloca(function, bind_name, field_val.get_type());
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
            return Ok(true);
        }
        Ok(false)
    }

    fn build_match_phi(
        &self,
        incoming: &[(BasicValueEnum<'ctx>, inkwell::basic_block::BasicBlock<'ctx>)],
    ) -> Result<Option<BasicValueEnum<'ctx>>, String> {
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
}
