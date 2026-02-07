use inkwell::values::BasicValueEnum;
use inkwell::{FloatPredicate, IntPredicate};

use aura_parser::ast;

use super::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn compile_binary_op(
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
}
