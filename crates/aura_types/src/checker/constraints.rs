use aura_common::Span;
use aura_parser::ast::*;

use crate::types::Type;

use super::{RefinedTypeInfo, TypeChecker, TypeError};

impl TypeChecker {
    pub(crate) fn can_convert_error_type(&mut self, source: &Type, target: &Type) -> bool {
        if self.apply(source) == self.apply(target) {
            return true;
        }

        let Some(source_name) = self.type_name_key(source) else {
            return false;
        };
        let Some(target_name) = self.type_name_key(target) else {
            return false;
        };

        if self
            .auto_from_variants
            .contains_key(&(source_name.clone(), target_name.clone()))
        {
            return true;
        }

        if let Some(inst) = self
            .concept_instances
            .get(&("From".to_string(), target_name.clone()))
        {
            if let Some(scheme) = inst.methods.get("from").cloned() {
                let (fn_ty, _) = self.instantiate_scheme_with_bounds(&scheme);
                if let Type::Function(params, ret) = fn_ty {
                    if params.len() == 1
                        && self.apply(&params[0]) == self.apply(source)
                        && self.apply(&ret) == self.apply(target)
                    {
                        return true;
                    }
                }
            }
        }

        false
    }

    pub(crate) fn check_refined_constraint_on_literal(
        &self,
        refined: &str,
        base_expr: &Expr,
        info: &RefinedTypeInfo,
    ) -> Option<bool> {
        self.eval_refined_constraint(refined, base_expr, &info.constraint)
    }

    pub(crate) fn eval_refined_constraint(&self, refined: &str, value: &Expr, expr: &Expr) -> Option<bool> {
        let _ = refined;
        match expr {
            Expr::Binary(lhs, BinOp::And, rhs, _) => {
                Some(self.eval_refined_constraint(refined, value, lhs)? && self.eval_refined_constraint(refined, value, rhs)?)
            }
            Expr::Binary(lhs, BinOp::Or, rhs, _) => {
                Some(self.eval_refined_constraint(refined, value, lhs)? || self.eval_refined_constraint(refined, value, rhs)?)
            }
            Expr::Unary(UnaryOp::Not, inner, _) => Some(!self.eval_refined_constraint(refined, value, inner)?),
            Expr::Binary(lhs, op, rhs, _) => {
                let lv = self.eval_constraint_numeric(value, lhs)?;
                let rv = self.eval_constraint_numeric(value, rhs)?;
                match op {
                    BinOp::Eq => Some((lv - rv).abs() < f64::EPSILON),
                    BinOp::NotEq => Some((lv - rv).abs() >= f64::EPSILON),
                    BinOp::Lt => Some(lv < rv),
                    BinOp::Gt => Some(lv > rv),
                    BinOp::LtEq => Some(lv <= rv),
                    BinOp::GtEq => Some(lv >= rv),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    pub(crate) fn eval_constraint_numeric(&self, self_value: &Expr, expr: &Expr) -> Option<f64> {
        match expr {
            Expr::Ident(name, _) if name == "self" => match self_value {
                Expr::IntLit(n, _) => Some(*n as f64),
                Expr::FloatLit(n, _) => Some(*n),
                _ => None,
            },
            Expr::IntLit(n, _) => Some(*n as f64),
            Expr::FloatLit(n, _) => Some(*n),
            Expr::Binary(lhs, BinOp::Add, rhs, _) => {
                Some(self.eval_constraint_numeric(self_value, lhs)? + self.eval_constraint_numeric(self_value, rhs)?)
            }
            Expr::Binary(lhs, BinOp::Sub, rhs, _) => {
                Some(self.eval_constraint_numeric(self_value, lhs)? - self.eval_constraint_numeric(self_value, rhs)?)
            }
            Expr::Binary(lhs, BinOp::Mul, rhs, _) => {
                Some(self.eval_constraint_numeric(self_value, lhs)? * self.eval_constraint_numeric(self_value, rhs)?)
            }
            Expr::Binary(lhs, BinOp::Div, rhs, _) => {
                Some(self.eval_constraint_numeric(self_value, lhs)? / self.eval_constraint_numeric(self_value, rhs)?)
            }
            Expr::Binary(lhs, BinOp::Mod, rhs, _) => {
                Some(self.eval_constraint_numeric(self_value, lhs)? % self.eval_constraint_numeric(self_value, rhs)?)
            }
            _ => None,
        }
    }

    pub(crate) fn validate_refined_constraint(&mut self, expr: &Expr, span: Span) {
        if !self.is_valid_refined_constraint_expr(expr) {
            self.errors.push(TypeError {
                message: "invalid refined constraint: only comparisons, boolean operators, arithmetic, self/field access, and limited built-in methods are allowed".into(),
                span,
            });
        }
    }

    pub(crate) fn is_valid_refined_constraint_expr(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Ident(name, _) => {
                name == "self"
                    || name
                        .chars()
                        .next()
                        .map(|c| c.is_ascii_uppercase())
                        .unwrap_or(false)
            }
            Expr::IntLit(_, _)
            | Expr::FloatLit(_, _)
            | Expr::StringLit(_, _)
            | Expr::BoolLit(_, _) => true,
            Expr::Unary(UnaryOp::Not | UnaryOp::Neg, inner, _) => {
                self.is_valid_refined_constraint_expr(inner)
            }
            Expr::Binary(lhs, op, rhs, _) => matches!(
                op,
                BinOp::Eq
                    | BinOp::NotEq
                    | BinOp::Lt
                    | BinOp::Gt
                    | BinOp::LtEq
                    | BinOp::GtEq
                    | BinOp::And
                    | BinOp::Or
                    | BinOp::Add
                    | BinOp::Sub
                    | BinOp::Mul
                    | BinOp::Div
                    | BinOp::Mod
            ) && self.is_valid_refined_constraint_expr(lhs)
                && self.is_valid_refined_constraint_expr(rhs),
            Expr::FieldAccess(base, _, _) => self.is_valid_refined_constraint_expr(base),
            Expr::MethodCall(base, name, args, _) => {
                matches!(name.as_str(), "len" | "matches" | "contains")
                    && self.is_valid_refined_constraint_expr(base)
                    && args.iter().all(|a| self.is_valid_refined_constraint_expr(a))
            }
            _ => false,
        }
    }
}
