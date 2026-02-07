use std::collections::{HashMap, HashSet};

use aura_common::Span;
use aura_parser::ast::*;
use aura_resolve::{DefId, ResolvedModule};

use super::{Effect, EffectSpec, EffectTerm, TypeChecker, TypeError};

impl TypeChecker {
    pub(crate) fn effect_spec_from_refs(&mut self, refs: &[EffectRef]) -> EffectSpec {
        let mut out = EffectSpec::default();
        for ef in refs {
            let term = self.effect_term_from_name(&ef.name, ef.span);
            out.terms.push(term);
        }
        out
    }

    pub(crate) fn effect_term_from_name(&mut self, name: &str, span: Span) -> EffectTerm {
        if name
            .chars()
            .next()
            .map(|c| c.is_ascii_lowercase())
            .unwrap_or(false)
        {
            return EffectTerm::Var(name.to_string());
        }

        if let Some(effect) = Self::effect_from_name(name) {
            EffectTerm::Concrete(effect)
        } else {
            self.errors.push(TypeError {
                message: format!("unknown effect '{}'", name),
                span,
            });
            EffectTerm::Var(name.to_string())
        }
    }

    pub(crate) fn effect_from_name(name: &str) -> Option<Effect> {
        match name {
            "Db.Read" => Some(Effect::DbRead),
            "Db.Write" => Some(Effect::DbWrite),
            "Net" => Some(Effect::Net),
            "Fs.Read" => Some(Effect::FsRead),
            "Fs.Write" => Some(Effect::FsWrite),
            "Log" => Some(Effect::Log),
            "Time" => Some(Effect::Time),
            "Random" => Some(Effect::Random),
            "Env" => Some(Effect::Env),
            _ => None,
        }
    }

    pub(crate) fn effect_implied(effect: Effect) -> Option<Effect> {
        match effect {
            Effect::DbWrite => Some(Effect::DbRead),
            Effect::FsWrite => Some(Effect::FsRead),
            _ => None,
        }
    }

    pub(crate) fn with_implied_effects(&self, effects: &HashSet<Effect>) -> HashSet<Effect> {
        let mut out = effects.clone();
        let mut changed = true;
        while changed {
            changed = false;
            let current: Vec<Effect> = out.iter().copied().collect();
            for e in current {
                if let Some(implied) = Self::effect_implied(e) {
                    if out.insert(implied) {
                        changed = true;
                    }
                }
            }
        }
        out
    }

    pub(crate) fn effect_vars(spec: &EffectSpec) -> HashSet<String> {
        let mut vars = HashSet::new();
        for t in &spec.terms {
            if let EffectTerm::Var(v) = t {
                vars.insert(v.clone());
            }
        }
        vars
    }

    pub(crate) fn effect_concretes(spec: &EffectSpec) -> HashSet<Effect> {
        let mut out = HashSet::new();
        for t in &spec.terms {
            if let EffectTerm::Concrete(e) = t {
                out.insert(*e);
            }
        }
        out
    }

    pub(crate) fn instantiate_effect_spec(
        &self,
        spec: &EffectSpec,
        bindings: &HashMap<String, HashSet<Effect>>,
    ) -> HashSet<Effect> {
        let mut out = HashSet::new();
        for t in &spec.terms {
            match t {
                EffectTerm::Concrete(e) => {
                    out.insert(*e);
                }
                EffectTerm::Var(v) => {
                    if let Some(bound) = bindings.get(v) {
                        out.extend(bound.iter().copied());
                    }
                }
            }
        }
        self.with_implied_effects(&out)
    }

    pub(crate) fn effect_set_to_string(&self, effects: &HashSet<Effect>) -> String {
        let mut names = effects
            .iter()
            .map(|e| match e {
                Effect::DbRead => "Db.Read",
                Effect::DbWrite => "Db.Write",
                Effect::Net => "Net",
                Effect::FsRead => "Fs.Read",
                Effect::FsWrite => "Fs.Write",
                Effect::Log => "Log",
                Effect::Time => "Time",
                Effect::Random => "Random",
                Effect::Env => "Env",
            })
            .collect::<Vec<_>>();
        names.sort();
        format!("[{}]", names.join(", "))
    }

    pub(crate) fn record_effect_usage(&mut self, effects: &HashSet<Effect>) {
        if let Some(top) = self.effect_usage_stack.last_mut() {
            top.extend(effects.iter().copied());
        }
    }

    pub(crate) fn callable_effects_from_expr(
        &self,
        expr: &Expr,
        resolved: &ResolvedModule,
    ) -> Option<HashSet<Effect>> {
        match expr {
            Expr::Lambda(_, _, _, span) => {
                self.lambda_effects.get(&(span.start, span.end)).cloned()
            }
            Expr::Ident(_, span) => {
                let id = resolved.references.get(&(span.start, span.end))?;
                let scheme = self.fn_effects.get(id)?;
                Some(self.with_implied_effects(&Self::effect_concretes(&scheme.declared)))
            }
            _ => None,
        }
    }

    pub(crate) fn infer_call_required_effects(
        &mut self,
        callee_id: DefId,
        args: &[Expr],
        resolved: &ResolvedModule,
        span: Span,
    ) -> HashSet<Effect> {
        let Some(scheme) = self.fn_effects.get(&callee_id).cloned() else {
            return HashSet::new();
        };

        let mut bindings: HashMap<String, HashSet<Effect>> = HashMap::new();
        for (arg, param_spec_opt) in args.iter().zip(scheme.param_effects.iter()) {
            let Some(param_spec) = param_spec_opt else {
                continue;
            };
            let Some(arg_effects) = self.callable_effects_from_expr(arg, resolved) else {
                continue;
            };

            let required = self.with_implied_effects(&Self::effect_concretes(param_spec));
            if !required.is_subset(&arg_effects) {
                self.errors.push(TypeError {
                    message: format!(
                        "call argument callback effects {} do not satisfy required {}",
                        self.effect_set_to_string(&arg_effects),
                        self.effect_set_to_string(&required)
                    ),
                    span: arg.span(),
                });
            }

            let vars = Self::effect_vars(param_spec)
                .into_iter()
                .collect::<Vec<_>>();
            let extra = arg_effects
                .difference(&required)
                .copied()
                .collect::<HashSet<_>>();

            if vars.is_empty() && !extra.is_empty() {
                self.errors.push(TypeError {
                    message: format!(
                        "effectful callback {} is not allowed by parameter effect annotation",
                        self.effect_set_to_string(&arg_effects)
                    ),
                    span: arg.span(),
                });
            }

            for v in vars {
                bindings.entry(v).or_default().extend(extra.iter().copied());
            }
        }

        let required = self.instantiate_effect_spec(&scheme.declared, &bindings);
        self.check_call_effects(&required, span);
        required
    }

    pub(crate) fn check_call_effects(&mut self, required: &HashSet<Effect>, span: Span) {
        self.record_effect_usage(required);

        if self.suspend_effect_checks > 0 {
            return;
        }

        let Some(ctx) = self.effect_context_stack.last() else {
            return;
        };
        let available = self.with_implied_effects(&ctx.allowed);
        let missing = required
            .difference(&available)
            .copied()
            .collect::<HashSet<_>>();

        if !missing.is_empty() && ctx.vars.is_empty() {
            self.errors.push(TypeError {
                message: format!(
                    "missing capabilities: required {}, available {}",
                    self.effect_set_to_string(required),
                    self.effect_set_to_string(&available)
                ),
                span,
            });
        }
    }
}
