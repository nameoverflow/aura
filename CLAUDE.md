# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Aura is an AI-native programming language compiler. The compiler is a Rust workspace targeting LLVM 14 via inkwell, producing native executables linked against a GC runtime.

## Build & Test Commands

```bash
cargo build                          # Build all crates
cargo test --workspace               # Run all ~222 tests
cargo test -p aura_codegen           # Test a single crate
cargo test -p aura_types -- test_name  # Run a single test by name

# Compile and run an Aura program
cargo run -p aurac -- file.aura -o /tmp/out && /tmp/out
cargo run -p aura -- run file.aura
cargo run -p aura -- check file.aura   # Type-check only (no codegen)
cargo run -p aurac -- file.aura --emit-ir  # Print LLVM IR

cargo fmt --all                      # Format
cargo clippy --workspace             # Lint
```

Requires LLVM 14. If non-standard path: `export LLVM_SYS_140_PREFIX=/path/to/llvm`

## Compiler Pipeline Architecture

```
Source string
  → aura_parser::parse()  (ast::Module — internally lexes via aura_lexer then parses via Chumsky)
  → aura_resolve           (ResolvedModule: AST + span→DefId references)
  → aura_types             (TypedModule: span→Type mappings)
  → aura_codegen           (LLVM IR → .o → linked executable via cc + libaura_rt.a)
```

Each stage consumes the output of the previous. No circular dependencies between crates.
The lexer and parser both use Chumsky 0.11 parser combinators; `aura_parser::parse(source, file_id)` is the unified entry point.

### Crate Roles

| Crate | Role |
|-------|------|
| `aura_common` | `Interner` (symbol table), `Span` (source locations) |
| `aura_lexer` | Chumsky-based tokenizer |
| `aura_parser` | Chumsky-based parser producing `ast::Module` |
| `aura_resolve` | Name resolution: binds identifier spans to `DefId`s, maps variants to parent types |
| `aura_types` | Hindley-Milner type inference, concept (trait) dispatch, effect checking |
| `aura_codegen` | LLVM IR emission via inkwell; struct/sum type layout; object file output |
| `aura_rt` | C-FFI runtime: mark-sweep GC, shadow stack, print/panic helpers |
| `aura` | Project CLI (`aura build/run/check`) |
| `aurac` | Direct compiler CLI (`aurac file.aura`) |

### Key Data Flow Between Stages

- **Resolver → TypeChecker**: `ResolvedModule` contains `references: HashMap<(u32,u32), DefId>` mapping expression spans to definition IDs, plus `variant_types: HashMap<String, String>` mapping variant names to parent types.
- **TypeChecker → CodeGen**: `TypedModule` contains `expr_types: HashMap<(u32,u32), Type>` — the resolved type for every expression by span. CodeGen calls `set_expr_types()` to receive this.
- **Sum types in codegen**: Tagged unions with `{ i64 tag, payload... }`. `SumTypeInfo` maps variant names to `(tag_index, payload_types)`.
- **Runtime linking**: Both CLIs auto-build `aura_rt` as a static library (`libaura_rt.a`) if missing, then link via `cc`.

## Test Organization

Tests use source fixtures in `tests/sources/` organized by feature:

- **`tests/sources/e2e/*.aura`** — End-to-end: compile + run + check output (in `crates/aurac/tests/e2e_tests.rs`)
- **`tests/sources/{feature}/*.aura`** — Type-check fixtures (in `crates/aura_types/tests/p{1,2,3}_sources.rs`)
  - `ok_*.aura` files must pass type checking
  - `err_*.aura` files must fail type checking
- **Unit tests** are `#[cfg(test)] mod tests` inside each crate's source files

Feature directories map to tiers: P1 (generics, concepts, pattern_matching, type_aliases, algebraic_types, with_expressions), P2 (effects, effect_polymorphism, contracts, refined_types, error_handling), P3 (async, parallel, race, timeout, runtime_bridge).

## Aura Language Syntax

Aura has its own syntax — it is not Rust, Haskell, or any existing language. Key rules:

- `def` for functions (not `fn`), `concept` for type classes (not `trait`), `instance` for impls
- `= { ... }` function bodies: `def foo(x: Int) -> Int = { x + 1 }`
- Haskell-style generics: `List a`, `Option User`, `Result a e` (not angle brackets)
- Nested generics use parens: `List (Result User Error)`
- `forall (Constraint a).` for concept bounds (only when bounds needed)
- Free lowercase type vars are implicitly universally quantified
- Lambdas: `(x) -> expr` (not `|x| expr`)
- Product types: `A * B * C`; values: `(a, b, c)`
- Block-only `if`: `if cond { a } else { b }` (no `then`)
- Sum type variants with payloads use space: `Failed String` in type def, `Failed(reason)` in pattern
- Variants are context-resolved: bare in match arms, qualified (`Status.Pending`) when ambiguous
- `pub` for visibility; module-private by default
- `::` for module paths in imports: `use std::collections::HashMap`
- Standalone type annotations on own line before `def` (only for polymorphic functions)
- Effects in function signatures: `def fetch(id: UUID) -> Result User DbError [Db.Read]`

## Codegen Status

Codegen currently supports: functions, arithmetic, comparisons, if/else, while/for, break/continue, match (int/bool/string/constructor + guards), structs, sum types, print/println, pipeline `|>`.

Deferred (front-end complete, codegen not yet): method calls, concept dispatch, monomorphization, lambdas, tuples, lists, try, ranges.

## Workflow Rules

After any feature change, update the relevant docs and status sections:
- `docs/impl/` implementation plans (mark completed items, update status)
- `docs/aura-lang-proposal.md` language spec if syntax/semantics changed
- Codegen Status section in this file and `README.md` if codegen coverage changed
- MEMORY.md compiler status if test counts or tier completion changed

## Implementation Docs

`docs/impl/` contains implementation plans organized by tier (p0-p4). `docs/aura-lang-proposal.md` is the full language specification (v0.6).
