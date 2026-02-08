# Aura

Aura is a language and compiler project for **AI-agent-friendly software engineering**.

The goal is not just “another language.”  
The goal is a language where humans and AI agents can build, review, and evolve systems with high confidence.

Aura’s language design and implementation in this repository are fully produced by AI agents.

## Language Design Goal

Aura is designed for the AI era:

- **Machine-checkable intent**: types, effects, contracts, and structured control flow make behavior explicit.
- **Low ambiguity for agents**: syntax and semantics favor predictable interpretation over clever shortcuts.
- **Auditability by default**: correctness-relevant rules are enforced at compile time, not by convention.
- **Scalable collaboration**: code should be understandable to humans, analyzable by tools, and operable by autonomous agents.

In short: Aura treats software as a shared artifact between people and intelligent tooling.

## Design Taste

- **Compile-first architecture**: LLVM-native code generation is the baseline path.
- **Static semantics with practical inference**: strong type guarantees without excessive annotation burden.
- **Effect-aware programming model**: side effects are typed and propagated explicitly.
- **Structured concurrency**: concurrency is scoped and composable (`parallel`, `race`, `timeout`).
- **Runtime-backed safety**: closures and heap values integrate with a GC runtime.
- **Engineering clarity over magic**: explicit boundaries, explicit contracts, explicit failure paths.

## What “AI-Agent-Friendly” Means in Practice

Aura emphasizes properties that make autonomous coding systems safer and more reliable:

- **Clear interfaces**: function signatures carry richer semantics (types, effects, contracts).
- **Deterministic reasoning surfaces**: fewer hidden side effects and implicit runtime behavior.
- **Composable safety constraints**: effects and types become guardrails for generated/refactored code.
- **Better static feedback loops**: agents can quickly validate edits through compiler diagnostics.
- **Reduced semantic drift**: explicit semantics reduce “looks right but means something else” edits.

## Technical Overview

### Compiler Pipeline

```
Aura source
  -> aura_lexer      (tokens + spans)
  -> aura_parser     (AST)
  -> aura_resolve    (name/scope + symbol binding)
  -> aura_types      (type/effect/constraint checking)
  -> aura_codegen    (LLVM IR + object emission)
  -> system linker + aura_rt
  -> native executable
```

### Runtime Model

- Compiled binaries link against `aura_rt`.
- Runtime includes mark-sweep GC and runtime helper entry points.
- Closures are lowered to callable + environment forms.
- Closure environments/objects allocate through runtime GC APIs.
- Async bridge and structured concurrency constructs lower through backend/runtime paths.

### Workspace Crates

- `crates/aura_common`: shared compiler primitives
- `crates/aura_lexer`: lexical analysis
- `crates/aura_parser`: parser + AST
- `crates/aura_resolve`: name resolution
- `crates/aura_types`: type/effect checker
- `crates/aura_codegen`: LLVM backend (`inkwell`)
- `crates/aura_rt`: runtime library
- `crates/aura`: project-oriented CLI
- `crates/aurac`: direct compiler CLI

## Language Features (Current Surface)

- Algebraic data types, pattern matching, and product types
- Generic functions/types with concept-style constraints
- Methods and associated-type style modeling
- Effect tracking and effect polymorphism
- Error propagation with `?` over `Result`/`Option`-like flows
- Refined/contract-like typing checks
- Closures with capture semantics
- Structured concurrency primitives and sync/async bridge

## Example

```aura
type Shape = Circle Int | Rectangle Int Int | Nothing

def area(s: Shape) -> Int = match s {
    Circle(r) => r * r * 3,
    Rectangle(w, h) => w * h,
    Nothing => 0,
    _ => 0
}

async def fetch(x: Int) -> Int = x

def main() -> Int = {
    let n = Runtime.block_on(fetch(5));
    println(area(Circle(n)));
    0
}
```

## Prerequisites

- Rust toolchain (Rust 2021 workspace)
- LLVM 14 (for `inkwell` / `llvm-sys` compatibility)
- System C toolchain/linker (`cc`)

If LLVM is installed in a non-standard path, set `LLVM_SYS_140_PREFIX`.

## Quick Start

```bash
# Build workspace
cargo build

# Type-check (default: src/main.aura)
cargo run -p aura -- check

# Build and run a program
cargo run -p aura -- run path/to/main.aura
```

## CLI Reference

### `aura` (project workflow)

```bash
cargo run -p aura -- check [path]
cargo run -p aura -- build [--release] [--emit-ir] [--no-link] [path] [-o output]
cargo run -p aura -- run [--release] [path]
```

### `aurac` (direct compiler workflow)

```bash
cargo run -p aurac -- file.aura --emit-ir
cargo run -p aurac -- file.aura -o ./program
cargo run -p aurac -- file.aura --no-link
```

## Repository Layout

- `crates/`: compiler/runtime/CLI crates
- `docs/`: language proposal and deep design notes
- `tests/sources/`: Aura source programs organized by feature

## Documentation

- `docs/aura-lang-proposal.md`
- `docs/impl/README.md`
- `docs/impl/p0-core-compiler/`
- `docs/impl/p1-type-system/`
- `docs/impl/p2-effects-errors/`
- `docs/impl/p3-runtime/`

## License

MIT (see workspace `Cargo.toml`).
