# Aura Implementation Plan

This directory contains detailed implementation plans for the Aura programming language, organized by priority tier from critical to optional.

## Priority Tiers

| Tier | Focus | Goal |
|------|-------|------|
| **P0** | Core Compiler | Parse, type-check, and compile a minimal Aura program to native code via LLVM |
| **P1** | Type System & Concepts | Full algebraic types, generics, pattern matching, concepts |
| **P2** | Effects & Error Handling | Effect system, `?` operator, refined types |
| **P3** | Runtime & Memory | Mark-sweep GC, closures, async, runtime system |
| **P4** | Stdlib & Tooling | Standard library, WASM target, optimizer, formatter, LSP, AI tools |

## Dependency Graph (Tiers)

```
P0 ──> P1 ──> P2 ──> P3
                 \      \
                  \──────>── P4
```

P0 is the absolute foundation (including basic LLVM codegen). P1 builds the full type system on P0's base. P2 adds the effect and error layers. P3 adds the GC runtime, closures, and async. P4 is largely parallelizable once P2 is underway.

## Milestones

What a working system looks like after completing each tier:

**After P0:** Can parse Aura source files into an AST, resolve names, type-check simple monomorphic programs, and compile them to native executables via LLVM. Basic programs with `let`, `if`, functions, and structs compile and run natively.

**After P0 + P1:** Full type system — generic types and functions with implicit quantification, concept definitions and implementations, exhaustive pattern matching, operator desugaring, methods via instance blocks. Can type-check and compile programs that use `List a`, `Option a`, `forall (Ord a).`, and user-defined concepts.

**After P0 + P1 + P2:** Effect-checked programs — the compiler verifies capability annotations, propagates effects through higher-order functions, and supports `?` with auto-`From` error conversion. Refined types with runtime validation work. Programs with `[Db.Read, Net, Log]` effect lists are verified.

**After P0 + P1 + P2 + P3:** Complete compiled language — mark-sweep GC for automatic memory management, closures with proper capture semantics, async/await with structured concurrency (`parallel`, `race`, `timeout`). Full CLI with `aura build`, `aura run`, `aura check`. This is the **minimum viable language**.

**After P4:** Production-ready ecosystem — WASM target, optimized code generation, standard library with collections, formatter, linter, LSP for IDE support, package manager, AI-oriented manifest/index/context tools.

## Directory Structure

```
docs/impl/
├── README.md                          # This file
├── p0-core-compiler/
│   ├── 01-lexer.md                    # Tokenization
│   ├── 02-parser-ast.md               # Parsing and AST design
│   ├── 03-name-resolution.md          # Scoping and name lookup
│   ├── 04-basic-types.md              # Core type representation and inference
│   └── 05-basic-codegen.md            # LLVM IR generation and compilation
├── p1-type-system/
│   ├── 01-algebraic-types.md          # Sum types, product types, structs
│   ├── 02-generics.md                 # Parametric polymorphism, implicit quantification
│   ├── 03-concepts.md                 # Concept definitions, implementations, dispatch
│   ├── 04-pattern-matching.md         # Exhaustiveness, guards, destructuring
│   └── 05-methods.md                  # Instance blocks, inherent methods, Self
├── p2-effects-errors/
│   ├── 01-effect-system.md            # Effect tracking, capability checking
│   ├── 02-effect-polymorphism.md      # Effect variables, inference, union
│   ├── 03-error-handling.md           # Result, ?, auto-From derivation
│   └── 04-refined-types.md            # Where clauses, constraint grammar, ConstraintError
├── p3-runtime/
│   ├── 01-gc-memory.md                # Mark-sweep garbage collector
│   ├── 02-closures.md                 # Capture semantics, GC integration
│   ├── 03-async-concurrency.md        # Async/await, parallel, race, timeout
│   └── 04-runtime-system.md           # Runtime library, CLI, entry point
└── p4-stdlib-tooling/
    ├── 01-stdlib-core.md              # Core types, Option, Result, collections
    ├── 02-ir-codegen.md               # Advanced codegen, WASM target, monomorphization
    ├── 03-optimizer.md                # Optimization passes
    ├── 04-formatter-linter.md         # aura fmt, aura lint
    ├── 05-lsp.md                      # Language server protocol
    ├── 06-package-manager.md          # aura add/remove/update, vendoring
    └── 07-ai-integration.md           # Manifest, semantic index, context extractor
```

## Complexity Summary

| Document | Estimated Complexity |
|----------|---------------------|
| **P0: Core Compiler** | |
| P0-01: Lexer | M |
| P0-02: Parser & AST | L |
| P0-03: Name Resolution | M |
| P0-04: Basic Types & Inference | L |
| P0-05: Basic Codegen (LLVM) | XL |
| **P1: Type System** | |
| P1-01: Algebraic Types | M |
| P1-02: Generics & Quantification | L |
| P1-03: Concepts | XL |
| P1-04: Pattern Matching | L |
| P1-05: Methods & Instance Blocks | M |
| **P2: Effects & Errors** | |
| P2-01: Effect System | M |
| P2-02: Effect Polymorphism | L |
| P2-03: Error Handling | M |
| P2-04: Refined Types | M |
| **P3: Runtime** | |
| P3-01: Mark-Sweep GC | L |
| P3-02: Closures | M |
| P3-03: Async & Concurrency | XL |
| P3-04: Runtime System | L |
| **P4: Stdlib & Tooling** | |
| P4-01: Standard Library Core | XL |
| P4-02: Advanced Codegen & WASM | XL |
| P4-03: Optimizer | L |
| P4-04: Formatter & Linter | M+M |
| P4-05: LSP | XL |
| P4-06: Package Manager | L |
| P4-07: AI Integration | L |

Sizes: **S** = days, **M** = 1-2 weeks, **L** = 2-4 weeks, **XL** = 1-2 months (rough estimates for a small team).

## Implementation Language

The compiler is implemented in Rust, targeting LLVM for native code generation. WASM support is added as a secondary target in P4.

## How to Read These Docs

Each document follows a consistent structure:

1. **Overview** - What this component does and why it matters
2. **Dependencies** - What must exist before this can be built
3. **Design Decisions** - Key choices and trade-offs
4. **Implementation Steps** - Ordered subtasks
5. **Data Structures** - Key types and representations
6. **Testing Strategy** - How to validate correctness
7. **Open Questions** - Unresolved decisions
8. **Estimated Complexity** - Relative sizing (S/M/L/XL)
