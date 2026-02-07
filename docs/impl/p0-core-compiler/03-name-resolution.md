# P0-03: Name Resolution

## Overview

Name resolution maps every identifier in the AST to its definition. This is the bridge between parsing (syntactic structure) and type checking (semantic analysis). It handles scoping rules, imports, visibility (`pub`), and variant resolution.

## Dependencies

- **P0-02: Parser & AST** — the AST to resolve names in

## Current Implementation Status (as of February 7, 2026)

- **Implemented:** Lexical-scope name resolution for locals, parameters, functions, types, variants, concepts, and instance method bodies.
- **Implemented:** Built-in prelude registration for core types/functions/variants plus newer runtime-adjacent names (`Duration`, `Timeout`, `Runtime`).
- **Implemented:** Resolution coverage for modern expression forms, including lambdas, pattern bindings, contracts, and P3 concurrency expressions (`parallel`, `race`, `timeout`).
- **Partial/Deferred:** Full package/module graph semantics (advanced imports/re-exports/orphan-style global coherence policies) remain beyond this phase.

## Design Decisions

### Scope Model

Aura uses **lexical scoping** with these scope levels (innermost to outermost):

1. **Block scope** — `let` bindings, `for` loop variables, `match` arm bindings, lambda parameters
2. **Function scope** — function parameters, type parameters
3. **Module scope** — top-level `def`, `type`, `concept`, `instance` definitions
4. **Package scope** — imported names via `use`
5. **Prelude scope** — built-in types (`Int`, `String`, `Bool`, `Option`, `Result`, etc.)

### Name Kinds

Names are categorized for resolution:

| Kind | Examples | Starts With |
|------|----------|-------------|
| Value | `x`, `user`, `calculate_total` | lowercase |
| Type | `Int`, `User`, `Option` | uppercase |
| Module | `user_service`, `std` | lowercase |
| Variant | `Some`, `None`, `Active`, `Failed` | uppercase |
| Effect | `Db.Read`, `Net`, `Log` | uppercase |

### Variant Resolution Rules

From the proposal: variants are context-resolved. Bare variant names are resolved based on the type being matched or the expected type from context:

1. **In `match` arms**: The matched expression's type determines which sum type the variants belong to
2. **In function arguments**: If the parameter type is known, bare variants resolve against it
3. **In `let` with type annotation**: `let x: Status = Pending` resolves `Pending` against `Status`
4. **Ambiguous contexts**: Must qualify with `Type.Variant` (e.g., `Status.Pending`)

### Module Path Resolution

`use std::collections::HashMap` resolves as: package `std` -> module `collections` -> name `HashMap`.

### File-to-Module Mapping

How source files map to modules:

| File Path | Module Name | Rule |
|-----------|-------------|------|
| `src/main.aura` | `module main` | Entry point convention |
| `src/user_service.aura` | `module user_service` | File name = module name |
| `src/auth/login.aura` | `module auth::login` | Directory path = module path |
| `src/auth/mod.aura` | `module auth` | `mod.aura` = parent module |

**Rules:**
- Every `.aura` file defines exactly one module
- The `module` declaration in the file must match its file path (if present)
- If the `module` declaration is omitted, it is inferred from the file path (see Open Question #6 in proposal)
- Module nesting follows directory structure: `src/a/b/c.aura` → `module a::b::c`
- Maximum 3 levels of nesting (lint warning beyond that)

**Circular import prevention:**
- Imports form a DAG (directed acyclic graph)
- If module A imports B and B imports A → compile error: "circular dependency between modules `a` and `b`"
- Detection: During import resolution (Pass 1), build the import graph and check for cycles using topological sort

### Shadowing

Inner scopes can shadow outer scopes. Shadowing a name produces a lint warning (not an error) for names in the same function.

## Implementation Steps

1. **Define `DefId` and `Resolution` types** — unique IDs for every definition, resolution result enum
2. **Build module-level symbol tables** — first pass: collect all top-level definitions per module
3. **Resolve imports** — process `use` declarations, populate import tables
4. **Check visibility** — `pub` items visible externally, non-`pub` items module-private
5. **Define scope stack** — push/pop scopes as the resolver walks the AST
6. **Resolve value references** — walk expressions, resolve identifiers against scope stack
7. **Resolve type references** — walk type expressions, resolve type names
8. **Resolve variant references** — implement context-dependent variant resolution
9. **Resolve effect references** — validate effect names against known effect definitions
10. **Detect errors** — undefined names, ambiguous variants, visibility violations, unused imports
11. **Produce a resolution map** — `HashMap<NodeId, DefId>` mapping each use to its definition

### Two-Pass Approach

Name resolution needs two passes over the AST:

**Pass 1: Collection** — Walk all modules, collect every top-level definition (types, functions, concepts, instances) into a global symbol table. This allows forward references (a function can call another function defined later in the file).

**Pass 2: Resolution** — Walk all code, resolve every identifier reference using the symbol tables from Pass 1 plus lexical scoping for locals.

## Data Structures

```rust
// Unique identifier for every definition in the program
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
struct DefId(u32);

enum DefKind {
    Function,
    Type,
    Concept,
    Instance,
    Variant(DefId),    // points to parent sum type
    Field(DefId),      // points to parent struct type
    TypeParam,
    Local,             // let binding, for variable, lambda param
    Import(DefId),     // alias for another DefId
}

struct DefInfo {
    id: DefId,
    kind: DefKind,
    name: Symbol,
    visibility: Visibility,
    span: Span,
    module: DefId,     // which module owns this
}

enum Visibility {
    Private,
    Public,
}

struct Scope {
    parent: Option<ScopeId>,
    bindings: HashMap<Symbol, DefId>,
}

// The output of name resolution
struct ResolvedNames {
    defs: HashMap<DefId, DefInfo>,
    resolutions: HashMap<NodeId, DefId>,  // each ident node -> what it refers to
    diagnostics: Vec<Diagnostic>,
}
```

## Testing Strategy

- **Basic resolution:** `let x = 1; x` — `x` resolves to the `let`
- **Shadowing:** Inner `let x` shadows outer
- **Forward reference:** Function A calls function B defined after it
- **Import resolution:** `use std::collections::HashMap` then `HashMap.new()`
- **Variant resolution:** Bare variant in `match`, qualified variant in expression
- **Visibility:** Private function accessed from another module -> error
- **Unused imports:** Warning for imported but unused names
- **Undefined names:** Reference to non-existent identifier -> error

## Open Questions

1. Should wildcard imports (`use std::collections::*`) be supported? (Recommendation: no — explicit imports align with "explicit over implicit" principle)
2. How to handle re-exports? (Recommendation: `pub use` re-exports, defer to later)
3. Method resolution ordering when a type has both inherent methods and concept methods with the same name? (Recommendation: inherent methods take priority, like Rust)

## Estimated Complexity

**M (Medium)** — Variant resolution is the trickiest part. Standard lexical scoping and import resolution are well-understood.
