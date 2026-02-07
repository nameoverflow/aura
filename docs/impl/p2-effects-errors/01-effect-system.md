# P2-01: Effect System

## Overview

Aura's effect system tracks **capabilities** — what external resources a function may access. Effects are declared in function signatures and verified at compile time. This is a **capability-based** effect system, not an algebraic effect system with handlers.

A function with no effect list is pure. The compiler guarantees that pure functions have no side effects, enabling safe parallelization, memoization, and reordering.

## Dependencies

- **P1-02: Generics** — effect variables are part of the generic system
- **P0-04: Basic Types** — effects extend the type system
- **P0-03: Name Resolution** — resolving effect names

## Current Implementation Status (as of February 7, 2026)

- **Implemented:** Effect-list parsing on function signatures and function types, built-in capability set, effect hierarchy (`Db.Write` => `Db.Read`, `Fs.Write` => `Fs.Read`), and call-site capability subset checks.
- **Implemented:** Pure-function behavior (empty effect set) is enforced in type checking.
- **Partial:** Effect checks run as part of the type checker rather than a separate dedicated effect pass.
- **Partial/Deferred:** No dedicated `main()` transitive-capability summary check yet; enforcement currently occurs through normal call-site checking.

## Design Decisions

### Effect Representation

Effects are a finite set of capability tags:

| Effect | Description |
|--------|-------------|
| `Db.Read` | Read from database |
| `Db.Write` | Write to database |
| `Net` | Network/HTTP requests |
| `Fs.Read` | Read from file system |
| `Fs.Write` | Write to file system |
| `Log` | Logging |
| `Time` | Access current time |
| `Random` | Randomness |
| `Env` | Environment variables |

Effects are **not** types — they live in a separate namespace. `Db.Read` is a capability identifier, not a type expression.

### Effect Checking Rules

1. **A function's effect list declares the maximum capabilities it may use**
2. **A function body may only call functions whose effects are a subset of its own declared effects**
3. **Pure functions** (no effect list or `[]`) may not call any effectful functions
4. **Effect lists are explicit** — there is no effect inference for top-level functions (effects must be written in the signature)
5. **Lambdas** may have their effects inferred from context (see P2-02)

### Effect Hierarchy

Some effects imply others:
- `Db.Write` implies `Db.Read` (writing requires reading capability)
- `Fs.Write` implies `Fs.Read`

This is a flat hierarchy — no deep nesting.

### Entry Point Effects

`def main()` declares the program's maximum capability set. All transitively called effects must be a subset:

```aura
def main() -> Result () AppError [Db.Read, Db.Write, Net, Log] = { ... }
```

### Effect Checking in the Compiler

Effect checking is a separate pass after type checking. It walks the call graph and verifies:
- Each function call's effects ⊆ the caller's declared effects
- The transitive closure of all effects matches the declared set
- Pure functions call only pure functions

### User-Defined Effects

The initial implementation supports only the built-in effects listed above. User-defined effects (custom capabilities) could be added later but are not in the proposal.

## Implementation Steps

1. **Define `Effect` type** — enum or interned identifier for known effects
2. **Parse effect lists** — `[Db.Read, Net, Log]` in function signatures
3. **Build effect environment** — map each function DefId to its declared effects
4. **Implement effect subset checking** — is `{Db.Read}` ⊆ `{Db.Read, Net}`? (yes)
5. **Implement call-site effect checking** — when function A calls function B, verify B's effects ⊆ A's effects
6. **Implement effect hierarchy** — `Db.Write` satisfies `Db.Read` requirement
7. **Handle pure functions** — no effect list means empty set; verify they call only pure functions
8. **Implement main() effect checking** — all transitive effects must be declared in main's effect list
9. **Emit clear error messages** — "function `fetch_user` requires `[Db.Read]` but `calculate_total` is declared pure"
10. **Implement effect display** — pretty-print effect sets in error messages and diagnostics

## Data Structures

```rust
#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug)]
enum Effect {
    DbRead,
    DbWrite,
    Net,
    FsRead,
    FsWrite,
    Log,
    Time,
    Random,
    Env,
}

impl Effect {
    // Db.Write implies Db.Read, Fs.Write implies Fs.Read
    fn implies(&self) -> Vec<Effect> {
        match self {
            Effect::DbWrite => vec![Effect::DbRead],
            Effect::FsWrite => vec![Effect::FsRead],
            _ => vec![],
        }
    }
}

// An effect set (small enough for a bitset)
#[derive(Clone, Eq, PartialEq, Hash)]
struct EffectSet(u16);  // 9 effects fit in a u16 bitfield

impl EffectSet {
    fn empty() -> Self;
    fn contains(&self, effect: Effect) -> bool;
    fn insert(&mut self, effect: Effect);
    fn is_subset_of(&self, other: &EffectSet) -> bool;  // accounts for implies()
    fn union(&self, other: &EffectSet) -> EffectSet;
    fn difference(&self, other: &EffectSet) -> EffectSet;
    // Expand implied effects: {Db.Write} -> {Db.Write, Db.Read}
    fn with_implied(&self) -> EffectSet;
}

// Effect information per function
struct FnEffects {
    declared: EffectSet,         // what the signature says
    actual: Option<EffectSet>,   // computed from body (for validation)
}

struct EffectError {
    kind: EffectErrorKind,
    span: Span,
    function: DefId,
    callee: DefId,
}

enum EffectErrorKind {
    MissingCapability {
        needed: EffectSet,      // effects the callee requires
        available: EffectSet,   // effects the caller provides
        missing: EffectSet,     // needed - available
    },
    PureFunctionCallsEffectful {
        callee_effects: EffectSet,
    },
}
```

## Testing Strategy

- **Pure function calls pure:** `def f() = g()` where `g` is pure → OK
- **Pure function calls effectful:** `def f() = fetch()` where `fetch` requires `[Net]` → error
- **Effect subset:** `def f() [Db.Read, Net] = g()` where `g` requires `[Db.Read]` → OK
- **Missing effect:** `def f() [Net] = g()` where `g` requires `[Db.Read]` → error
- **Effect hierarchy:** `def f() [Db.Write] = g()` where `g` requires `[Db.Read]` → OK (Write implies Read)
- **Main effects:** All transitive effects must be in main's list
- **Error messages:** Clear indication of which effect is missing and where

## Open Questions

1. Should the compiler **infer** the minimal effect set for a function and compare it to the declared set, warning on unnecessary effects? (Recommendation: yes, emit a warning for declared-but-unused effects — helps keep signatures honest)
2. Should user-defined effects be supported in Phase 1? (Recommendation: no — built-in effects only initially)
3. How to handle FFI calls — what effects do foreign functions have? (Recommendation: FFI functions must declare effects explicitly, defaulting to `[unsafe]` or similar marker)

## Estimated Complexity

**M (Medium)** — The core algorithm (subset checking on capability sets) is simple. The complexity is in correctly propagating effects through the call graph and producing good error messages.
