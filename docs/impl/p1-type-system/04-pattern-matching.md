# P1-04: Pattern Matching

## Overview

Pattern matching is a core language feature in Aura. It enables destructuring of algebraic types, provides exhaustiveness checking (the compiler verifies all cases are handled), and supports guards for conditional matching. Pattern matching is used in `match` expressions, `let` bindings, and `for` loops.

## Dependencies

- **P1-01: Algebraic Types** — types to match against
- **P0-04: Basic Types** — type checking of patterns
- **P0-03: Name Resolution** — resolving variant names in patterns

## Current Implementation Status (as of February 7, 2026)

- **Implemented:** Wildcard/variable/literal/constructor/tuple/struct/rest/or-pattern parsing and typing, nested pattern binding, guard typing, and destructuring in `let`/`for`.
- **Implemented (subset):** Exhaustiveness checks for common sum-type and `Bool` cases, including awareness of guarded arms and or-pattern constructor coverage.
- **Implemented:** Redundancy (unreachable arm) detection: catch-all-after-coverage, duplicate constructors, duplicate literals.
- **Partial:** Exhaustiveness uses pragmatic checks, not a full Maranget matrix algorithm yet. Codegen support covers basic patterns (int/bool/string/constructor with guards) but not struct patterns or or-patterns.

## Design Decisions

### Pattern Kinds

| Pattern | Example | Matches |
|---------|---------|---------|
| Wildcard | `_` | Anything, binds nothing |
| Variable | `x` | Anything, binds to `x` |
| Literal | `42`, `"hello"`, `true` | Exact value |
| Constructor | `Some(x)`, `Failed(reason)` | Sum type variant |
| Tuple | `(a, b, c)` | Product type value |
| Struct | `User { name, .. }` | Struct with field extraction |
| Rest | `..` | Remaining fields (in struct patterns) |
| Or | `A \| B` | Either pattern (binds same names) |

### Exhaustiveness Checking

The compiler must verify that `match` expressions cover all possible cases. Algorithm: **usefulness checking** (Maranget's algorithm).

Key rules from the proposal:
- All variants of a sum type must be covered
- When guards are present, a catch-all arm is required for the guarded type (the compiler cannot prove guards are exhaustive)
- Non-exhaustive matches are a **compile error**, not a warning

### Guard Expressions

```aura
match value {
    Some(n) if n > 0 => "positive",
    Some(n) if n < 0 => "negative",
    Some(_) => "zero",       // required catch-all for guarded Some
    None => "nothing"
}
```

Guards are `if condition` after the pattern. When guards are present:
- The guard is only evaluated if the pattern matches
- The compiler treats guarded arms as potentially non-exhaustive
- A catch-all arm for the guarded constructor is required

### Pattern Matching Compilation

**Decision trees** — compile patterns into efficient nested conditionals/switches in the MIR. This avoids redundant checks and produces optimal LLVM code.

### Binding Modes

- Patterns bind by value (matching moves/copies the value into the binding)
- With GC, "moving" is a pointer copy — the GC tracks reachability

### Nested Patterns

Patterns can nest arbitrarily:

```aura
match result {
    Ok(Some(User { name, status: Active })) => use_name(name),
    Ok(Some(User { status: Suspended })) => show_suspended(),
    Ok(None) => show_not_found(),
    Err(e) => handle_error(e),
}
```

## Implementation Steps

1. **Implement pattern parsing** — all pattern kinds from the grammar
2. **Implement pattern type checking** — verify patterns are compatible with the matched type
3. **Implement variable binding in patterns** — `Some(x)` binds `x` to the payload
4. **Implement nested pattern matching** — recursive matching on sub-patterns
5. **Implement guard evaluation** — evaluate guard expression after pattern match
6. **Implement exhaustiveness checking** — Maranget's usefulness algorithm
7. **Implement redundancy checking** — warn on unreachable arms
8. **Implement struct patterns** — `User { name, .. }` extracts `name`, ignores rest
9. **Implement or-patterns** — `A | B` matches either (same bindings in both)
10. **Implement let destructuring** — `let (x, y) = pair` uses pattern matching
11. **Implement for destructuring** — `for (key, value) in map`
12. **Emit pattern matching in LLVM** — decision tree → branch/switch instructions

### Exhaustiveness Algorithm (Maranget)

The algorithm works by building a "pattern matrix" and checking if any input could fail to match:

1. Represent all match arms as rows in a matrix
2. For each constructor of the matched type, check if there exists a "useful" pattern that the matrix doesn't cover
3. If any useful pattern exists, the match is non-exhaustive → error
4. A catch-all (`_` or variable) covers all remaining constructors

## Data Structures

```rust
// Extended pattern representation for matching
enum Pattern {
    Wildcard(Span),
    Var(Symbol, Span),
    Literal(LiteralValue, Span),
    Constructor(DefId, Vec<Pattern>, Span),  // variant + sub-patterns
    Tuple(Vec<Pattern>, Span),
    Struct(DefId, Vec<FieldPattern>, bool /* has .. */, Span),
    Or(Vec<Pattern>, Span),
}

struct FieldPattern {
    name: Symbol,
    pattern: Pattern,   // defaults to Var(name) for shorthand
    span: Span,
}

struct MatchArm {
    pattern: Pattern,
    guard: Option<Expr>,
    body: Expr,
    span: Span,
}

// For exhaustiveness checking
struct PatternMatrix {
    rows: Vec<Vec<Pattern>>,   // each row is one arm's pattern(s)
    types: Vec<Type>,          // column types
}

enum ExhaustivenessResult {
    Exhaustive,
    NonExhaustive(Vec<Pattern>),  // example missing patterns
}
```

## Testing Strategy

- **Basic match:** Match on `Option Int` with `Some(n)` and `None`
- **Exhaustiveness pass:** All variants covered → compiles
- **Exhaustiveness fail:** Missing `None` arm → compile error with suggested pattern
- **Guards with catch-all:** Guarded `Some` arms + catch-all `Some(_)` → OK
- **Guards without catch-all:** Missing catch-all → compile error
- **Nested patterns:** `Ok(Some(x))` matches `Result (Option Int) e`
- **Struct patterns:** `User { name, .. }` extracts `name`
- **Tuple destructuring:** `let (a, b) = (1, 2)` binds correctly
- **Or patterns:** `None | Some(0)` matches both
- **Redundancy warning:** `Some(_)` after `Some(x)` → warning (unreachable)
- **Literal patterns:** `match n { 0 => ..., 1 => ..., _ => ... }`

## Open Questions

1. Should or-patterns (`A | B`) bind variables? If so, both sides must bind the same names with the same types. (Recommendation: yes, like Rust — both sides must bind the same set of names)
2. Should `@` bindings be supported? e.g., `Some(x @ Positive)` binds `x` while also matching `Positive`. (Recommendation: defer — not in the proposal, adds complexity)
3. How to display missing patterns in exhaustiveness errors? (Recommendation: show one concrete example, e.g., "pattern `None` is not covered")

## Estimated Complexity

**L (Large)** — Exhaustiveness checking (Maranget's algorithm) is the main complexity. The rest of pattern matching is well-understood but has many cases to handle correctly.
