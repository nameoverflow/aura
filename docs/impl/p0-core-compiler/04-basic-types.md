# P0-04: Basic Type Representation & Inference

## Overview

This covers the core type system infrastructure: how types are represented internally, how type inference works, and how type checking is performed. This is the minimal type system needed to check simple programs — full generics, concepts, and effects are layered on top in P1 and P2.

## Dependencies

- **P0-03: Name Resolution** — resolved names and definition IDs

## Design Decisions

### Type Representation

Types are represented as a graph of `Type` nodes. During inference, types may be "unknown" (type variables) that get unified with concrete types.

### Inference Algorithm

**Hindley-Milner with extensions.** The base is Algorithm W / constraint-based inference:

1. **Generate type variables** for unknowns
2. **Generate constraints** from expressions (e.g., `x + y` generates `typeof(x): Add, typeof(x) == typeof(y)`)
3. **Unify constraints** to solve type variables
4. **Generalize** unconstrained type variables at `let`/`def` bindings

Aura's extensions to basic HM:
- Product types (`A * B * C`)
- Named struct types (not structurally typed)
- Sum types with variants
- Effect annotations (P2)
- Concept bounds (P1)

### Unification

Standard unification with occurs check:
- Two concrete types unify if they're the same constructor with unifiable arguments
- A type variable unifies with any type (unless it occurs in that type — the "occurs check")
- Unification errors produce type mismatch diagnostics

### When to Infer vs Require Annotations

- **Function parameters:** Required for top-level functions, optional for lambdas
- **Return types:** Required for top-level functions, inferred for lambdas and blocks
- **Let bindings:** Always inferred (optional annotation for documentation)
- **Polymorphic functions:** The standalone type annotation provides the polymorphic signature

### Subtyping

Aura has **no subtyping**. All type relationships are via concepts (bounded polymorphism). This simplifies inference significantly.

## Implementation Steps

1. **Define internal `Type` representation** — the core type enum
2. **Implement type variable management** — fresh variable generation, substitution
3. **Implement unification** — with occurs check and meaningful error messages
4. **Implement basic type checking for literals** — `42: Int`, `3.14: Float64`, `"hello": String`, `true: Bool`
5. **Implement type checking for `let` bindings** — infer RHS type, bind to name
6. **Implement type checking for function definitions** — check body against declared return type
7. **Implement type checking for function calls** — match argument types to parameter types
8. **Implement type checking for operators** — desugar to concept method calls (P1 dependency, stub initially)
9. **Implement type checking for `if`/`else`** — both branches must have the same type
10. **Implement type checking for blocks** — type is the last expression's type
11. **Implement type checking for struct literals** — check all fields present and correctly typed
12. **Implement type checking for field access** — look up field type in struct definition
13. **Implement `let` polymorphism** — generalize type at `let` bindings

## Data Structures

```rust
// Unique type variable ID
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
struct TypeVar(u32);

enum Type {
    // Primitives
    Int,
    Int8, Int16, Int32, Int64,
    UInt, UInt8, UInt16, UInt32, UInt64,
    Float32, Float64,
    Decimal, BigDecimal,
    Bool,
    Char,
    String,
    Unit,                            // ()

    // Type variable (unknown, to be inferred)
    Var(TypeVar),

    // Named user types
    Named(DefId, Vec<Type>),         // e.g., User, Option Int, Result User Error

    // Sum type variant (carries parent type + variant index)
    Variant(DefId, usize),

    // Product type: A * B * C
    Product(Vec<Type>),

    // Function type: (params) -> return [effects]
    Function(Vec<Type>, Box<Type>, Vec<Effect>),

    // Error sentinel (for recovery after type errors)
    Error,
}

// Manages type variables and their bindings
struct TypeEnv {
    // type variable -> its current binding (None if unbound)
    bindings: HashMap<TypeVar, Type>,
    next_var: u32,
}

impl TypeEnv {
    fn fresh_var(&mut self) -> Type;
    fn unify(&mut self, a: &Type, b: &Type) -> Result<(), TypeError>;
    fn resolve(&self, ty: &Type) -> Type;  // follow bindings to ground type
}

struct TypeError {
    expected: Type,
    found: Type,
    span: Span,
    context: String,  // e.g., "in argument 2 of call to `foo`"
}
```

## Testing Strategy

- **Literal inference:** `let x = 42` infers `Int`, `let y = "hi"` infers `String`
- **Function type checking:** Argument/return type mismatches produce errors
- **Block typing:** `{ let x = 1; x + 2 }` has type `Int`
- **If/else typing:** Branches with different types produce error
- **Struct literal checking:** Missing field, wrong field type, unknown field
- **Type variable resolution:** `let x = f(y)` where `f: Int -> String` infers `x: String`
- **Occurs check:** Detect infinite types
- **Error quality:** Messages point to exact location with context

## Open Questions

1. Default integer literal type: `42` is `Int` (which is what size?). Recommendation: `Int` is the platform word size (64-bit on 64-bit systems), consistent with most modern languages.
2. Float literal default: `3.14` defaults to `Float64` (like Rust's `f64`).
3. Integer literal overflow: Should `let x: Int8 = 300` be a compile-time error or runtime? (Recommendation: compile-time error for literals that provably overflow.)

## Estimated Complexity

**L (Large)** — Type inference with unification is algorithmically well-understood but requires careful implementation for good error messages. This is the foundation everything else builds on.
