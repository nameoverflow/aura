# P1-02: Generics & Implicit Quantification

## Overview

Aura uses Haskell-style parametric polymorphism with implicit quantification. Free lowercase type variables in signatures are universally quantified automatically. `forall` is only needed when attaching concept bounds. This document covers the implementation of generics including type application, instantiation, and let-polymorphism.

## Dependencies

- **P0-04: Basic Types** — type inference and unification
- **P1-01: Algebraic Types** — generic type definitions

## Design Decisions

### Implicit Quantification

All free lowercase type variables in a type signature are universally quantified:

```aura
identity: a -> a              // implicitly: forall a. a -> a
swap: a * b -> b * a          // implicitly: forall a b. a * b -> b * a
```

The compiler collects free type variables from the signature and wraps them in an implicit `forall`.

### Explicit `forall` for Concept Bounds

When concept bounds are needed, `forall` makes them explicit:

```aura
max: forall (Ord a). a * a -> a
```

Without `forall`, there's no place to attach the `Ord a` constraint.

### Type Application (Instantiation)

When a polymorphic function is called, type variables are instantiated with fresh type variables that get unified with concrete types:

```aura
identity: a -> a
identity(42)        // a instantiated to Int
identity("hello")   // a instantiated to String
```

This is standard HM instantiation — no explicit type application syntax.

### Generics in Type Definitions

```aura
type Pair a b { first: a, second: b }
type Option a = Some a | None
type Result a e = Ok a | Err e
```

Type parameters are positional. Nested generics use parentheses for disambiguation:

```aura
List Int                    // List applied to Int
List (Option Int)           // List applied to (Option applied to Int)
Result User (List Error)    // Result applied to User and (List applied to Error)
```

### Kind System

For the initial implementation, Aura uses a simple kind system:
- `*` — the kind of types (`Int`, `String`, `User`)
- `* -> *` — type constructor taking one argument (`List`, `Option`)
- `* -> * -> *` — type constructor taking two arguments (`Result`, `Map`)

Kind checking ensures type constructors are applied to the right number of arguments.

### Monomorphization vs Type Erasure

Aura uses **monomorphization** (like Rust) — generic functions are specialized into concrete versions for each type combination used. This eliminates runtime dispatch overhead.

- **LLVM:** Monomorphization generates specialized code for each concrete type
- **WASM:** Same monomorphization strategy, targeting WASM instructions

## Implementation Steps

1. **Implement free variable collection** — scan a type expression, collect all lowercase identifiers not bound by an enclosing `forall`
2. **Implement implicit quantification** — wrap collected free variables in a `TypeScheme`
3. **Implement `forall` parsing** — `forall (Constraint a, Constraint2 b). type_expr`
4. **Implement type scheme instantiation** — replace quantified variables with fresh type variables at each use site
5. **Implement type scheme generalization** — at `let`/`def` bindings, generalize unconstrained type variables
6. **Implement kind checking** — verify type constructors receive the correct number of arguments
7. **Implement generic type definition checking** — ensure type parameters are used consistently in the body
8. **Implement type application in expressions** — when calling a generic function, instantiate its type scheme
9. **Implement generic struct/sum instantiation** — `Pair Int String` creates a concrete type
10. **Update unification for generic types** — `List a` unifies with `List Int` by unifying `a = Int`
11. **Implement the "value restriction"** — prevent polymorphic mutable bindings (standard ML/HM restriction)

## Data Structures

```rust
// A polymorphic type with quantified variables and bounds
struct TypeScheme {
    // Quantified type variables (a, b, e, ...)
    quantified: Vec<TypeVar>,
    // Concept bounds (Ord a, Eq b, ...)
    bounds: Vec<ConceptBound>,
    // The underlying type (with free vars matching quantified)
    body: Type,
}

struct ConceptBound {
    concept: DefId,         // which concept
    type_var: TypeVar,      // which type variable is bounded
    assoc_types: HashMap<Symbol, Type>,  // associated type bindings if any
    span: Span,
}

// Kind system
enum Kind {
    Star,                   // *
    Arrow(Box<Kind>, Box<Kind>),  // k1 -> k2
}
```

### Instantiation Example

```
// Given:   identity: forall a. a -> a
// At call: identity(42)

1. Create fresh TypeVar ?0
2. Instantiate scheme: ?0 -> ?0
3. Unify argument: ?0 ~ Int
4. Result: Int -> Int
5. Return type: Int
```

## Testing Strategy

- **Identity function:** `identity(42)` returns `Int`, `identity("hello")` returns `String`
- **Generic pairs:** `Pair { first: 1, second: "hi" }` infers `Pair Int String`
- **Nested generics:** `List (Option Int)` kind-checks correctly
- **Wrong arity:** `Option Int String` is a kind error (Option takes 1 argument)
- **Let polymorphism:** `let id = (x) -> x; id(1); id("hello")` — `id` is polymorphic
- **Monomorphic binding:** `let f = id(1)` — `f` is `Int`, not polymorphic
- **Free variable collection:** `a * b -> Result a b` collects `{a, b}`
- **Forall with bounds:** `forall (Ord a). a * a -> a` — `a` has `Ord` bound

## Open Questions

1. Higher-kinded types (HKTs): Should Aura support `type F a` where `F` itself is a type variable? (Recommendation: defer — HKTs are powerful but complex. The proposal doesn't mention them explicitly.)
2. Type variable naming convention: Should the compiler warn if a type variable is uppercase? (Recommendation: yes, as a lint — lowercase for type variables is a firm convention)
3. Rank-N types: Should `forall` be allowed in arbitrary positions, not just top-level signatures? (Recommendation: defer — Rank-1 polymorphism is sufficient for most use cases)

## Estimated Complexity

**L (Large)** — Hindley-Milner inference with implicit quantification, kind checking, and proper instantiation/generalization is the algorithmic core of the type system. Getting this right is critical.
