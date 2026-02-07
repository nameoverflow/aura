# P1-01: Algebraic Data Types

## Overview

Aura's type system is built on algebraic data types (ADTs): **product types** (structs/records/tuples) and **sum types** (tagged unions/enums). This document covers the full implementation of ADTs beyond the basic struct support in P0.

## Dependencies

- **P0-04: Basic Types** — core type representation and inference
- **P0-03: Name Resolution** — definition IDs and scope resolution

## Design Decisions

### Product Types

Three flavors of product types in Aura:

1. **Named structs** — `type User { id: UUID, name: String }`
2. **Generic structs** — `type Pair a b { first: a, second: b }`
3. **Tuple types** — `A * B * C` (anonymous product), values `(a, b, c)`

Named structs and generic structs use nominal typing (a `User` is not a `Pair String String` even if fields match). Tuple types are structural.

### Sum Types

All sum types are **named and nominal**:

```aura
type Status = Pending | Processing | Complete | Failed String
```

Key properties:
- Variants are constructors that belong to the sum type
- Variants with no payload are unit-like (`Pending`)
- Variants with payload carry data (`Failed String`)
- No inline/anonymous unions — all sum types must be named

### Standard Library ADTs

`Option` and `Result` are regular sum types defined in the standard library prelude:

```aura
type Option a = Some a | None
type Result a e = Ok a | Err e
```

They receive no special compiler treatment except that `?` desugars to a `match` on `Result` (see P2-03).

### Recursive Types

Types can be recursive:

```aura
type Expr = Lit Int | Add Expr Expr | Mul Expr Expr
```

Recursive types are heap-allocated (GC-managed). The compiler should detect and allow recursion through indirection (all non-primitive types are GC-allocated).

### Type Representation in the Compiler

Each type definition produces:
- A `DefId` for the type itself
- `DefId`s for each variant (sum types) or field (product types)
- A `TypeScheme` capturing generic parameters

## Implementation Steps

1. **Extend `Type` enum** for sum type variants with payloads
2. **Implement generic type definition parsing** — `type Pair a b { ... }`, `type Result a e = ...`
3. **Implement type parameter scoping** — `a` and `b` are in scope within the type body
4. **Implement variant constructors** — `Some(42)` creates a `Value::Variant`
5. **Implement tuple type representation** — `A * B * C` as `Type::Product(vec![A, B, C])`
6. **Implement tuple value construction** — `(1, "hello", true)` creates a `Value::Tuple`
7. **Implement tuple destructuring** — `let (x, y, z) = tuple`
8. **Implement struct field access** — `user.name` resolves to the field's type
9. **Implement `with` syntax type checking** — verify overridden fields type-check
10. **Implement type definition cycle checking** — detect recursive types, ensure they are heap-allocated (the GC handles cycles naturally)
11. **Implement Display for ADTs** — pretty-print types in error messages

## Data Structures

```rust
struct TypeDef {
    id: DefId,
    name: Symbol,
    type_params: Vec<Symbol>,   // [a, b] for Pair a b
    kind: TypeDefKind,
    visibility: Visibility,
    span: Span,
}

enum TypeDefKind {
    Struct(Vec<FieldDef>),
    Sum(Vec<VariantDef>),
    Refined(Box<Type>, ConstraintExpr),  // base type + where clause (P2)
}

struct FieldDef {
    id: DefId,
    name: Symbol,
    ty: Type,
    span: Span,
}

struct VariantDef {
    id: DefId,
    name: Symbol,
    payload: Option<Type>,   // None for unit variants like Pending
    span: Span,
}

// Polymorphic type scheme (for generics)
struct TypeScheme {
    params: Vec<TypeVar>,        // universally quantified variables
    bounds: Vec<ConceptBound>,   // concept constraints (P1-03)
    ty: Type,                    // the type with free variables
}
```

## Testing Strategy

- **Struct creation and access:** `User { id: 1, name: "Alice" }.name == "Alice"`
- **Sum type construction:** `let s: Status = Failed("oops")`
- **Tuple construction:** `let t: Int * String = (1, "hello")`
- **Tuple destructuring:** `let (x, y) = (1, "hello"); x == 1`
- **Generic instantiation:** `Pair { first: 1, second: "hi" }` infers `Pair Int String`
- **Option/Result:** `Some(42)`, `None`, `Ok("yes")`, `Err("no")`
- **Recursive types:** `type List a = Cons a (List a) | Nil` works without stack overflow
- **With syntax:** `user with { name: "Bob" }` type-checks correctly
- **Error cases:** Wrong variant payload type, missing struct field, extra struct field

## Open Questions

1. Should unit variants (`Pending`) be zero-sized or still allocate? (Recommendation: zero-sized, optimized at the value representation level)
2. Should `type Alias = ExistingType` (type aliases without `where`) be supported? (Recommendation: yes, as transparent aliases — they're the same type, just a new name for readability)
3. Layout optimization for sum types: Should the compiler optimize `Option (Ref a)` to use null-pointer optimization like Rust? (Recommendation: defer to P4-03 optimizer)

## Estimated Complexity

**M (Medium)** — The basic ADT representation builds on P0's foundation. Generic instantiation and variant resolution (from P0-03) are the main complexity points.
