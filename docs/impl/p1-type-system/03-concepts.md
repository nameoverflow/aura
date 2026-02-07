# P1-03: Concept System

## Overview

Concepts are Aura's mechanism for ad-hoc polymorphism (similar to Rust traits, Haskell type classes). They define a set of methods that types can implement, enabling generic programming with constraints. Concepts are central to the language — operators, string conversion, equality, and collections all depend on them.

## Dependencies

- **P1-02: Generics** — concept bounds on type variables
- **P1-01: Algebraic Types** — types that implement concepts
- **P0-03: Name Resolution** — resolving concept/instance names

## Current Implementation Status (as of February 7, 2026)

- **Implemented:** Concept/instance parsing, concept and instance tables in type checking, superclass requirement checks, duplicate instance detection, concept-bound enforcement for `forall`, associated type declarations/bindings (including defaults), and explicit concept method disambiguation (`Concept.method(value, ...)`).
- **Implemented (type level):** Operator-to-concept resolution for arithmetic/comparison in type checking.
- **Partial:** Coherence currently enforces overlap checks and superclass constraints, but full module-level orphan-rule enforcement is not fully modeled.
- **Partial/Deferred:** Default concept methods are recognized at signature level; full executable default-body dispatch/codegen remains incomplete.
- **Deferred:** Auto-derive (`Eq`, `Debug`, etc.) and full dictionary/runtime dispatch strategies are not fully implemented.

## Design Decisions

### Concept Definition

```aura
concept Display {
    def display(self) -> String
}

concept Ord: Eq {                    // superclass constraint
    def compare(self, other: Self) -> Ordering
}

concept Add (rhs = Self) {           // associated type default
    type Output
    def add(self, other: rhs) -> Self.Output
}
```

A concept has:
- A name (`Display`, `Ord`, `Add`)
- Optional superclass constraints (`Ord: Eq`)
- Optional associated type parameters with defaults (`rhs = Self`)
- Associated type declarations (`type Output`)
- Method signatures (with or without default implementations)

### Instance (Implementation)

```aura
instance Display for User {
    def display(self) -> String = "User({self.id}): {self.name}"
}

instance Add for Vec2 {
    type Output = Vec2
    def add(self, other: Vec2) -> Vec2 = Vec2 { x: self.x + other.x, y: self.y + other.y }
}
```

### Coherence Rules

To prevent conflicting implementations, Aura enforces coherence (the "orphan rule"):
- An `instance Concept for Type` can only be defined in the module that defines `Concept` OR the module that defines `Type`
- No overlapping instances: at most one `instance Concept for Type` per (Concept, Type) pair

### Method Resolution Order

When `value.method()` is called:
1. Check inherent methods (from `instance Type { ... }`)
2. Check concept methods (from `instance Concept for Type { ... }`)
3. If ambiguous, require explicit disambiguation: `Concept.method(value)`

### Operator Desugaring

Operators desugar to concept method calls:

| Operator | Desugars to |
|----------|-------------|
| `a + b` | `Add.add(a, b)` |
| `a - b` | `Sub.sub(a, b)` |
| `a * b` | `Mul.mul(a, b)` |
| `a / b` | `Div.div(a, b)` |
| `a % b` | `Rem.rem(a, b)` |
| `a == b` | `Eq.eq(a, b)` |
| `a != b` | `Eq.ne(a, b)` |
| `a < b` | `Ord.lt(a, b)` |
| `a > b` | `Ord.gt(a, b)` |
| `a <= b` | `Ord.le(a, b)` |
| `a >= b` | `Ord.ge(a, b)` |

### Concept Dispatch Strategy

**Monomorphization** (primary strategy). Generic functions with concept bounds are specialized at each call site. The concept methods are resolved statically and inlined into the specialized version. No runtime dispatch overhead.

For cases where monomorphization is impractical (very large generic code), **dictionary passing** can be used as a fallback: each concept instance is a dictionary (struct) of function pointers passed as a hidden argument.

### Built-in Concept Deriving

Some concepts can be automatically derived for ADTs:
- `Eq` — structural equality (all fields implement `Eq`)
- `Ord` — lexicographic ordering (all fields implement `Ord`)
- `Debug` — debug string representation
- `Clone` — deep clone (all fields implement `Clone`)
- `Hash` — hash computation (all fields implement `Hash`)
- `Default` — all fields have defaults

Syntax TBD — possibly `type User { ... } deriving (Eq, Debug, Clone)` or via annotation `@derive(Eq, Debug, Clone)`.

### Self and Self.Output

- `Self` refers to the implementing type within a concept or instance block
- `Self.Output` refers to an associated type

## Implementation Steps

1. **Parse concept definitions** — name, superclasses, associated types, method signatures
2. **Parse instance definitions** — concept + type, method implementations
3. **Build concept table** — map from concept DefId to its definition (methods, supers, assoc types)
4. **Build instance table** — map from (Concept, Type) to its implementation
5. **Implement coherence checking** — verify orphan rules, detect overlapping instances
6. **Implement superclass checking** — if `concept Ord: Eq`, then `instance Ord for T` requires `instance Eq for T`
7. **Implement concept bound checking** — when `forall (Ord a). ...`, verify that the concrete type at call site has `Ord`
8. **Implement operator desugaring** — `a + b` -> `Add.add(a, b)` during type checking
9. **Implement associated type resolution** — `Self.Output` resolves to the concrete type from the instance
10. **Implement method dispatch in codegen** — emit correct call targets for concept methods (monomorphized or dictionary-based)
11. **Implement default method bodies** — if a concept provides a default, use it when the instance doesn't override
12. **Implement auto-derive** for `Eq`, `Debug`, `Clone`, `Hash` (basic set)
13. **Implement `From`/`Into` derivation** for wrapping variants (see **P2-03: Error Handling** for the authoritative design of auto-`From` — this step implements the concept infrastructure that P2-03's derivation logic depends on)

## Data Structures

```rust
struct ConceptDef {
    id: DefId,
    name: Symbol,
    type_params: Vec<TypeVar>,           // the "self" type + any extra params
    superclasses: Vec<ConceptBound>,     // Ord: Eq means Eq is a superclass
    assoc_types: Vec<AssocTypeDef>,      // type Output
    methods: Vec<MethodSig>,            // method signatures
    default_methods: HashMap<Symbol, Expr>, // default implementations
    span: Span,
}

struct AssocTypeDef {
    name: Symbol,
    default: Option<Type>,              // type Output = Self (default)
    span: Span,
}

struct MethodSig {
    name: Symbol,
    params: Vec<(Symbol, Type)>,        // includes self
    return_type: Type,
    effects: Vec<Effect>,
    span: Span,
}

struct InstanceDef {
    id: DefId,
    concept: DefId,
    implementing_type: Type,            // the concrete type
    type_params: Vec<TypeVar>,          // for generic instances
    assoc_type_bindings: HashMap<Symbol, Type>,  // type Output = Int
    methods: HashMap<Symbol, FnDef>,
    span: Span,
}

// Instance lookup table
struct InstanceTable {
    // (concept DefId, type constructor DefId) -> instance DefId
    instances: HashMap<(DefId, DefId), DefId>,
}

// Dictionary for fallback dispatch (when monomorphization is impractical)
struct ConceptDict {
    concept: DefId,
    methods: HashMap<Symbol, Value>,    // method name -> function value
    assoc_types: HashMap<Symbol, Type>, // associated type bindings
}
```

## Testing Strategy

- **Basic concept:** Define `Display`, implement for a type, call `display()`
- **Superclass:** `Ord: Eq` — can't implement `Ord` without `Eq`
- **Operator desugaring:** `1 + 2` calls `Add.add(1, 2)` -> `3`
- **Associated types:** `Add` with `type Output = Int`, verify return type
- **Generic function with bound:** `max: forall (Ord a). a * a -> a` called with `Int`
- **Concept bound error:** Calling `max` with a type that doesn't implement `Ord` -> error
- **Coherence violation:** Implement concept in wrong module -> error
- **Overlapping instances:** Two `instance Display for Int` -> error
- **Default methods:** Concept provides default, instance doesn't override, default is used
- **Auto-derive:** `Eq` derived for a struct with all `Eq` fields
- **Auto-From:** Wrapping variant `Io(IoError)` auto-derives `From IoError`

## Open Questions

1. Derive syntax: `deriving (Eq, Debug)` after type definition, or `@derive(Eq, Debug)` annotation? (Recommendation: `@derive(Eq, Debug)` — consistent with other annotations)
2. Blanket implementations: `instance Display for a where a: Debug` — should Aura support these? (Recommendation: defer — they add complexity and can cause coherence issues)
3. Negative bounds: `instance Foo for a where not a: Bar` — needed? (Recommendation: no — specialization and negative bounds are complex, defer)
4. Concept objects (dynamic dispatch): `value: dyn Display` — should Aura support this? (Recommendation: defer — monomorphization is the initial strategy)

## Estimated Complexity

**XL (Extra Large)** — The concept system is the most complex type system feature. Coherence checking, superclass resolution, operator desugaring, associated types, and dictionary-passing dispatch all interact. This is the single largest implementation effort in the type system.
