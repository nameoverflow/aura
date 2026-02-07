# P1-05: Methods & Instance Blocks

## Overview

Aura supports methods through `instance` blocks. There are two kinds: **inherent methods** (defined directly on a type) and **concept methods** (defined as part of a concept implementation). Both use the same call syntax: `value.method(args)`.

## Dependencies

- **P1-03: Concepts** — concept method dispatch
- **P1-01: Algebraic Types** — types that have methods
- **P0-03: Name Resolution** — method name lookup

## Current Implementation Status (as of February 7, 2026)

- **Implemented:** Inherent instance blocks, concept instance blocks, method/associated-function type checking, `Self` resolution in instance/concept contexts, method resolution priority (inherent over concept), ambiguity diagnostics, and pipeline `.method(...)` parser sugar.
- **Implemented:** Associated functions via `Type.name(...)` are resolved and type-checked.
- **Partial:** Mutable `self` is effectively rejected by grammar/parameter rules rather than a dedicated semantic diagnostic.
- **Deferred:** Full method dispatch code generation for all method forms is still incomplete.

## Design Decisions

### Inherent Methods

```aura
instance Point {
    def distance_to(self, other: Point) -> Float64 = {
        let dx = self.x - other.x
        let dy = self.y - other.y
        (dx * dx + dy * dy).sqrt()
    }

    def origin() -> Point = Point { x: 0.0, y: 0.0 }
}
```

Key rules:
- `self` is always **immutable** — no `mut self`
- Methods that "change" the value return a new value
- Associated functions (no `self`) are called via `Type.name(...)` syntax
- Inherent methods take priority over concept methods with the same name

### Method Call Resolution

When the compiler sees `expr.method(args)`:

1. Determine the type of `expr` (call it `T`)
2. Look up `method` in `T`'s inherent methods
3. If not found, look up `method` in all concepts that `T` implements
4. If found in exactly one concept, use that
5. If found in multiple concepts, emit an ambiguity error (user must disambiguate with `Concept.method(value, args)`)
6. If not found anywhere, emit an "unknown method" error

### Associated Functions

Functions without `self` are **associated functions**, not methods:

```aura
Point.origin()           // associated function call
NonZero.new(42)         // refined type constructor (also an associated function)
```

These are called using `Type.function_name(args)` syntax, never through a value.

### Self Type

- `self` (lowercase) — the current instance, used as a parameter name
- `Self` (uppercase) — the implementing type, used in type annotations

```aura
concept Clone {
    def clone(self) -> Self     // Self = the type implementing Clone
}

instance Clone for Point {
    def clone(self) -> Self = Point { x: self.x, y: self.y }
    // Self = Point here
}
```

### Pipeline and Methods

The pipeline operator interacts with methods:

```aura
value |> .method(args)    // desugars to value.method(args)
value |> function(args)   // desugars to function(value, args)
```

The `.method` form in pipeline context is syntactic sugar for method calls.

## Implementation Steps

1. **Parse `instance Type { ... }` blocks** — inherent method definitions
2. **Parse `instance Concept for Type { ... }` blocks** — concept implementations
3. **Build method table per type** — map from (Type, method_name) to method definition
4. **Implement method call type checking** — resolve method, check argument types
5. **Implement `self` binding** — `self` is an implicit first parameter of the receiver type
6. **Implement `Self` type resolution** — `Self` resolves to the implementing type within instance blocks
7. **Implement associated function calls** — `Type.function(args)` syntax
8. **Implement method resolution priority** — inherent > concept, with ambiguity detection
9. **Implement `.method` in pipeline** — desugar `|> .method(args)` to `method(args)`
10. **Implement method dispatch in codegen** — emit correct LLVM call targets for method calls
11. **Verify no `mut self`** — reject mutable self parameters

## Data Structures

```rust
// Method table for a single type
struct MethodTable {
    // Inherent methods: name -> method def
    inherent: HashMap<Symbol, MethodDef>,
    // Concept methods: (concept, name) -> method def
    concept_methods: HashMap<(DefId, Symbol), MethodDef>,
    // Associated functions (no self): name -> function def
    associated: HashMap<Symbol, FnDef>,
}

struct MethodDef {
    id: DefId,
    name: Symbol,
    self_type: Type,          // the receiver type
    params: Vec<Param>,       // excluding self
    return_type: Type,
    effects: Vec<Effect>,
    body: Expr,
    span: Span,
}

enum MethodResolution {
    Inherent(DefId),
    Concept(DefId, DefId),    // (concept, method)
    Ambiguous(Vec<(DefId, DefId)>),  // multiple concept matches
    NotFound,
}
```

## Testing Strategy

- **Inherent method call:** `point.distance_to(other)` resolves and type-checks
- **Associated function:** `Point.origin()` returns a `Point`
- **Self type:** `Self` in instance block resolves to the correct type
- **Method priority:** Inherent method shadows concept method of same name
- **Ambiguity:** Two concepts provide `method` for the same type → error
- **Pipeline method:** `value |> .to_string()` works correctly
- **No mut self:** `def mutate(mut self)` → compile error
- **Method on generic type:** `instance Option a { def unwrap(self) -> a }`
- **Chained methods:** `point.distance_to(other).sqrt()` — method returns value, call method on result

## Open Questions

1. Extension methods: Should Aura allow adding methods to types defined in other modules (beyond concept implementations)? (Recommendation: no — only concept implementations can add methods from outside the defining module. This prevents "monkey patching".)
2. Method visibility: Should individual methods in an instance block have independent `pub` visibility? (Recommendation: methods inherit visibility from the `instance` block's `pub` modifier)
3. Static dispatch only: Should Aura support dynamic dispatch (trait objects)? (Recommendation: defer — static/monomorphized dispatch only for now)

## Estimated Complexity

**M (Medium)** — Method resolution is the main complexity. The rest builds on existing concept and type infrastructure.
