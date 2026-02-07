# P2-02: Effect Polymorphism

## Overview

Higher-order functions like `map`, `filter`, and `reduce` must be generic over the effects of their closure arguments. Without effect polymorphism, these functions could only accept pure closures, making the language impractical for real-world use.

Effect polymorphism allows effect variables (like type variables) to be universally quantified and instantiated at call sites.

## Dependencies

- **P2-01: Effect System** — basic effect checking
- **P1-02: Generics** — implicit quantification applies to effect variables too

## Design Decisions

### Effect Variables

Effect variables are lowercase identifiers in effect position, implicitly quantified:

```aura
map: List a * (a -> b [e]) -> List b [e]
```

Here `e` is an effect variable — `map` inherits whatever effects the callback requires. If the callback is pure, `e` = `[]` and `map` is pure.

### Implicit Quantification of Effect Variables

Just like type variables, free lowercase effect variables in a signature are universally quantified:

```aura
map: List a * (a -> b [e]) -> List b [e]
// Equivalent to: forall a b e. List a * (a -> b [e]) -> List b [e]
```

### Effect Variable Inference at Call Sites

When a closure is passed to a higher-order function, the effect variable is instantiated with the closure's concrete effects:

```aura
// pure closure: e = []
let doubled = list |> map((x) -> x * 2)          // map is pure here

// effectful closure: e = [Log]
let logged = list |> map((x) -> { Log.info("{x}"); x })  // map has [Log] here
```

### Multiple Effect Variables

When a function takes multiple closures, they can have independent effect variables:

```aura
combine: (() -> a [e1]) * (() -> b [e2]) -> a * b [e1, e2]
```

The resulting effect set is the **union** of all effect variable instantiations.

### Effect Variable Unification

Effect variables unify differently from type variables:

- A concrete effect set `[Db.Read, Log]` unifies with effect variable `e` by binding `e = {Db.Read, Log}`
- Two effect variables `e1` and `e2` can remain independent
- The empty set `[]` unifies with any effect variable (pure is a special case of effectful)
- Union of effect sets: `[e1, Db.Read]` where `e1 = [Log]` resolves to `[Log, Db.Read]`

### Row-Based vs Set-Based

Two main approaches for effect polymorphism:

1. **Set-based** (simpler): Effect variables represent sets of capabilities. Union is set union.
2. **Row-based** (more expressive): Effects are rows that can be extended. Enables effect subtraction and handlers.

**Recommendation: Set-based.** Aura's effect system is capability-based (no handlers), so set semantics are sufficient and simpler.

## Implementation Steps

1. **Extend `Type` to support effect variables** — effect variables are similar to type variables but live in effect position
2. **Implement effect variable collection** — scan function signatures for free effect variable names
3. **Implement effect variable instantiation** — at call sites, create fresh effect variables
4. **Implement effect variable unification** — bind effect variables to concrete effect sets
5. **Implement effect set union** — combine effect variables: `[e1, e2]` → union of their bindings
6. **Implement effect propagation for HOFs** — when calling `map(list, closure)`, propagate closure's effects to `map`'s return effects
7. **Implement pure-closure optimization** — when `e = []`, the HOF call is pure
8. **Update effect checking pass** — handle effect variables in addition to concrete effects
9. **Implement effect variable display** — show effect variables in error messages

## Data Structures

```rust
// Extended effect representation
enum EffectExpr {
    // Concrete effect
    Concrete(Effect),
    // Effect variable (to be resolved)
    Var(EffectVar),
    // Union of effect expressions
    Union(Vec<EffectExpr>),
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
struct EffectVar(u32);

// Effect environment during checking
struct EffectEnv {
    // Effect variable bindings
    bindings: HashMap<EffectVar, EffectSet>,
    next_var: u32,
}

impl EffectEnv {
    fn fresh_var(&mut self) -> EffectExpr;
    fn unify_effect(&mut self, var: EffectVar, set: EffectSet) -> Result<(), EffectError>;
    fn resolve(&self, expr: &EffectExpr) -> EffectSet;
}

// Function type with effect polymorphism
// Example: (a -> b [e]) has effect_params = [e]
struct FunctionType {
    params: Vec<Type>,
    return_type: Type,
    effects: Vec<EffectExpr>,    // may contain EffectVar
}
```

### Worked Example

```aura
// Definition:
map: List a * (a -> b [e]) -> List b [e]

// Call site:
let results = users |> map((u) -> { Log.info("{u}"); u.name })

// Inference:
// 1. a = User, b = String
// 2. Closure body has effects: [Log]
// 3. e unifies with [Log]
// 4. map's return effects: [e] = [Log]
// 5. results: List String, with [Log] effect on the enclosing function
```

## Testing Strategy

- **Pure HOF:** `map(list, (x) -> x * 2)` has no effects
- **Effectful HOF:** `map(list, (x) -> { Log.info("{x}"); x })` has `[Log]`
- **Multiple effect vars:** `combine(f, g)` where `f: [Net]` and `g: [Log]` → `[Net, Log]`
- **Nested HOFs:** `map(list, (x) -> filter(x.items, (i) -> i.valid))` — effects compose correctly
- **Effect variable in caller:** Caller must declare effects that flow through HOFs
- **Missing effect through HOF:** Caller is pure but passes effectful closure → error
- **Pipeline with effects:** `items |> map((x) -> fetch(x)) |> filter(...)` chains effects

## Open Questions

1. Should effect variables be explicitly nameable or always inferred? (Current: implicit quantification, which is sufficient)
2. Can effect variables appear in type aliases? `type Handler e = Request -> Response [e]` (Recommendation: yes, this is natural)
3. Effect variable bounds: Can you constrain an effect variable? e.g., `forall (e: {Db.Read}).` meaning "at least Db.Read"? (Recommendation: defer — not needed for initial implementation)

## Estimated Complexity

**L (Large)** — Effect variable unification and propagation through higher-order functions is the most technically challenging part of the effect system. The interaction between type inference and effect inference requires careful design.
