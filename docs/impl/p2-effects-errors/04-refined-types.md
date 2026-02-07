# P2-04: Refined Types

## Overview

Refined types are wrapper types with attached constraints validated at construction. They implement the "parse, don't validate" principle — once a value has been validated into a refined type, the constraint is guaranteed to hold.

Refined types are **runtime-checked newtypes** (smart constructors), not compile-time-proven propositions. No dependent types or SMT solvers are required.

## Dependencies

- **P1-01: Algebraic Types** — refined types are a special case of type definitions
- **P1-05: Methods** — `.new()` and `.value` accessors
- **P2-03: Error Handling** — `.new()` returns `Result T ConstraintError`

## Current Implementation Status (as of February 7, 2026)

- **Implemented (front-end/typechecker):** Refined type parsing, refined-constraint grammar validation (restricted subset), `self` resolution in constraints, generated `Type.new(...)` typing as `Result Type ConstraintError`, and `.value` field typing.
- **Implemented (static check subset):** Literal-argument validation for some refined constraints at compile time.
- **Implemented (contracts front-end):** `requires`/`ensures` parsing, resolver support (`result` in ensures), and Bool-typechecking of contract clauses.
- **Partial/Deferred:** Full runtime validation codegen for refined `.new()` and debug-only runtime contract enforcement are not complete yet.
- **Partial:** Constraint evaluation/constant support is currently limited compared to the full aspirational grammar.

## Design Decisions

### Syntax

```aura
type NonZero = Int where self != 0
type Positive = Int where self > 0
type NonEmpty a = List a where self.len > 0
type Percentage = Float where self >= 0.0 and self <= 100.0
type Email = String where self.matches(EMAIL_REGEX)
```

### Constraint Grammar

The `where` clause supports a restricted expression language:

| Feature | Examples |
|---------|---------|
| Comparisons | `self == 0`, `self > 0`, `self != ""` |
| Boolean connectives | `... and ...`, `... or ...`, `not ...` |
| Arithmetic | `self + 1`, `self % 2` |
| Field access | `self.field` (for product types) |
| Built-in methods | `self.len`, `self.matches(pattern)`, `self.contains(value)` |
| Constants | Module-level constants like `EMAIL_REGEX` |

**Not allowed:** arbitrary function calls, closures, side effects.

### Construction and Access

```aura
// Construction always validates — returns Result
let n: Result NonZero ConstraintError = NonZero.new(x)

// Access unwraps to base type
def divide(a: Int, b: NonZero) -> Int = a / b.value
```

- `.new(value)` — associated function that validates and wraps
- `.value` — field that unwraps to the base type
- The compiler auto-generates both

### ConstraintError

A standard library type:

```aura
type ConstraintError {
    message: String
}
```

All refined type `.new()` constructors return `Result T ConstraintError`.

### Const-Parameterized Refined Types (Phase 2)

Deferred to Phase 2 of the language:

```aura
type Bounded (const Min: Int) (const Max: Int) = Int where self >= Min and self <= Max
```

This requires const generics, which is a significant extension.

### Compile-Time Optimization

When the argument to `.new()` is a literal, the compiler can evaluate the constraint at compile time:

```aura
let n = NonZero.new(42)    // compiler knows 42 != 0, could optimize to Ok(NonZero(42))
let z = NonZero.new(0)     // compiler knows 0 == 0, could emit error or warning
```

This is an optimization, not a requirement.

## Implementation Steps

1. **Parse refined type definitions** — `type Name = BaseType where constraint`
2. **Parse constraint expressions** — limited expression grammar (comparisons, boolean, arithmetic)
3. **Validate constraint grammar** — reject arbitrary function calls, side effects
4. **Resolve constants in constraints** — `EMAIL_REGEX` must be a module-level constant
5. **Generate `.new()` associated function** — validates constraint, returns `Result T ConstraintError`
6. **Generate `.value` field** — unwraps to the base type
7. **Type check refined types** — a `NonZero` is distinct from `Int` (nominal typing)
8. **Emit constraint validation in codegen** — runtime check at `.new()` call compiled to native code
9. **Implement ConstraintError message generation** — human-readable description of the failed constraint
10. **Implement generic refined types** — `NonEmpty a` with generic base type
11. **Optional: compile-time constraint evaluation** for literal arguments

### Constraint Compilation

The constraint `where self >= 0.0 and self <= 100.0` compiles to a validation function:

```rust
fn validate_percentage(value: f64) -> Result<Percentage, ConstraintError> {
    if value >= 0.0 && value <= 100.0 {
        Ok(Percentage(value))
    } else {
        Err(ConstraintError {
            message: format!("expected 0.0 <= value <= 100.0, got {}", value),
        })
    }
}
```

## Data Structures

```rust
// Refined type definition
struct RefinedTypeDef {
    id: DefId,
    name: Symbol,
    type_params: Vec<Symbol>,     // for generic refined types like NonEmpty a
    base_type: Type,              // the wrapped type
    constraint: ConstraintExpr,   // the where clause
    span: Span,
}

// Constraint expression (restricted subset of Expr)
enum ConstraintExpr {
    // Comparisons
    Compare(Box<ConstraintExpr>, CompareOp, Box<ConstraintExpr>),
    // Boolean
    And(Box<ConstraintExpr>, Box<ConstraintExpr>),
    Or(Box<ConstraintExpr>, Box<ConstraintExpr>),
    Not(Box<ConstraintExpr>),
    // Values
    SelfRef,                      // self
    FieldAccess(Box<ConstraintExpr>, Symbol),  // self.field
    MethodCall(Box<ConstraintExpr>, Symbol, Vec<ConstraintExpr>),  // self.len, self.matches(pat)
    Literal(LiteralValue),        // 0, 0.0, ""
    Constant(Symbol),             // EMAIL_REGEX
    // Arithmetic
    Arithmetic(Box<ConstraintExpr>, ArithOp, Box<ConstraintExpr>),
}

enum CompareOp { Eq, NotEq, Lt, Gt, LtEq, GtEq }
enum ArithOp { Add, Sub, Mul, Div, Mod }
```

## Testing Strategy

- **Basic validation:** `NonZero.new(42)` returns `Ok`, `NonZero.new(0)` returns `Err`
- **Range constraint:** `Percentage.new(50.0)` → `Ok`, `Percentage.new(101.0)` → `Err`
- **String constraint:** `Email.new("user@example.com")` → `Ok`, `Email.new("invalid")` → `Err`
- **Generic refined type:** `NonEmpty.new([1, 2, 3])` → `Ok`, `NonEmpty.new([])` → `Err`
- **Value access:** `nz.value` returns the underlying `Int`
- **Type distinction:** `NonZero` is not `Int` — passing `NonZero` where `Int` is expected requires `.value`
- **Compound constraints:** `self >= 0.0 and self <= 100.0` both conditions checked
- **Error message quality:** Failed constraint produces readable message
- **Constant in constraint:** `EMAIL_REGEX` resolves to its value
- **Invalid constraint:** Function call in where clause → compile error

## Open Questions

1. Should refined types implement all concepts of their base type automatically? e.g., `NonZero: Add + Sub + ...`? (Recommendation: no — `Add` on `NonZero` could produce zero. Let users opt-in to specific concepts with custom implementations.)
2. Refined type composition: `type PositiveNonZero = Positive`? (Recommendation: type aliases work; for composing constraints, `type PosNZ = Int where self > 0` is clearer)
3. Should `.value` be a field or a method? (Recommendation: field syntax for consistency with struct access, but compiler-generated)

---

## Addendum: Function Contracts (`requires` / `ensures`)

Contracts are closely related to refined types but apply to function preconditions and postconditions rather than type invariants.

### Syntax

```aura
def withdraw(account: Account, amount: Positive) -> Result Account InsufficientFunds
  requires account.status == Active
  ensures result.ok?.balance == account.balance - amount.value
= {
  if account.balance < amount.value {
    return Err(InsufficientFunds)
  }
  Ok(account with { balance: account.balance - amount.value })
}
```

### Contract Expression Language

Contracts use a **superset** of the refined type constraint grammar. Additional features:

| Feature | Example | Description |
|---------|---------|-------------|
| `result` keyword | `result.is_ok()` | Refers to the function's return value (in `ensures` only) |
| Parameter references | `account.balance` | Any function parameter name |
| Optional chaining | `result.ok?.balance` | Evaluates field if `Ok`, skips check if `Err` |
| Result inspection | `result.is_ok()`, `result.is_err()` | Check result variant |

### When to Use Which

- **Refined types:** Data invariants on a single value (`NonEmpty`, `Positive`, `Email`)
- **Contracts:** Relational preconditions involving multiple parameters or complex state

### Runtime Behavior

- **Debug builds:** `requires` checked before function body, `ensures` checked after
- **Release builds:** Contracts are **not checked** (documentation only)
- **Violation:** Contract violation in debug mode panics with a clear message including the failed expression and parameter values

### Implementation Steps (Contracts)

1. **Parse `requires`/`ensures` clauses** — between return type and `=` body
2. **Validate contract expressions** — superset of constraint grammar (allow `result`, params, optional chaining)
3. **Resolve names in contracts** — parameter names, `result`, field access
4. **Type check contract expressions** — must evaluate to `Bool`
5. **Generate debug-mode checks** — insert `requires` check at function entry, `ensures` check before return
6. **Skip in release builds** — conditional compilation, contracts become no-ops
7. **Error messages on violation** — include failed expression, parameter values, source location

### Testing Strategy (Contracts)

- **Requires pass:** Call with valid precondition → no panic
- **Requires fail (debug):** Call with invalid precondition → panic with message
- **Ensures pass:** Function returns value satisfying postcondition → no panic
- **Ensures fail (debug):** Postcondition violated → panic with message
- **Release mode:** Contracts not checked, no overhead
- **Optional chaining:** `result.ok?.field` skips check when result is `Err`

## Estimated Complexity

**M (Medium)** — The constraint grammar is intentionally limited. The main implementation work is the `.new()` code generation and constraint evaluation. Generic refined types add some complexity. Contracts add a small amount on top (M overall stays M — contracts reuse the constraint evaluation infrastructure).
