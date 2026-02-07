# P2-03: Error Handling

## Overview

Aura uses `Result a e` for all error handling — no exceptions, no panics (except for unrecoverable bugs). The `?` operator provides ergonomic error propagation, and **automatic `From` derivation** for wrapping variants enables seamless error type conversion.

## Dependencies

- **P1-01: Algebraic Types** — `Result` and `Option` are ADTs
- **P1-03: Concepts** — `From` concept for error conversion
- **P0-04: Basic Types** — type checking of `?` expressions

## Design Decisions

### The `?` Operator

`expr?` on a `Result a e` expression:
- If `Ok(value)`: unwraps to `value`
- If `Err(error)`: converts the error using `From` and returns early with `Err(converted_error)`

Desugaring:

```aura
let x = expr?

// Desugars to:
let x = match expr {
    Ok(value) => value,
    Err(e) => return Err(From.from(e)),
}
```

### Automatic `From` Derivation for Wrapping Variants

When a sum type has a single-payload variant that wraps another type, the compiler auto-derives `From`:

```aura
type AppError = Io(IoError) | Db(DbError) | InvalidInput(String)

// Auto-derived:
// instance From IoError for AppError { def from(e) = Io(e) }
// instance From DbError for AppError { def from(e) = Db(e) }
```

This enables `?` to automatically wrap errors:

```aura
def load_data() -> Result Data AppError [Fs.Read, Db.Read] = {
    let content = Fs.read("data.json")?  // IoError auto-wraps to AppError via Io variant
    let parsed = Db.query(content)?      // DbError auto-wraps via Db variant
    Ok(parsed)
}
```

### Derivation Rules

Auto-`From` is derived when:
1. The sum type has a variant with exactly one payload type
2. The payload type is distinct (no two variants wrap the same type)
3. The variant name wraps the error type (convention, not enforced)

If two variants wrap the same type, no auto-`From` is derived (ambiguous) — the user must implement `From` manually.

### `?` on Option

`?` also works on `Option a`:
- `Some(value)`: unwraps to `value`
- `None`: returns early with `None`

This requires the enclosing function to return `Option`. Mixing `?` on `Result` and `Option` in the same function requires explicit conversion.

### No Exception-Like Behavior

- No `try`/`catch`
- No `throw`
- `panic` exists but is for unrecoverable bugs (assertion failures, "impossible" states), not error handling
- `panic` is not an effect — it indicates a program bug, not an expected failure

## Implementation Steps

1. **Implement `?` operator parsing** — postfix `?` on expressions
2. **Implement `?` desugaring for Result** — match + early return + From conversion
3. **Implement `?` desugaring for Option** — match + early return with None
4. **Implement auto-`From` derivation** — scan sum type variants, generate `From` instances
5. **Implement `From` concept** — the core conversion concept
6. **Implement `Into` auto-derivation** — `Into` is the reverse of `From`, auto-derived
7. **Type check `?` usage** — verify the function returns `Result`/`Option`, verify `From` instance exists
8. **Implement error type inference** — when `?` is used, infer the function's error type from the `From` chain
9. **Implement `panic` as a built-in** — aborts with message, not catchable
10. **Clear error messages** — "no `From` instance for converting `ParseError` to `AppError`; consider adding a wrapping variant"

### Auto-`From` Algorithm

```
For each sum type definition:
    For each variant with exactly one payload type T:
        Check no other variant also has payload type T
        If unique:
            Generate: instance From T for SumType { def from(e) = Variant(e) }
```

## Data Structures

```rust
// The From concept (built-in)
// concept From a {
//     def from(value: a) -> Self
// }

// Auto-derived From instance
struct AutoFromInstance {
    from_type: Type,            // the payload type being wrapped
    to_type: DefId,             // the sum type
    variant: DefId,             // which variant does the wrapping
}

// ? operator desugaring context
struct TryContext {
    return_type: Type,          // must be Result or Option
    error_type: Option<Type>,   // the error type E in Result a E
}

// Error propagation chain for diagnostics
struct ErrorChain {
    source: Type,               // original error type
    target: Type,               // target error type
    conversion: ConversionKind,
}

enum ConversionKind {
    AutoFrom(DefId),            // via auto-derived From (variant DefId)
    ManualFrom(DefId),          // via manually implemented From
    NoConversion,               // error: no conversion available
}
```

## Testing Strategy

- **Basic `?` on Result:** `Ok(42)?` returns `42`, `Err("oops")?` propagates error
- **`?` on Option:** `Some(42)?` returns `42`, `None?` returns `None`
- **Auto-From wrapping:** `type AE = Io(IoError)` → `io_result?` auto-wraps `IoError` to `AE`
- **Multiple wrapping variants:** `type AE = Io(IoError) | Db(DbError)` — both auto-derive
- **Ambiguous wrapping:** Two variants with same payload type → no auto-derive, requires manual `From`
- **Manual From:** User-defined `From` conversion works with `?`
- **Nested `?`:** Multiple `?` operations in sequence, each converting different error types
- **Wrong return type:** Using `?` in a function that returns `Int` → error
- **Missing From:** `?` on error type with no `From` instance → clear error message

## Open Questions

1. Should `?` work on custom types beyond `Result` and `Option`? e.g., a `Try` concept? (Recommendation: no — keep it simple, only `Result` and `Option`)
2. Should there be a `.unwrap()` method that panics on `Err`/`None`? (Recommendation: yes, for prototyping and cases where failure is a bug — but with a lint warning in production code)
3. Error chaining / `cause`: Should `AppError` automatically carry the source error for debugging? (Recommendation: defer — the wrapping variant already preserves the original error)

## Estimated Complexity

**M (Medium)** — `?` desugaring and auto-`From` are well-understood patterns. The main work is wiring up type checking for the `From` concept interaction.
