# P3-02: Closures & Capture Semantics

## Overview

Closures in Aura capture variables by reference. Captured values are kept alive by the garbage collector — as long as the closure is reachable, its captured values are reachable too. Closures are first-class, GC-managed values.

This document covers closure creation, capture analysis, closure conversion for LLVM codegen, and integration with the GC.

## Dependencies

- **P3-01: GC** — closures and captured values are GC-managed objects
- **P0-05: Basic Codegen** — LLVM closure representation
- **P1-02: Generics** — closures may be polymorphic (in effect, via HOFs)
- **P2-02: Effect Polymorphism** — closures carry effects

## Current Implementation Status (as of February 7, 2026)

- **Implemented (front-end):** Lambda syntax and basic lambda typing/effect capture are supported in parser/typechecker.
- **Implemented (front-end foundations):** Closure expressions participate in effect-polymorphic callback analysis.
- **Partial/Deferred:** Capture analysis, environment-struct generation, closure conversion, and closure call lowering are not implemented in LLVM codegen yet.
- **Partial/Deferred:** Mutable-capture promotion and closure/GC runtime integration are planned but not wired into generated programs.

## Design Decisions

### Capture Mode

**Capture by reference (GC-traced).** When a closure captures a variable:
1. The captured value is stored in a GC-allocated environment struct
2. The closure holds a pointer to the environment
3. The GC keeps the environment (and its contents) alive as long as the closure is reachable
4. Cycles between closures and captured data are handled naturally by the GC

### Closure Representation (LLVM)

A closure is compiled to a pair:
1. **Function pointer** — points to the closure's compiled body (which takes the environment as its first argument)
2. **Environment pointer** — GC-allocated struct containing captured variables

```aura
let x = 10
let f = (y) -> x + y  // captures x
f(5)  // returns 15
```

After closure conversion:

```llvm
; Environment struct for f
%Env_f = type { i64 }       ; captured x

; Closure body — takes environment as first arg
define i64 @f_impl(%Env_f* %env, i64 %y) {
  %x = load i64, %Env_f* %env, i32 0
  %result = add i64 %x, %y
  ret i64 %result
}

; At call site — allocate env, store captures, make closure pair
%env = call %Env_f* @aura_gc_alloc(...)
store i64 10, %Env_f* %env, i32 0
; f = { @f_impl, %env }
```

### Closure Types

Closures are typed as function types:

```aura
(x) -> x + 1           // Int -> Int (if context determines Int)
(a, b) -> a + b         // a * b -> c with appropriate constraints
(x) -> { Log.info("{x}"); x }  // a -> a [Log]
```

In the type system, closures are `Function(params, return_type, effects)`. There is no distinction between function pointers and closures at the type level. At the LLVM level, all function values are closure pairs (bare functions use a null environment pointer).

### Closures as Values

Closures are first-class values:
- Can be assigned to variables
- Can be passed as arguments
- Can be returned from functions
- Can be stored in data structures
- Are GC-managed (the closure pair and its environment are traced by the GC)

### Mutable Captures

When a `let mut` binding is captured by a closure:

1. The binding is promoted to a GC-allocated **cell** (a mutable box on the heap)
2. Both the original scope and the closure reference the same cell
3. Mutations through either are visible to the other

```aura
let mut counter = 0
let inc = () -> { counter = counter + 1 }
inc()
inc()
// counter is now 2
```

Internally, `counter` is promoted from a stack `alloca` to a GC-allocated `MutCell { value: Int }`. The original scope and the closure both hold pointers to this cell.

This is safe because:
- The GC prevents use-after-free
- No concurrent access issues (async closures use the async runtime's synchronization)

**Lint:** Emit a warning when a `let mut` binding is captured by a closure, suggesting alternatives.

### Capture Analysis

The compiler performs capture analysis to determine which variables a closure references:

1. Walk the closure body
2. For each free variable (not defined within the closure), add it to the capture set
3. If the variable is `let mut`, promote it to a heap cell
4. Generate environment struct type and allocation code

## Implementation Steps

1. **Implement closure parsing** — `(params) -> expr` syntax
2. **Implement capture analysis** — identify free variables in closure body
3. **Implement environment struct generation** — for each closure, define a struct type with captured variables
4. **Implement closure conversion in MIR** — lower closures to function pointer + environment allocation
5. **Emit LLVM code for closure creation** — `gc_alloc` environment, store captures, create pair
6. **Emit LLVM code for closure call** — extract function pointer and environment, call with environment as first argument
7. **Implement closure type checking** — infer parameter types from context, check body
8. **Implement closure effect inference** — determine effects from body
9. **Implement mutable capture promotion** — `let mut` captured by closure → heap cell
10. **Register closure environments as GC roots** — environment struct fields are traced
11. **Optimize single-use closures** — inline if called exactly once and not stored

## Data Structures

```rust
// Capture analysis result
struct CaptureInfo {
    closure_span: Span,
    captures: Vec<CapturedVar>,
}

struct CapturedVar {
    name: Symbol,
    def_id: DefId,
    is_mutable: bool,   // if true, promote to heap cell
    ty: Type,
}

// Closure conversion output (MIR level)
struct ClosureConversion {
    // The environment struct type
    env_type: MirType,
    // The converted function (takes env as first param)
    converted_fn: MirFunction,
    // Allocation + store instructions for creating the closure
    creation_code: Vec<MirInst>,
}

// LLVM closure pair representation
// { func_ptr: fn_type, env_ptr: *mut Env }
// For bare functions: env_ptr = null
```

## Testing Strategy

- **Basic closure:** `let f = (x) -> x + 1; f(5)` returns `6`
- **Capture by reference:** `let x = 10; let f = (y) -> x + y; f(5)` returns `15`
- **Closure outlives scope:** Return closure from function, call it later — GC keeps env alive
- **Nested closures:** Closure within closure, outer captures visible in inner
- **Closure as argument:** `map(list, (x) -> x * 2)` works
- **Closure with effects:** `(x) -> { Log.info("{x}"); x }` has `[Log]` effect
- **Mutable capture:** `let mut x = 0; let f = () -> { x = x + 1 }; f(); f(); x == 2`
- **Multiple captures:** Closure captures several variables from different scopes
- **Closure and GC:** Create many closures, trigger GC, verify captured values survive

## Open Questions

1. Should closures support explicit capture lists? e.g., `[x, y] (z) -> x + y + z`? (Recommendation: no — implicit capture is simpler and sufficient)
2. Recursive closures: `let f = (n) -> if n == 0 { 1 } else { n * f(n-1) }` — is `f` available inside its own body? (Recommendation: yes, via late binding — the closure's environment is updated to include itself after creation)
3. Uniform closure representation: Should all function values use the closure pair representation, even non-capturing functions? (Recommendation: yes, simplifies calling convention. Null env pointer for bare functions.)

## Estimated Complexity

**M (Medium)** — Capture analysis is straightforward. Closure conversion for LLVM is well-understood (standard compiler technique). GC integration is simpler than ARC — no retain/release logic, just allocate and let the GC trace.
