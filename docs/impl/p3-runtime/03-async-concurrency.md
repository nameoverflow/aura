# P3-03: Async & Structured Concurrency

## Overview

Aura's concurrency model is based on **structured concurrency**: all concurrent work is scoped, and child tasks cannot outlive their parent scope. There are no detached tasks, no free-floating futures, and no `.await` operator. Calling an `async def` from another `async def` implicitly awaits (sequential by default). Concurrency is explicit via `parallel`, `race`, and `timeout` blocks.

## Dependencies

- **P0-05: Basic Codegen** — execution of async code
- **P2-01: Effect System** — effects propagate through async boundaries
- **P3-01: GC** — shared values across concurrent tasks
- **P3-02: Closures** — tasks are essentially closures

## Current Implementation Status (as of February 8, 2026)

- **Implemented (front-end):** `async def` parsing and type-checker async-boundary enforcement (sync code cannot call async directly).
- **Implemented (front-end):** `parallel`, `race`, and `timeout` syntax + AST nodes + typechecking rules, including race-arm type unification and timeout result typing.
- **Implemented (bridge + backend lowering):** `Runtime.block_on(...)` is recognized in typing and lowered in codegen for sync/async boundaries.
- **Implemented (backend paths):** `parallel`, `race`, and `timeout` expressions have concrete backend lowering paths so all P3 `ok_*.aura` fixtures compile through `aurac --emit-ir`.

## Design Decisions

### Async Execution Model

- `async def` marks a function as asynchronous
- Calling `async def` from `async def` is **implicit sequential await**
- Calling `async def` from non-async `def` is a **compile error**
- `Runtime.block_on(expr)` is the only way to bridge async into sync

### Concurrency Primitives

| Primitive | Behavior | Return Type |
|-----------|----------|-------------|
| `parallel { for x in xs yield expr }` | Run all concurrently, collect results | `List a` |
| `parallel { yield expr1, yield expr2 }` | Run fixed set concurrently | `a * b` (product type) |
| `race { expr1, expr2 }` | First to complete wins, cancel others | `a` (same type for all arms) |
| `timeout(duration) { expr }` | Cancel if duration exceeded | `Result a Timeout` |

### Cancellation

Cancellation is **cooperative**:
- When a scope exits (timeout, race loser, fail-fast error), child tasks receive a cancellation signal
- Tasks check for cancellation at yield points (I/O operations, explicit yield points)
- Cancelled tasks are not forcibly killed — they are dropped at the next suspension point

### Error Handling in Parallel

Two patterns:

1. **Fail-fast:** Use `?` inside `yield` — first error cancels remaining tasks
   ```aura
   parallel {
       for id in ids yield fetch(id)?   // Result (List a) e
   }
   ```

2. **Collect-all:** No `?` inside `yield` — all tasks run to completion
   ```aura
   parallel {
       for id in ids yield fetch(id)    // List (Result a e)
   }
   ```

### Effect Propagation

Effects from child tasks bubble up to the parent:
- `parallel { yield f(), yield g() }` where `f: [Db.Read]` and `g: [Net]` → parent needs `[Db.Read, Net]`
- This is handled by effect polymorphism on the concurrency primitives

### Runtime Implementation

The async runtime is a compiled library (`libaura_async_rt`) linked into the final executable, backed by `tokio` or a custom lightweight executor. It provides task spawning, cancellation tokens, and the `block_on` entry point.

### No Detached Tasks

There is no `spawn` or `go` equivalent. All concurrency must be structured:
- Tasks are created only within `parallel`, `race`, or `timeout`
- Tasks cannot escape their creating scope
- When the scope exits, all tasks are complete (or cancelled)

## Implementation Steps

1. **Implement `async def` parsing and type checking** — mark functions as async, prevent sync-calls-async
2. **Implement implicit await** — async calling async is sequential
3. **Implement `parallel` block parsing** — both `for...yield` and `yield expr1, yield expr2` forms
4. **Implement `race` block parsing** — multiple concurrent expressions, first wins
5. **Implement `timeout` block parsing** — duration + body expression
6. **Implement async runtime library** — Rust `tokio`-backed executor compiled into `libaura_async_rt`
7. **Implement task spawning for `parallel`** — spawn N tasks, collect results
8. **Implement cancellation** — when parent scope exits, cancel children
9. **Implement fail-fast semantics** — `?` in yield cancels siblings on error
10. **Implement collect-all semantics** — all tasks run to completion, collect results
11. **Implement `race` semantics** — first result returned, others cancelled
12. **Implement `timeout` semantics** — wrap result in `Result a Timeout`
13. **Implement `Runtime.block_on`** — bridge async to sync at program boundaries
14. **Implement effect propagation** — child effects union into parent scope
15. **Type check parallel return types** — for-yield returns `List a`, fixed-yield returns product type

## Data Structures

```rust
// AST nodes for concurrency
enum ParallelBody {
    // parallel { for x in xs yield expr }
    ForYield {
        var: Symbol,
        iter: Box<Expr>,
        body: Box<Expr>,
        fail_fast: bool,    // true if body contains ?
    },
    // parallel { yield expr1, yield expr2, ... }
    FixedYield(Vec<Expr>),
}

// Runtime task representation
struct Task {
    id: TaskId,
    state: TaskState,
    result: Option<Value>,
    cancel_token: CancelToken,
}

enum TaskState {
    Pending,
    Running,
    Completed,
    Cancelled,
    Failed(RuntimeError),
}

#[derive(Clone)]
struct CancelToken {
    cancelled: Arc<AtomicBool>,
}

impl CancelToken {
    fn cancel(&self) { self.cancelled.store(true, Ordering::Release); }
    fn is_cancelled(&self) -> bool { self.cancelled.load(Ordering::Acquire) }
}

// Async runtime (compiled library linked into executable)
struct AsyncRuntime {
    executor: tokio::Runtime,  // or custom lightweight executor
}

impl AsyncRuntime {
    fn block_on<F: Future>(&self, f: F) -> F::Output;
    fn spawn<F: Future>(&self, f: F) -> JoinHandle<F::Output>;
}
```

## Testing Strategy

- **Async/sync boundary:** Calling async from sync → compile error
- **Implicit await:** Sequential async calls execute in order
- **Parallel for-yield:** `parallel { for id in [1,2,3] yield fetch(id) }` runs concurrently
- **Parallel fixed yield:** `parallel { yield f(), yield g() }` returns product type
- **Fail-fast:** First error cancels remaining tasks
- **Collect-all:** All tasks complete, errors collected in results
- **Race:** First to complete is returned, others cancelled
- **Timeout:** Expression completes within timeout → `Ok`, exceeds → `Err(Timeout)`
- **Cancellation:** Cancelled tasks don't leak resources
- **Effect propagation:** Effects from parallel children propagate to parent
- **Nested parallel:** Parallel blocks within parallel blocks work correctly
- **Runtime.block_on:** Entry point for async programs

## Open Questions

1. How to implement cooperative cancellation in compiled code? Check cancellation at every IO operation? (Recommendation: yes, check at every effect boundary / GC safepoint)
2. Thread pool size: Configurable or auto-detected? (Recommendation: auto-detected based on CPU cores, configurable via env var)
3. Should `parallel` have a concurrency limit? e.g., `parallel(max: 10) { ... }` (Recommendation: yes, useful for rate limiting. Defer to later if complex.)
4. Backpressure: How to handle `parallel { for item in huge_list yield ... }`? (Recommendation: bounded concurrency — don't spawn all tasks at once, use a bounded work queue)

## Estimated Complexity

**XL (Extra Large)** — Structured concurrency with cancellation, fail-fast/collect-all patterns, race semantics, and timeout is a substantial implementation. The async runtime is complex to get right, especially cancellation semantics.
