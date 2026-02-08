# P3-01: Mark-Sweep Garbage Collector

## Overview

Aura uses a **mark-sweep garbage collector (GC)** for automatic memory management. The GC traces reachable objects from root references and frees unreachable memory. This eliminates the need for manual memory management, reference counting, or weak references — cycles are handled naturally.

## Dependencies

- **P0-05: Basic Codegen** — the LLVM backend needs GC allocation calls
- **P1-01: Algebraic Types** — object layout for heap-allocated types

## Current Implementation Status (as of February 8, 2026)

- **Implemented:** Runtime crate (`aura_rt`) includes a stop-the-world mark-sweep collector with object headers, heap object list tracking, and adaptive threshold-triggered collection.
- **Implemented:** Shadow stack frame API (`aura_gc_push_frame`/`aura_gc_pop_frame`) and explicit collection entrypoints (`aura_gc_collect`, `aura_runtime_gc`).
- **Implemented:** Runtime GC tests cover unreachable collection, rooted reachability, and cycle collection behavior.
- **Implemented:** LLVM closure lowering now allocates closure environments/objects through runtime GC allocation (`aura_gc_alloc`) and generated binaries link against `aura_rt`.

## Design Decisions

### Why Mark-Sweep (Not ARC)

| Concern | Mark-Sweep GC | ARC |
|---------|--------------|-----|
| Cycles | Handled automatically | Requires `Weak` references |
| Throughput | Higher (no per-operation overhead) | Lower (retain/release on every copy) |
| Latency | Pause during collection | Predictable (no pauses) |
| Complexity | Runtime is complex, user code is simple | Runtime is simple, user code manages cycles |
| Concurrency | Natural (trace all live objects) | Atomic refcounts for thread safety |

For an AI-native language optimizing for simplicity and correctness, GC is the better trade-off. Developers never think about cycles or `Weak` — they just write code.

### GC Architecture

**Stop-the-world mark-sweep** for the initial implementation. This is the simplest correct GC:

1. **Allocate:** All heap objects are allocated via the GC allocator
2. **Mark:** Starting from GC roots, recursively mark all reachable objects
3. **Sweep:** Scan the heap, free all unmarked objects
4. **Reset:** Clear mark bits for the next cycle

Future improvements (not in initial implementation):
- Generational collection (young/old generations)
- Concurrent marking (reduce pause times)
- Incremental collection

### GC Roots

Roots are pointers the GC starts tracing from:

| Root Kind | Location |
|-----------|----------|
| Stack variables | Local `let` bindings in active call frames |
| Global variables | Module-level definitions |
| Registers | CPU registers holding GC pointers (spilled to stack at GC safepoints) |

### Safepoints

GC can only run at **safepoints** — points where all GC roots are known and consistent. Safepoints are inserted at:

- Function call sites (before the call)
- Loop back-edges (at the top of each loop iteration)
- Allocation sites (before allocating, in case collection is needed)

At each safepoint, the compiler generates a **stack map** that tells the GC where all root pointers live on the stack.

### Object Layout

Every GC-managed object has a header:

```
┌──────────────┬──────────────┬─────────────────┐
│  Mark bit(s) │  Type tag    │  Payload ...    │
│  (1-2 bytes) │  (pointer)   │  (fields)       │
└──────────────┴──────────────┴─────────────────┘
```

- **Mark bit:** Used during mark phase (1 = reachable, 0 = garbage)
- **Type tag:** Pointer to type descriptor (needed for tracing fields)
- **Payload:** The actual object data (struct fields, variant tag + data, etc.)

### Stack vs Heap Allocation

| Allocation | Types | Rule |
|------------|-------|------|
| Stack | Primitives (`Int`, `Float64`, `Bool`, `Char`), small structs known not to escape | Escape analysis or explicit `Copy` |
| Heap (GC) | `String`, `List`, closures, large structs, anything that escapes its scope | Default for non-primitive types |

The compiler may perform **escape analysis** to stack-allocate objects that don't escape the current function, but this is an optimization (P4-03), not required for correctness.

### Collection Triggers

GC runs when:
- The heap reaches a threshold size (e.g., 2x the live data from the last collection)
- An allocation fails (out of memory)
- Explicitly requested (for testing: `Runtime.gc()`)

The threshold is adaptive — it grows as the program's live data set grows.

### Integration with LLVM

LLVM has built-in GC support via:
- `gc "statepoint-example"` function attribute — marks functions as GC-aware
- **Statepoints** — LLVM intrinsics that generate stack maps at safepoints
- **Stack map format** — LLVM emits `.llvm_stackmaps` section with root locations

Alternatively, use a **shadow stack** approach (simpler, slightly slower):
- Maintain a linked list of stack frames with GC root pointers
- Each function pushes/pops its roots onto the shadow stack
- The GC walks the shadow stack to find all roots

**Recommendation: Shadow stack first**, upgrade to LLVM statepoints later. Shadow stack is simpler and sufficient for initial implementation.

## Implementation Steps

1. **Design GC object header** — mark bit, type tag, size
2. **Implement GC allocator** — `gc_alloc(size, type_tag) -> *mut Object`
3. **Implement type descriptors** — for each type, a descriptor listing which fields are GC pointers
4. **Implement shadow stack** — push/pop GC roots at function entry/exit
5. **Implement mark phase** — recursive tracing from roots through object graph
6. **Implement sweep phase** — scan heap, free unmarked objects, reset mark bits
7. **Implement collection trigger** — check heap size threshold at allocation sites
8. **Emit GC allocation calls in codegen** — replace `malloc` calls with `gc_alloc`
9. **Emit shadow stack operations in codegen** — root registration at function boundaries
10. **Implement safepoint insertion** — at loop back-edges and call sites
11. **Implement `Runtime.gc()`** — explicit collection for testing
12. **Test with stress scenarios** — allocate many objects, verify no leaks, no dangling pointers

## Data Structures

```rust
// GC runtime (implemented in Rust, linked into compiled Aura programs)

// Object header — prepended to every GC-managed allocation
#[repr(C)]
struct GcHeader {
    mark: u8,                    // 0 = white (garbage), 1 = gray, 2 = black (reachable)
    type_desc: *const TypeDesc,  // pointer to type descriptor
    next: *mut GcHeader,         // linked list of all allocated objects
}

// Type descriptor — one per type, shared by all instances
#[repr(C)]
struct TypeDesc {
    size: usize,                 // total size of object (excluding header)
    name: *const u8,             // type name for debugging
    num_gc_fields: usize,        // number of fields that are GC pointers
    gc_field_offsets: *const usize, // byte offsets of GC pointer fields
}

// Shadow stack frame
#[repr(C)]
struct ShadowFrame {
    prev: *mut ShadowFrame,      // link to caller's frame
    num_roots: usize,            // number of GC roots in this frame
    roots: [*mut GcHeader; 0],   // flexible array of root pointers
}

// The GC itself
struct GarbageCollector {
    // All allocated objects (linked list via GcHeader.next)
    all_objects: *mut GcHeader,
    // Shadow stack top
    shadow_stack: *mut ShadowFrame,
    // Heap statistics
    bytes_allocated: usize,
    threshold: usize,            // trigger collection when bytes_allocated > threshold
    // Tuning
    growth_factor: f64,          // threshold = live_bytes * growth_factor (default 2.0)
}

impl GarbageCollector {
    fn alloc(&mut self, type_desc: &TypeDesc) -> *mut GcHeader;
    fn collect(&mut self);
    fn mark(&mut self);          // trace from roots
    fn sweep(&mut self);         // free unmarked objects
}
```

### Shadow Stack Usage in Generated Code

```llvm
; Function entry: push shadow frame
%frame = alloca { %ShadowFrame*, i64, [N x %Object*] }
store %ShadowFrame* @shadow_stack_top, %frame.prev
store i64 N, %frame.num_roots
store %frame, @shadow_stack_top

; Register a root
store %my_local, %frame.roots[0]

; Function exit: pop shadow frame
store %frame.prev, @shadow_stack_top
```

## Testing Strategy

- **Basic allocation and collection:** Allocate objects, trigger GC, verify live objects survive
- **Cycle collection:** Create A → B → A cycle, drop all external references, GC collects both
- **Stress test:** Allocate millions of small objects, verify GC keeps memory bounded
- **Correctness:** No dangling pointers — access all live objects after GC, verify integrity
- **Root accuracy:** Stack roots are correctly identified — no premature collection
- **Finalizer ordering:** If finalizers are added later, verify correct order
- **Performance:** Measure GC pause times and throughput on benchmark programs

## Open Questions

1. Generational GC: When to add generational collection? (Recommendation: after the basic mark-sweep is proven correct. Generational GC is a pure performance optimization.)
2. Concurrent/incremental GC: When to reduce pause times? (Recommendation: only when real-world programs show problematic pauses. LLVM statepoints would be the prerequisite.)
3. Finalizers: Should Aura support destructors/finalizers? (Recommendation: no explicit finalizers — use `defer`-like patterns or RAII wrappers for resource cleanup. Finalizers complicate GC and have unpredictable timing.)
4. Value types: Should small structs with all-primitive fields be stack-allocated automatically? (Recommendation: yes, as an optimization in P4-03 via escape analysis. For correctness, treat everything as heap-allocated initially.)

## Estimated Complexity

**L (Large)** — The basic mark-sweep algorithm is well-understood but integrating it with LLVM codegen (shadow stack, safepoints, type descriptors) adds significant complexity. The GC runtime is a standalone Rust/C library linked into every Aura program.
