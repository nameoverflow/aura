# P4-02: Advanced Code Generation & WASM Target

## Overview

Basic LLVM code generation lives in P0-05. This document covers **advanced codegen** features built on top of the basic backend: the WASM target, incremental compilation, advanced lowering passes (closure conversion, monomorphization), and the full IR pipeline maturation.

## Dependencies

- **P0-05: Basic Codegen** — the basic LLVM backend
- **P3-01: GC** — GC-aware code generation
- **P3-02: Closures** — closure conversion
- **P3-03: Async Concurrency** — async runtime code generation

## Design Decisions

### What Moves From P0-05 to P4-02

P0-05 covers the minimum viable LLVM backend (literals, functions, control flow, structs). This doc covers everything beyond that:

| Feature | Where |
|---------|-------|
| Basic LLVM (ints, functions, if/else, structs) | P0-05 |
| Pattern match → decision trees | P1-04 + this doc |
| Closure conversion → LLVM | P3-02 + this doc |
| Monomorphization | This doc |
| GC integration (safepoints, stack maps) | P3-01 + this doc |
| WASM target | This doc |
| Incremental compilation | This doc |
| Debug info (full DWARF) | This doc |
| C FFI / interop | This doc |

### WASM Target

WASM as a secondary compilation target, sharing the same MIR:

```
MIR (optimized)
    ├──→ LLVM IR → native executable     (primary)
    └──→ WASM bytecode → .wasm module    (secondary)
```

WASM-specific considerations:
- **GC:** WASM GC proposal (WasmGC) or bring-your-own GC compiled to WASM linear memory
- **Async:** No native threads in WASM — map `parallel` to `Promise.all` or Web Workers
- **I/O:** WASI for file system, network, etc.
- **String encoding:** UTF-8 interop with JavaScript host

### Monomorphization

Generic functions and types are specialized for each concrete type used:

```aura
max: forall (Ord a). a * a -> a
max(1, 2)         // generates max_Int
max("a", "b")     // generates max_String
```

Monomorphization happens at the MIR level, before LLVM emission:
1. Collect all concrete type instantiations from the call graph
2. For each instantiation, specialize the generic function/type
3. Replace generic calls with calls to specialized versions
4. Dead-code-eliminate unused specializations

### Incremental Compilation

Only recompile modules that changed or whose dependencies changed:

1. **Content hash** each source file
2. **Dependency graph** tracks which modules depend on which
3. **Cache** compiled MIR and LLVM object files per module
4. **Invalidate** a module's cache when its hash or any dependency's public API changes
5. **Relink** — even if only one module changed, relinking is fast

### C FFI

For interop with C libraries:

```aura
@extern("C")
def c_printf(fmt: CString, ...) -> Int [Fs.Write]
```

- `@extern("C")` marks a function as having C calling convention
- C types map to Aura types: `CString`, `CInt`, `CPtr a`
- No automatic marshaling — explicit conversion required

## Implementation Steps

1. **Implement monomorphization pass** — specialize generic functions at MIR level
2. **Implement pattern match → decision tree lowering** — compile `match` to efficient switches/branches
3. **Implement full closure conversion** — environment structs, function pointer pairs
4. **Implement GC safepoint emission** — insert safepoints at loop back-edges and call sites
5. **Implement GC stack map emission** — generate root maps for each safepoint
6. **Implement full DWARF debug info** — source locations, variable info, type info
7. **Implement WASM backend** — MIR → WASM bytecode translation
8. **Implement WASM GC integration** — either WasmGC or linear-memory GC
9. **Implement C FFI** — `@extern("C")` functions, C calling convention
10. **Implement incremental compilation** — content hashing, dependency tracking, caching
11. **Implement link-time optimization (LTO)** — cross-module inlining via LLVM LTO

## Data Structures

```rust
// Monomorphization
struct MonomorphizePass {
    // (generic DefId, concrete type args) -> specialized DefId
    specializations: HashMap<(DefId, Vec<Type>), DefId>,
    // Work queue of pending specializations
    worklist: Vec<(DefId, Vec<Type>)>,
}

// Incremental compilation
struct CompilationCache {
    // source file path -> content hash
    file_hashes: HashMap<PathBuf, u64>,
    // module -> cached MIR
    mir_cache: HashMap<Symbol, CachedModule>,
    // module -> cached object file
    obj_cache: HashMap<Symbol, PathBuf>,
    // dependency graph
    deps: HashMap<Symbol, Vec<Symbol>>,
}

struct CachedModule {
    hash: u64,
    mir: Vec<MirFunction>,
    public_api_hash: u64,  // hash of public signatures only
}
```

## Testing Strategy

- **Monomorphization:** Generic function called with 3 different types → 3 specialized versions
- **WASM output:** Generated WASM passes `wasm-validate`
- **WASM execution:** Run WASM in a runtime (wasmtime), verify same output as native
- **Incremental:** Change one file, only that module recompiles
- **C FFI:** Call C `printf` from Aura, verify output
- **Debug info:** Source locations correct in GDB/LLDB
- **LTO:** Cross-module inlining produces faster code

## Open Questions

1. WASM GC strategy: WasmGC proposal vs compiling mark-sweep to linear memory? (Recommendation: WasmGC if available, linear memory fallback)
2. Should Aura support calling Aura from C (not just C from Aura)? (Recommendation: yes, via `@export("C")` annotation)
3. Monomorphization explosion: How to handle `List (Option (Result a b))` creating many specializations? (Recommendation: set a specialization limit, fall back to boxed/erased representation beyond it)

## Estimated Complexity

**XL (Extra Large)** — Monomorphization, WASM backend, incremental compilation, and C FFI are each substantial. This is the largest P4 component after the standard library.
