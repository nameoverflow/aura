# P4-03: Optimization Passes

## Overview

Optimization passes transform the MIR to produce faster, smaller code. Aura-specific optimizations focus on GC pressure reduction (fewer allocations) and effect-based optimizations (e.g., reordering pure code).

## Dependencies

- **P0-05: Basic Codegen** — MIR as input
- **P3-01: GC** — GC-aware optimizations

## Design Decisions

### Optimization Pipeline

Passes run in a fixed order, some iterated until convergence:

1. **Constant folding** — evaluate constant expressions at compile time
2. **Dead code elimination** — remove unused computations
3. **Inlining** — inline small functions at call sites
4. **Escape analysis** — stack-allocate objects that don't escape the function
5. **Copy propagation** — eliminate unnecessary variable copies
6. **Common subexpression elimination** — reuse identical computations
7. **Loop optimizations** — loop-invariant code motion, strength reduction
8. **Tail call optimization** — convert tail calls to jumps
9. **Monomorphization cleanup** — remove unused specializations
10. **Allocation sinking/merging** — reduce GC allocation frequency

### GC-Aware Optimizations

These reduce GC pressure and improve cache behavior:

1. **Escape analysis:** If an object doesn't escape the function that creates it, allocate it on the stack instead of the GC heap. This is the single most impactful GC optimization.

   ```aura
   def distance(a: Point, b: Point) -> Float64 = {
     let diff = Point { x: a.x - b.x, y: a.y - b.y }  // diff doesn't escape
     (diff.x * diff.x + diff.y * diff.y).sqrt()
   }
   // After escape analysis: diff is stack-allocated, no GC allocation
   ```

2. **Allocation sinking:** Delay allocation until it's actually needed (after early returns, guards, etc.)

3. **Allocation merging:** If multiple small objects are always created together and have the same lifetime, allocate them as a single larger block.

4. **Scalar replacement of aggregates (SRA):** If a struct's fields are accessed individually and the struct doesn't escape, replace it with individual scalar variables (no allocation at all).

### Effect-Based Optimizations

Pure functions (no effects) enable:
1. **Memoization:** Cache results of pure function calls
2. **Reordering:** Pure expressions can be reordered for better locality
3. **Parallelization:** Pure loops can be auto-parallelized (future)
4. **Dead call elimination:** If a pure function's result is unused, the call can be removed

### Optimization Levels

| Level | Behavior |
|-------|----------|
| `-O0` | No optimization (debug builds) — fastest compile time |
| `-O1` | Basic: constant folding, dead code elimination |
| `-O2` | Standard: all passes above + inlining, CSE, escape analysis |
| `-O3` | Aggressive: all passes + loop optimizations, aggressive inlining |

LLVM's own optimization passes run after Aura's MIR-level passes, providing additional target-specific optimizations.

## Implementation Steps

1. **Implement constant folding** — evaluate `1 + 2` → `3` at compile time
2. **Implement dead code elimination** — remove instructions whose results are unused
3. **Implement function inlining** — inline small functions (configurable threshold)
4. **Implement escape analysis** — determine which allocations don't escape, mark for stack allocation
5. **Implement copy propagation** — replace `let y = x; use(y)` with `use(x)`
6. **Implement CSE** — detect and reuse common subexpressions
7. **Implement tail call optimization** — detect tail position calls, convert to jumps
8. **Implement pure function optimization** — dead call elimination for pure functions
9. **Implement allocation sinking** — delay GC allocations past early exits
10. **Implement scalar replacement** — decompose non-escaping structs into scalars
11. **Implement optimization level flags** — `-O0` through `-O3`

## Data Structures

```rust
// Optimization pass interface
trait OptimizationPass {
    fn name(&self) -> &str;
    fn run(&self, function: &mut MirFunction) -> bool;  // returns true if changed
}

// Escape analysis
struct EscapeAnalysis {
    // For each allocation, whether it escapes the current function
    escapes: HashMap<MirVar, EscapeState>,
}

enum EscapeState {
    NoEscape,           // safe to stack-allocate
    ArgEscape,          // escapes via function argument (may still be optimizable)
    GlobalEscape,       // escapes to heap (must be GC-allocated)
}

// Optimization pipeline
struct OptPipeline {
    passes: Vec<Box<dyn OptimizationPass>>,
    max_iterations: usize,  // for convergence
}
```

## Testing Strategy

- **Correctness:** Optimized programs produce the same output as unoptimized
- **Escape analysis:** Non-escaping struct is stack-allocated (verify via LLVM IR inspection)
- **Constant folding:** `let x = 1 + 2` folds to `let x = 3`
- **Dead code:** Unused pure function calls are removed
- **Inlining:** Small functions are inlined at call sites
- **Performance benchmarks:** Measure speedup at each optimization level
- **GC pressure:** Fewer GC allocations after optimization (measure allocation count)

## Open Questions

1. Should LLVM's optimization passes be relied on instead of custom MIR optimization? (Recommendation: do both — MIR optimizations handle Aura-specific concerns like escape analysis and effect-based opts, LLVM handles general-purpose low-level optimization)
2. Profile-guided optimization (PGO)? (Recommendation: defer to future versions)
3. Whole-program optimization: Should the optimizer see all modules at once? (Recommendation: via LLVM LTO — cross-module inlining without needing our own whole-program pass)

## Estimated Complexity

**L (Large)** — Each optimization pass is a substantial piece of work. Escape analysis in particular requires careful dataflow analysis. However, passes can be implemented incrementally.
