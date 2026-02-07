# P0-05: Basic Code Generation (LLVM)

## Overview

Aura targets LLVM directly — there is no interpreter. The code generator transforms the type-checked AST through intermediate representations and emits LLVM IR, which LLVM compiles to native machine code. This is the critical path from source to executable.

## Dependencies

- **P0-04: Basic Types** — type-checked AST

## Design Decisions

### Why LLVM First (No Interpreter)

- Native performance from day one — no "interpreter tax" on early users
- LLVM's mature optimization pipeline handles general-purpose optimizations
- Forces proper IR design early, avoiding a throwaway interpreter
- Debug info, profiling, and sanitizer support come for free via LLVM

### IR Pipeline

```
AST (typed)
    ↓  Desugar (AST → HIR)
    ↓  - Pipeline operator → function calls
    ↓  - String interpolation → concat calls
    ↓  - ? operator → match + return
    ↓  - with syntax → struct copy + field override
    ↓  - for loops → while loops + iterator protocol
    ↓
  HIR (High-level IR)
    ↓  Lower (HIR → MIR)
    ↓  - Pattern matching → decision trees / switches
    ↓  - Closures → closure conversion (env struct + function pointer)
    ↓  - Generics → monomorphization
    ↓  - GC allocation → runtime calls
    ↓
  MIR (Mid-level IR, SSA form)
    ↓  Optimize (P4-03, later)
    ↓
  MIR (optimized)
    ↓  Emit (MIR → LLVM IR)
    ↓
  LLVM IR → native executable
```

### What P0 Codegen Covers

The minimum to compile and run a simple Aura program:

- Integer/float/bool/string literals
- `let` bindings (immutable and `mut`)
- Function definitions and calls
- `if`/`else` → LLVM branch instructions
- `return` → LLVM ret
- Arithmetic and comparison operators (direct LLVM instructions before concept desugaring)
- Struct construction and field access
- `print` / `println` as FFI calls to C's `printf`
- Program entry point: `def main()` → LLVM `main` function

What to **defer** to later tiers:
- Pattern matching compilation (P1-04)
- Closures / closure conversion (P3-02)
- GC integration (P3-01)
- Async / concurrency (P3-03)
- Optimizations beyond what LLVM does by default (P4-03)

### Memory Strategy for P0

Before the GC is implemented (P3-01), P0 uses a simple strategy:
- Primitives (`Int`, `Float64`, `Bool`) are stack-allocated
- Strings use heap allocation via `malloc`/`free` (leak-tolerant for bootstrapping)
- Structs are stack-allocated if small, heap-allocated if large
- No collection types yet (List, Map, Set come with stdlib)

This is a temporary arrangement. P3-01 replaces it with the mark-sweep GC.

### LLVM Integration

Use the `inkwell` Rust crate (safe wrapper around LLVM C API) or `llvm-sys` (raw bindings).

**Recommendation: `inkwell`** — type-safe, well-maintained, covers the needed LLVM surface area.

## Implementation Steps

1. **Set up LLVM bindings** — add `inkwell` dependency, verify LLVM version compatibility
2. **Design HIR** — desugared high-level IR (see Data Structures)
3. **Implement AST → HIR lowering** — desugar pipeline, string interpolation, `with` syntax
4. **Design MIR** — SSA-form basic blocks with terminators
5. **Implement HIR → MIR lowering** — lower control flow to basic blocks
6. **Implement MIR → LLVM IR emission** — translate each MIR instruction to LLVM IR
7. **Implement integer/float/bool code generation** — LLVM constant values and arithmetic
8. **Implement string literal code generation** — global string constants
9. **Implement function code generation** — LLVM function definitions, parameters, return
10. **Implement `let` bindings** — LLVM `alloca` for locals
11. **Implement `let mut` and assignment** — store to alloca'd location
12. **Implement `if`/`else`** — LLVM conditional branch (`br i1`)
13. **Implement function calls** — LLVM `call` instruction
14. **Implement struct layout** — compute field offsets, `getelementptr` for access
15. **Implement string operations** — basic string creation, `printf` FFI for output
16. **Implement entry point** — `def main()` becomes LLVM `@main`
17. **Implement linking** — invoke system linker to produce executable
18. **Implement `aura build` CLI** — compile project to executable

## Data Structures

```rust
// HIR - desugared but still high-level
enum HirExpr {
    Literal(LiteralValue),
    Var(DefId),
    Call(DefId, Vec<HirExpr>),
    If(Box<HirExpr>, Box<HirExpr>, Box<HirExpr>),
    While(Box<HirExpr>, Box<HirExpr>),
    Let(DefId, Box<HirExpr>),
    Assign(DefId, Box<HirExpr>),
    StructLit(DefId, Vec<(Symbol, HirExpr)>),
    FieldAccess(Box<HirExpr>, Symbol),
    Return(Box<HirExpr>),
    Block(Vec<HirStmt>, Box<HirExpr>),
}

// MIR - SSA form, basic blocks
struct MirFunction {
    name: Symbol,
    params: Vec<(MirVar, MirType)>,
    return_type: MirType,
    blocks: Vec<BasicBlock>,
    entry: BlockId,
}

struct BasicBlock {
    id: BlockId,
    instructions: Vec<MirInst>,
    terminator: Terminator,
}

enum MirInst {
    // Values
    Const(MirVar, LiteralValue),
    Copy(MirVar, MirVar),
    // Arithmetic
    BinOp(MirVar, BinOp, MirVar, MirVar),
    UnOp(MirVar, UnOp, MirVar),
    // Functions
    Call(MirVar, DefId, Vec<MirVar>),
    // Memory
    StackAlloc(MirVar, MirType),            // alloca
    HeapAlloc(MirVar, MirType),             // malloc (temporary, replaced by GC in P3)
    Load(MirVar, MirVar, usize),            // load field at offset
    Store(MirVar, usize, MirVar),           // store to field at offset
    // Struct/variant
    ConstructStruct(MirVar, DefId, Vec<MirVar>),
    GetField(MirVar, MirVar, usize),
}

enum Terminator {
    Return(MirVar),
    Branch(MirVar, BlockId, BlockId),    // condition, then, else
    Jump(BlockId),
    Unreachable,
}

// LLVM emission context
struct LlvmEmitter<'ctx> {
    context: &'ctx inkwell::context::Context,
    module: inkwell::module::Module<'ctx>,
    builder: inkwell::builder::Builder<'ctx>,
    // Type mappings
    type_map: HashMap<DefId, inkwell::types::StructType<'ctx>>,
    // Function mappings
    fn_map: HashMap<DefId, inkwell::values::FunctionValue<'ctx>>,
}
```

## Testing Strategy

- **Arithmetic:** `def main() = { println(1 + 2) }` → compiles and prints `3`
- **Functions:** `def f(x: Int) -> Int = x + 1; def main() = println(f(5))` → prints `6`
- **Control flow:** `if true { println("yes") } else { println("no") }` → prints `yes`
- **Structs:** Create struct, access field, print it
- **LLVM IR validation:** Generated IR passes `llvm-as` and `opt -verify`
- **End-to-end:** Compile and run, compare stdout to expected output
- **Cross-platform:** Test on Linux (primary), macOS, Windows

## Open Questions

1. LLVM version: Which minimum LLVM version to target? (Recommendation: LLVM 17+, the latest stable)
2. Debug info from day one or defer? (Recommendation: basic debug info from day one — line numbers at minimum. Full DWARF debug info can come later.)
3. Build artifact location: `target/` directory like Cargo? (Recommendation: yes, `target/debug/` and `target/release/`)

## Estimated Complexity

**XL (Extra Large)** — IR design, three lowering passes (AST→HIR→MIR→LLVM), and LLVM integration are each substantial. This is the largest P0 component but is the critical path for the entire compiler.
