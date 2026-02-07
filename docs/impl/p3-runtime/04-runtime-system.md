# P3-04: Runtime System

## Overview

The runtime system is the supporting library linked into every compiled Aura program. It provides the GC, async executor, built-in functions, panic handling, and program entry point. This is the "runtime" that LLVM-compiled code calls into.

## Dependencies

- **P0-05: Basic Codegen** — LLVM backend emits calls to runtime functions
- **P3-01: GC** — garbage collector is the largest runtime component
- **P3-02: Closures** — closure representation at runtime
- **P3-03: Async Concurrency** — async executor lives in the runtime

## Design Decisions

### Runtime as a Linked Library

The runtime is implemented in Rust (or C) and compiled to a static library (`libaura_rt.a`). Every Aura executable links against it. The runtime provides:

| Component | Functions |
|-----------|-----------|
| GC | `aura_gc_alloc`, `aura_gc_collect`, shadow stack operations |
| I/O | `aura_print`, `aura_println`, `aura_read_line` |
| Strings | `aura_string_new`, `aura_string_concat`, `aura_string_interp` |
| Panic | `aura_panic`, `aura_assert`, `aura_assert_eq` |
| Entry | `main` wrapper that initializes runtime, calls Aura's `main()` |
| Async | `aura_runtime_block_on`, `aura_spawn`, `aura_timeout` |

### Program Entry Point

The LLVM `@main` function is a thin wrapper that:
1. Initializes the GC (allocate heap, set up shadow stack)
2. Initializes the async runtime (if the program uses `async`)
3. Calls the Aura `module main`'s `def main()` function
4. Handles the return value (`Result () AppError` → exit code)
5. Shuts down the runtime (final GC, flush I/O)

```c
// Generated entry point (pseudo-C)
int main(int argc, char** argv) {
    aura_rt_init();
    AuraResult result = aura_main();  // calls the user's def main()
    if (result.is_err) {
        aura_print_error(result.error);
        aura_rt_shutdown();
        return 1;
    }
    aura_rt_shutdown();
    return 0;
}
```

### Built-in Functions

Before the standard library is fully implemented, the runtime provides built-in functions:

| Function | Signature | Implementation |
|----------|-----------|----------------|
| `print(value)` | `forall (Display a). a -> () [Log]` | FFI to `printf` / `write` |
| `println(value)` | `forall (Display a). a -> () [Log]` | FFI to `printf` with newline |
| `panic(msg)` | `String -> !` | Print message + stack trace, abort |
| `assert(cond)` | `Bool -> ()` | Panic if false |
| `assert_eq(a, b)` | `forall (Eq a, Debug a). a * a -> ()` | Panic if not equal |

### Error Reporting

Runtime errors (panics, assertion failures, contract violations in debug mode) produce:
- Error message
- Source location (file, line, column) from LLVM debug info
- Stack trace (via LLVM's `llvm.returnaddress` / platform unwinding)

```
panic at src/main.aura:15:3
  Division by zero in expression: a / b.value

Stack trace:
  process_order (src/order_service.aura:42:5)
  main (src/main.aura:15:3)
```

### CLI Interface

```bash
aura build                     # Compile project to target/debug/
aura build --release           # Compile with optimizations to target/release/
aura run                       # Build and run (shorthand for build + execute)
aura run src/main.aura         # Build and run a single file
aura check                     # Type-check without compiling
aura check src/main.aura       # Type-check a single file
```

### Debug vs Release Builds

| Feature | Debug (`-O0`) | Release (`-O2`) |
|---------|--------------|-----------------|
| Optimizations | None | Full (MIR + LLVM) |
| Debug info | Full DWARF | Minimal |
| Contract checks | Enabled | Disabled |
| GC stress mode | Optional | Disabled |
| Assertions | Enabled | Enabled |

## Implementation Steps

1. **Create `libaura_rt` Rust crate** — the runtime library
2. **Implement GC initialization/shutdown** — `aura_rt_init()`, `aura_rt_shutdown()`
3. **Implement `main` wrapper** — initialize runtime, call user's `main()`, handle result
4. **Implement `aura_print`/`aura_println`** — FFI to system I/O
5. **Implement `aura_panic`** — print message, stack trace, abort
6. **Implement `aura_assert`/`aura_assert_eq`** — condition checking
7. **Implement string runtime** — allocation, concatenation, interpolation helpers
8. **Implement stack trace capture** — platform-specific unwinding
9. **Implement CLI** — `aura build`, `aura run`, `aura check`
10. **Implement multi-file compilation** — resolve modules, compile each, link together
11. **Implement debug/release profiles** — optimization level, debug info, contract checking
12. **Implement `Runtime.block_on`** — bridge async to sync at program entry

## Data Structures

```rust
// Runtime initialization (called before user code)
#[no_mangle]
pub extern "C" fn aura_rt_init() {
    // Initialize GC
    // Initialize thread pool for async (if needed)
    // Set up signal handlers
}

#[no_mangle]
pub extern "C" fn aura_rt_shutdown() {
    // Final GC collection
    // Flush I/O buffers
    // Join thread pool
}

// Panic handler
#[no_mangle]
pub extern "C" fn aura_panic(message: *const u8, len: usize, file: *const u8, line: u32) -> ! {
    // Print message and location
    // Capture and print stack trace
    // Abort process
    std::process::abort();
}

// String operations
#[no_mangle]
pub extern "C" fn aura_string_concat(a: *const AuraString, b: *const AuraString) -> *mut AuraString {
    // GC-allocate new string, copy contents
}

// CLI
enum Command {
    Build { release: bool },
    Run { path: Option<PathBuf> },
    Check { path: Option<PathBuf> },
    Fmt { path: PathBuf },       // P4-04
    Lint { path: PathBuf },      // P4-04
}
```

## Testing Strategy

- **End-to-end programs:** Compile Aura programs, run executables, verify output
- **Panic handling:** Panics produce correct message and stack trace
- **Assertions:** `assert(false)` panics, `assert(true)` is a no-op
- **Multi-file projects:** Imports across files compile and link correctly
- **Debug vs release:** Debug build has contract checks, release doesn't
- **CLI:** `aura build`, `aura run`, `aura check` work as expected
- **Exit codes:** `def main() -> Result () e` maps `Ok` to 0, `Err` to 1
- **Performance baseline:** Fibonacci, sorting benchmarks — compare to C/Rust

## Open Questions

1. Cross-compilation: Should `aura build --target wasm32` be supported from the start? (Recommendation: defer — native targets first, WASM later)
2. REPL: Should a REPL be supported despite no interpreter? (Recommendation: defer — a JIT-based REPL can be added later using LLVM's ORC JIT. Not critical for initial release.)
3. Build caching: Should compiled modules be cached for incremental builds? (Recommendation: yes, essential for development speed. Use content hashing to detect changes.)

## Estimated Complexity

**L (Large)** — The runtime library, CLI, and linking pipeline are each substantial. However, much of this is well-understood systems programming (FFI, process management, file I/O).
