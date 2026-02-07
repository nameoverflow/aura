# P4-01: Standard Library (Core)

## Overview

The standard library provides the fundamental types, concepts, and collection operations that all Aura programs depend on. This covers the "prelude" (auto-imported types and functions) and core modules.

## Dependencies

- **P1-03: Concepts** — built-in concepts (Eq, Ord, Display, etc.)
- **P1-01: Algebraic Types** — Option, Result, collections
- **P2-03: Error Handling** — ConstraintError, From/Into
- **P3-03: Async Concurrency** — async runtime types

## Design Decisions

### Prelude (Auto-Imported)

These types and functions are available without explicit `use`:

**Types:**
- `Int`, `Int8`, `Int16`, `Int32`, `Int64`
- `UInt`, `UInt8`, `UInt16`, `UInt32`, `UInt64`
- `Float32`, `Float64`
- `Decimal`, `BigDecimal`
- `Bool`, `Char`, `String`
- `Option a` (`Some`, `None`)
- `Result a e` (`Ok`, `Err`)
- `List a`, `Map k v`, `Set a`
- `Ordering` (`Less`, `Equal`, `Greater`)
- `ConstraintError`

**Concepts:**
- `Eq`, `Ord`, `Display`, `Debug`, `Default`, `Clone`, `Copy`, `Hash`
- `Add`, `Sub`, `Mul`, `Div`, `Rem`
- `From a`, `Into a`

**Functions:**
- `print`, `println`, `panic`, `assert`, `assert_eq`

### Collection Operations

All collections share a consistent API:

```aura
// Transformation
map: List a * (a -> b [e]) -> List b [e]
filter: List a * (a -> Bool [e]) -> List a [e]
reduce: List a * b * (b * a -> b [e]) -> b [e]

// Query
find: List a * (a -> Bool) -> Option a
any: List a * (a -> Bool) -> Bool
all: List a * (a -> Bool) -> Bool
len: List a -> Int
is_empty: List a -> Bool

// Slicing
take: List a * Int -> List a
skip: List a * Int -> List a
first: List a -> Option a
last: List a -> Option a

// Ordering
sort_by: forall (Ord b). List a * (a -> b) -> List a
group_by: forall (Eq b, Hash b). List a * (a -> b) -> Map b (List a)

// Aggregation
sum: forall (Add a, Default a). List a -> a
min: forall (Ord a). List a -> Option a
max: forall (Ord a). List a -> Option a

// Construction
range: Int * Int -> List Int         // 0..n
range_inclusive: Int * Int -> List Int  // 0..=n
repeat: a * Int -> List a
zip: List a * List b -> List (a * b)
enumerate: List a -> List (Int * a)
```

### Eager by Default, Lazy on Request

```aura
// Eager (each step materializes)
list |> map(f) |> filter(g) |> take(10)

// Lazy (deferred until collect)
list |> lazy |> map(f) |> filter(g) |> take(10) |> collect
```

`lazy` returns a `LazyList a` adapter. `collect` materializes it back to `List a`.

### Map and Set

```aura
// Map operations
Map.from: List (k * v) -> Map k v
Map.get: Map k v * k -> Option v
Map.insert: Map k v * k * v -> Map k v      // returns new map (immutable)
Map.remove: Map k v * k -> Map k v
Map.contains_key: Map k v * k -> Bool
Map.keys: Map k v -> List k
Map.values: Map k v -> List v
Map.entries: Map k v -> List (k * v)

// Set operations
Set.from: List a -> Set a
Set.contains: Set a * a -> Bool
Set.insert: Set a * a -> Set a
Set.remove: Set a * a -> Set a
Set.union: Set a * Set a -> Set a
Set.intersection: Set a * Set a -> Set a
Set.difference: Set a * Set a -> Set a
```

### String Operations

```aura
String.len: String -> Int
String.contains: String * String -> Bool
String.starts_with: String * String -> Bool
String.ends_with: String * String -> Bool
String.split: String * String -> List String
String.join: List String * String -> String
String.trim: String -> String
String.to_upper: String -> String
String.to_lower: String -> String
String.replace: String * String * String -> String
String.matches: String * String -> Bool   // regex
```

## Implementation Steps

1. **Define prelude types** — primitives, Option, Result, Ordering
2. **Implement built-in concepts** — Eq, Ord, Display, Debug for all primitives
3. **Implement arithmetic concepts** — Add, Sub, Mul, Div, Rem for numeric types
4. **Implement List type** — backed by a vector (dynamic array)
5. **Implement Map type** — backed by a hash map (requires Hash + Eq on keys)
6. **Implement Set type** — backed by a hash set (requires Hash + Eq on values)
7. **Implement collection operations** — map, filter, reduce, find, etc.
8. **Implement lazy adapter** — LazyList with deferred evaluation
9. **Implement String operations** — all string methods
10. **Implement From/Into derivation** — auto-derive for wrapping variants
11. **Implement Display for all types** — human-readable string output
12. **Implement Debug for all types** — programmer-readable string output
13. **Implement Default for types with obvious defaults** — `0`, `""`, `[]`, `None`

## Data Structures

```rust
// Standard library is partially implemented in Rust (as the runtime library)
// and partially in Aura (once self-hosting becomes possible)

// Collections are GC-allocated objects with the following backing types:
// List a  → GC-managed dynamic array (Vec<Value> behind GcHeader)
// Map k v → GC-managed hash map (HashMap<Value, Value> behind GcHeader)
// Set a   → GC-managed hash set (HashSet<Value> behind GcHeader)
//
// All collection values are traced by the GC via TypeDesc gc_field_offsets.

// Lazy evaluation
struct LazyList {
    source: Box<dyn Iterator<Item = Value>>,
    // Or: a chain of transformation closures
    transforms: Vec<LazyTransform>,
}

enum LazyTransform {
    Map(ClosureValue),
    Filter(ClosureValue),
    Take(usize),
    Skip(usize),
}
```

## Testing Strategy

- **Prelude types:** All primitives, Option, Result work without imports
- **Collection operations:** map, filter, reduce on lists with various inputs
- **Pipeline chains:** `list |> map(f) |> filter(g) |> take(n)` produces correct results
- **Lazy evaluation:** Lazy pipeline only processes needed elements
- **Map operations:** Insert, get, remove, contains_key
- **Set operations:** Union, intersection, difference
- **String operations:** All string methods with edge cases (empty string, Unicode)
- **Concept implementations:** `1 + 2`, `"a" == "b"`, `[3,1,2] |> sort_by((x) -> x)` all work via concepts
- **Empty collections:** Operations on empty list, map, set don't crash

## Open Questions

1. Should the standard library be written in Aura or Rust? (Recommendation: Rust initially for the runtime library, rewrite key parts in Aura once self-hosting is viable)
2. Persistent vs mutable collections: The proposal shows immutable collections (insert returns new map). Should mutable alternatives exist? (Recommendation: immutable by default. Mutable variants can be added later for performance-critical code.)
3. Should `List` be a linked list, dynamic array, or persistent vector (like Clojure)? (Recommendation: dynamic array (Vec) for performance. Persistent data structures can be added as an alternative.)

## Estimated Complexity

**XL (Extra Large)** — The standard library is vast. Even the "core" subset (prelude types, basic collections, string operations) is a large body of code. This is ongoing work throughout the project.
