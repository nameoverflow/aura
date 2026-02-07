# P4-07: AI Integration Tooling

## Overview

Aura's core differentiator is its AI-native design. This document covers the tooling that makes Aura code AI-consumable: manifest generation, semantic indexing, and context extraction. These tools enable AI systems to understand, analyze, and generate Aura code with high accuracy.

## Dependencies

- **All P0-P2 features** — complete type and effect information
- **P4-01: Stdlib** — standard types and concepts in manifest
- **P4-06: Package Manager** — dependency information

## Current Implementation Status (as of February 7, 2026)

- **Not Started:** AI integration tooling in this document has not been implemented yet.
- **Available foundation:** Compiler front-end metadata (typed/resolved structures and effect information) exists and can support future manifest/index extraction.
- **Deferred:** Manifest generation, semantic index build/retrieval, context extraction CLI, and policy/security filters for AI workflows.

## Design Decisions

### Manifest Generator: `aura.manifest.json`

The compiler generates a machine-readable manifest containing all public API information:

```json
{
  "modules": {
    "user_service": {
      "functions": {
        "create_user": {
          "signature": "UserInput -> Result User CreateError",
          "effects": ["Db.Write", "Log", "Time", "Random"],
          "doc": "Creates a new user from the provided input data.",
          "params": [
            { "name": "data", "type": "UserInput" }
          ],
          "return_type": "Result User CreateError",
          "contracts": {
            "requires": [],
            "ensures": ["result.ok?.status == Active"]
          }
        }
      },
      "types": {
        "User": {
          "kind": "struct",
          "fields": [
            { "name": "id", "type": "UUID" },
            { "name": "name", "type": "String" },
            { "name": "email", "type": "Email" },
            { "name": "created", "type": "Timestamp" },
            { "name": "status", "type": "UserStatus" }
          ]
        },
        "UserStatus": {
          "kind": "sum",
          "variants": ["Active", "Suspended", "Deleted"]
        }
      },
      "concepts": {},
      "dependencies": ["std::collections", "db", "log"]
    }
  },
  "dependency_graph": { ... }
}
```

The manifest is generated automatically on every build and stored at the project root.

### Semantic Indexer: `aura index`

Generates embeddings and semantic search index for AI-powered code retrieval:

```bash
aura index ./src --output embeddings.idx
```

The index contains:
- Function-level embeddings (function signature + doc + body summary)
- Type-level embeddings (type definition + concept implementations)
- Module-level embeddings (module doc + public API summary)
- Cross-reference graph (who calls whom, who implements what)

### Context Extractor: `aura context`

Extracts the minimal context needed to understand a specific function:

```bash
aura context src/user_service.aura::create_user
```

Output:
```aura
// Types used
type UserInput { name: String, email: String }
type User { id: UUID, name: String, email: Email, created: Timestamp, status: UserStatus }
type UserStatus = Active | Suspended | Deleted
type CreateError = Validation(String) | Duplicate(String) | Db(DbError)

// Effects used
// Db.Write: Write to database
// Log: Logging
// Time: Access current time
// Random: Randomness

// The function
pub def create_user(data: UserInput) -> Result User CreateError [Db.Write, Log, Time, Random] = {
  ...
}
```

Key: Extract **only** what's needed to understand this function — types, effects, direct dependencies. Not the entire module.

### Context Budget

The context extractor respects a **token budget**:

```bash
aura context src/user_service.aura::create_user --budget 2000
```

If the full context exceeds the budget, it progressively drops:
1. Implementation bodies of helper functions (keep signatures)
2. Fields of tangentially-related types
3. Concept implementations (keep concept names)

## Implementation Steps

### Manifest Generator

1. **Collect all public definitions** — walk all modules, filter by visibility
2. **Serialize type information** — types, fields, variants, generics
3. **Serialize function information** — signatures, effects, contracts, doc comments
4. **Serialize concept information** — methods, superclasses, instances
5. **Build dependency graph** — module-to-module and function-to-function
6. **Output JSON** — `aura.manifest.json`
7. **Incremental generation** — only regenerate for changed modules

### Semantic Indexer

1. **Extract text representations** — for each function/type/module, generate a text summary
2. **Generate embeddings** — use an embedding model (local or API) to vectorize summaries
3. **Build search index** — store embeddings in an efficient ANN index (e.g., HNSW)
4. **Implement search CLI** — `aura search "fetch user by email"` returns ranked results
5. **Build cross-reference graph** — who-calls-whom, who-implements-what

### Context Extractor

1. **Identify the target** — parse the specifier (`module::function`)
2. **Trace dependencies** — from the target function, find all referenced types, functions, effects
3. **Compute transitive closure** — include types referenced by types, etc. (up to configurable depth)
4. **Apply token budget** — progressively drop detail to fit within budget
5. **Format output** — produce a self-contained Aura code snippet with all necessary context
6. **Output** — stdout or JSON format

## Data Structures

```rust
// Manifest
struct Manifest {
    modules: HashMap<String, ModuleManifest>,
    dependency_graph: DependencyGraph,
    version: String,
    generated_at: String,
}

struct ModuleManifest {
    functions: HashMap<String, FunctionManifest>,
    types: HashMap<String, TypeManifest>,
    concepts: HashMap<String, ConceptManifest>,
    dependencies: Vec<String>,
}

struct FunctionManifest {
    signature: String,
    effects: Vec<String>,
    doc: Option<String>,
    params: Vec<ParamManifest>,
    return_type: String,
    contracts: ContractManifest,
}

// Context extractor
struct ContextRequest {
    target: String,          // module::function
    budget: Option<usize>,   // max tokens
    depth: usize,            // dependency traversal depth (default: 2)
    format: OutputFormat,    // aura source or json
}

struct ContextResult {
    types: Vec<TypeDef>,
    functions: Vec<FnSignature>,
    effects: Vec<EffectDesc>,
    target: FnDef,
    token_count: usize,
}

// Semantic index
struct SemanticIndex {
    embeddings: Vec<(DefId, Vec<f32>)>,
    metadata: HashMap<DefId, IndexEntry>,
    // ANN index for fast similarity search
    ann: HnswIndex,
}

struct IndexEntry {
    kind: DefKind,
    name: String,
    module: String,
    summary: String,
    file: PathBuf,
    span: Span,
}
```

## Testing Strategy

### Manifest
- **Completeness:** All public definitions appear in manifest
- **Accuracy:** Types, signatures, effects match source
- **Incremental:** Changed module updates only its section
- **JSON validity:** Output is valid JSON, parseable by standard tools

### Context Extractor
- **Minimal context:** Only necessary types/functions included
- **Self-contained:** Extracted context type-checks in isolation
- **Budget compliance:** Output respects token budget
- **Depth control:** Deeper dependencies included at appropriate depth

### Semantic Index
- **Relevance:** "fetch user by email" finds `fetch_user_by_email` function
- **Ranking:** More relevant results ranked higher
- **Performance:** Search completes in < 100ms for typical codebases

## Open Questions

1. Embedding model: Which model to use for semantic indexing? Local (lightweight) vs API (more accurate)? (Recommendation: support both — local by default, API opt-in)
2. Should the manifest include private definitions for internal tooling? (Recommendation: optional flag `--include-private`)
3. Token counting: Which tokenizer to use for budget? (Recommendation: cl100k_base from tiktoken, as it's the most common for current AI models)
4. IDE integration: Should context extraction be automatic in the LSP hover? (Recommendation: yes — show context on hover for imported functions)

## Estimated Complexity

**L (Large)** — The manifest generator is M (serialization of existing data). The context extractor is M (graph traversal with budget). The semantic indexer is L (embedding generation, ANN indexing). Together they're L-XL.
