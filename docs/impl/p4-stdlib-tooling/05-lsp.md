# P4-05: Language Server Protocol (LSP)

## Overview

The Aura language server provides IDE features: code completion, go-to-definition, hover information, diagnostics, and more. It communicates via the Language Server Protocol, enabling support in VS Code, Neovim, Zed, and any LSP-compatible editor.

## Dependencies

- **P0-01 through P0-04** — parsing and type checking for diagnostics
- **P0-03: Name Resolution** — go-to-definition, find references
- **P1-03: Concepts** — method completion
- **P2-01: Effect System** — effect information in hover
- **P4-04: Formatter** — format-on-save

## Current Implementation Status (as of February 7, 2026)

- **Not Started:** No Aura LSP server implementation exists yet.
- **Available building blocks:** Parser, resolver, and typechecker APIs are usable and test-covered, which provides the core analysis substrate for future LSP work.
- **Deferred:** Incremental analysis engine, editor protocol handlers, semantic tokens, and code-action pipelines.

## Design Decisions

### Architecture

**Incremental analysis.** The LSP server maintains an in-memory representation of the project. When a file changes, only the affected module and its dependents are re-analyzed.

Key components:
1. **File watcher** — detect changes to source files
2. **Incremental parser** — re-parse only changed regions (or full file if changes are large)
3. **Incremental type checker** — re-check only affected modules
4. **Query system** — demand-driven computation (salsa-style)

### LSP Features (Priority Order)

| Feature | Priority | Description |
|---------|----------|-------------|
| Diagnostics | P0 | Real-time error/warning reporting |
| Go to definition | P0 | Jump to definition of any identifier |
| Hover | P0 | Show type, effects, and docs on hover |
| Completion | P1 | Suggest completions for identifiers, fields, methods |
| Find references | P1 | Find all uses of a definition |
| Rename | P2 | Rename a symbol across the project |
| Format | P2 | Format document/selection via `aura fmt` |
| Code actions | P2 | Quick fixes (add import, fix naming) |
| Signature help | P2 | Show parameter info during function calls |
| Semantic tokens | P3 | Semantic syntax highlighting |
| Inlay hints | P3 | Show inferred types inline |

### Hover Information

Hovering over a function should show:

```
def fetch_user(id: UUID) -> Result User DbError [Db.Read]

/// Fetches a user by ID from the database.
/// Returns NotFound if the user doesn't exist.
```

Hovering over a variable should show its inferred type:

```
let user: User
```

### Completion

Context-aware completion:
- After `.`: field names and method names for the receiver type
- After `|>`: functions whose first parameter matches the pipeline value type
- After `::`: module members
- In type position: type names
- In pattern position: constructors and variants
- In effect position: known effect names

### Inlay Hints

Show inferred types where they're not explicit:

```aura
let x/*: Int*/ = 42
let users/*: List User*/ = fetch_all()?
```

Show effect information:

```aura
let result/*[Db.Read]*/ = fetch_user(id)
```

## Implementation Steps

1. **Set up LSP server scaffolding** — JSON-RPC over stdio, capability negotiation
2. **Implement document sync** — track open documents, handle incremental changes
3. **Implement diagnostics** — parse + type check on change, publish diagnostics
4. **Implement go-to-definition** — map cursor position to DefId, map DefId to source location
5. **Implement hover** — at cursor position, show type signature and doc comment
6. **Implement completion** — context-aware suggestion engine
7. **Implement find references** — reverse lookup: DefId → all use sites
8. **Implement rename** — rename a DefId, update all reference sites
9. **Implement formatting** — delegate to `aura fmt`
10. **Implement signature help** — during function call, show parameter info
11. **Implement semantic tokens** — classify tokens by semantic role (type, function, effect, etc.)
12. **Implement inlay hints** — show inferred types and effects
13. **Implement incremental analysis** — query-based system for efficient re-analysis
14. **VS Code extension** — package LSP client as VS Code extension

## Data Structures

```rust
// LSP server state
struct AuraLanguageServer {
    // Project state
    files: HashMap<PathBuf, SourceFile>,
    // Analysis results (cached, incremental)
    analysis: AnalysisCache,
    // Client capabilities
    client_caps: ClientCapabilities,
}

struct SourceFile {
    content: String,
    version: i32,
    // Cached analysis for this file
    tokens: Option<Vec<Token>>,
    ast: Option<Module>,
    types: Option<TypeInfo>,
}

struct AnalysisCache {
    // Demand-driven query results
    parsed: HashMap<PathBuf, Module>,
    resolved: HashMap<PathBuf, ResolvedNames>,
    typed: HashMap<PathBuf, TypeInfo>,
    // Reverse maps for "find references"
    def_to_uses: HashMap<DefId, Vec<Span>>,
}
```

## Testing Strategy

- **Protocol tests:** LSP request/response round-trips
- **Diagnostics:** Type error appears as diagnostic at correct location
- **Go-to-definition:** Jumping to function/type/variable definitions
- **Hover:** Correct type information displayed
- **Completion:** Relevant suggestions in various contexts
- **Incremental:** Changing one file only re-analyzes affected modules
- **Performance:** Response time < 100ms for common operations

## Open Questions

1. Query framework: Use Salsa (Rust query system) or build custom? (Recommendation: Salsa — proven in rust-analyzer)
2. VS Code only initially, or also Neovim/Zed? (Recommendation: the LSP server works with all editors. Package VS Code extension first, then others.)
3. Workspace vs single-file mode? (Recommendation: both — single-file for quick scripts, workspace for projects)

## Estimated Complexity

**XL (Extra Large)** — A good language server is one of the most complex tools in a language ecosystem. Incremental analysis, the query system, and completion alone are each substantial. However, it can be built incrementally — start with diagnostics and go-to-definition, add features over time.
