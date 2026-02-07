# P4-04: Formatter & Linter

## Overview

Aura mandates a single canonical format (like `gofmt`) and provides a built-in linter for code quality. Both are essential for the "predictability" design principle — code should look the same everywhere.

## Dependencies

- **P0-02: Parser & AST** — the formatter operates on the AST (or CST)
- **P0-03: Name Resolution** — the linter needs resolved names
- **P2-01: Effect System** — linter checks effect correctness

## Current Implementation Status (as of February 7, 2026)

- **Not Started:** `aura fmt` and `aura lint` are not implemented yet.
- **Current CLI scope:** The `aura` binary currently provides `build`, `run`, and `check` only.
- **Deferred:** Canonical formatting rules engine, lint rule framework, diagnostics autofix/code-actions integration.

## Design Decisions

### Formatter: `aura fmt`

**Zero configuration.** There are no options, no style files, no overrides. One canonical format.

Key formatting rules:
- **Indentation:** 2 spaces (no tabs)
- **Line width:** 100 characters, soft limit
- **Trailing commas:** Always in multi-line constructs
- **Braces:** Same line for `if`, `match`, function bodies
- **Blank lines:** One between top-level definitions, none within functions
- **Import ordering:** Std imports first, then external, then internal, alphabetical within groups
- **Pipeline formatting:** Each `|>` stage on its own line if the pipeline exceeds the line width

Implementation approach: **Pretty-printing from AST.** Parse the source, produce AST, pretty-print back. This means all formatting choices are made by the printer, not preserved from source.

For comment preservation, the AST (or a CST) must retain comments attached to the nearest AST node.

### Linter: `aura lint`

Built-in lint rules (not pluggable — consistent with "one canonical way"):

| Rule | Severity | Description |
|------|----------|-------------|
| `unused-import` | Warning | Imported name not used |
| `unused-variable` | Warning | Local binding not used |
| `unused-effect` | Warning | Declared effect not needed |
| `module-depth` | Warning | Module nesting > 3 levels |
| `shadowing` | Warning | Variable shadows outer binding |
| `unreachable-pattern` | Warning | Pattern arm can never match |
| `missing-pub` | Info | Public function without doc comment |
| `cycle-risk` | Warning | Type definition may create reference cycles |
| `large-function` | Warning | Function exceeds complexity threshold |
| `naming-convention` | Warning | Type not PascalCase, value not snake_case |
| `empty-match` | Error | Match expression with no arms |
| `partial-match` | Error | Non-exhaustive pattern match |

### Lint Suppression

Individual lints can be suppressed per-item:

```aura
@allow(unused-variable)
def experimental() = { ... }
```

No global suppression — each suppression must be local and documented.

## Implementation Steps

### Formatter

1. **Implement comment attachment** — associate comments with nearest AST nodes
2. **Implement pretty-printer** — convert AST to formatted string
3. **Implement line-breaking algorithm** — Wadler-Lindig or similar for optimal line breaks
4. **Implement import sorting** — group and alphabetize imports
5. **Implement pipeline formatting** — multi-line pipeline layout
6. **Implement `aura fmt` CLI** — format files in-place or check mode
7. **Implement format-on-save** — integration with editors

### Linter

1. **Define lint rule interface** — each rule is a function from AST/context to diagnostics
2. **Implement unused import detection** — compare imports to used names
3. **Implement unused variable detection** — walk scopes, find unread bindings
4. **Implement naming convention checks** — PascalCase for types, snake_case for values
5. **Implement module depth check** — count nesting levels
6. **Implement shadowing detection** — compare binding names across scopes
7. **Implement complexity metric** — cyclomatic complexity or similar
8. **Implement lint suppression** — `@allow(rule)` annotation handling
9. **Implement `aura lint` CLI** — run all lint rules, output diagnostics

## Data Structures

```rust
// Formatter
struct Formatter {
    line_width: usize,    // 100, not configurable
    indent_width: usize,  // 2, not configurable
}

// Pretty-printing document (Wadler-Lindig)
enum Doc {
    Text(String),
    Line,                  // line break or space
    Indent(usize, Box<Doc>),
    Group(Box<Doc>),       // try on one line, break if too wide
    Concat(Vec<Doc>),
}

// Linter
struct LintContext {
    resolved_names: ResolvedNames,
    type_info: TypeInfo,
    effect_info: EffectInfo,
}

struct LintDiagnostic {
    rule: LintRule,
    severity: Severity,
    message: String,
    span: Span,
    suggestion: Option<String>,  // "consider removing this import"
}

enum Severity { Error, Warning, Info }

enum LintRule {
    UnusedImport,
    UnusedVariable,
    UnusedEffect,
    ModuleDepth,
    Shadowing,
    UnreachablePattern,
    MissingPub,
    CycleRisk,
    LargeFunction,
    NamingConvention,
    EmptyMatch,
    PartialMatch,
}
```

## Testing Strategy

### Formatter
- **Idempotence:** Formatting already-formatted code produces the same output
- **Round-trip:** Format -> parse -> format produces the same output
- **Comment preservation:** Comments appear in correct positions after formatting
- **Snapshot tests:** Known input files formatted and compared to expected output
- **Edge cases:** Very long lines, deeply nested expressions, empty files

### Linter
- **Each lint rule:** Dedicated test with positive (triggers) and negative (doesn't trigger) cases
- **Suppression:** `@allow` prevents the lint from firing
- **Error messages:** Each lint produces a helpful message with suggestion

## Open Questions

1. Should the formatter preserve manual line breaks in some cases? (Recommendation: no — true zero-config means the formatter decides all layout)
2. Should the linter be extensible via plugins? (Recommendation: no — one canonical set of rules, consistent with "one way to do it")
3. Auto-fix: Should the linter offer automatic fixes? (Recommendation: yes for simple cases like unused imports, naming conventions)

## Estimated Complexity

**M (Medium) each** — The formatter is M (Wadler-Lindig is well-documented). The linter is M (each rule is independent and straightforward). Together they're L.
