# P4-06: Package Manager

## Overview

Aura's package manager handles dependency resolution, version locking, and vendoring. It follows the principle of **locked and vendored by default** — no version ranges, deterministic builds.

## Dependencies

- **P3-04: Runtime System** — `aura run` uses the package manager to find dependencies
- **P0-02: Parser** — parse `aura.toml` configuration

## Current Implementation Status (as of February 7, 2026)

- **Not Started:** No package/dependency manager implementation exists yet.
- **Current CLI scope:** `aura` command supports single-file/project-local compile-check-run flows, but no dependency resolution or lockfile/vendoring.
- **Deferred:** `aura.toml` dependency graph resolution, lock management, download/vendoring pipeline, and registry/git integration.

## Design Decisions

### Project Configuration: `aura.toml`

```toml
[project]
name = "my_app"
version = "0.1.0"
authors = ["Team Name"]
edition = "2025"

[dependencies]
http = "1.2.0"
json = "0.9.0"
database = "2.1.0"

[dev-dependencies]
test-utils = "0.3.0"
```

### No Version Ranges

Dependencies specify **exact versions**, not ranges. This ensures deterministic builds:

```toml
# Yes
http = "1.2.0"

# No (not supported)
http = "^1.2.0"
http = ">=1.0.0"
```

`aura update` checks for new versions and updates the lock file interactively.

### Lock File: `aura.lock`

Records exact versions of all transitive dependencies. Generated from `aura.toml`, committed to version control.

### Vendoring

Dependencies are downloaded to `vendor/` by default. The entire dependency tree is available locally — no network access needed after initial download.

### CLI Commands

| Command | Description |
|---------|-------------|
| `aura add <pkg>` | Add a dependency (latest version) |
| `aura add <pkg> <version>` | Add a specific version |
| `aura remove <pkg>` | Remove a dependency |
| `aura update` | Check for updates, update lock file |
| `aura update <pkg>` | Update a specific package |
| `aura install` | Download and vendor all dependencies |
| `aura publish` | Publish a package to the registry |

### Package Registry

A central registry (like crates.io, npm) for discovering and publishing packages. Implementation deferred — initially, dependencies are specified as git URLs:

```toml
[dependencies]
http = { git = "https://github.com/aura-lang/http", tag = "v1.2.0" }
```

## Implementation Steps

1. **Define `aura.toml` format** — project metadata, dependencies
2. **Implement TOML parser** (use existing Rust crate)
3. **Implement dependency resolution** — resolve exact versions, detect conflicts
4. **Implement lock file generation** — `aura.lock` with full dependency tree
5. **Implement vendoring** — download dependencies to `vendor/`
6. **Implement `aura add` CLI** — add dependency, update toml and lock
7. **Implement `aura remove` CLI** — remove dependency
8. **Implement `aura update` CLI** — check for new versions
9. **Implement `aura install` CLI** — download all vendored dependencies
10. **Integrate with compiler** — resolve `use` imports against vendored packages
11. **Future: Package registry** — central repository for publishing/discovering packages

## Data Structures

```rust
struct ProjectConfig {
    name: String,
    version: String,
    authors: Vec<String>,
    dependencies: HashMap<String, DependencySpec>,
    dev_dependencies: HashMap<String, DependencySpec>,
}

enum DependencySpec {
    Version(String),           // "1.2.0"
    Git { url: String, tag: Option<String>, branch: Option<String> },
    Path(PathBuf),             // local path dependency
}

struct LockFile {
    packages: Vec<LockedPackage>,
}

struct LockedPackage {
    name: String,
    version: String,
    source: String,            // registry URL, git URL, or path
    checksum: String,          // SHA256 of package contents
    dependencies: Vec<String>, // names of dependencies
}
```

## Testing Strategy

- **Add/remove:** `aura add http` adds entry to `aura.toml` and `aura.lock`
- **Deterministic:** Same `aura.toml` always produces same `aura.lock`
- **Vendoring:** Dependencies appear in `vendor/` after `aura install`
- **Conflict detection:** Two dependencies requiring different versions of the same package → error
- **Git dependencies:** Git-sourced dependencies are cloned and locked
- **Path dependencies:** Local path dependencies resolve correctly

## Open Questions

1. Central registry: When to build it? (Recommendation: defer — git dependencies are sufficient initially)
2. Private registries: Should organizations be able to host private package registries? (Recommendation: yes, eventually)
3. Workspace/monorepo support: Multiple packages in one repository? (Recommendation: yes, like Cargo workspaces — important for large projects)

## Estimated Complexity

**L (Large)** — Dependency resolution, vendoring, and CLI are each non-trivial. A central registry adds another large component. However, git-only dependencies simplify the initial implementation significantly.
