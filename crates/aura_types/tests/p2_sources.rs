use std::fs;
use std::path::{Path, PathBuf};

use aura_resolve::Resolver;
use aura_types::TypeChecker;

const FEATURE_DIRS: &[&str] = &[
    "effects",
    "effect_polymorphism",
    "contracts",
    "refined_types",
    "error_handling",
];

fn workspace_root() -> PathBuf {
    // aura_types crate is at crates/aura_types, so go up twice.
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.pop(); // crates/
    path.pop(); // workspace root
    path
}

fn fixture_root() -> PathBuf {
    workspace_root().join("tests").join("sources")
}

fn typecheck_file(path: &Path) -> Result<(), String> {
    let source = fs::read_to_string(path)
        .map_err(|e| format!("failed to read fixture '{}': {}", path.display(), e))?;

    let module = aura_parser::parse(&source, 0).map_err(|errors| {
        errors
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join("\n")
    })?;

    let resolver = Resolver::new();
    let resolved = resolver.resolve(module).map_err(|errors| {
        errors
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join("\n")
    })?;

    let checker = TypeChecker::new();
    checker.check(&resolved).map(|_| ()).map_err(|errors| {
        errors
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join("\n")
    })
}

fn fixtures_with_prefix(prefix: &str) -> Vec<PathBuf> {
    let root = fixture_root();
    let mut fixtures = Vec::new();

    for feature_dir in FEATURE_DIRS {
        let dir = root.join(feature_dir);
        let entries = fs::read_dir(&dir)
            .unwrap_or_else(|_| panic!("failed to read fixture directory '{}'", dir.display()));
        for entry in entries {
            let path = entry
                .expect("failed to read fixture directory entry")
                .path();
            let name = match path.file_name().and_then(|n| n.to_str()) {
                Some(name) => name,
                None => continue,
            };
            if path.extension().and_then(|e| e.to_str()) == Some("aura") && name.starts_with(prefix)
            {
                fixtures.push(path);
            }
        }
    }

    fixtures.sort();
    fixtures
}

#[test]
fn test_p2_source_fixtures_ok() {
    let fixtures = fixtures_with_prefix("ok_");
    assert!(
        !fixtures.is_empty(),
        "expected at least one ok_*.aura fixture under {} for feature dirs {:?}",
        fixture_root().display(),
        FEATURE_DIRS
    );

    for fixture in fixtures {
        let result = typecheck_file(&fixture);
        assert!(
            result.is_ok(),
            "expected fixture '{}' to typecheck, got:\n{}",
            fixture.display(),
            result.unwrap_err()
        );
    }
}

#[test]
fn test_p2_source_fixtures_err() {
    let fixtures = fixtures_with_prefix("err_");
    assert!(
        !fixtures.is_empty(),
        "expected at least one err_*.aura fixture under {} for feature dirs {:?}",
        fixture_root().display(),
        FEATURE_DIRS
    );

    for fixture in fixtures {
        let result = typecheck_file(&fixture);
        assert!(
            result.is_err(),
            "expected fixture '{}' to fail typechecking",
            fixture.display()
        );
    }
}
