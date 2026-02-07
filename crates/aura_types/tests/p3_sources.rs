use std::fs;
use std::path::{Path, PathBuf};

use aura_lexer::Lexer;
use aura_parser::Parser;
use aura_resolve::Resolver;
use aura_types::TypeChecker;

fn workspace_root() -> PathBuf {
    // aura_types crate is at crates/aura_types, so go up twice.
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.pop(); // crates/
    path.pop(); // workspace root
    path
}

fn fixture_dir() -> PathBuf {
    workspace_root().join("tests").join("sources").join("p3")
}

fn typecheck_file(path: &Path) -> Result<(), String> {
    let source = fs::read_to_string(path)
        .map_err(|e| format!("failed to read fixture '{}': {}", path.display(), e))?;

    let mut lexer = Lexer::new(&source, 0);
    let tokens = lexer.tokenize();

    for tok in &tokens {
        if let aura_lexer::TokenKind::Error(msg) = &tok.kind {
            return Err(format!(
                "lexer error at {}..{}: {}",
                tok.span.start, tok.span.end, msg
            ));
        }
    }

    let mut parser = Parser::new(tokens);
    let module = parser.parse_module().map_err(|errors| {
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
    let mut fixtures = fs::read_dir(fixture_dir())
        .expect("failed to read P3 fixture directory")
        .filter_map(|entry| {
            let entry = entry.ok()?;
            let path = entry.path();
            let name = path.file_name()?.to_str()?;
            if path.extension().and_then(|e| e.to_str()) == Some("aura") && name.starts_with(prefix)
            {
                Some(path)
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
    fixtures.sort();
    fixtures
}

#[test]
fn test_p3_source_fixtures_ok() {
    let fixtures = fixtures_with_prefix("ok_");
    assert!(
        !fixtures.is_empty(),
        "expected at least one ok_*.aura fixture in {}",
        fixture_dir().display()
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
fn test_p3_source_fixtures_err() {
    let fixtures = fixtures_with_prefix("err_");
    assert!(
        !fixtures.is_empty(),
        "expected at least one err_*.aura fixture in {}",
        fixture_dir().display()
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
