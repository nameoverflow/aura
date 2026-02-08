use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

const FEATURE_DIRS: &[&str] = &["async", "runtime_bridge", "parallel", "race", "timeout"];

fn workspace_root() -> PathBuf {
    // aurac crate is at crates/aurac, so go up twice.
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.pop(); // crates/
    path.pop(); // workspace root
    path
}

fn aurac_binary() -> PathBuf {
    workspace_root().join("target").join("debug").join("aurac")
}

fn p3_fixture_root() -> PathBuf {
    workspace_root().join("tests").join("sources")
}

fn p3_fixtures(prefix: &str) -> Vec<PathBuf> {
    let root = p3_fixture_root();
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

fn compile_emit_ir(source: &Path) -> (bool, String) {
    let output = Command::new(aurac_binary())
        .arg(source.to_str().expect("non-utf8 fixture path"))
        .arg("--emit-ir")
        .output()
        .expect("failed to run aurac");

    let stderr = String::from_utf8_lossy(&output.stderr).to_string();
    (output.status.success(), stderr)
}

#[test]
fn test_p3_ok_fixtures_compile_with_codegen() {
    let fixtures = p3_fixtures("ok_");
    assert!(
        !fixtures.is_empty(),
        "expected at least one ok_*.aura fixture under {} for feature dirs {:?}",
        p3_fixture_root().display(),
        FEATURE_DIRS
    );

    for fixture in fixtures {
        let (ok, stderr) = compile_emit_ir(&fixture);
        assert!(
            ok,
            "expected fixture '{}' to compile with aurac --emit-ir, got:\n{}",
            fixture.display(),
            stderr
        );
    }
}

#[test]
fn test_p3_err_fixtures_rejected_by_compiler() {
    let fixtures = p3_fixtures("err_");
    assert!(
        !fixtures.is_empty(),
        "expected at least one err_*.aura fixture under {} for feature dirs {:?}",
        p3_fixture_root().display(),
        FEATURE_DIRS
    );

    for fixture in fixtures {
        let (ok, _stderr) = compile_emit_ir(&fixture);
        assert!(
            !ok,
            "expected fixture '{}' to fail compilation",
            fixture.display()
        );
    }
}
