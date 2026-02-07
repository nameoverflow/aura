use std::path::PathBuf;
use std::process::Command;

fn workspace_root() -> PathBuf {
    // aurac crate is at crates/aurac, so go up twice
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.pop(); // crates/
    path.pop(); // workspace root
    path
}

fn aurac_binary() -> PathBuf {
    workspace_root().join("target").join("debug").join("aurac")
}

fn compile_and_run(fixture: &str) -> (bool, String, i32) {
    let root = workspace_root();
    let source = root.join("tests").join("fixtures").join(fixture);
    let stem = fixture.strip_suffix(".aura").unwrap();
    let output = std::env::temp_dir().join(format!("aura_test_{}", stem));

    // Compile
    let compile_result = Command::new(aurac_binary())
        .arg(source.to_str().unwrap())
        .arg("-o")
        .arg(output.to_str().unwrap())
        .output()
        .expect("failed to run aurac");

    if !compile_result.status.success() {
        let stderr = String::from_utf8_lossy(&compile_result.stderr).to_string();
        return (false, format!("compile failed: {}", stderr), 1);
    }

    // Run
    let run_result = Command::new(&output)
        .output()
        .expect("failed to run compiled program");

    // Clean up
    let _ = std::fs::remove_file(&output);

    let stdout = String::from_utf8_lossy(&run_result.stdout).to_string();
    let code = run_result.status.code().unwrap_or(-1);
    (run_result.status.success(), stdout, code)
}

#[test]
fn test_hello() {
    let (ok, output, _) = compile_and_run("hello.aura");
    assert!(ok, "program exited with error");
    assert_eq!(output.trim(), "Hello, Aura!");
}

#[test]
fn test_arithmetic() {
    let (ok, output, _) = compile_and_run("arithmetic.aura");
    assert!(ok, "program exited with error");
    assert_eq!(output.trim(), "14");
}

#[test]
fn test_booleans() {
    let (ok, output, _) = compile_and_run("booleans.aura");
    assert!(ok, "program exited with error");
    let lines: Vec<&str> = output.trim().lines().collect();
    assert_eq!(lines, vec!["true", "false", "true", "true"]);
}

#[test]
fn test_fibonacci() {
    let (ok, output, _) = compile_and_run("fibonacci.aura");
    assert!(ok, "program exited with error");
    assert_eq!(output.trim(), "55");
}

#[test]
fn test_for_loop() {
    let (ok, output, _) = compile_and_run("for_loop.aura");
    assert!(ok, "program exited with error");
    assert_eq!(output.trim(), "45");
}

#[test]
fn test_functions() {
    let (ok, output, _) = compile_and_run("functions.aura");
    assert!(ok, "program exited with error");
    let lines: Vec<&str> = output.trim().lines().collect();
    assert_eq!(lines, vec!["25", "27"]);
}

#[test]
fn test_if_else() {
    let (ok, output, _) = compile_and_run("if_else.aura");
    assert!(ok, "program exited with error");
    let lines: Vec<&str> = output.trim().lines().collect();
    assert_eq!(lines, vec!["42", "10"]);
}

#[test]
fn test_match_expr() {
    let (ok, output, _) = compile_and_run("match_expr.aura");
    assert!(ok, "program exited with error");
    let lines: Vec<&str> = output.trim().lines().collect();
    assert_eq!(lines, vec!["0", "100", "999"]);
}

#[test]
fn test_nested_calls() {
    let (ok, output, _) = compile_and_run("nested_calls.aura");
    assert!(ok, "program exited with error");
    assert_eq!(output.trim(), "6");
}

#[test]
fn test_while_loop() {
    let (ok, output, _) = compile_and_run("while_loop.aura");
    assert!(ok, "program exited with error");
    assert_eq!(output.trim(), "128");
}

#[test]
fn test_structs() {
    let (ok, output, _) = compile_and_run("structs.aura");
    assert!(ok, "program exited with error: {}", output);
    let lines: Vec<&str> = output.trim().lines().collect();
    assert_eq!(lines, vec!["10", "20"]);
}

#[test]
fn test_break_continue() {
    let (ok, output, _) = compile_and_run("break_continue.aura");
    assert!(ok, "program exited with error: {}", output);
    let lines: Vec<&str> = output.trim().lines().collect();
    // break at 5: 0+1+2+3+4 = 10
    // continue at 3: 0+1+2+4+5+6+7+8+9 = 42
    // while break at >=64: 1*2*2*2*2*2*2 = 64
    assert_eq!(lines, vec!["10", "42", "64"]);
}

#[test]
fn test_hex_literals() {
    let (ok, output, _) = compile_and_run("hex_literals.aura");
    assert!(ok, "program exited with error: {}", output);
    let lines: Vec<&str> = output.trim().lines().collect();
    assert_eq!(lines, vec!["255", "10", "63"]);
}

/// Test that compiling an invalid program produces a compile error
fn compile_only(fixture: &str) -> (bool, String) {
    let root = workspace_root();
    let source = root.join("tests").join("fixtures").join(fixture);
    let stem = fixture.strip_suffix(".aura").unwrap();
    let output = std::env::temp_dir().join(format!("aura_test_{}", stem));

    let compile_result = Command::new(aurac_binary())
        .arg(source.to_str().unwrap())
        .arg("-o")
        .arg(output.to_str().unwrap())
        .output()
        .expect("failed to run aurac");

    let _ = std::fs::remove_file(&output);
    let stderr = String::from_utf8_lossy(&compile_result.stderr).to_string();
    (compile_result.status.success(), stderr)
}

#[test]
fn test_error_undefined_var() {
    let (ok, stderr) = compile_only("error_undefined_var.aura");
    assert!(!ok, "expected compile failure");
    assert!(
        stderr.contains("undefined") || stderr.contains("not found") || stderr.contains("error"),
        "expected error about undefined variable, got: {}",
        stderr
    );
}

#[test]
fn test_sum_types() {
    let (ok, output, _) = compile_and_run("sum_types.aura");
    assert!(ok, "program exited with error: {}", output);
    let lines: Vec<&str> = output.trim().lines().collect();
    assert_eq!(lines, vec!["1", "2", "3"]);
}

#[test]
fn test_sum_types_payload() {
    let (ok, output, _) = compile_and_run("sum_types_payload.aura");
    assert!(ok, "program exited with error: {}", output);
    let lines: Vec<&str> = output.trim().lines().collect();
    // Circle(5) => 5*5*3=75, Rectangle(4,6) => 4*6=24, Nothing => 0
    assert_eq!(lines, vec!["75", "24", "0"]);
}

#[test]
fn test_match_guards() {
    let (ok, output, _) = compile_and_run("match_guards.aura");
    assert!(ok, "program exited with error: {}", output);
    let lines: Vec<&str> = output.trim().lines().collect();
    // classify(-5)=0, classify(0)=1, classify(7)=2, classify(100)=3
    assert_eq!(lines, vec!["0", "1", "2", "3"]);
}

#[test]
fn test_string_match() {
    let (ok, output, _) = compile_and_run("string_match.aura");
    assert!(ok, "program exited with error: {}", output);
    let lines: Vec<&str> = output.trim().lines().collect();
    assert_eq!(lines, vec!["1", "2", "0"]);
}
