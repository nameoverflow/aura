use super::*;
use aura_lexer::Lexer;
use aura_parser::Parser;

fn resolve_str(input: &str) -> Result<ResolvedModule, Vec<ResolveError>> {
    let mut lexer = Lexer::new(input, 0);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    let module = parser.parse_module().unwrap();
    let resolver = Resolver::new();
    resolver.resolve(module)
}

#[test]
fn test_resolve_simple_function() {
    let result = resolve_str("def add(a: Int, b: Int) -> Int = a + b");
    assert!(result.is_ok());
}

#[test]
fn test_resolve_undefined_variable() {
    let result = resolve_str("def test() -> Int = x");
    assert!(result.is_err());
    let errors = result.unwrap_err();
    assert!(errors[0].message.contains("undefined variable 'x'"));
}

#[test]
fn test_resolve_let_binding() {
    let result = resolve_str("def test() -> Int = { let x = 42; x }");
    assert!(result.is_ok());
}

#[test]
fn test_resolve_nested_scope() {
    let result = resolve_str("def test() -> Int = { let x = 1; { let y = x; y } }");
    assert!(result.is_ok());
}

#[test]
fn test_resolve_function_call() {
    let result = resolve_str(
        "def add(a: Int, b: Int) -> Int = a + b\n\
         def main() -> Int = add(1, 2)",
    );
    assert!(result.is_ok());
}

#[test]
fn test_resolve_type_def() {
    let result = resolve_str(
        "type Status = Pending | Complete\n\
         def test() -> Int = 0",
    );
    assert!(result.is_ok());
    let resolved = result.unwrap();
    assert!(resolved.type_defs.contains_key("Status"));
    assert!(resolved.variant_types.contains_key("Pending"));
    assert!(resolved.variant_types.contains_key("Complete"));
}

#[test]
fn test_resolve_match_binding() {
    let result = resolve_str("def test(x: Int) -> Int = match x { n => n }");
    assert!(result.is_ok());
}

#[test]
fn test_resolve_lambda() {
    let result = resolve_str("def test() -> Int = { let f = (x) -> x + 1; f(10) }");
    assert!(result.is_ok());
}

#[test]
fn test_resolve_for_loop_binding() {
    let result = resolve_str("def test() -> Int = for i in 0..10 { i }");
    assert!(result.is_ok());
}

#[test]
fn test_resolve_builtin_types() {
    let result = resolve_str("def test(x: Int, y: String, z: Bool) -> Float64 = 0.0");
    assert!(result.is_ok());
}

#[test]
fn test_resolve_multi_char_type_vars() {
    let result = resolve_str(
        "id: value -> value\n\
         def id(x) = x\n\
         def test(x: Result value error) -> Int = 0",
    );
    assert!(result.is_ok());
}

#[test]
fn test_resolve_print() {
    let result = resolve_str("def main() -> Int = { println(42); 0 }");
    assert!(result.is_ok());
}

#[test]
fn test_resolve_duplicate_def() {
    let result = resolve_str("def foo() -> Int = 1\ndef foo() -> Int = 2");
    assert!(result.is_err());
    assert!(result.unwrap_err()[0].message.contains("duplicate"));
}

#[test]
fn test_resolve_option_variants() {
    let result = resolve_str("def test(x: Int) -> Int = match x { 0 => 0, n => n }");
    assert!(result.is_ok());
}

#[test]
fn test_resolve_struct_type() {
    let result = resolve_str(
        "type User = { name: String, age: Int }\n\
         def test() -> Int = 0",
    );
    assert!(result.is_ok());
    let resolved = result.unwrap();
    assert!(resolved.type_defs.contains_key("User"));
}

#[test]
fn test_resolve_struct_pattern() {
    let result = resolve_str(
        "type User = { name: String, age: Int }\n\
         def test(u: User) -> String = match u { User { name, .. } => name }",
    );
    assert!(result.is_ok());
}

#[test]
fn test_resolve_or_pattern() {
    let result = resolve_str(
        "type OptionInt = Some Int | None\n\
         def test(x: OptionInt) -> Int = match x { Some(n) | None => 0, Some(m) => m }",
    );
    assert!(result.is_ok());
}

#[test]
fn test_resolve_let_pattern() {
    let result = resolve_str("def test() -> Int = { let (a, b) = (1, 2); a }");
    assert!(result.is_ok());
}

#[test]
fn test_resolve_for_pattern() {
    let result = resolve_str("def test() -> Int = { for _ in 0..10 { 0 }; 1 }");
    assert!(result.is_ok());
}

#[test]
fn test_resolve_refined_type_self_constraint() {
    let result = resolve_str(
        "type NonZero = Int where self != 0\n\
         def test(x: Int) -> Int = x",
    );
    assert!(result.is_ok());
}

#[test]
fn test_resolve_function_contracts() {
    let result = resolve_str(
        "def f(x: Int) -> Int requires x > 0 ensures result > 0 = x",
    );
    assert!(result.is_ok());
}
