use super::*;
use aura_lexer::Lexer;
use aura_parser::Parser;
use aura_resolve::Resolver;
use aura_types::TypeChecker;

fn compile_to_ir(input: &str) -> String {
    let mut lexer = Lexer::new(input, 0);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    let module = parser.parse_module().unwrap();
    let resolved = Resolver::new().resolve(module.clone()).unwrap();
    let typed = TypeChecker::new().check(&resolved).unwrap();

    let context = Context::create();
    let mut codegen = CodeGen::new(&context, "test");
    codegen.set_expr_types(typed.expr_types.clone());
    codegen.compile_module(&module).unwrap();
    codegen.print_ir()
}

#[test]
fn test_simple_function() {
    let ir = compile_to_ir("def main() -> Int = 42");
    assert!(ir.contains("define i64 @main()"));
    assert!(ir.contains("ret i64 42"));
}

#[test]
fn test_function_with_params() {
    let ir = compile_to_ir("def add(a: Int, b: Int) -> Int = a + b");
    assert!(ir.contains("define i64 @add(i64"));
    assert!(ir.contains("add i64"));
}

#[test]
fn test_if_else() {
    let ir = compile_to_ir("def test(x: Bool) -> Int = if x { 1 } else { 2 }");
    assert!(ir.contains("br i1"));
    assert!(ir.contains("then"));
    assert!(ir.contains("else"));
}

#[test]
fn test_let_binding() {
    let ir = compile_to_ir("def main() -> Int = { let x = 42; x }");
    assert!(ir.contains("store i64 42"));
    assert!(ir.contains("load i64"));
}

#[test]
fn test_while_loop() {
    let ir = compile_to_ir("def test(x: Bool) -> Int = { while x { 0 }; 1 }");
    assert!(ir.contains("while_cond"));
    assert!(ir.contains("while_body"));
}

#[test]
fn test_println_int() {
    let ir = compile_to_ir("def main() -> Int = { println(42); 0 }");
    assert!(ir.contains("call i32"));
    assert!(ir.contains("printf"));
}

#[test]
fn test_println_string() {
    let ir = compile_to_ir("def main() -> Int = { println(\"hello\"); 0 }");
    assert!(ir.contains("printf"));
}

#[test]
fn test_arithmetic_ops() {
    let ir = compile_to_ir("def test(a: Int, b: Int) -> Int = a * b + a / b");
    assert!(ir.contains("mul i64"));
    assert!(ir.contains("sdiv i64"));
    assert!(ir.contains("add i64"));
}

#[test]
fn test_comparison() {
    let ir = compile_to_ir("def test(a: Int, b: Int) -> Bool = a < b");
    assert!(ir.contains("icmp slt"));
}

#[test]
fn test_function_call() {
    let ir = compile_to_ir(
        "def add(a: Int, b: Int) -> Int = a + b\n\
         def main() -> Int = add(1, 2)",
    );
    assert!(ir.contains("call i64 @add(i64 1, i64 2)"));
}

#[test]
fn test_for_loop() {
    let ir = compile_to_ir("def main() -> Int = { for i in 0..10 { println(i) }; 0 }");
    assert!(ir.contains("for_cond"));
    assert!(ir.contains("for_body"));
    assert!(ir.contains("for_inc"));
}

#[test]
fn test_negation() {
    let ir = compile_to_ir("def test(x: Int) -> Int = -x");
    assert!(ir.contains("sub i64 0"));
}

#[test]
fn test_return_stmt() {
    let ir = compile_to_ir("def test() -> Int = { return 42 }");
    assert!(ir.contains("ret i64 42"));
}

#[test]
fn test_float_operations() {
    let ir = compile_to_ir("def test(a: Float64, b: Float64) -> Float64 = a + b");
    assert!(ir.contains("fadd double"));
}

#[test]
fn test_bool_println() {
    let ir = compile_to_ir("def main() -> Int = { println(true); 0 }");
    assert!(ir.contains("true_str") || ir.contains("printf"));
}

#[test]
fn test_match_int() {
    let ir = compile_to_ir("def test(x: Int) -> Int = match x { 1 => 10, _ => 0 }");
    assert!(ir.contains("match_cmp"));
    assert!(ir.contains("match_arm"));
}

#[test]
fn test_string_literal() {
    let ir = compile_to_ir("def test() -> String = \"hello\"");
    assert!(ir.contains("hello"));
}

#[test]
fn test_multiple_functions() {
    let ir = compile_to_ir(
        "def double(x: Int) -> Int = x * 2\n\
         def triple(x: Int) -> Int = x * 3\n\
         def main() -> Int = double(triple(1))",
    );
    assert!(ir.contains("@double"));
    assert!(ir.contains("@triple"));
    assert!(ir.contains("@main"));
}

#[test]
fn test_struct_type_and_literal() {
    let ir = compile_to_ir(
        "type Point = { x: Int, y: Int }\n\
         def main() -> Int = {\n\
           let p = Point { x: 10, y: 20 };\n\
           0\n\
         }",
    );
    assert!(ir.contains("Point"));
    assert!(ir.contains("store i64 10"));
    assert!(ir.contains("store i64 20"));
}

#[test]
fn test_struct_field_access() {
    let ir = compile_to_ir(
        "type Point = { x: Int, y: Int }\n\
         def main() -> Int = {\n\
           let p = Point { x: 10, y: 20 };\n\
           p.x\n\
         }",
    );
    assert!(
        ir.contains("Point.x.ptr") || ir.contains("struct_gep") || ir.contains("getelementptr")
    );
}

#[test]
fn test_while_break() {
    let ir = compile_to_ir(
        "def main() -> Int = {\n\
           let mut x = 0;\n\
           while true {\n\
             x = x + 1;\n\
             if x == 5 { break } else { () }\n\
           };\n\
           x\n\
         }",
    );
    assert!(ir.contains("while_cond"));
    assert!(ir.contains("while_body"));
    assert!(ir.contains("while_end"));
    // break should produce an unconditional branch to while_end
    assert!(ir.contains("br label %while_end"));
}

#[test]
fn test_for_break() {
    let ir = compile_to_ir(
        "def main() -> Int = {\n\
           let mut total = 0;\n\
           for i in 0..100 {\n\
             if i == 5 { break } else { () };\n\
             total = total + i\n\
           };\n\
           total\n\
         }",
    );
    assert!(ir.contains("for_cond"));
    assert!(ir.contains("for_body"));
    assert!(ir.contains("for_end"));
    // break should produce an unconditional branch to for_end
    assert!(ir.contains("br label %for_end"));
}

#[test]
fn test_for_continue() {
    let ir = compile_to_ir(
        "def main() -> Int = {\n\
           let mut total = 0;\n\
           for i in 0..10 {\n\
             if i == 3 { continue } else { () };\n\
             total = total + i\n\
           };\n\
           total\n\
         }",
    );
    assert!(ir.contains("for_cond"));
    assert!(ir.contains("for_inc"));
    // continue should produce an unconditional branch to for_inc
    assert!(ir.contains("br label %for_inc"));
}

#[test]
fn test_hex_literal_codegen() {
    let ir = compile_to_ir("def main() -> Int = 0xFF");
    assert!(ir.contains("ret i64 255"));
}

#[test]
fn test_modulo_op() {
    let ir = compile_to_ir("def test(a: Int, b: Int) -> Int = a % b");
    assert!(ir.contains("srem i64"));
}

#[test]
fn test_boolean_and_or() {
    let ir = compile_to_ir("def test(a: Bool, b: Bool) -> Bool = a and b");
    assert!(ir.contains("and i1"));
}

#[test]
fn test_not_operator() {
    let ir = compile_to_ir("def test(a: Bool) -> Bool = not a");
    // not a  is  a XOR true
    assert!(ir.contains("xor i1"));
}

#[test]
fn test_sum_type_no_payload() {
    let ir = compile_to_ir(
        "type Color = Red | Green | Blue\n\
         def main() -> Int = { let c = Red; 0 }",
    );
    // Should have stored tag 0 for Red
    assert!(ir.contains("store i64 0"));
}

#[test]
fn test_sum_type_with_payload() {
    let ir = compile_to_ir(
        "type Maybe = Nothing | Just Int\n\
         def main() -> Int = { let x = Just(42); 0 }",
    );
    // Tag for Just = 1, payload = 42
    assert!(ir.contains("store i64 1"));
    assert!(ir.contains("store i64 42"));
}

#[test]
fn test_match_constructor_pattern() {
    let ir = compile_to_ir(
        "type Color = Red | Green | Blue\n\
         def test(c: Color) -> Int = match c {\n\
           Red => 1,\n\
           Green => 2,\n\
           Blue => 3,\n\
           _ => 0\n\
         }",
    );
    // Should compare tag values
    assert!(ir.contains("match_cmp"));
    assert!(ir.contains("match_arm"));
}

#[test]
fn test_match_string_pattern() {
    let ir = compile_to_ir(
        "def test(s: String) -> Int = match s {\n\
           \"hello\" => 1,\n\
           _ => 0\n\
         }",
    );
    assert!(ir.contains("strcmp"));
    assert!(ir.contains("hello"));
}

#[test]
fn test_match_guard() {
    let ir = compile_to_ir(
        "def test(x: Int) -> Int = match x {\n\
           n if n > 0 => 1,\n\
           _ => 0\n\
         }",
    );
    // Guard should produce a conditional branch
    assert!(ir.contains("guard_pass") || ir.contains("icmp sgt"));
}

#[test]
fn test_pipeline_call() {
    let ir = compile_to_ir(
        "def double(x: Int) -> Int = x * 2\n\
         def main() -> Int = 5 |> double()",
    );
    // Pipeline should compile to a call to double with 5 as argument
    assert!(ir.contains("call i64 @double(i64"));
}

#[test]
fn test_pipeline_with_extra_args() {
    let ir = compile_to_ir(
        "def add(x: Int, y: Int) -> Int = x + y\n\
         def main() -> Int = 10 |> add(3)",
    );
    // Pipeline should compile to a call to add with 10 and 3
    assert!(ir.contains("call i64 @add(i64"));
}

#[test]
fn test_pipeline_bare_function() {
    let ir = compile_to_ir(
        "def negate(x: Int) -> Int = 0 - x\n\
         def main() -> Int = 42 |> negate",
    );
    // Pipeline with bare function name should call negate(42)
    assert!(ir.contains("call i64 @negate(i64"));
}
