use super::*;
use aura_resolve::Resolver;

fn typecheck_str(input: &str) -> Result<TypedModule, Vec<TypeError>> {
    let module = aura_parser::parse(input, 0).unwrap();
    let resolver = Resolver::new();
    let resolved = resolver.resolve(module).unwrap();
    let checker = TypeChecker::new();
    checker.check(&resolved)
}

#[test]
fn test_int_literal() {
    let result = typecheck_str("def test() -> Int = 42");
    assert!(result.is_ok());
}

#[test]
fn test_float_literal() {
    let result = typecheck_str("def test() -> Float64 = 3.14");
    assert!(result.is_ok());
}

#[test]
fn test_string_literal() {
    let result = typecheck_str("def test() -> String = \"hello\"");
    assert!(result.is_ok());
}

#[test]
fn test_bool_literal() {
    let result = typecheck_str("def test() -> Bool = true");
    assert!(result.is_ok());
}

#[test]
fn test_arithmetic() {
    let result = typecheck_str("def test(a: Int, b: Int) -> Int = a + b");
    assert!(result.is_ok());
}

#[test]
fn test_comparison() {
    let result = typecheck_str("def test(a: Int, b: Int) -> Bool = a < b");
    assert!(result.is_ok());
}

#[test]
fn test_if_expression() {
    let result = typecheck_str("def test(x: Bool) -> Int = if x { 1 } else { 2 }");
    assert!(result.is_ok());
}

#[test]
fn test_type_mismatch() {
    let result = typecheck_str("def test() -> Int = true");
    assert!(result.is_err());
}

#[test]
fn test_let_binding_infer() {
    let result = typecheck_str("def test() -> Int = { let x = 42; x }");
    assert!(result.is_ok());
}

#[test]
fn test_function_call() {
    let result = typecheck_str(
        "def add(a: Int, b: Int) -> Int = a + b\n\
         def main() -> Int = add(1, 2)",
    );
    assert!(result.is_ok());
}

#[test]
fn test_struct_field_access() {
    let result = typecheck_str(
        "type User = { name: String, age: Int }\n\
         def test(u: User) -> String = u.name",
    );
    assert!(result.is_ok());
}

#[test]
fn test_lambda_type() {
    let result = typecheck_str("def test() -> Int = { let f = (x: Int) -> x + 1; f(10) }");
    assert!(result.is_ok());
}

#[test]
fn test_if_branch_mismatch() {
    let result = typecheck_str("def test(x: Bool) -> Int = if x { 1 } else { \"hello\" }");
    assert!(result.is_err());
}

#[test]
fn test_logical_operators() {
    let result = typecheck_str("def test(a: Bool, b: Bool) -> Bool = a and b or not a");
    assert!(result.is_ok());
}

#[test]
fn test_while_loop() {
    let result = typecheck_str("def test(x: Bool) -> Int = { while x { 0 }; 1 }");
    assert!(result.is_ok());
}

#[test]
fn test_match_expression() {
    let result = typecheck_str("def test(x: Int) -> Int = match x { 1 => 10, _ => 0 }");
    assert!(result.is_ok());
}

#[test]
fn test_list_literal() {
    let result = typecheck_str("def test() -> Int = { let xs = [1, 2, 3]; 0 }");
    assert!(result.is_ok());
}

#[test]
fn test_tuple_literal() {
    let result = typecheck_str("def test() -> Int = { let t = (1, \"hello\"); 0 }");
    assert!(result.is_ok());
}

#[test]
fn test_let_pattern_destructuring() {
    let result = typecheck_str("def test() -> Int = { let (a, b) = (1, 2); a + b }");
    assert!(result.is_ok(), "{:?}", result.err());
}

#[test]
fn test_nested_function_calls() {
    let result = typecheck_str(
        "def double(x: Int) -> Int = x * 2\n\
         def main() -> Int = double(double(5))",
    );
    assert!(result.is_ok());
}

#[test]
fn test_generic_function_annotation() {
    let result = typecheck_str(
        "id: a -> a\n\
         def id(x) = x\n\
         def test() -> Int = id(42)",
    );
    assert!(result.is_ok());
}

#[test]
fn test_let_polymorphism() {
    let result = typecheck_str(
        "def test() -> Int = {\n\
            let id = (x) -> x;\n\
            let a = id(1);\n\
            let b = id(2);\n\
            a + b\n\
        }",
    );
    assert!(result.is_ok());
}

#[test]
fn test_variant_constructor_arity_error() {
    let result = typecheck_str(
        "type Status = Failed String\n\
         def test() -> Status = Failed()",
    );
    assert!(result.is_err());
}

#[test]
fn test_struct_literal_missing_field_error() {
    let result = typecheck_str(
        "type User = { name: String, age: Int }\n\
         def test() -> User = User { name: \"a\" }",
    );
    assert!(result.is_err());
}

#[test]
fn test_match_non_exhaustive_error() {
    let result = typecheck_str(
        "type OptionInt = Some Int | None\n\
         def test(x: OptionInt) -> Int = match x { Some(n) => n }",
    );
    assert!(result.is_err());
}

#[test]
fn test_type_arity_error() {
    let result = typecheck_str("def test(x: Option Int String) -> Int = 0");
    assert!(result.is_err());
}

#[test]
fn test_with_field_typecheck() {
    let result = typecheck_str(
        "type User = { name: String, age: Int }\n\
         def test(u: User) -> User = u with { age: 42 }",
    );
    assert!(result.is_ok());
}

#[test]
fn test_concept_method_call() {
    let result = typecheck_str(
        "type User = { name: String }\n\
         concept Display { def display(self) -> String }\n\
         instance Display for User { def display(self) -> String = self.name }\n\
         def test(u: User) -> String = u.display()",
    );
    assert!(result.is_ok());
}

#[test]
fn test_associated_function_call() {
    let result = typecheck_str(
        "type User = { name: String }\n\
         instance User { def origin() -> User = User { name: \"root\" } }\n\
         def test() -> User = User.origin()",
    );
    assert!(result.is_ok(), "{:?}", result.err());
}

#[test]
fn test_inherent_method_priority_over_concept() {
    let result = typecheck_str(
        "type User = { name: String }\n\
         concept Size { def size(self) -> Int }\n\
         instance Size for User { def size(self) -> Int = 1 }\n\
         instance User { def size(self) -> String = self.name }\n\
         def test(u: User) -> String = u.size()",
    );
    assert!(result.is_ok());
}

#[test]
fn test_method_ambiguity_between_concepts() {
    let result = typecheck_str(
        "type User = { name: String }\n\
         concept A { def m(self) -> Int }\n\
         concept B { def m(self) -> Int }\n\
         instance A for User { def m(self) -> Int = 1 }\n\
         instance B for User { def m(self) -> Int = 2 }\n\
         def test(u: User) -> Int = u.m()",
    );
    assert!(result.is_err());
}

#[test]
fn test_concept_superclass_requirement() {
    let result = typecheck_str(
        "type User = { name: String }\n\
         concept Eq { def eq(self, other: Self) -> Bool }\n\
         concept Ord: Eq { def lt(self, other: Self) -> Bool }\n\
         instance Ord for User { def lt(self, other: Self) -> Bool = true }\n\
         def test() -> Int = 0",
    );
    assert!(result.is_err());
}

#[test]
fn test_forall_bound_success() {
    let result = typecheck_str(
        "type User = { name: String }\n\
         concept Display { def display(self) -> String }\n\
         instance Display for User { def display(self) -> String = self.name }\n\
         show: forall (Display a). a -> String\n\
         def show(x) = x.display()\n\
         def test(u: User) -> String = show(u)",
    );
    assert!(result.is_ok(), "{:?}", result.err());
}

#[test]
fn test_forall_bound_failure() {
    let result = typecheck_str(
        "concept Display { def display(self) -> String }\n\
         show: forall (Display a). a -> String\n\
         def show(x) = x.display()\n\
         def test() -> String = show(1)",
    );
    assert!(result.is_err());
}

#[test]
fn test_operator_desugar_via_concept() {
    let result = typecheck_str(
        "type Vec2 = { x: Int }\n\
         concept Add { def add(self, other: Self) -> Int }\n\
         instance Add for Vec2 { def add(self, other: Self) -> Int = self.x + other.x }\n\
         def test(a: Vec2, b: Vec2) -> Int = a + b",
    );
    assert!(result.is_ok());
}

#[test]
fn test_struct_pattern_with_rest() {
    let result = typecheck_str(
        "type User = { name: String, age: Int }\n\
         def test(u: User) -> String = match u { User { name, .. } => name }",
    );
    assert!(result.is_ok(), "{:?}", result.err());
}

#[test]
fn test_struct_pattern_missing_fields_error() {
    let result = typecheck_str(
        "type User = { name: String, age: Int }\n\
         def test(u: User) -> String = match u { User { name } => name }",
    );
    assert!(result.is_err());
}

#[test]
fn test_or_pattern_exhaustive() {
    let result = typecheck_str("def test(x: Bool) -> Int = match x { true | false => 1 }");
    assert!(result.is_ok(), "{:?}", result.err());
}

#[test]
fn test_or_pattern_binding_mismatch_error() {
    let result = typecheck_str(
        "type OptionInt = Some Int | None\n\
         def test(x: OptionInt) -> Int = match x { Some(n) | None => n }",
    );
    assert!(result.is_err());
}

#[test]
fn test_explicit_concept_disambiguation_call() {
    let result = typecheck_str(
        "type User = { name: String }\n\
         concept A { def m(self) -> Int }\n\
         concept B { def m(self) -> Int }\n\
         instance A for User { def m(self) -> Int = 1 }\n\
         instance B for User { def m(self) -> Int = 2 }\n\
         def test(u: User) -> Int = A.m(u)",
    );
    assert!(result.is_ok(), "{:?}", result.err());
}

#[test]
fn test_concept_assoc_type_default() {
    let result = typecheck_str(
        "type Vec2 = { x: Int }\n\
         concept Add {\n\
           type Output = Int\n\
           def add(self, other: Self) -> Self.Output\n\
         }\n\
         instance Add for Vec2 {\n\
           def add(self, other: Self) -> Int = self.x + other.x\n\
         }\n\
         def test(a: Vec2, b: Vec2) -> Int = a.add(b)",
    );
    assert!(result.is_ok(), "{:?}", result.err());
}

#[test]
fn test_effect_missing_capability_error() {
    let result = typecheck_str(
        "fetch: Int -> Int [Net]\n\
         def fetch(x) = x\n\
         def pure(x: Int) -> Int = fetch(x)",
    );
    assert!(result.is_err());
}

#[test]
fn test_effect_declared_capability_ok() {
    let result = typecheck_str(
        "fetch: Int -> Int [Net]\n\
         def fetch(x) = x\n\
         def caller(x: Int) -> Int [Net] = fetch(x)",
    );
    assert!(result.is_ok(), "{:?}", result.err());
}

#[test]
fn test_effect_hierarchy_write_implies_read() {
    let result = typecheck_str(
        "read_db: Int -> Int [Db.Read]\n\
         def read_db(x) = x\n\
         def write_db(x: Int) -> Int [Db.Write] = read_db(x)",
    );
    assert!(result.is_ok(), "{:?}", result.err());
}

#[test]
fn test_effect_polymorphism_from_callback() {
    let result = typecheck_str(
        "map1: (Int -> Int [e]) * Int -> Int [e]\n\
         def map1(f, x) = f(x)\n\
         fetch: Int -> Int [Net]\n\
         def fetch(x) = x\n\
         def caller(x: Int) -> Int [Net] = map1(fetch, x)",
    );
    assert!(result.is_ok(), "{:?}", result.err());
}

#[test]
fn test_effect_polymorphism_missing_in_caller() {
    let result = typecheck_str(
        "map1: (Int -> Int [e]) * Int -> Int [e]\n\
         def map1(f, x) = f(x)\n\
         fetch: Int -> Int [Net]\n\
         def fetch(x) = x\n\
         def caller(x: Int) -> Int = map1(fetch, x)",
    );
    assert!(result.is_err());
}

#[test]
fn test_contracts_bool_typechecking() {
    let result = typecheck_str("def f(x: Int) -> Int requires x > 0 ensures result > 0 = x");
    assert!(result.is_ok(), "{:?}", result.err());
}

#[test]
fn test_contract_requires_must_be_bool() {
    let result = typecheck_str("def f(x: Int) -> Int requires x + 1 = x");
    assert!(result.is_err());
}

#[test]
fn test_try_option_success() {
    let result = typecheck_str(
        "head: List Int -> Option Int\n\
         def head(xs) = None\n\
         def test(xs: List Int) -> Option Int = { let n = head(xs)?; Some(n) }",
    );
    assert!(result.is_ok(), "{:?}", result.err());
}

#[test]
fn test_try_option_wrong_return_type_error() {
    let result = typecheck_str(
        "head: List Int -> Option Int\n\
         def head(xs) = None\n\
         def test(xs: List Int) -> Int = { let n = head(xs)?; n }",
    );
    assert!(result.is_err());
}

#[test]
fn test_try_result_auto_from_conversion() {
    let result = typecheck_str(
        "type IoError = Failed\n\
         type AppError = Io IoError | Other String\n\
         load: Int -> Result Int IoError\n\
         def load(x) = Err(Failed)\n\
         def run(x: Int) -> Result Int AppError = { let v = load(x)?; Ok(v) }",
    );
    assert!(result.is_ok(), "{:?}", result.err());
}

#[test]
fn test_try_result_missing_conversion_error() {
    let result = typecheck_str(
        "type E1 = A\n\
         type E2 = B\n\
         get: Int -> Result Int E1\n\
         def get(x) = Err(A)\n\
         def run(x: Int) -> Result Int E2 = { let v = get(x)?; Ok(v) }",
    );
    assert!(result.is_err());
}

#[test]
fn test_refined_type_new_and_value() {
    let result = typecheck_str(
        "type NonZero = Int where self != 0\n\
         def div(a: Int, b: NonZero) -> Int = a / b.value\n\
         def mk() -> Result NonZero ConstraintError = NonZero.new(5)",
    );
    assert!(result.is_ok(), "{:?}", result.err());
}

#[test]
fn test_refined_literal_constraint_failure() {
    let result = typecheck_str(
        "type NonZero = Int where self != 0\n\
         def mk() -> Result NonZero ConstraintError = NonZero.new(0)",
    );
    assert!(result.is_err());
}

#[test]
fn test_refined_invalid_constraint_expression_error() {
    let result = typecheck_str(
        "type Bad = Int where ((x) -> x)(self)\n\
         def x() -> Int = 1",
    );
    assert!(result.is_err());
}

#[test]
fn test_duplicate_instance_error() {
    let result = typecheck_str(
        "type Dog = { name: String }\n\
         concept Greetable { def greet(self) -> String }\n\
         instance Greetable for Dog { def greet(self) -> String = self.name }\n\
         instance Greetable for Dog { def greet(self) -> String = self.name }",
    );
    assert!(result.is_err());
    let errors = result.err().expect("expected errors");
    assert!(errors.iter().any(|e| e.message.contains("duplicate")));
}

#[test]
fn test_missing_method_error() {
    let result = typecheck_str(
        "type Dog = { name: String }\n\
         concept Greetable {\n\
           def greet(self) -> String\n\
           def farewell(self) -> String\n\
         }\n\
         instance Greetable for Dog { def greet(self) -> String = self.name }",
    );
    assert!(result.is_err());
    let errors = result.err().expect("expected errors");
    assert!(errors.iter().any(|e| e.message.contains("missing")));
}

#[test]
fn test_default_method_inherited() {
    let result = typecheck_str(
        "type Dog = { name: String }\n\
         concept Greetable {\n\
           def greet(self) -> String\n\
           def loud_greet(self) -> String = self.greet()\n\
         }\n\
         instance Greetable for Dog { def greet(self) -> String = self.name }\n\
         def test(d: Dog) -> String = d.loud_greet()",
    );
    assert!(result.is_ok(), "errors: {:?}", result.err());
}

#[test]
fn test_recursive_type_def() {
    let result = typecheck_str(
        "type Expr = Lit Int | Add Expr Expr\n\
         def compute(e: Expr) -> Int = match e {\n\
           Lit(n) => n,\n\
           Add(l, r) => compute(l) + compute(r)\n\
         }",
    );
    assert!(result.is_ok(), "errors: {:?}", result.err());
}

#[test]
fn test_ambiguous_auto_from_no_conversion() {
    // Two variants wrapping the same type should NOT auto-derive From
    let result = typecheck_str(
        "type Error = A String | B String\n\
         type Wrapper = WrapA String | WrapB String\n\
         wrap_err: String -> Result Int Wrapper\n\
         def wrap_err(s) = Err(WrapA(s))\n\
         convert: String -> Result Int Error\n\
         def convert(s) = {\n\
           let w = wrap_err(s)?\n\
           Ok(w)\n\
         }",
    );
    // Should error because Wrapper cannot auto-convert to Error
    assert!(result.is_err());
}

#[test]
fn test_try_on_plain_type_error() {
    // Using ? on a plain Int should error
    let result = typecheck_str("def bad(x: Int) -> Int = x?");
    assert!(result.is_err());
}

#[test]
fn test_redundant_match_arm() {
    let result = typecheck_str(
        "def test(x: Int) -> Int = match x {\n\
           _ => 1,\n\
           0 => 2\n\
         }",
    );
    assert!(result.is_err());
    let errors = result.err().expect("expected errors");
    assert!(errors.iter().any(|e| e.message.contains("unreachable")));
}

#[test]
fn test_compound_refined_constraint() {
    let result = typecheck_str(
        "type Bounded = Int where self >= 0 and self <= 100\n\
         def use_bounded(b: Bounded) -> Int = b.value",
    );
    assert!(result.is_ok(), "errors: {:?}", result.err());
}

#[test]
fn test_type_alias_transparent() {
    let result = typecheck_str(
        "type MyInt = Int\n\
         def add_one(x: MyInt) -> MyInt = x + 1",
    );
    assert!(result.is_ok(), "errors: {:?}", result.err());
}

#[test]
fn test_with_expression() {
    let result = typecheck_str(
        "type Point = { x: Int, y: Int }\n\
         def move_right(p: Point) -> Point = p with { x: p.x + 1 }",
    );
    assert!(result.is_ok(), "errors: {:?}", result.err());
}

#[test]
fn test_pipeline_type_checking() {
    let result = typecheck_str(
        "def double(x: Int) -> Int = x * 2\n\
         def main() -> Int = 5 |> double()",
    );
    assert!(result.is_ok(), "errors: {:?}", result.err());
}
