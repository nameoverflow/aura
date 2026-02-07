use super::*;
use crate::ast::*;
use aura_lexer::Lexer;

fn parse_expr_str(input: &str) -> Expr {
    // Wrap in a function for the parser
    let wrapped = format!("def __test__() -> Int = {input}");
    let mut lexer2 = Lexer::new(&wrapped, 0);
    let tokens2 = lexer2.tokenize();
    let mut parser = Parser::new(tokens2);
    let module = parser.parse_module().unwrap();
    match &module.items[0] {
        Item::Function(f) => f.body.clone(),
        _ => panic!("expected function"),
    }
}

fn parse_module_str(input: &str) -> Module {
    let mut lexer = Lexer::new(input, 0);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    parser.parse_module().unwrap()
}

#[test]
fn test_parse_int_literal() {
    let expr = parse_expr_str("42");
    assert!(matches!(expr, Expr::IntLit(42, _)));
}

#[test]
fn test_parse_binary_ops() {
    let expr = parse_expr_str("1 + 2 * 3");
    // Should parse as 1 + (2 * 3) due to precedence
    match expr {
        Expr::Binary(lhs, BinOp::Add, rhs, _) => {
            assert!(matches!(*lhs, Expr::IntLit(1, _)));
            match *rhs {
                Expr::Binary(l, BinOp::Mul, r, _) => {
                    assert!(matches!(*l, Expr::IntLit(2, _)));
                    assert!(matches!(*r, Expr::IntLit(3, _)));
                }
                _ => panic!("expected mul"),
            }
        }
        _ => panic!("expected add"),
    }
}

#[test]
fn test_parse_function_def() {
    let module = parse_module_str("def add(a: Int, b: Int) -> Int = a + b");
    assert_eq!(module.items.len(), 1);
    match &module.items[0] {
        Item::Function(f) => {
            assert_eq!(f.name, "add");
            assert_eq!(f.params.len(), 2);
            assert_eq!(f.params[0].name, "a");
            assert_eq!(f.params[1].name, "b");
        }
        _ => panic!("expected function"),
    }
}

#[test]
fn test_parse_if_expr() {
    let module = parse_module_str("def test() -> Int = if x { 1 } else { 2 }");
    match &module.items[0] {
        Item::Function(f) => {
            assert!(matches!(&f.body, Expr::If(_, _, Some(_), _)));
        }
        _ => panic!("expected function"),
    }
}

#[test]
fn test_parse_let_binding() {
    let module = parse_module_str("def test() -> Int = { let x = 42; x }");
    match &module.items[0] {
        Item::Function(f) => match &f.body {
            Expr::Block(exprs, _) => {
                assert_eq!(exprs.len(), 2);
                assert!(matches!(&exprs[0], Expr::Let(name, false, None, _, _) if name == "x"));
            }
            _ => panic!("expected block"),
        },
        _ => panic!("expected function"),
    }
}

#[test]
fn test_parse_let_mut() {
    let module = parse_module_str("def test() -> Int = { let mut x = 0; x }");
    match &module.items[0] {
        Item::Function(f) => match &f.body {
            Expr::Block(exprs, _) => {
                assert!(matches!(&exprs[0], Expr::Let(_, true, None, _, _)));
            }
            _ => panic!("expected block"),
        },
        _ => panic!("expected function"),
    }
}

#[test]
fn test_parse_type_def_sum() {
    let module = parse_module_str("type Status = Pending | Processing | Complete | Failed String");
    match &module.items[0] {
        Item::TypeDef(td) => {
            assert_eq!(td.name, "Status");
            match &td.kind {
                TypeDefKind::Sum(variants) => {
                    assert_eq!(variants.len(), 4);
                    assert_eq!(variants[0].name, "Pending");
                    assert_eq!(variants[3].name, "Failed");
                    assert_eq!(variants[3].fields.len(), 1);
                }
                _ => panic!("expected sum type"),
            }
        }
        _ => panic!("expected type def"),
    }
}

#[test]
fn test_parse_type_def_struct() {
    let module = parse_module_str("type User = { name: String, age: Int }");
    match &module.items[0] {
        Item::TypeDef(td) => {
            assert_eq!(td.name, "User");
            match &td.kind {
                TypeDefKind::Struct(fields) => {
                    assert_eq!(fields.len(), 2);
                    assert_eq!(fields[0].name, "name");
                    assert_eq!(fields[1].name, "age");
                }
                _ => panic!("expected struct type"),
            }
        }
        _ => panic!("expected type def"),
    }
}

#[test]
fn test_parse_use() {
    let module = parse_module_str("use std::collections::HashMap");
    match &module.items[0] {
        Item::Use(u) => {
            assert_eq!(u.path, vec!["std", "collections", "HashMap"]);
        }
        _ => panic!("expected use"),
    }
}

#[test]
fn test_parse_match() {
    let module = parse_module_str("def test(x: Int) -> Int = match x { 1 => 10, _ => 0 }");
    match &module.items[0] {
        Item::Function(f) => match &f.body {
            Expr::Match(_, arms, _) => {
                assert_eq!(arms.len(), 2);
            }
            _ => panic!("expected match"),
        },
        _ => panic!("expected function"),
    }
}

#[test]
fn test_parse_or_pattern() {
    let module = parse_module_str(
        "def test(x: Option Int) -> Int = match x { None | Some(0) => 0, Some(n) => n }",
    );
    match &module.items[0] {
        Item::Function(f) => match &f.body {
            Expr::Match(_, arms, _) => {
                assert_eq!(arms.len(), 2);
                assert!(matches!(arms[0].pattern, Pattern::Or(_, _)));
            }
            _ => panic!("expected match"),
        },
        _ => panic!("expected function"),
    }
}

#[test]
fn test_parse_struct_pattern_with_rest() {
    let module =
        parse_module_str("def test(u: User) -> String = match u { User { name, .. } => name }");
    match &module.items[0] {
        Item::Function(f) => match &f.body {
            Expr::Match(_, arms, _) => match &arms[0].pattern {
                Pattern::Struct(name, fields, has_rest, _) => {
                    assert_eq!(name, "User");
                    assert_eq!(fields.len(), 1);
                    assert_eq!(fields[0].name, "name");
                    assert!(*has_rest);
                }
                _ => panic!("expected struct pattern"),
            },
            _ => panic!("expected match"),
        },
        _ => panic!("expected function"),
    }
}

#[test]
fn test_parse_for_loop() {
    let module = parse_module_str("def test() -> Int = for i in 0..10 { i }");
    match &module.items[0] {
        Item::Function(f) => {
            assert!(matches!(&f.body, Expr::For(var, _, _, _) if var == "i"));
        }
        _ => panic!("expected function"),
    }
}

#[test]
fn test_parse_for_destructuring_loop() {
    let module = parse_module_str("def test() -> Int = for (a, b) in pairs { a }");
    match &module.items[0] {
        Item::Function(f) => {
            assert!(matches!(
                &f.body,
                Expr::ForPattern(Pattern::Tuple(_, _), _, _, _)
            ));
        }
        _ => panic!("expected function"),
    }
}

#[test]
fn test_parse_lambda() {
    let module = parse_module_str("def test() -> Int = (x) -> x + 1");
    match &module.items[0] {
        Item::Function(f) => {
            assert!(matches!(&f.body, Expr::Lambda(_, _, _, _)));
        }
        _ => panic!("expected function"),
    }
}

#[test]
fn test_parse_pipeline() {
    let module = parse_module_str("def test(x: Int) -> Int = x |> add(1)");
    match &module.items[0] {
        Item::Function(f) => {
            assert!(matches!(&f.body, Expr::Pipeline(_, _, _)));
        }
        _ => panic!("expected function"),
    }
}

#[test]
fn test_parse_method_call() {
    let module = parse_module_str("def test(x: Int) -> Int = x.to_string()");
    match &module.items[0] {
        Item::Function(f) => {
            assert!(matches!(&f.body, Expr::MethodCall(_, name, _, _) if name == "to_string"));
        }
        _ => panic!("expected function"),
    }
}

#[test]
fn test_parse_field_access() {
    let module = parse_module_str("def test(x: Int) -> Int = user.name");
    match &module.items[0] {
        Item::Function(f) => {
            assert!(matches!(&f.body, Expr::FieldAccess(_, name, _) if name == "name"));
        }
        _ => panic!("expected function"),
    }
}

#[test]
fn test_parse_list_literal() {
    let module = parse_module_str("def test() -> Int = [1, 2, 3]");
    match &module.items[0] {
        Item::Function(f) => match &f.body {
            Expr::ListLit(elems, _) => assert_eq!(elems.len(), 3),
            _ => panic!("expected list"),
        },
        _ => panic!("expected function"),
    }
}

#[test]
fn test_parse_type_app() {
    let module = parse_module_str("def test(x: List Int) -> Int = 0");
    match &module.items[0] {
        Item::Function(f) => match &f.params[0].ty {
            Some(TypeExpr::App(base, args, _)) => {
                assert!(matches!(base.as_ref(), TypeExpr::Named(n, _) if n == "List"));
                assert_eq!(args.len(), 1);
            }
            _ => panic!("expected type app"),
        },
        _ => panic!("expected function"),
    }
}

#[test]
fn test_parse_type_app_with_multi_char_type_var() {
    let module = parse_module_str("def test(x: Result value error) -> Int = 0");
    match &module.items[0] {
        Item::Function(f) => match &f.params[0].ty {
            Some(TypeExpr::App(base, args, _)) => {
                assert!(matches!(base.as_ref(), TypeExpr::Named(n, _) if n == "Result"));
                assert_eq!(args.len(), 2);
                assert!(matches!(&args[0], TypeExpr::Named(n, _) if n == "value"));
                assert!(matches!(&args[1], TypeExpr::Named(n, _) if n == "error"));
            }
            _ => panic!("expected type app"),
        },
        _ => panic!("expected function"),
    }
}

#[test]
fn test_parse_product_type() {
    let module = parse_module_str("def test(x: Int * String) -> Int = 0");
    match &module.items[0] {
        Item::Function(f) => {
            assert!(matches!(&f.params[0].ty, Some(TypeExpr::Product(_, _))));
        }
        _ => panic!("expected function"),
    }
}

#[test]
fn test_parse_nested_blocks() {
    let module = parse_module_str("def test() -> Int = { let x = { 1 + 2 }; x }");
    match &module.items[0] {
        Item::Function(f) => match &f.body {
            Expr::Block(exprs, _) => {
                assert_eq!(exprs.len(), 2);
            }
            _ => panic!("expected block"),
        },
        _ => panic!("expected function"),
    }
}

#[test]
fn test_parse_let_pattern_binding() {
    let module = parse_module_str("def test() -> Int = { let (a, b) = pair; a }");
    match &module.items[0] {
        Item::Function(f) => match &f.body {
            Expr::Block(exprs, _) => {
                assert!(matches!(
                    &exprs[0],
                    Expr::LetPattern(Pattern::Tuple(_, _), false, None, _, _)
                ));
            }
            _ => panic!("expected block"),
        },
        _ => panic!("expected function"),
    }
}

#[test]
fn test_parse_unary() {
    let module = parse_module_str("def test() -> Int = -42");
    match &module.items[0] {
        Item::Function(f) => {
            assert!(matches!(&f.body, Expr::Unary(UnaryOp::Neg, _, _)));
        }
        _ => panic!("expected function"),
    }
}

#[test]
fn test_parse_forall_type() {
    let module = parse_module_str("id: forall (Ord a). a -> a\ndef id(x) = x");
    match &module.items[0] {
        Item::TypeAnnotation(ta) => {
            assert!(matches!(ta.ty, TypeExpr::Forall(_, _, _)));
        }
        _ => panic!("expected type annotation"),
    }
}

#[test]
fn test_parse_function_type_effects() {
    let module = parse_module_str("map: List a * (a -> b [e]) -> List b [e]\ndef map(xs, f) = xs");
    match &module.items[0] {
        Item::TypeAnnotation(ta) => match &ta.ty {
            TypeExpr::Function(_, _, Some(effects), _) => {
                assert_eq!(effects.len(), 1);
                assert_eq!(effects[0].name, "e");
            }
            _ => panic!("expected function type with effects"),
        },
        _ => panic!("expected type annotation"),
    }
}

#[test]
fn test_parse_fn_effects_and_contracts() {
    let module =
        parse_module_str("def f(x: Int) -> Int [Net, Log] requires x > 0 ensures result > 0 = x");
    match &module.items[0] {
        Item::Function(f) => {
            assert_eq!(f.effects.len(), 2);
            assert_eq!(f.effects[0].name, "Net");
            assert_eq!(f.effects[1].name, "Log");
            assert_eq!(f.requires.len(), 1);
            assert_eq!(f.ensures.len(), 1);
        }
        _ => panic!("expected function"),
    }
}

#[test]
fn test_parse_concept_def() {
    let module = parse_module_str(
        "concept Display {\n\
           type Output\n\
           def display(self) -> String\n\
         }",
    );
    match &module.items[0] {
        Item::ConceptDef(c) => {
            assert_eq!(c.name, "Display");
            assert_eq!(c.assoc_types.len(), 1);
            assert_eq!(c.methods.len(), 1);
        }
        _ => panic!("expected concept def"),
    }
}

#[test]
fn test_parse_instance_def() {
    let module = parse_module_str(
        "instance Display for User {\n\
           type Output = String\n\
           def display(self) -> String = self.name\n\
         }",
    );
    match &module.items[0] {
        Item::InstanceDef(inst) => {
            assert!(
                matches!(&inst.kind, InstanceKind::Concept { concept, .. } if concept == "Display")
            );
            assert_eq!(inst.assoc_types.len(), 1);
            assert_eq!(inst.methods.len(), 1);
        }
        _ => panic!("expected instance def"),
    }
}

#[test]
fn test_parse_pipeline_method_sugar() {
    let module = parse_module_str("def test(x: Int) -> Int = x |> .to_string()");
    match &module.items[0] {
        Item::Function(f) => {
            assert!(matches!(&f.body, Expr::MethodCall(_, name, _, _) if name == "to_string"));
        }
        _ => panic!("expected function"),
    }
}

#[test]
fn test_parse_async_function_def() {
    let module = parse_module_str("async def fetch(x: Int) -> Int = x");
    match &module.items[0] {
        Item::Function(f) => {
            assert!(f.is_async);
            assert_eq!(f.name, "fetch");
        }
        _ => panic!("expected function"),
    }
}

#[test]
fn test_parse_parallel_for_yield() {
    let module = parse_module_str(
        "def test(xs: List Int) -> List Int = parallel { for x in xs yield x + 1 }",
    );
    match &module.items[0] {
        Item::Function(f) => match &f.body {
            Expr::Parallel(
                ParallelBody::ForYield {
                    pattern,
                    iter,
                    body,
                    ..
                },
                _,
            ) => {
                assert!(matches!(pattern, Pattern::Ident(name, _) if name == "x"));
                assert!(matches!(iter.as_ref(), Expr::Ident(name, _) if name == "xs"));
                assert!(matches!(body.as_ref(), Expr::Binary(_, BinOp::Add, _, _)));
            }
            _ => panic!("expected parallel for-yield"),
        },
        _ => panic!("expected function"),
    }
}

#[test]
fn test_parse_parallel_fixed_yield() {
    let module = parse_module_str("def test() -> Int * Int = parallel { yield 1, yield 2 }");
    match &module.items[0] {
        Item::Function(f) => match &f.body {
            Expr::Parallel(ParallelBody::FixedYield(values), _) => {
                assert_eq!(values.len(), 2);
            }
            _ => panic!("expected parallel fixed-yield"),
        },
        _ => panic!("expected function"),
    }
}

#[test]
fn test_parse_race() {
    let module = parse_module_str("def test() -> Int = race { 1, 2 }");
    match &module.items[0] {
        Item::Function(f) => match &f.body {
            Expr::Race(arms, _) => assert_eq!(arms.len(), 2),
            _ => panic!("expected race"),
        },
        _ => panic!("expected function"),
    }
}

#[test]
fn test_parse_timeout() {
    let module = parse_module_str("def test() -> Int = timeout(5) { 1 }");
    match &module.items[0] {
        Item::Function(f) => match &f.body {
            Expr::Timeout(_, body, _) => {
                assert!(matches!(body.as_ref(), Expr::Block(_, _)));
            }
            _ => panic!("expected timeout"),
        },
        _ => panic!("expected function"),
    }
}
