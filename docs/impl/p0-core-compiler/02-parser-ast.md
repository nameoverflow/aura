# P0-02: Parser & AST

## Overview

The parser consumes the token stream from the lexer and produces an Abstract Syntax Tree (AST). The AST is the central data structure that all subsequent compiler phases operate on.

## Dependencies

- **P0-01: Lexer** — token stream input

## Design Decisions

### Parser Type

**Recursive descent (Pratt parser for expressions).** Reasons:

- Aura's grammar is LL(1) or LL(2) for most constructs
- Pratt parsing handles operator precedence naturally
- Easy to produce good error messages
- Easy to extend as the language evolves

### AST Node Design

Every AST node carries a `Span` for error reporting and tooling. The AST should be:

- **Untyped** — types are added in a later phase (type checking)
- **Faithful to source** — preserves enough info for formatter/LSP
- **Interned strings** — identifiers stored as interned `Symbol` values for fast comparison

### Operator Precedence (Lowest to Highest)

| Precedence | Operators | Associativity |
|------------|-----------|---------------|
| 1 | `\|>` (pipeline) | Left |
| 2 | `or` | Left |
| 3 | `and` | Left |
| 4 | `not` | Prefix |
| 5 | `==`, `!=`, `<`, `>`, `<=`, `>=` | Non-associative |
| 6 | `..`, `..=` | Non-associative |
| 7 | `+`, `-` | Left |
| 8 | `*`, `/`, `%` | Left |
| 9 | Unary `-` | Prefix |
| 10 | `?` | Postfix |
| 11 | `.` (field/method), function call `f(x)` | Left |

### Grammar Sketch (Key Productions)

```
program       = module_decl? (use_decl | item)*
module_decl   = "module" IDENT
use_decl      = "use" path ("{" IDENT ("," IDENT)* "}")?
path          = IDENT ("::" IDENT)*

item          = annotation* (type_def | concept_def | instance_def | fn_def)
annotation    = "@" IDENT "(" annotation_args ")"

type_def      = "pub"? "type" TYPE_IDENT generics? "=" sum_variants
              | "pub"? "type" TYPE_IDENT generics? "{" field_list "}"
              | "pub"? "type" TYPE_IDENT "=" base_type "where" constraint_expr

sum_variants  = variant ("|" variant)*
variant       = TYPE_IDENT type_expr?

concept_def   = "pub"? "concept" TYPE_IDENT (":" supertraits)? ("(" assoc_defaults ")")? "{" concept_body "}"
instance_def  = "pub"? "instance" type_expr ("for" type_expr)? "{" method_list "}"

fn_def        = type_annotation? "pub"? "async"? "def" IDENT "(" param_list ")" ("->" type_expr)? effects? contracts? "=" expr
type_annotation = IDENT ":" type_expr

effects       = "[" effect ("," effect)* "]"
effect        = TYPE_IDENT ("." TYPE_IDENT)?

contracts     = ("requires" constraint_expr)* ("ensures" constraint_expr)*

expr          = block | if_expr | match_expr | for_expr | while_expr
              | return_expr | break_expr | continue_expr
              | let_expr | assign_expr | binary_expr | unary_expr
              | call_expr | field_expr | method_expr | lambda_expr
              | literal | ident | parallel_expr | race_expr | timeout_expr

block         = "{" (expr | stmt)* "}"
if_expr       = "if" expr block ("else" (if_expr | block))?
match_expr    = "match" expr "{" match_arm ("," match_arm)* "}"
match_arm     = pattern ("if" expr)? "=>" expr
```

## Implementation Steps

1. **Define AST node types** — `Module`, `Item`, `Expr`, `Pattern`, `TypeExpr`, `Stmt`
2. **Implement string interning** — `Symbol` type backed by a global intern table
3. **Implement token cursor** — peek, advance, expect, eat (with error recovery)
4. **Parse top-level** — `module`, `use` declarations, items
5. **Parse type definitions** — structs, sum types, refined types
6. **Parse function definitions** — with optional type annotation, effects, contracts
7. **Parse expressions** — Pratt parser with precedence table
8. **Parse patterns** — for `match` arms and `let` destructuring
9. **Parse type expressions** — including generics, product types (`A * B`), effect lists
10. **Parse concurrency blocks** — `parallel`, `race`, `timeout`
11. **Parse string interpolation** — assemble fragments from lexer into `StringInterp` AST node
12. **Error recovery** — synchronize on statement boundaries, produce partial AST

## Data Structures

```rust
// Core AST types (simplified)

struct Module {
    name: Option<Symbol>,
    uses: Vec<UseDecl>,
    items: Vec<Item>,
    span: Span,
}

enum Item {
    TypeDef(TypeDef),
    ConceptDef(ConceptDef),
    InstanceDef(InstanceDef),
    FnDef(FnDef),
}

struct FnDef {
    name: Symbol,
    type_annotation: Option<TypeExpr>,
    is_pub: bool,
    is_async: bool,
    params: Vec<Param>,
    return_type: Option<TypeExpr>,
    effects: Option<Vec<Effect>>,
    requires: Vec<Expr>,
    ensures: Vec<Expr>,
    body: Expr,
    span: Span,
}

struct Param {
    name: Symbol,
    ty: Option<TypeExpr>,  // None when inferred from standalone annotation
    span: Span,
}

enum Expr {
    // Literals
    IntLit(i64, Span),
    FloatLit(f64, Span),
    StringLit(String, Span),
    StringInterp(Vec<StringPart>, Span),
    BoolLit(bool, Span),

    // Names
    Ident(Symbol, Span),
    QualifiedIdent(Symbol, Symbol, Span),   // Type.Variant

    // Operators
    Binary(Box<Expr>, BinOp, Box<Expr>, Span),
    Unary(UnaryOp, Box<Expr>, Span),
    Pipeline(Box<Expr>, Box<Expr>, Span),   // a |> f
    Try(Box<Expr>, Span),                   // expr?

    // Compound
    Block(Vec<Stmt>, Option<Box<Expr>>, Span),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>, Span),
    Match(Box<Expr>, Vec<MatchArm>, Span),
    For(Pattern, Box<Expr>, Box<Expr>, Span),
    While(Box<Expr>, Box<Expr>, Span),
    Return(Option<Box<Expr>>, Span),
    Break(Span),
    Continue(Span),
    Let(Pattern, bool /* mut */, Option<TypeExpr>, Box<Expr>, Span),

    // Functions
    Call(Box<Expr>, Vec<Expr>, Span),
    MethodCall(Box<Expr>, Symbol, Vec<Expr>, Span),
    Lambda(Vec<Param>, Box<Expr>, Span),
    FieldAccess(Box<Expr>, Symbol, Span),

    // Records
    StructLit(Symbol, Vec<(Symbol, Expr)>, Span),
    With(Box<Expr>, Vec<(Symbol, Expr)>, Span),     // expr with { field: val }

    // Concurrency
    Parallel(ParallelBody, Span),
    Race(Vec<Expr>, Span),
    Timeout(Box<Expr>, Box<Expr>, Span),

    // Collections
    ListLit(Vec<Expr>, Span),
    TupleLit(Vec<Expr>, Span),
}

enum TypeExpr {
    Named(Symbol, Span),                       // Int, User
    App(Box<TypeExpr>, Vec<TypeExpr>, Span),   // List a, Result a e
    Product(Vec<TypeExpr>, Span),              // A * B * C
    Function(Vec<TypeExpr>, Box<TypeExpr>, Option<Vec<Effect>>, Span), // args -> ret [effects]
    SelfType(Span),                            // Self
    Assoc(Box<TypeExpr>, Symbol, Span),        // Self.Output
}

enum Pattern {
    Wildcard(Span),
    Ident(Symbol, Span),
    Literal(Expr, Span),
    Constructor(Symbol, Vec<Pattern>, Span),     // Some(x), Failed(reason)
    Tuple(Vec<Pattern>, Span),                   // (a, b, c)
    Struct(Symbol, Vec<(Symbol, Pattern)>, Span), // User { name, .. }
}
```

## Testing Strategy

- **Parser round-trip tests:** Parse source -> pretty-print AST -> verify structure
- **Error message tests:** Malformed input produces helpful diagnostics with correct spans
- **Snapshot tests:** Full programs parsed and AST serialized to JSON/debug format
- **Fuzzing:** Feed random/mutated token streams to test crash resistance
- **Edge cases:** Empty programs, single expressions, deeply nested blocks

## Open Questions

1. Should the parser produce a CST (Concrete Syntax Tree) for formatting fidelity, or is an AST with spans sufficient? (Recommendation: AST with spans is sufficient for the first iteration; CST can be added for the formatter if needed)
2. How to handle the ambiguity between `expr * expr` (multiplication) and `Type * Type` (product type)? (Resolution: context — `*` in type position is product, in expression position is multiplication. The parser switches mode based on context.)
3. Semicolons/statement separators: Aura appears to use newlines and `{ }` blocks for structure. Should the parser require any explicit separators between statements in a block? (Recommendation: no mandatory separators — expressions in blocks are separated by whitespace. The parser determines statement boundaries by looking at what can start an expression.)

## Estimated Complexity

**L (Large)** — The parser is substantial. Expression parsing, type expression parsing, and pattern parsing are each non-trivial. String interpolation and concurrency blocks add additional complexity.
