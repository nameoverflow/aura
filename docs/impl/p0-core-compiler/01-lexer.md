# P0-01: Lexer

## Overview

The lexer (tokenizer) converts raw Aura source text into a stream of tokens. This is the very first stage of the compiler pipeline — everything else depends on it.

## Dependencies

None. This is the entry point.

## Current Implementation Status (as of February 9, 2026)

- **Implemented:** Full tokenization for Aura keywords, identifiers, operators, delimiters, comments/doc comments, numeric literals (including hex/bin/oct), and string literals.
- **Implemented:** String interpolation lexing with fragment tokens and escaped braces (`{{`, `}}`), plus dedicated tokens for interpolation boundaries.
- **Implemented:** Keywords added for newer features (`async`, `parallel`, `race`, `timeout`, `yield`) and covered by lexer tests.
- **Rewritten with Chumsky 0.11:** The lexer was rewritten from a hand-written state machine to declarative Chumsky parser combinators. The public API is `aura_lexer::lex(source, file_id) -> Vec<Token>`. String interpolation is handled as a post-pass (`interp.rs`) that expands `StringLit` tokens containing `{` into `StringStart`/`StringMid`/`StringEnd` sequences. 26 unit tests.
- **Partial/Deferred:** Optional extensions from open questions (for example raw string variants) remain deferred.

## Design Decisions

### Token Categories

| Category | Examples |
|----------|---------|
| Keywords | `def`, `let`, `mut`, `type`, `concept`, `instance`, `match`, `if`, `else`, `for`, `while`, `in`, `return`, `break`, `continue`, `pub`, `use`, `module`, `async`, `parallel`, `race`, `timeout`, `yield`, `where`, `forall`, `requires`, `ensures`, `with`, `and`, `or`, `not`, `lazy`, `collect` |
| Identifiers | `foo`, `user_service`, `MyType`, `a`, `e1` |
| Literals | `42`, `3.14`, `"hello {name}"`, `true`, `false` |
| Operators | `+`, `-`, `*`, `/`, `%`, `==`, `!=`, `<`, `>`, `<=`, `>=`, `=`, `\|>`, `->`, `?`, `..`, `..=` |
| Delimiters | `(`, `)`, `{`, `}`, `[`, `]`, `,`, `:`, `::`, `.` |
| Special | `@` (annotations), `///` (doc comments), `//` (line comments) |

### String Interpolation Strategy

Aura strings use `{expr}` for interpolation and `{{`/`}}` for literal braces. The lexer should handle this by:

1. Tokenizing `"Hello, {name}!"` as a sequence: `StringStart`, `StringLiteral("Hello, ")`, `InterpStart`, `Ident("name")`, `InterpEnd`, `StringLiteral("!")`, `StringEnd`
2. Alternatively, emit the entire string as a single token and let the parser handle interpolation parsing.

**Recommended approach:** Emit interpolated strings as a sequence of fragments. This simplifies parsing and is the approach used by Kotlin, Dart, and Swift compilers.

### Significant Whitespace

Aura does **not** use significant whitespace (no Python-style indentation). Whitespace is skipped except for separating tokens. Newlines are not significant.

### Identifier vs Keyword Disambiguation

All keywords are reserved. Identifiers are checked against the keyword table during lexing.

### Type vs Value Identifiers

By convention, types start with uppercase and values/functions start with lowercase. The lexer should emit a single `Ident` token — disambiguation happens during name resolution/parsing based on context.

## Implementation Steps

1. **Define `Token` enum** with all variants, each carrying a `Span` (byte offset range in source)
2. **Define `Span` type** — `{ file_id: u32, start: u32, end: u32 }`
3. **Implement character iterator** with peek/advance and position tracking
4. **Implement single-character tokens** — `(`, `)`, `{`, `}`, `[`, `]`, `,`, `:`, `.`, `?`, `@`
5. **Implement multi-character tokens** — `::`, `..`, `..=`, `->`, `|>`, `<=`, `>=`, `==`, `!=`
6. **Implement number literals** — integers and floats (including `_` separators: `1_000_000`)
7. **Implement string literals** with interpolation fragment emission
8. **Implement identifiers and keywords** — lex as identifier, check keyword table
9. **Implement comments** — `//` line comments (skip), `///` doc comments (emit)
10. **Error recovery** — emit `Error` token for invalid characters, continue lexing

## Data Structures

```rust
struct Span {
    file_id: u32,
    start: u32,
    end: u32,
}

enum Token {
    // Keywords
    Def, Let, Mut, Type, Concept, Instance, Match, If, Else,
    For, While, In, Return, Break, Continue, Pub, Use, Module,
    Async, Parallel, Race, Timeout, Yield, Where, Forall,
    Requires, Ensures, With, And, Or, Not, Lazy, Collect,
    True, False, None, Some, Ok, Err, Self_, SelfType, // self, Self

    // Literals
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),     // non-interpolated fragment
    StringStart,               // opening " of interpolated string
    StringEnd,                 // closing " of interpolated string
    InterpStart,               // { inside string
    InterpEnd,                 // } inside string

    // Identifiers
    Ident(String),

    // Operators
    Plus, Minus, Star, Slash, Percent,
    Eq, NotEq, Lt, Gt, LtEq, GtEq,
    Assign,                    // =
    Pipe,                      // |>
    Arrow,                     // ->
    Question,                  // ?
    DotDot,                    // ..
    DotDotEq,                  // ..=

    // Delimiters
    LParen, RParen,
    LBrace, RBrace,
    LBracket, RBracket,
    Comma, Colon, ColonColon, Dot,

    // Special
    At,                        // @
    DocComment(String),        // ///

    // Meta
    Eof,
    Error(String),
}
```

## Testing Strategy

- **Unit tests:** One test per token type (keyword, operator, delimiter, literal)
- **String interpolation tests:** Nested expressions, escaped braces, empty interpolation
- **Error cases:** Invalid characters, unterminated strings, malformed numbers
- **Property tests:** Round-trip (source -> tokens -> reconstructed source preserves semantics)
- **Snapshot tests:** Full source files tokenized and compared to expected output

## Open Questions

1. Should `Some`, `Ok`, `Err`, `None` be keywords or just convention-recognized identifiers? (Recommendation: identifiers resolved during name resolution, since they're constructors of standard library types)
2. Number literal syntax: Support hex (`0xFF`), binary (`0b1010`), octal (`0o77`)? (Recommendation: yes, standard practice)
3. Raw strings (no interpolation): e.g., `r"no {interpolation} here"`? (Recommendation: defer to later)

## Estimated Complexity

**M (Medium)** — String interpolation handling is the main complexity driver. The rest is standard lexer work.
