use crate::token::TokenKind::*;
use crate::Lexer;
use aura_common::Span;

fn lex(source: &str) -> Vec<crate::token::TokenKind> {
    let mut lexer = Lexer::new(source, 0);
    lexer.tokenize().into_iter().map(|t| t.kind).collect()
}

fn lex_no_eof(source: &str) -> Vec<crate::token::TokenKind> {
    let mut toks = lex(source);
    if toks.last() == Some(&Eof) {
        toks.pop();
    }
    toks
}

#[test]
fn test_keywords() {
    assert_eq!(lex_no_eof("def"), vec![Def]);
    assert_eq!(lex_no_eof("let mut"), vec![Let, Mut]);
    assert_eq!(
        lex_no_eof("type concept instance"),
        vec![Type, Concept, Instance]
    );
    assert_eq!(lex_no_eof("match if else"), vec![Match, If, Else]);
    assert_eq!(lex_no_eof("for while in"), vec![For, While, In]);
    assert_eq!(
        lex_no_eof("return break continue"),
        vec![Return, Break, Continue]
    );
    assert_eq!(lex_no_eof("pub use module"), vec![Pub, Use, Module]);
    assert_eq!(
        lex_no_eof("async parallel race timeout yield"),
        vec![Async, Parallel, Race, Timeout, Yield]
    );
    assert_eq!(
        lex_no_eof("where forall requires ensures"),
        vec![Where, Forall, Requires, Ensures]
    );
    assert_eq!(lex_no_eof("with and or not"), vec![With, And, Or, Not]);
    assert_eq!(lex_no_eof("true false"), vec![True, False]);
    assert_eq!(lex_no_eof("lazy collect"), vec![Lazy, Collect]);
}

#[test]
fn test_identifiers() {
    assert_eq!(lex_no_eof("foo"), vec![Ident("foo".into())]);
    assert_eq!(
        lex_no_eof("user_service"),
        vec![Ident("user_service".into())]
    );
    assert_eq!(lex_no_eof("MyType"), vec![UpperIdent("MyType".into())]);
    assert_eq!(lex_no_eof("Option"), vec![UpperIdent("Option".into())]);
    assert_eq!(lex_no_eof("_"), vec![Underscore]);
    assert_eq!(lex_no_eof("_foo"), vec![Ident("_foo".into())]);
}

#[test]
fn test_integer_literals() {
    assert_eq!(lex_no_eof("0"), vec![IntLit(0)]);
    assert_eq!(lex_no_eof("42"), vec![IntLit(42)]);
    assert_eq!(lex_no_eof("1_000_000"), vec![IntLit(1000000)]);
}

#[test]
fn test_float_literals() {
    assert_eq!(lex_no_eof("3.14"), vec![FloatLit(3.14)]);
    assert_eq!(lex_no_eof("0.5"), vec![FloatLit(0.5)]);
}

#[test]
fn test_string_literal() {
    assert_eq!(lex_no_eof(r#""hello""#), vec![StringLit("hello".into())]);
    assert_eq!(
        lex_no_eof(r#""hello\nworld""#),
        vec![StringLit("hello\nworld".into())]
    );
    assert_eq!(lex_no_eof(r#""""#), vec![StringLit("".into())]);
}

#[test]
fn test_string_interpolation() {
    let tokens = lex_no_eof(r#""hello {name}!""#);
    assert_eq!(
        tokens,
        vec![
            StringStart("hello ".into()),
            Ident("name".into()),
            StringEnd("!".into()),
        ]
    );
}

#[test]
fn test_string_interpolation_multi() {
    let tokens = lex_no_eof(r#""a{x}b{y}c""#);
    assert_eq!(
        tokens,
        vec![
            StringStart("a".into()),
            Ident("x".into()),
            StringMid("b".into()),
            Ident("y".into()),
            StringEnd("c".into()),
        ]
    );
}

#[test]
fn test_string_escaped_braces() {
    assert_eq!(
        lex_no_eof(r#""Use {{expr}}""#),
        vec![StringLit("Use {expr}".into())]
    );
}

#[test]
fn test_operators() {
    assert_eq!(
        lex_no_eof("+ - * / %"),
        vec![Plus, Minus, Star, Slash, Percent]
    );
    assert_eq!(
        lex_no_eof("== != < > <= >="),
        vec![Eq, NotEq, Lt, Gt, LtEq, GtEq]
    );
    assert_eq!(
        lex_no_eof("= |> -> ?"),
        vec![Assign, Pipeline, Arrow, Question]
    );
    assert_eq!(lex_no_eof(".. ..="), vec![DotDot, DotDotEq]);
    assert_eq!(lex_no_eof("=>"), vec![FatArrow]);
}

#[test]
fn test_delimiters() {
    assert_eq!(
        lex_no_eof("( ) { } [ ]"),
        vec![LParen, RParen, LBrace, RBrace, LBracket, RBracket]
    );
    assert_eq!(
        lex_no_eof(", : :: . ;"),
        vec![Comma, Colon, ColonColon, Dot, Semicolon]
    );
    assert_eq!(lex_no_eof("@"), vec![At]);
    assert_eq!(lex_no_eof("|"), vec![Pipe]);
}

#[test]
fn test_doc_comment() {
    let tokens = lex_no_eof("/// This is a doc comment\ndef foo");
    assert_eq!(
        tokens,
        vec![
            DocComment("This is a doc comment".into()),
            Def,
            Ident("foo".into()),
        ]
    );
}

#[test]
fn test_line_comment_skipped() {
    let tokens = lex_no_eof("// this is a comment\ndef foo");
    assert_eq!(tokens, vec![Def, Ident("foo".into())]);
}

#[test]
fn test_function_def() {
    let tokens = lex_no_eof("def add(a: Int, b: Int) -> Int = a + b");
    assert_eq!(
        tokens,
        vec![
            Def,
            Ident("add".into()),
            LParen,
            Ident("a".into()),
            Colon,
            UpperIdent("Int".into()),
            Comma,
            Ident("b".into()),
            Colon,
            UpperIdent("Int".into()),
            RParen,
            Arrow,
            UpperIdent("Int".into()),
            Assign,
            Ident("a".into()),
            Plus,
            Ident("b".into()),
        ]
    );
}

#[test]
fn test_type_def() {
    let tokens = lex_no_eof("type Status = Pending | Processing | Complete | Failed String");
    assert_eq!(
        tokens,
        vec![
            Type,
            UpperIdent("Status".into()),
            Assign,
            UpperIdent("Pending".into()),
            Pipe,
            UpperIdent("Processing".into()),
            Pipe,
            UpperIdent("Complete".into()),
            Pipe,
            UpperIdent("Failed".into()),
            UpperIdent("String".into()),
        ]
    );
}

#[test]
fn test_use_import() {
    let tokens = lex_no_eof("use std::collections::HashMap");
    assert_eq!(
        tokens,
        vec![
            Use,
            Ident("std".into()),
            ColonColon,
            Ident("collections".into()),
            ColonColon,
            UpperIdent("HashMap".into()),
        ]
    );
}

#[test]
fn test_complex_expression() {
    let tokens = lex_no_eof("x |> map((y) -> y * 2)");
    assert_eq!(
        tokens,
        vec![
            Ident("x".into()),
            Pipeline,
            Ident("map".into()),
            LParen,
            LParen,
            Ident("y".into()),
            RParen,
            Arrow,
            Ident("y".into()),
            Star,
            IntLit(2),
            RParen,
        ]
    );
}

#[test]
fn test_range_vs_dot() {
    assert_eq!(lex_no_eof("0..10"), vec![IntLit(0), DotDot, IntLit(10)]);
    assert_eq!(lex_no_eof("0..=10"), vec![IntLit(0), DotDotEq, IntLit(10)]);
    assert_eq!(
        lex_no_eof("a.b"),
        vec![Ident("a".into()), Dot, Ident("b".into())]
    );
}

#[test]
fn test_match_expression() {
    let tokens = lex_no_eof("match status { Pending => 1, Complete => 2, _ => 0 }");
    assert_eq!(
        tokens,
        vec![
            Match,
            Ident("status".into()),
            LBrace,
            UpperIdent("Pending".into()),
            FatArrow,
            IntLit(1),
            Comma,
            UpperIdent("Complete".into()),
            FatArrow,
            IntLit(2),
            Comma,
            Underscore,
            FatArrow,
            IntLit(0),
            RBrace,
        ]
    );
}

#[test]
fn test_interp_with_expression() {
    let tokens = lex_no_eof(r#""result: {a + b}""#);
    assert_eq!(
        tokens,
        vec![
            StringStart("result: ".into()),
            Ident("a".into()),
            Plus,
            Ident("b".into()),
            StringEnd("".into()),
        ]
    );
}

#[test]
fn test_spans_correct() {
    let mut lexer = Lexer::new("def foo", 0);
    let tokens = lexer.tokenize();
    assert_eq!(tokens[0].span, Span::new(0, 0, 3)); // "def"
    assert_eq!(tokens[1].span, Span::new(0, 4, 7)); // "foo"
}

#[test]
fn test_hex_literal() {
    assert_eq!(lex_no_eof("0xFF"), vec![IntLit(255)]);
    assert_eq!(lex_no_eof("0x1A"), vec![IntLit(26)]);
    assert_eq!(lex_no_eof("0X10"), vec![IntLit(16)]);
    assert_eq!(lex_no_eof("0xff"), vec![IntLit(255)]);
}

#[test]
fn test_binary_literal() {
    assert_eq!(lex_no_eof("0b1010"), vec![IntLit(10)]);
    assert_eq!(lex_no_eof("0B1111"), vec![IntLit(15)]);
    assert_eq!(lex_no_eof("0b0"), vec![IntLit(0)]);
}

#[test]
fn test_octal_literal() {
    assert_eq!(lex_no_eof("0o77"), vec![IntLit(63)]);
    assert_eq!(lex_no_eof("0O10"), vec![IntLit(8)]);
    assert_eq!(lex_no_eof("0o0"), vec![IntLit(0)]);
}

#[test]
fn test_radix_with_underscores() {
    assert_eq!(lex_no_eof("0xFF_FF"), vec![IntLit(0xFFFF)]);
    assert_eq!(lex_no_eof("0b1111_0000"), vec![IntLit(0xF0)]);
    assert_eq!(lex_no_eof("0o77_77"), vec![IntLit(0o7777)]);
}

#[test]
fn test_radix_empty_digits() {
    let tokens = lex_no_eof("0x");
    assert!(matches!(&tokens[0], Error(_)));
}
