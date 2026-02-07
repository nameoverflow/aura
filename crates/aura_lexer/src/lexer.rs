use crate::token::{Token, TokenKind};
use aura_common::Span;

pub struct Lexer<'a> {
    source: &'a str,
    chars: Vec<char>,
    pos: usize,
    file_id: u32,
    /// Stack for tracking string interpolation nesting.
    /// Each entry is the brace depth within an interpolation.
    interp_stack: Vec<u32>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str, file_id: u32) -> Self {
        Self {
            source,
            chars: source.chars().collect(),
            pos: 0,
            file_id,
            interp_stack: Vec::new(),
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        loop {
            let tok = self.next_token();
            let is_eof = tok.kind == TokenKind::Eof;
            tokens.push(tok);
            if is_eof {
                break;
            }
        }
        tokens
    }

    fn byte_pos(&self) -> u32 {
        if self.pos >= self.chars.len() {
            self.source.len() as u32
        } else {
            self.chars[..self.pos]
                .iter()
                .map(|c| c.len_utf8() as u32)
                .sum()
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn peek_next(&self) -> Option<char> {
        self.chars.get(self.pos + 1).copied()
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.chars.get(self.pos).copied();
        self.pos += 1;
        c
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            // Skip whitespace
            while let Some(c) = self.peek() {
                if c.is_whitespace() {
                    self.advance();
                } else {
                    break;
                }
            }

            // Skip line comments (but not doc comments)
            if self.peek() == Some('/') && self.peek_next() == Some('/') {
                // Check for doc comment ///
                if self.chars.get(self.pos + 2) == Some(&'/') {
                    break; // Don't skip doc comments
                }
                // Regular comment, skip to end of line
                while let Some(c) = self.advance() {
                    if c == '\n' {
                        break;
                    }
                }
                continue;
            }

            break;
        }
    }

    fn next_token(&mut self) -> Token {
        // If we're inside a string interpolation and see }, end the interpolation
        if !self.interp_stack.is_empty() {
            if let Some(&depth) = self.interp_stack.last() {
                if depth == 0 && self.peek() == Some('}') {
                    let start = self.byte_pos();
                    self.advance();
                    self.interp_stack.pop();
                    // Continue scanning string after interpolation
                    return self.lex_string_continuation(start);
                }
            }
        }

        self.skip_whitespace_and_comments();

        let start = self.byte_pos();
        let ch = match self.advance() {
            Some(c) => c,
            None => {
                return Token {
                    kind: TokenKind::Eof,
                    span: Span::new(self.file_id, start, start),
                };
            }
        };

        match ch {
            // Doc comments
            '/' if self.peek() == Some('/') && self.chars.get(self.pos + 1) == Some(&'/') => {
                self.advance(); // second /
                self.advance(); // third /
                                // Skip optional leading space
                if self.peek() == Some(' ') {
                    self.advance();
                }
                let mut text = String::new();
                while let Some(c) = self.peek() {
                    if c == '\n' {
                        break;
                    }
                    text.push(c);
                    self.advance();
                }
                let end = self.byte_pos();
                Token {
                    kind: TokenKind::DocComment(text),
                    span: Span::new(self.file_id, start, end),
                }
            }

            // String literals with interpolation
            '"' => self.lex_string(start),

            // Numbers
            c if c.is_ascii_digit() => self.lex_number(start, c),

            // Identifiers & keywords
            c if c.is_alphabetic() || c == '_' => self.lex_identifier(start, c),

            // Operators & delimiters
            '+' => self.make_token(TokenKind::Plus, start),
            '-' => {
                if self.peek() == Some('>') {
                    self.advance();
                    self.make_token(TokenKind::Arrow, start)
                } else {
                    self.make_token(TokenKind::Minus, start)
                }
            }
            '*' => self.make_token(TokenKind::Star, start),
            '/' => self.make_token(TokenKind::Slash, start),
            '%' => self.make_token(TokenKind::Percent, start),
            '=' => {
                if self.peek() == Some('=') {
                    self.advance();
                    self.make_token(TokenKind::Eq, start)
                } else if self.peek() == Some('>') {
                    self.advance();
                    self.make_token(TokenKind::FatArrow, start)
                } else {
                    self.make_token(TokenKind::Assign, start)
                }
            }
            '!' => {
                if self.peek() == Some('=') {
                    self.advance();
                    self.make_token(TokenKind::NotEq, start)
                } else {
                    self.make_token(TokenKind::Error("unexpected '!'".into()), start)
                }
            }
            '<' => {
                if self.peek() == Some('=') {
                    self.advance();
                    self.make_token(TokenKind::LtEq, start)
                } else {
                    self.make_token(TokenKind::Lt, start)
                }
            }
            '>' => {
                if self.peek() == Some('=') {
                    self.advance();
                    self.make_token(TokenKind::GtEq, start)
                } else {
                    self.make_token(TokenKind::Gt, start)
                }
            }
            '|' => {
                if self.peek() == Some('>') {
                    self.advance();
                    self.make_token(TokenKind::Pipeline, start)
                } else {
                    self.make_token(TokenKind::Pipe, start)
                }
            }
            '?' => self.make_token(TokenKind::Question, start),
            '.' => {
                if self.peek() == Some('.') {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        self.make_token(TokenKind::DotDotEq, start)
                    } else {
                        self.make_token(TokenKind::DotDot, start)
                    }
                } else {
                    self.make_token(TokenKind::Dot, start)
                }
            }
            '(' => self.make_token(TokenKind::LParen, start),
            ')' => self.make_token(TokenKind::RParen, start),
            '{' => {
                // Track brace depth inside interpolation
                if let Some(depth) = self.interp_stack.last_mut() {
                    *depth += 1;
                }
                self.make_token(TokenKind::LBrace, start)
            }
            '}' => {
                // Track brace depth inside interpolation
                if let Some(depth) = self.interp_stack.last_mut() {
                    *depth = depth.saturating_sub(1);
                }
                self.make_token(TokenKind::RBrace, start)
            }
            '[' => self.make_token(TokenKind::LBracket, start),
            ']' => self.make_token(TokenKind::RBracket, start),
            ',' => self.make_token(TokenKind::Comma, start),
            ':' => {
                if self.peek() == Some(':') {
                    self.advance();
                    self.make_token(TokenKind::ColonColon, start)
                } else {
                    self.make_token(TokenKind::Colon, start)
                }
            }
            ';' => self.make_token(TokenKind::Semicolon, start),
            '@' => self.make_token(TokenKind::At, start),

            c => {
                let end = self.byte_pos();
                Token {
                    kind: TokenKind::Error(format!("unexpected character '{c}'")),
                    span: Span::new(self.file_id, start, end),
                }
            }
        }
    }

    fn make_token(&self, kind: TokenKind, start: u32) -> Token {
        Token {
            kind,
            span: Span::new(self.file_id, start, self.byte_pos()),
        }
    }

    fn lex_number(&mut self, start: u32, first: char) -> Token {
        // Check for hex (0x), binary (0b), octal (0o)
        if first == '0' {
            match self.peek() {
                Some('x') | Some('X') => {
                    self.advance();
                    return self.lex_radix_int(start, 16, |c| c.is_ascii_hexdigit());
                }
                Some('b') | Some('B') => {
                    self.advance();
                    return self.lex_radix_int(start, 2, |c| c == '0' || c == '1');
                }
                Some('o') | Some('O') => {
                    self.advance();
                    return self.lex_radix_int(start, 8, |c| ('0'..='7').contains(&c));
                }
                _ => {}
            }
        }

        let mut num_str = String::new();
        num_str.push(first);
        let mut is_float = false;

        while let Some(c) = self.peek() {
            if c.is_ascii_digit() || c == '_' {
                if c != '_' {
                    num_str.push(c);
                }
                self.advance();
            } else if c == '.' && self.peek_next() != Some('.') && !is_float {
                // Check it's not .. range operator
                if let Some(next) = self.peek_next() {
                    if next.is_ascii_digit() {
                        is_float = true;
                        num_str.push('.');
                        self.advance();
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        let end = self.byte_pos();
        let kind = if is_float {
            match num_str.parse::<f64>() {
                Ok(v) => TokenKind::FloatLit(v),
                Err(e) => TokenKind::Error(format!("invalid float literal: {e}")),
            }
        } else {
            match num_str.parse::<i64>() {
                Ok(v) => TokenKind::IntLit(v),
                Err(e) => TokenKind::Error(format!("invalid integer literal: {e}")),
            }
        };
        Token {
            kind,
            span: Span::new(self.file_id, start, end),
        }
    }

    fn lex_radix_int(&mut self, start: u32, radix: u32, is_digit: impl Fn(char) -> bool) -> Token {
        let mut num_str = String::new();
        while let Some(c) = self.peek() {
            if is_digit(c) {
                num_str.push(c);
                self.advance();
            } else if c == '_' {
                self.advance();
            } else {
                break;
            }
        }
        let end = self.byte_pos();
        if num_str.is_empty() {
            return Token {
                kind: TokenKind::Error("expected digits after radix prefix".into()),
                span: Span::new(self.file_id, start, end),
            };
        }
        let kind = match i64::from_str_radix(&num_str, radix) {
            Ok(v) => TokenKind::IntLit(v),
            Err(e) => TokenKind::Error(format!("invalid integer literal: {e}")),
        };
        Token {
            kind,
            span: Span::new(self.file_id, start, end),
        }
    }

    fn lex_identifier(&mut self, start: u32, first: char) -> Token {
        let mut ident = String::new();
        ident.push(first);

        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                ident.push(c);
                self.advance();
            } else {
                break;
            }
        }

        let end = self.byte_pos();
        // Check for sole underscore (wildcard pattern)
        if ident == "_" {
            return Token {
                kind: TokenKind::Underscore,
                span: Span::new(self.file_id, start, end),
            };
        }

        // Check keywords
        if let Some(kw) = TokenKind::keyword_from_str(&ident) {
            return Token {
                kind: kw,
                span: Span::new(self.file_id, start, end),
            };
        }

        // Distinguish upper vs lower identifiers
        let kind = if first.is_uppercase() {
            TokenKind::UpperIdent(ident)
        } else {
            TokenKind::Ident(ident)
        };

        Token {
            kind,
            span: Span::new(self.file_id, start, end),
        }
    }

    fn lex_string(&mut self, start: u32) -> Token {
        let mut text = String::new();
        let has_interpolation = false;

        loop {
            match self.peek() {
                None => {
                    let end = self.byte_pos();
                    return Token {
                        kind: TokenKind::Error("unterminated string literal".into()),
                        span: Span::new(self.file_id, start, end),
                    };
                }
                Some('"') => {
                    self.advance();
                    let end = self.byte_pos();
                    if has_interpolation {
                        return Token {
                            kind: TokenKind::StringEnd(text),
                            span: Span::new(self.file_id, start, end),
                        };
                    } else {
                        return Token {
                            kind: TokenKind::StringLit(text),
                            span: Span::new(self.file_id, start, end),
                        };
                    }
                }
                Some('{') => {
                    // Check for escaped brace {{
                    if self.peek_next() == Some('{') {
                        self.advance();
                        self.advance();
                        text.push('{');
                        continue;
                    }
                    // Start interpolation
                    self.advance(); // consume {
                    self.interp_stack.push(0);
                    let end = self.byte_pos();
                    let kind = if has_interpolation {
                        TokenKind::StringMid(text)
                    } else {
                        TokenKind::StringStart(text)
                    };
                    return Token {
                        kind,
                        span: Span::new(self.file_id, start, end),
                    };
                }
                Some('}') if self.peek_next() == Some('}') => {
                    self.advance();
                    self.advance();
                    text.push('}');
                }
                Some('\\') => {
                    self.advance();
                    match self.advance() {
                        Some('n') => text.push('\n'),
                        Some('t') => text.push('\t'),
                        Some('r') => text.push('\r'),
                        Some('\\') => text.push('\\'),
                        Some('"') => text.push('"'),
                        Some('0') => text.push('\0'),
                        Some(c) => {
                            text.push('\\');
                            text.push(c);
                        }
                        None => {
                            let end = self.byte_pos();
                            return Token {
                                kind: TokenKind::Error("unterminated string escape".into()),
                                span: Span::new(self.file_id, start, end),
                            };
                        }
                    }
                }
                Some(c) => {
                    self.advance();
                    text.push(c);
                    // Track if we ever had interpolation for StringEnd
                    // (This will be overwritten if we actually hit interpolation)
                }
            }
        }
    }

    fn lex_string_continuation(&mut self, start: u32) -> Token {
        // We just consumed the } that ended an interpolation.
        // Now continue scanning the string.
        let mut text = String::new();

        loop {
            match self.peek() {
                None => {
                    let end = self.byte_pos();
                    return Token {
                        kind: TokenKind::Error("unterminated string literal".into()),
                        span: Span::new(self.file_id, start, end),
                    };
                }
                Some('"') => {
                    self.advance();
                    let end = self.byte_pos();
                    return Token {
                        kind: TokenKind::StringEnd(text),
                        span: Span::new(self.file_id, start, end),
                    };
                }
                Some('{') => {
                    if self.peek_next() == Some('{') {
                        self.advance();
                        self.advance();
                        text.push('{');
                        continue;
                    }
                    self.advance();
                    self.interp_stack.push(0);
                    let end = self.byte_pos();
                    return Token {
                        kind: TokenKind::StringMid(text),
                        span: Span::new(self.file_id, start, end),
                    };
                }
                Some('}') if self.peek_next() == Some('}') => {
                    self.advance();
                    self.advance();
                    text.push('}');
                }
                Some('\\') => {
                    self.advance();
                    match self.advance() {
                        Some('n') => text.push('\n'),
                        Some('t') => text.push('\t'),
                        Some('r') => text.push('\r'),
                        Some('\\') => text.push('\\'),
                        Some('"') => text.push('"'),
                        Some('0') => text.push('\0'),
                        Some(c) => {
                            text.push('\\');
                            text.push(c);
                        }
                        None => {
                            let end = self.byte_pos();
                            return Token {
                                kind: TokenKind::Error("unterminated string escape".into()),
                                span: Span::new(self.file_id, start, end),
                            };
                        }
                    }
                }
                Some(c) => {
                    self.advance();
                    text.push(c);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::TokenKind::*;

    fn lex(source: &str) -> Vec<TokenKind> {
        let mut lexer = Lexer::new(source, 0);
        lexer.tokenize().into_iter().map(|t| t.kind).collect()
    }

    fn lex_no_eof(source: &str) -> Vec<TokenKind> {
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
}
