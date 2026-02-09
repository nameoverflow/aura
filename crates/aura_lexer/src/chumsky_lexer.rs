use aura_common::Span;
use chumsky::prelude::*;

use crate::token::TokenKind;

type ChumskySpan = SimpleSpan<usize>;

/// Lex source code into a sequence of `(TokenKind, Span)` pairs.
///
/// String interpolation is NOT handled here — call `expand_interpolations`
/// on the result for that.
pub fn lex(source: &str, file_id: u32) -> Vec<(TokenKind, Span)> {
    let raw = lexer().parse(source).into_result().unwrap_or_default();
    raw.into_iter()
        .map(|(tok, span)| {
            let s = Span::new(file_id, span.start as u32, span.end as u32);
            (tok, s)
        })
        .collect()
}

/// Build the chumsky lexer combinator.
fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<(TokenKind, ChumskySpan)>, extra::Err<Rich<'src, char>>> {
    let token = token_parser();

    // Line comments (not doc comments): // but NOT ///
    let line_comment = just("//")
        .then(just('/').not().rewind())
        .then(any().and_is(just('\n').not()).repeated())
        .padded();

    let skip = choice((
        line_comment.ignored(),
        any()
            .filter(|c: &char| c.is_whitespace())
            .ignored()
            .repeated()
            .at_least(1)
            .ignored(),
    ))
    .repeated();

    token
        .map_with(|tok, e| (tok, e.span()))
        .padded_by(skip)
        .repeated()
        .collect()
}

/// Core token parser — no whitespace/comment handling.
fn token_parser<'src>(
) -> impl Parser<'src, &'src str, TokenKind, extra::Err<Rich<'src, char>>> {
    // ── Doc comments ──
    let doc_comment = just("///")
        .ignore_then(just(' ').or_not())
        .ignore_then(
            any()
                .and_is(just('\n').not())
                .repeated()
                .to_slice(),
        )
        .map(|s: &str| TokenKind::DocComment(s.to_string()));

    // ── Numbers ──
    let hex_int = just("0x")
        .or(just("0X"))
        .ignore_then(
            any()
                .filter(|c: &char| c.is_ascii_hexdigit() || *c == '_')
                .repeated()
                .to_slice(),
        )
        .map(parse_radix_str(16));

    let bin_int = just("0b")
        .or(just("0B"))
        .ignore_then(
            any()
                .filter(|c: &char| *c == '0' || *c == '1' || *c == '_')
                .repeated()
                .to_slice(),
        )
        .map(parse_radix_str(2));

    let oct_int = just("0o")
        .or(just("0O"))
        .ignore_then(
            any()
                .filter(|c: &char| ('0'..='7').contains(c) || *c == '_')
                .repeated()
                .to_slice(),
        )
        .map(parse_radix_str(8));

    // Float: digits.digits (must check that '.' is not '..' range)
    let float_lit = any()
        .filter(|c: &char| c.is_ascii_digit())
        .repeated()
        .at_least(1)
        .then(just('.'))
        .then(any().filter(|c: &char| c.is_ascii_digit()).repeated().at_least(1))
        .to_slice()
        .map(|s: &str| {
            let clean: String = s.chars().filter(|c| *c != '_').collect();
            match clean.parse::<f64>() {
                Ok(v) => TokenKind::FloatLit(v),
                Err(e) => TokenKind::Error(format!("invalid float literal: {e}")),
            }
        });

    // Integer (with optional underscores)
    let int_lit = any()
        .filter(|c: &char| c.is_ascii_digit())
        .then(
            any()
                .filter(|c: &char| c.is_ascii_digit() || *c == '_')
                .repeated(),
        )
        .to_slice()
        .map(|s: &str| {
            let clean: String = s.chars().filter(|c| *c != '_').collect();
            match clean.parse::<i64>() {
                Ok(v) => TokenKind::IntLit(v),
                Err(e) => TokenKind::Error(format!("invalid integer literal: {e}")),
            }
        });

    let number = choice((hex_int, bin_int, oct_int, float_lit, int_lit));

    // ── String literals ──
    // Simple string (interpolation handled in post-pass)
    let escape = just('\\').ignore_then(choice((
        just('n').to('\n'),
        just('t').to('\t'),
        just('r').to('\r'),
        just('\\').to('\\'),
        just('"').to('"'),
        just('0').to('\0'),
    )));

    let string_char = choice((
        just("{{").to('{'),
        just("}}").to('}'),
        escape,
        none_of("\\\""),
    ));

    let string_lit = string_char
        .repeated()
        .collect::<String>()
        .delimited_by(just('"'), just('"'))
        .map(TokenKind::StringLit);

    // ── Identifiers & Keywords ──
    let ident_or_keyword = any()
        .filter(|c: &char| c.is_alphabetic() || *c == '_')
        .then(
            any()
                .filter(|c: &char| c.is_alphanumeric() || *c == '_')
                .repeated(),
        )
        .to_slice()
        .map(|s: &str| {
            if s == "_" {
                return TokenKind::Underscore;
            }
            if let Some(kw) = TokenKind::keyword_from_str(s) {
                return kw;
            }
            if s.starts_with(|c: char| c.is_uppercase()) {
                TokenKind::UpperIdent(s.to_string())
            } else {
                TokenKind::Ident(s.to_string())
            }
        });

    // ── Multi-char operators (longest-first) ──
    let operators = choice((
        just("|>").to(TokenKind::Pipeline),
        just("..=").to(TokenKind::DotDotEq),
        just("..").to(TokenKind::DotDot),
        just("->").to(TokenKind::Arrow),
        just("=>").to(TokenKind::FatArrow),
        just("==").to(TokenKind::Eq),
        just("!=").to(TokenKind::NotEq),
        just("<=").to(TokenKind::LtEq),
        just(">=").to(TokenKind::GtEq),
        just("::").to(TokenKind::ColonColon),
    ));

    // ── Single-char operators & delimiters ──
    let single_char = choice((
        just('+').to(TokenKind::Plus),
        just('-').to(TokenKind::Minus),
        just('*').to(TokenKind::Star),
        just('/').to(TokenKind::Slash),
        just('%').to(TokenKind::Percent),
        just('=').to(TokenKind::Assign),
        just('<').to(TokenKind::Lt),
        just('>').to(TokenKind::Gt),
        just('|').to(TokenKind::Pipe),
        just('?').to(TokenKind::Question),
        just('.').to(TokenKind::Dot),
        just('(').to(TokenKind::LParen),
        just(')').to(TokenKind::RParen),
        just('{').to(TokenKind::LBrace),
        just('}').to(TokenKind::RBrace),
        just('[').to(TokenKind::LBracket),
        just(']').to(TokenKind::RBracket),
        just(',').to(TokenKind::Comma),
        just(':').to(TokenKind::Colon),
        just(';').to(TokenKind::Semicolon),
        just('@').to(TokenKind::At),
    ));

    // ── Error recovery ──
    let error_token = any().map(|c: char| TokenKind::Error(format!("unexpected character '{c}'")));

    // Order matters: try longest/most-specific first
    choice((
        doc_comment,
        string_lit,
        number,
        ident_or_keyword,
        operators,
        single_char,
        error_token,
    ))
}

/// Helper: parse a string of digits (with underscores) using a given radix.
fn parse_radix_str(radix: u32) -> impl Fn(&str) -> TokenKind {
    move |s: &str| {
        let clean: String = s.chars().filter(|c| *c != '_').collect();
        if clean.is_empty() {
            TokenKind::Error("expected digits after radix prefix".into())
        } else {
            match i64::from_str_radix(&clean, radix) {
                Ok(v) => TokenKind::IntLit(v),
                Err(e) => TokenKind::Error(format!("invalid integer literal: {e}")),
            }
        }
    }
}
