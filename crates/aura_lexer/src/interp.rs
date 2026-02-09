use aura_common::Span;

use crate::chumsky_lexer;
use crate::token::TokenKind;

/// Expand string interpolations in the token stream.
///
/// Finds `StringLit` tokens whose original source contains unescaped `{`
/// and splits them into `StringStart`/`StringMid`/`StringEnd` sequences,
/// recursively lexing the interpolated expressions.
pub fn expand_interpolations(
    source: &str,
    file_id: u32,
    tokens: Vec<(TokenKind, Span)>,
) -> Vec<(TokenKind, Span)> {
    let mut result = Vec::with_capacity(tokens.len());

    for (tok, span) in tokens {
        if matches!(&tok, TokenKind::StringLit(_)) {
            // Check the original source for unescaped `{`
            let raw = &source[span.start as usize..span.end as usize];
            if raw_has_interpolation(raw) {
                let expanded = lex_interpolated_string(source, file_id, span.start, span.end);
                result.extend(expanded);
                continue;
            }
        }
        result.push((tok, span));
    }

    result
}

/// Check if the raw source of a string literal (including quotes) has
/// an unescaped `{` that isn't `{{`.
fn raw_has_interpolation(raw: &str) -> bool {
    let bytes = raw.as_bytes();
    let mut i = 1; // skip opening quote
    while i < bytes.len().saturating_sub(1) {
        // skip closing quote range
        if bytes[i] == b'\\' {
            i += 2; // skip escape sequence
            continue;
        }
        if bytes[i] == b'{' {
            if i + 1 < bytes.len() && bytes[i + 1] == b'{' {
                i += 2; // skip {{
                continue;
            }
            return true; // single { = interpolation
        }
        i += 1;
    }
    false
}

/// Lex an interpolated string from source, producing the full token sequence.
///
/// `start` and `end` are byte offsets into `source` for the full string literal
/// (including surrounding `"`).
fn lex_interpolated_string(
    source: &str,
    file_id: u32,
    start: u32,
    end: u32,
) -> Vec<(TokenKind, Span)> {
    let raw = &source[start as usize..end as usize];
    let bytes = raw.as_bytes();
    let mut result = Vec::new();
    let mut pos = 1; // skip opening "
    let mut text = String::new();
    let mut text_start = start + 1;
    let mut is_first = true;

    while pos < bytes.len() - 1 {
        // closing quote
        if bytes[pos] == b'\\' {
            // Escape sequence
            if pos + 1 < bytes.len() {
                let escaped = match bytes[pos + 1] {
                    b'n' => '\n',
                    b't' => '\t',
                    b'r' => '\r',
                    b'\\' => '\\',
                    b'"' => '"',
                    b'0' => '\0',
                    c => {
                        text.push('\\');
                        c as char
                    }
                };
                text.push(escaped);
                pos += 2;
            } else {
                pos += 1;
            }
        } else if bytes[pos] == b'{' && pos + 1 < bytes.len() && bytes[pos + 1] == b'{' {
            // Escaped brace {{
            text.push('{');
            pos += 2;
        } else if bytes[pos] == b'}' && pos + 1 < bytes.len() && bytes[pos + 1] == b'}' {
            // Escaped brace }}
            text.push('}');
            pos += 2;
        } else if bytes[pos] == b'{' {
            // Start of interpolation
            let interp_start = start + pos as u32;
            let kind = if is_first {
                TokenKind::StringStart(text.clone())
            } else {
                TokenKind::StringMid(text.clone())
            };
            let token_end = interp_start + 1; // after the {
            result.push((kind, Span::new(file_id, text_start, token_end)));
            is_first = false;
            text.clear();
            pos += 1; // skip {

            // Find matching } accounting for nesting
            let mut depth = 1u32;
            let expr_start = start + pos as u32;
            while pos < bytes.len() - 1 && depth > 0 {
                if bytes[pos] == b'{' {
                    depth += 1;
                } else if bytes[pos] == b'}' {
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                }
                pos += 1;
            }
            let expr_end = start + pos as u32;

            // Lex the interpolated expression
            let expr_source = &source[expr_start as usize..expr_end as usize];
            let expr_tokens = chumsky_lexer::lex(expr_source, file_id);
            // Adjust spans: the inner lex produces spans relative to expr_source,
            // but we need them relative to the full source
            for (etok, espan) in expr_tokens {
                let adjusted = Span::new(
                    file_id,
                    espan.start + expr_start,
                    espan.end + expr_start,
                );
                result.push((etok, adjusted));
            }

            pos += 1; // skip }
            text_start = start + pos as u32;
        } else {
            text.push(bytes[pos] as char);
            pos += 1;
        }
    }

    // Emit StringEnd
    let end_span = Span::new(file_id, text_start, end);
    result.push((TokenKind::StringEnd(text), end_span));

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_raw_has_interpolation() {
        assert!(raw_has_interpolation(r#""hello {name}!""#));
        assert!(!raw_has_interpolation(r#""hello world""#));
        assert!(!raw_has_interpolation(r#""Use {{expr}}""#));
        assert!(raw_has_interpolation(r#""a{x}b{y}c""#));
    }
}
