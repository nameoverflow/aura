use crate::ast::*;
use aura_common::Span;
use aura_lexer::token::TokenKind;

// Operator precedence (binding power)
// Higher = tighter binding

pub const PREFIX_BP: u8 = 17; // unary -, not

pub fn infix_bp(token: &TokenKind) -> Option<(u8, u8)> {
    match token {
        // |> pipeline (lowest)
        TokenKind::Pipeline => Some((1, 2)),
        // or
        TokenKind::Or => Some((3, 4)),
        // and
        TokenKind::And => Some((5, 6)),
        // comparison (non-associative, use same l and r)
        TokenKind::Eq
        | TokenKind::NotEq
        | TokenKind::Lt
        | TokenKind::Gt
        | TokenKind::LtEq
        | TokenKind::GtEq => Some((7, 8)),
        // range
        TokenKind::DotDot | TokenKind::DotDotEq => Some((9, 10)),
        // + -
        TokenKind::Plus | TokenKind::Minus => Some((11, 12)),
        // * / %
        TokenKind::Star | TokenKind::Slash | TokenKind::Percent => Some((13, 14)),
        // with (functional update)
        TokenKind::With => Some((1, 2)),
        // assignment
        TokenKind::Assign => Some((1, 2)),
        _ => None,
    }
}

pub fn postfix_bp(token: &TokenKind) -> (u8, u8) {
    match token {
        TokenKind::Question => (19, 19),
        TokenKind::Dot => (21, 21),
        TokenKind::LParen => (21, 21), // function call
        _ => (0, 0),
    }
}

pub fn token_to_binop(token: &TokenKind) -> Option<BinOp> {
    match token {
        TokenKind::Plus => Some(BinOp::Add),
        TokenKind::Minus => Some(BinOp::Sub),
        TokenKind::Star => Some(BinOp::Mul),
        TokenKind::Slash => Some(BinOp::Div),
        TokenKind::Percent => Some(BinOp::Mod),
        TokenKind::Eq => Some(BinOp::Eq),
        TokenKind::NotEq => Some(BinOp::NotEq),
        TokenKind::Lt => Some(BinOp::Lt),
        TokenKind::Gt => Some(BinOp::Gt),
        TokenKind::LtEq => Some(BinOp::LtEq),
        TokenKind::GtEq => Some(BinOp::GtEq),
        TokenKind::And => Some(BinOp::And),
        TokenKind::Or => Some(BinOp::Or),
        _ => None,
    }
}

pub fn pattern_span(pattern: &Pattern) -> Span {
    match pattern {
        Pattern::Wildcard(span)
        | Pattern::Ident(_, span)
        | Pattern::Literal(_, span)
        | Pattern::Constructor(_, _, span)
        | Pattern::Tuple(_, span)
        | Pattern::Struct(_, _, _, span)
        | Pattern::Or(_, span) => *span,
    }
}
