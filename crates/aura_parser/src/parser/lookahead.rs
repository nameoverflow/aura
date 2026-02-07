use aura_lexer::token::TokenKind;

use super::Parser;

impl Parser {
    /// Lookahead: does the type definition RHS contain a `|` at the top level?
    /// If yes, it's a sum type. If no, it's a type alias.
    pub(crate) fn has_pipe_in_type_def_ahead(&self) -> bool {
        let mut i = self.pos;
        let mut paren_depth = 0i32;
        while i < self.tokens.len() {
            match &self.tokens[i].kind {
                TokenKind::LParen => paren_depth += 1,
                TokenKind::RParen => paren_depth = (paren_depth - 1).max(0),
                TokenKind::Pipe if paren_depth == 0 => return true,
                TokenKind::Where if paren_depth == 0 => return false,
                TokenKind::Def
                | TokenKind::Type
                | TokenKind::Concept
                | TokenKind::Instance
                | TokenKind::Use
                | TokenKind::Module
                | TokenKind::Pub
                | TokenKind::Eof
                    if paren_depth == 0 =>
                {
                    return false;
                }
                _ => {}
            }
            i += 1;
        }
        false
    }

    /// Lookahead: does the type def RHS look like a type alias (as opposed to
    /// a sum type with variants)?  Returns true when the RHS starts with a
    /// known primitive type name or when `*` / `->` appear at top level
    /// *before* any end-of-definition boundary (indicating a product/function
    /// type expression, not variant syntax).
    pub(crate) fn looks_like_type_alias_ahead(&self) -> bool {
        // Check if the first UpperIdent is a known primitive type name.
        if let TokenKind::UpperIdent(name) = self.peek() {
            let known = matches!(
                name.as_str(),
                "Int" | "Int8" | "Int16" | "Int32" | "Int64"
                    | "UInt" | "UInt8" | "UInt16" | "UInt32" | "UInt64"
                    | "Float32" | "Float64" | "Decimal" | "BigDecimal"
                    | "Bool" | "Char" | "String" | "Unit"
            );
            if known {
                return true;
            }
        }

        // Scan ahead for `*` or `->` at the top level (paren-depth 0)
        // within this type definition only (stop at item boundaries).
        let mut i = self.pos;
        let mut paren_depth = 0i32;
        while i < self.tokens.len() {
            match &self.tokens[i].kind {
                TokenKind::LParen => paren_depth += 1,
                TokenKind::RParen => paren_depth = (paren_depth - 1).max(0),
                TokenKind::Star | TokenKind::Arrow if paren_depth == 0 => return true,
                TokenKind::Pipe if paren_depth == 0 => return false,
                // Stop scanning at any item-boundary keyword.
                TokenKind::Def
                | TokenKind::Type
                | TokenKind::Concept
                | TokenKind::Instance
                | TokenKind::Use
                | TokenKind::Module
                | TokenKind::Pub
                | TokenKind::Eof
                    if paren_depth == 0 =>
                {
                    return false;
                }
                // A standalone type annotation (`name: Type`) means we hit the
                // next definition — stop.
                TokenKind::Colon if paren_depth == 0 => return false,
                _ => {}
            }
            i += 1;
        }
        false
    }

    pub(crate) fn is_refined_type_def_ahead(&self) -> bool {
        let mut i = self.pos;
        let mut paren_depth = 0i32;
        while i < self.tokens.len() {
            match &self.tokens[i].kind {
                TokenKind::LParen => paren_depth += 1,
                TokenKind::RParen => paren_depth = (paren_depth - 1).max(0),
                TokenKind::Where if paren_depth == 0 => return true,
                TokenKind::Pipe if paren_depth == 0 => return false,
                TokenKind::Def
                | TokenKind::Type
                | TokenKind::Concept
                | TokenKind::Instance
                | TokenKind::Use
                | TokenKind::Module
                | TokenKind::Pub
                | TokenKind::Eof
                    if paren_depth == 0 =>
                {
                    return false;
                }
                _ => {}
            }
            i += 1;
        }
        false
    }

    pub(crate) fn is_top_level_type_annotation_start(&self) -> bool {
        matches!(self.peek(), TokenKind::Ident(_))
            && self.pos + 1 < self.tokens.len()
            && matches!(self.tokens[self.pos + 1].kind, TokenKind::Colon)
    }

    pub(crate) fn is_lambda_ahead(&self) -> bool {
        // Look ahead to determine if this is a lambda: (params) -> ...
        // Heuristic: if after matching parens we see ->, it's a lambda
        let mut depth = 1u32;
        let mut i = self.pos;
        while i < self.tokens.len() {
            match &self.tokens[i].kind {
                TokenKind::LParen => depth += 1,
                TokenKind::RParen => {
                    depth -= 1;
                    if depth == 0 {
                        // Check if next token is ->
                        return i + 1 < self.tokens.len()
                            && matches!(self.tokens[i + 1].kind, TokenKind::Arrow);
                    }
                }
                TokenKind::Eof => return false,
                _ => {}
            }
            i += 1;
        }
        false
    }

    pub(crate) fn is_struct_lit_ahead(&self) -> bool {
        // Look ahead: UpperIdent { ident: ... }
        // vs UpperIdent { expr } (which would be a variant + block)
        let mut i = self.pos + 1; // skip {
        if i >= self.tokens.len() {
            return false;
        }
        match &self.tokens[i].kind {
            TokenKind::Ident(_) => {
                i += 1;
                if i >= self.tokens.len() {
                    return false;
                }
                matches!(self.tokens[i].kind, TokenKind::Colon)
            }
            TokenKind::RBrace => true, // empty struct lit
            _ => false,
        }
    }

    pub(crate) fn is_concept_instance_ahead(&self) -> bool {
        // instance Concept for Type { ... }
        // scan until '{' and see whether top-level `for` appears first.
        let mut i = self.pos;
        while i < self.tokens.len() {
            match self.tokens[i].kind {
                TokenKind::For => return true,
                TokenKind::LBrace | TokenKind::Eof => return false,
                _ => i += 1,
            }
        }
        false
    }

    pub(crate) fn is_expr_start(&self) -> bool {
        matches!(
            self.peek(),
            TokenKind::IntLit(_)
                | TokenKind::FloatLit(_)
                | TokenKind::StringLit(_)
                | TokenKind::StringStart(_)
                | TokenKind::True
                | TokenKind::False
                | TokenKind::Ident(_)
                | TokenKind::UpperIdent(_)
                | TokenKind::LParen
                | TokenKind::LBrace
                | TokenKind::LBracket
                | TokenKind::If
                | TokenKind::Match
                | TokenKind::For
                | TokenKind::While
                | TokenKind::Let
                | TokenKind::Return
                | TokenKind::Break
                | TokenKind::Continue
                | TokenKind::Minus
                | TokenKind::Not
        )
    }
}
