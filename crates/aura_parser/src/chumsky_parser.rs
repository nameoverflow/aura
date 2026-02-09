use aura_common::Span;
use aura_lexer::token::TokenKind;
use chumsky::input::{Stream, ValueInput};
use chumsky::prelude::*;

use crate::ast::*;

type TokenSpan = (TokenKind, Span);
type E<'t> = extra::Err<Rich<'t, TokenKind, Span>>;

/// Parse source code into a Module.
pub fn parse(source: &str, file_id: u32) -> Result<Module, Vec<ParseError>> {
    let tokens = aura_lexer::lex(source, file_id);
    let tokens: Vec<TokenSpan> = tokens.into_iter().map(|t| (t.kind, t.span)).collect();
    let eoi = Span::new(file_id, source.len() as u32, source.len() as u32);
    let stream = Stream::from_iter(tokens).map(eoi, |t: TokenSpan| t);
    let result = module_p().parse(stream);
    match result.into_result() {
        Ok(m) => Ok(m),
        Err(errs) => Err(errs.into_iter().map(|e| {
            ParseError { message: format!("{:?}", e), span: *e.span() }
        }).collect()),
    }
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "parse error at {}..{}: {}", self.span.start, self.span.end, self.message)
    }
}

fn ps(p: &Pattern) -> Span {
    match p {
        Pattern::Wildcard(s) | Pattern::Ident(_, s) | Pattern::Literal(_, s)
        | Pattern::Constructor(_, _, s) | Pattern::Tuple(_, s)
        | Pattern::Struct(_, _, _, s) | Pattern::Or(_, s) => *s,
    }
}

// All parsers are generic over input I but use .boxed() at key points to prevent
// type explosion. The top-level recursive parsers (type_expr, pattern, expr)
// return Boxed to keep compile times manageable.

fn type_expr_p<'t, I: ValueInput<'t, Token=TokenKind, Span=Span> + 't>()
    -> Boxed<'t, 't, I, TypeExpr, E<'t>>
{
    recursive(|te| {
        let atom = choice((
            select! { TokenKind::Ident(n) = e => (n, e.span()), TokenKind::UpperIdent(n) = e => (n, e.span()) }
                .map(|(n, s)| TypeExpr::Named(n, s)),
            just(TokenKind::LParen).ignore_then(
                just(TokenKind::RParen).map_with(|_: TokenKind, e| TypeExpr::Unit(e.span()))
                    .or(te.clone().then_ignore(just(TokenKind::RParen)))
            ),
        ));
        let dotted = atom.foldl(
            just(TokenKind::Dot).ignore_then(select! {
                TokenKind::Ident(n) = e => (n, e.span()),
                TokenKind::UpperIdent(n) = e => (n, e.span()),
            }).repeated(),
            |base, (seg, ss)| {
                let bs = base.span();
                let n = match base { TypeExpr::Named(n, _) => n, _ => "?".into() };
                TypeExpr::Named(format!("{n}.{seg}"), bs.merge(ss))
            },
        );
        let app = dotted.clone().foldl(dotted.repeated(), |base: TypeExpr, arg: TypeExpr| {
            match &base {
                TypeExpr::Named(..) | TypeExpr::App(..) => {
                    let s = base.span().merge(arg.span());
                    match base {
                        TypeExpr::App(b, mut a, _) => { a.push(arg); let s2 = b.span().merge(a.last().unwrap().span()); TypeExpr::App(b, a, s2) }
                        _ => TypeExpr::App(Box::new(base), vec![arg], s),
                    }
                }
                _ => base,
            }
        });
        let prod = app.clone().foldl(just(TokenKind::Star).ignore_then(app).repeated(), |a, b| {
            let s = a.span().merge(b.span());
            match a { TypeExpr::Product(mut v, _) => { v.push(b); let s2 = v.first().unwrap().span().merge(v.last().unwrap().span()); TypeExpr::Product(v, s2) } _ => TypeExpr::Product(vec![a, b], s) }
        });
        let el = just(TokenKind::LBracket).ignore_then(
            effect_ref_p::<I>().separated_by(just(TokenKind::Comma)).allow_trailing().collect::<Vec<_>>()
        ).then_ignore(just(TokenKind::RBracket));
        let func = prod.clone().then(
            just(TokenKind::Arrow).ignore_then(te.clone()).then(el.or_not()).or_not()
        ).map(|(f, a): (TypeExpr, Option<(TypeExpr, Option<Vec<EffectRef>>)>)| match a {
            Some((ret, effs)) => {
                let es = effs.as_ref().and_then(|e| e.last().map(|x| x.span)).unwrap_or_else(|| ret.span());
                let s = f.span().merge(es);
                let ps = match f { TypeExpr::Product(ts, _) => ts, o => vec![o] };
                TypeExpr::Function(ps, Box::new(ret), effs, s)
            }
            None => f,
        });
        let forall = just(TokenKind::Forall).map_with(|_: TokenKind, e| -> Span { e.span() })
            .then_ignore(just(TokenKind::LParen))
            .then(select! { TokenKind::Ident(n) = e => (n, e.span()), TokenKind::UpperIdent(n) = e => (n, e.span()) }
                .then(select! { TokenKind::Ident(n) = e => (n, e.span()) })
                .map(|((c, cs), (tv, vs)): ((String, Span), (String, Span))| ConceptConstraint { concept: c, ty_var: tv, span: cs.merge(vs) })
                .separated_by(just(TokenKind::Comma)).allow_trailing().collect::<Vec<_>>())
            .then_ignore(just(TokenKind::RParen)).then_ignore(just(TokenKind::Dot))
            .then(te)
            .map(|((start, cs), body): ((Span, Vec<ConceptConstraint>), TypeExpr)| {
                TypeExpr::Forall(cs, Box::new(body.clone()), start.merge(body.span()))
            });
        choice((forall, func)).boxed()
    }).boxed()
}

fn effect_ref_p<'t, I: ValueInput<'t, Token=TokenKind, Span=Span>>()
    -> impl Parser<'t, I, EffectRef, E<'t>> + Clone
{
    select! { TokenKind::Ident(n) = e => (n, e.span()), TokenKind::UpperIdent(n) = e => (n, e.span()) }
        .then(just(TokenKind::Dot).ignore_then(
            select! { TokenKind::Ident(n) = e => (n, e.span()), TokenKind::UpperIdent(n) = e => (n, e.span()) }
        ).repeated().collect::<Vec<_>>())
        .map(|((mut name, start), segs): ((String, Span), Vec<(String, Span)>)| {
            let mut end = start;
            for (s, ss) in segs { name.push('.'); name.push_str(&s); end = ss; }
            EffectRef { name, span: start.merge(end) }
        })
}

fn pattern_p<'t, I: ValueInput<'t, Token=TokenKind, Span=Span> + 't>()
    -> Boxed<'t, 't, I, Pattern, E<'t>>
{
    recursive(|pat| {
        #[derive(Clone)] enum CS { St(Vec<FieldPattern>, bool, Span), Ar(Vec<Pattern>, Span), Bare }
        let ctor = select! { TokenKind::UpperIdent(n) = e => (n, e.span()) }.then(choice((
            just(TokenKind::LBrace).ignore_then(choice((
                just(TokenKind::DotDot).to(None::<FieldPattern>),
                select! { TokenKind::Ident(n) = e => (n, e.span()) }
                    .then(just(TokenKind::Colon).ignore_then(pat.clone()).or_not())
                    .map(|((n, fs), sub)| {
                        let fp = sub.unwrap_or_else(|| Pattern::Ident(n.clone(), fs));
                        Some(FieldPattern { name: n, pattern: fp.clone(), span: fs.merge(ps(&fp)) })
                    }),
            )).separated_by(just(TokenKind::Comma)).allow_trailing().collect::<Vec<_>>())
            .then_ignore(just(TokenKind::RBrace)).map_with(|items: Vec<Option<FieldPattern>>, e| {
                let mut fs = Vec::new(); let mut hr = false;
                for i in items { match i { None => hr = true, Some(f) => fs.push(f) } }
                CS::St(fs, hr, e.span())
            }),
            just(TokenKind::LParen).ignore_then(
                pat.clone().separated_by(just(TokenKind::Comma)).collect::<Vec<_>>()
            ).then_ignore(just(TokenKind::RParen)).map_with(|a: Vec<Pattern>, e| CS::Ar(a, e.span())),
            empty().to(CS::Bare),
        ))).map(|((name, start), suf): ((String, Span), CS)| match suf {
            CS::St(fs, hr, end) => Pattern::Struct(name, fs, hr, start.merge(end)),
            CS::Ar(a, end) => Pattern::Constructor(name, a, start.merge(end)),
            CS::Bare => Pattern::Constructor(name, vec![], start),
        });

        let atom = choice((
            just(TokenKind::Underscore).map_with(|_: TokenKind, e| Pattern::Wildcard(e.span())),
            select! { TokenKind::IntLit(n) = e => Pattern::Literal(LitPattern::Int(n), e.span()) },
            select! { TokenKind::FloatLit(n) = e => Pattern::Literal(LitPattern::Float(n), e.span()) },
            select! { TokenKind::StringLit(s) = e => Pattern::Literal(LitPattern::String(s), e.span()) },
            just(TokenKind::True).map_with(|_: TokenKind, e| Pattern::Literal(LitPattern::Bool(true), e.span())),
            just(TokenKind::False).map_with(|_: TokenKind, e| Pattern::Literal(LitPattern::Bool(false), e.span())),
            ctor,
            select! { TokenKind::Ident(n) = e => (n, e.span()) }.map(|(n, s)| Pattern::Ident(n, s)),
            just(TokenKind::LParen).map_with(|_: TokenKind, e| e.span())
                .then(pat.clone().separated_by(just(TokenKind::Comma)).collect::<Vec<_>>())
                .then_ignore(just(TokenKind::RParen))
                .map_with(|(start, ps): (Span, Vec<Pattern>), e| Pattern::Tuple(ps, start.merge(e.span()))),
        ));
        atom.separated_by(just(TokenKind::Pipe)).at_least(1).collect::<Vec<Pattern>>()
            .map(|mut ps: Vec<Pattern>| {
                if ps.len() == 1 { ps.pop().unwrap() }
                else { let f = self::ps(&ps[0]); let l = self::ps(ps.last().unwrap()); Pattern::Or(ps, f.merge(l)) }
            })
    }).boxed()
}

fn expr_p<'t, I: ValueInput<'t, Token=TokenKind, Span=Span> + 't>()
    -> Boxed<'t, 't, I, Expr, E<'t>>
{
    recursive(|expr| {
        let blk = just(TokenKind::LBrace).map_with(|_: TokenKind, e| e.span())
            .then(expr.clone().then_ignore(just(TokenKind::Semicolon).or_not()).repeated().collect::<Vec<_>>())
            .then_ignore(just(TokenKind::RBrace))
            .map_with(|(start, es): (Span, Vec<Expr>), e| Expr::Block(es, start.merge(e.span())));

        // Atoms
        let interp = select! { TokenKind::StringStart(s) = e => (s, e.span()) }
            .then(expr.clone())
            .then(select! { TokenKind::StringMid(s) = e => (s, e.span()) }.then(expr.clone()).repeated().collect::<Vec<_>>())
            .then(select! { TokenKind::StringEnd(s) = e => (s, e.span()) })
            .map(|((((ft, start), fe), mids), (et, es))| {
                let mut parts = vec![
                    StringPart { kind: StringPartKind::Literal(ft), span: start },
                    StringPart { kind: StringPartKind::Expr(fe), span: start },
                ];
                for ((mt, ms), me) in mids {
                    parts.push(StringPart { kind: StringPartKind::Literal(mt), span: ms });
                    parts.push(StringPart { kind: StringPartKind::Expr(me), span: ms });
                }
                parts.push(StringPart { kind: StringPartKind::Literal(et), span: es });
                Expr::StringInterp(parts, start.merge(es))
            });

        #[derive(Clone)] enum US { Q(String, Span), SL(Vec<((String, Span), Expr)>, Span), No }
        let upper_e = select! { TokenKind::UpperIdent(n) = e => (n, e.span()) }.then(choice((
            just(TokenKind::Dot).ignore_then(select! { TokenKind::UpperIdent(n) = e => (n, e.span()) }).map(|(v, vs)| US::Q(v, vs)),
            just(TokenKind::LBrace).ignore_then(
                select! { TokenKind::Ident(n) = e => (n, e.span()) }.then_ignore(just(TokenKind::Colon)).then(expr.clone())
                    .separated_by(just(TokenKind::Comma)).allow_trailing().collect::<Vec<_>>()
            ).then_ignore(just(TokenKind::RBrace)).map_with(|fs: Vec<((String, Span), Expr)>, e| US::SL(fs, e.span())),
            empty().to(US::No),
        ))).map(|((n, s), suf): ((String, Span), US)| match suf {
            US::Q(v, vs) => Expr::QualifiedIdent(n, v, s.merge(vs)),
            US::SL(fs, end) => { let fl = fs.into_iter().map(|((n, _), v)| (n, v)).collect(); Expr::StructLit(n, fl, s.merge(end)) }
            US::No => Expr::Ident(n, s),
        });

        #[derive(Clone)] enum PC { U(Span), L(Vec<Param>, Expr), G(Expr), T(Vec<Expr>) }
        let paren_e = just(TokenKind::LParen).map_with(|_: TokenKind, e| e.span()).then(choice((
            just(TokenKind::RParen).map_with(|_: TokenKind, e| PC::U(e.span())),
            select! { TokenKind::Ident(n) = e => (n, e.span()) }
                .then(just(TokenKind::Colon).ignore_then(type_expr_p()).or_not())
                .map(|((n, s), ty): ((String, Span), Option<TypeExpr>)| {
                    let end = ty.as_ref().map(|t| t.span()).unwrap_or(s);
                    Param { name: n, ty, span: s.merge(end) }
                })
                .separated_by(just(TokenKind::Comma)).allow_trailing().collect::<Vec<_>>()
                .then_ignore(just(TokenKind::RParen)).then_ignore(just(TokenKind::Arrow))
                .then(expr.clone()).map(|(ps, b)| PC::L(ps, b)),
            expr.clone().separated_by(just(TokenKind::Comma)).at_least(1).allow_trailing().collect::<Vec<_>>()
                .then_ignore(just(TokenKind::RParen))
                .map(|es: Vec<Expr>| if es.len() == 1 { PC::G(es.into_iter().next().unwrap()) } else { PC::T(es) }),
        ))).map(|(start, c): (Span, PC)| match c {
            PC::U(end) => Expr::Unit(start.merge(end)),
            PC::L(ps, b) => { let s = start.merge(b.span()); Expr::Lambda(ps, None, Box::new(b), s) }
            PC::G(e) => e,
            PC::T(es) => { let s = start.merge(es.last().unwrap().span()); Expr::TupleLit(es, s) }
        });

        let list_e = just(TokenKind::LBracket).map_with(|_: TokenKind, e| e.span())
            .then(expr.clone().separated_by(just(TokenKind::Comma)).allow_trailing().collect::<Vec<_>>())
            .then_ignore(just(TokenKind::RBracket))
            .map_with(|(start, es): (Span, Vec<Expr>), e| Expr::ListLit(es, start.merge(e.span())));

        let if_e = recursive(|if_rec| {
            just(TokenKind::If).map_with(|_: TokenKind, e| e.span())
                .then(expr.clone()).then(blk.clone())
                .then(just(TokenKind::Else).ignore_then(choice((if_rec, blk.clone()))).or_not())
                .map(|(((s, c), tb), eb): (((Span, Expr), Expr), Option<Expr>)| {
                    let end = eb.as_ref().map(|e| e.span()).unwrap_or_else(|| tb.span());
                    Expr::If(Box::new(c), Box::new(tb), eb.map(Box::new), s.merge(end))
                })
        });

        let match_e = just(TokenKind::Match).map_with(|_: TokenKind, e| e.span())
            .then(expr.clone()).then_ignore(just(TokenKind::LBrace))
            .then(
                pattern_p().then(just(TokenKind::If).ignore_then(expr.clone()).or_not())
                    .then_ignore(just(TokenKind::FatArrow)).then(expr.clone())
                    .map(|((pat, guard), body): ((Pattern, Option<Expr>), Expr)| {
                        let psp = ps(&pat);
                        MatchArm { pattern: pat, guard, body: body.clone(), span: psp.merge(body.span()) }
                    })
                    .separated_by(just(TokenKind::Comma)).allow_trailing().collect::<Vec<_>>()
            )
            .then_ignore(just(TokenKind::RBrace))
            .map_with(|((start, scr), arms): ((Span, Expr), Vec<MatchArm>), e| {
                Expr::Match(Box::new(scr), arms, start.merge(e.span()))
            });

        let for_e = just(TokenKind::For).map_with(|_: TokenKind, e| e.span())
            .then(pattern_p()).then_ignore(just(TokenKind::In)).then(expr.clone()).then(blk.clone())
            .map(|(((start, pat), ie), body): (((Span, Pattern), Expr), Expr)| {
                let end = body.span();
                if let Pattern::Ident(v, _) = pat { Expr::For(v, Box::new(ie), Box::new(body), start.merge(end)) }
                else { Expr::ForPattern(pat, Box::new(ie), Box::new(body), start.merge(end)) }
            });

        let while_e = just(TokenKind::While).map_with(|_: TokenKind, e| e.span())
            .then(expr.clone()).then(blk.clone())
            .map(|((s, c), b): ((Span, Expr), Expr)| { let end = b.span(); Expr::While(Box::new(c), Box::new(b), s.merge(end)) });

        let let_e = just(TokenKind::Let).map_with(|_: TokenKind, e| e.span())
            .then(just(TokenKind::Mut).or_not().map(|o| o.is_some()))
            .then(pattern_p())
            .then(just(TokenKind::Colon).ignore_then(type_expr_p()).or_not())
            .then_ignore(just(TokenKind::Assign)).then(expr.clone())
            .map(|((((start, im), pat), ty), val): ((((Span, bool), Pattern), Option<TypeExpr>), Expr)| {
                let end = val.span();
                if im {
                    if let Pattern::Ident(n, _) = pat { Expr::Let(n, true, ty, Box::new(val), start.merge(end)) }
                    else { Expr::Let("_".into(), true, ty, Box::new(val), start.merge(end)) }
                } else if let Pattern::Ident(n, _) = pat { Expr::Let(n, false, ty, Box::new(val), start.merge(end)) }
                else { Expr::LetPattern(pat, false, ty, Box::new(val), start.merge(end)) }
            });

        let ret_e = just(TokenKind::Return).map_with(|_: TokenKind, e| e.span())
            .then(expr.clone().or_not())
            .map(|(s, v): (Span, Option<Expr>)| {
                let end = v.as_ref().map(|e| e.span()).unwrap_or(s);
                Expr::Return(v.map(Box::new), s.merge(end))
            });

        let par_e = just(TokenKind::Parallel).map_with(|_: TokenKind, e| e.span())
            .then_ignore(just(TokenKind::LBrace))
            .then(choice((
                just(TokenKind::For).ignore_then(pattern_p()).then_ignore(just(TokenKind::In))
                    .then(expr.clone()).then_ignore(just(TokenKind::Yield)).then(expr.clone())
                    .map(|((p, i), b): ((Pattern, Expr), Expr)| {
                        ParallelBody::ForYield { pattern: p, iter: Box::new(i), body: Box::new(b.clone()), fail_fast: expr_contains_try(&b) }
                    }),
                just(TokenKind::Yield).ignore_then(expr.clone())
                    .separated_by(just(TokenKind::Comma)).at_least(1).allow_trailing().collect::<Vec<_>>()
                    .map(ParallelBody::FixedYield),
            )))
            .then_ignore(just(TokenKind::RBrace))
            .map_with(|(s, body): (Span, ParallelBody), e| Expr::Parallel(body, s.merge(e.span())));

        let race_e = just(TokenKind::Race).map_with(|_: TokenKind, e| e.span())
            .then_ignore(just(TokenKind::LBrace))
            .then(expr.clone().separated_by(just(TokenKind::Comma)).allow_trailing().collect::<Vec<_>>())
            .then_ignore(just(TokenKind::RBrace))
            .map_with(|(s, a): (Span, Vec<Expr>), e| Expr::Race(a, s.merge(e.span())));

        let timeout_e = just(TokenKind::Timeout).map_with(|_: TokenKind, e| e.span())
            .then_ignore(just(TokenKind::LParen)).then(expr.clone()).then_ignore(just(TokenKind::RParen))
            .then(blk.clone())
            .map(|((s, d), b): ((Span, Expr), Expr)| { let end = b.span(); Expr::Timeout(Box::new(d), Box::new(b), s.merge(end)) });

        let atom = choice((
            select! { TokenKind::IntLit(n) = e => Expr::IntLit(n, e.span()) },
            select! { TokenKind::FloatLit(n) = e => Expr::FloatLit(n, e.span()) },
            select! { TokenKind::StringLit(s) = e => Expr::StringLit(s, e.span()) },
            interp,
            just(TokenKind::True).map_with(|_: TokenKind, e| Expr::BoolLit(true, e.span())),
            just(TokenKind::False).map_with(|_: TokenKind, e| Expr::BoolLit(false, e.span())),
            let_e, ret_e,
            just(TokenKind::Break).map_with(|_: TokenKind, e| Expr::Break(e.span())),
            just(TokenKind::Continue).map_with(|_: TokenKind, e| Expr::Continue(e.span())),
            if_e, match_e, for_e, while_e, par_e, race_e, timeout_e,
            upper_e,
            select! { TokenKind::Ident(n) = e => Expr::Ident(n, e.span()) },
            paren_e, list_e, blk,
        )).boxed();

        let unary = choice((
            just(TokenKind::Minus).map_with(|_: TokenKind, e| (UnaryOp::Neg, e.span())),
            just(TokenKind::Not).map_with(|_: TokenKind, e| (UnaryOp::Not, e.span())),
        )).repeated().foldr(atom, |(op, os): (UnaryOp, Span), i: Expr| {
            let is = i.span(); Expr::Unary(op, Box::new(i), os.merge(is))
        }).boxed();

        #[derive(Clone)] enum PF { C(Vec<Expr>, Span), F(String, Span), M(String, Vec<Expr>, Span), T(Span) }
        let postfix = unary.foldl(choice((
            just(TokenKind::LParen).ignore_then(
                expr.clone().separated_by(just(TokenKind::Comma)).allow_trailing().collect::<Vec<_>>()
            ).then_ignore(just(TokenKind::RParen)).map_with(|a: Vec<Expr>, e| PF::C(a, e.span())),
            just(TokenKind::Dot).ignore_then(select! {
                TokenKind::Ident(n) = e => (n, e.span()),
                TokenKind::UpperIdent(n) = e => (n, e.span()),
            }).then(
                just(TokenKind::LParen).ignore_then(
                    expr.clone().separated_by(just(TokenKind::Comma)).allow_trailing().collect::<Vec<_>>()
                ).then_ignore(just(TokenKind::RParen))
                    .map_with(|a: Vec<Expr>, e| Some((a, e.span())))
                    .or_not().map(|o: Option<Option<_>>| o.flatten())
            ).map(|((n, ns), c): ((String, Span), Option<(Vec<Expr>, Span)>)| match c {
                Some((a, end)) => PF::M(n, a, ns.merge(end)),
                None => PF::F(n, ns),
            }),
            just(TokenKind::Question).map_with(|_: TokenKind, e| PF::T(e.span())),
        )).repeated(), |l: Expr, op: PF| {
            let ls = l.span();
            match op {
                PF::C(a, end) => Expr::Call(Box::new(l), a, ls.merge(end)),
                PF::F(n, ns) => Expr::FieldAccess(Box::new(l), n, ls.merge(ns)),
                PF::M(n, a, end) => Expr::MethodCall(Box::new(l), n, a, ls.merge(end)),
                PF::T(end) => Expr::Try(Box::new(l), ls.merge(end)),
            }
        }).boxed();

        let prod = postfix.clone().foldl(
            choice((just(TokenKind::Star).to(BinOp::Mul), just(TokenKind::Slash).to(BinOp::Div), just(TokenKind::Percent).to(BinOp::Mod)))
                .then(postfix).repeated(),
            |l: Expr, (o, r): (BinOp, Expr)| { let s = l.span().merge(r.span()); Expr::Binary(Box::new(l), o, Box::new(r), s) },
        );
        let sum = prod.clone().foldl(
            choice((just(TokenKind::Plus).to(BinOp::Add), just(TokenKind::Minus).to(BinOp::Sub))).then(prod).repeated(),
            |l: Expr, (o, r): (BinOp, Expr)| { let s = l.span().merge(r.span()); Expr::Binary(Box::new(l), o, Box::new(r), s) },
        ).boxed();
        let rng = sum.clone().then(
            choice((just(TokenKind::DotDot).to(false), just(TokenKind::DotDotEq).to(true))).then(sum).or_not()
        ).map(|(l, r): (Expr, Option<(bool, Expr)>)| match r {
            Some((inc, end)) => { let ls = l.span(); let es = end.span(); Expr::Range(Box::new(l), Box::new(end), inc, ls.merge(es)) },
            None => l,
        });
        let cmp = rng.clone().foldl(
            choice((just(TokenKind::Eq).to(BinOp::Eq), just(TokenKind::NotEq).to(BinOp::NotEq),
                just(TokenKind::Lt).to(BinOp::Lt), just(TokenKind::Gt).to(BinOp::Gt),
                just(TokenKind::LtEq).to(BinOp::LtEq), just(TokenKind::GtEq).to(BinOp::GtEq)))
                .then(rng).repeated(),
            |l: Expr, (o, r): (BinOp, Expr)| { let s = l.span().merge(r.span()); Expr::Binary(Box::new(l), o, Box::new(r), s) },
        );
        let and = cmp.clone().foldl(just(TokenKind::And).ignore_then(cmp).repeated(),
            |l: Expr, r: Expr| { let s = l.span().merge(r.span()); Expr::Binary(Box::new(l), BinOp::And, Box::new(r), s) });
        let or = and.clone().foldl(just(TokenKind::Or).ignore_then(and).repeated(),
            |l: Expr, r: Expr| { let s = l.span().merge(r.span()); Expr::Binary(Box::new(l), BinOp::Or, Box::new(r), s) }).boxed();

        #[derive(Clone)] enum IR { P(Expr), PM(String, Vec<Expr>, Span), W(Vec<((String, Span), Expr)>, Span), A(Expr) }
        or.clone().foldl(choice((
            just(TokenKind::Pipeline).ignore_then(
                just(TokenKind::Dot).ignore_then(select! {
                    TokenKind::Ident(n) = e => (n, e.span()),
                    TokenKind::UpperIdent(n) = e => (n, e.span()),
                }).then(just(TokenKind::LParen).ignore_then(
                    expr.clone().separated_by(just(TokenKind::Comma)).allow_trailing().collect::<Vec<_>>()
                ).then_ignore(just(TokenKind::RParen)).or_not())
                .map(|((m, ms), a): ((String, Span), Option<Vec<Expr>>)| IR::PM(m, a.unwrap_or_default(), ms))
                .or(or.clone().map(IR::P))
            ),
            just(TokenKind::With).ignore_then(just(TokenKind::LBrace)).ignore_then(
                select! { TokenKind::Ident(n) = e => (n, e.span()) }.then_ignore(just(TokenKind::Colon)).then(expr.clone())
                    .separated_by(just(TokenKind::Comma)).allow_trailing().collect::<Vec<_>>()
            ).then_ignore(just(TokenKind::RBrace)).map_with(|fs: Vec<((String, Span), Expr)>, e| IR::W(fs, e.span())),
            just(TokenKind::Assign).ignore_then(or).map(IR::A),
        )).repeated(), |l: Expr, r: IR| {
            let ls = l.span();
            match r {
                IR::P(r) => { let s = ls.merge(r.span()); Expr::Pipeline(Box::new(l), Box::new(r), s) }
                IR::PM(m, a, ms) => Expr::MethodCall(Box::new(l), m, a, ls.merge(ms)),
                IR::W(fs, end) => {
                    let fl = fs.into_iter().map(|((n, _), v)| (n, v)).collect();
                    Expr::With(Box::new(l), fl, ls.merge(end))
                }
                IR::A(r) => { let s = ls.merge(r.span()); Expr::Assign(Box::new(l), Box::new(r), s) }
            }
        })
    }).boxed()
}

/// Expression parser that excludes top-level assignment, pipeline, and with.
/// Used for requires/ensures contract clauses where `=` terminates the clause.
fn contract_expr_p<'t, I: ValueInput<'t, Token=TokenKind, Span=Span> + 't>()
    -> Boxed<'t, 't, I, Expr, E<'t>>
{
    // Collect tokens that aren't Requires/Ensures/Assign keywords (contract terminators),
    // then re-parse them as an expression using the full parser.
    any().and_is(
        just(TokenKind::Requires).not()
            .then(just(TokenKind::Ensures).not())
            .then(just(TokenKind::Assign).not())
    ).map_with(|tok: TokenKind, e| (tok, e.span()))
    .repeated().at_least(1).collect::<Vec<(TokenKind, Span)>>()
    .try_map(|tokens: Vec<(TokenKind, Span)>, span| {
        let eoi = tokens.last().map(|(_, s)| Span::new(s.file_id, s.end, s.end)).unwrap_or(span);
        let stream = Stream::from_iter(tokens).map(eoi, |t: (TokenKind, Span)| t);
        expr_p().parse(stream).into_result()
            .map_err(|errs| Rich::custom(span, format!("contract expression error: {:?}", errs)))
    }).boxed()
}

// Item parsers
fn fn_def_p<'t, I: ValueInput<'t, Token=TokenKind, Span=Span> + 't>(is_pub: bool, is_async: bool)
    -> Boxed<'t, 't, I, Item, E<'t>>
{
    just(TokenKind::Def).map_with(|_: TokenKind, e| -> Span { e.span() })
        .then(select! { TokenKind::Ident(n) = e => (n, e.span()) })
        .then_ignore(just(TokenKind::LParen))
        .then(param_p().separated_by(just(TokenKind::Comma)).allow_trailing().collect::<Vec<_>>())
        .then_ignore(just(TokenKind::RParen))
        .then(just(TokenKind::Arrow).ignore_then(type_expr_p()).or_not())
        .boxed()
        .then(just(TokenKind::LBracket).ignore_then(
            effect_ref_p().separated_by(just(TokenKind::Comma)).allow_trailing().collect::<Vec<_>>()
        ).then_ignore(just(TokenKind::RBracket)).or_not().map(|o: Option<Vec<EffectRef>>| o.unwrap_or_default()))
        .then(just(TokenKind::Requires).ignore_then(contract_expr_p()).repeated().collect::<Vec<_>>())
        .then(just(TokenKind::Ensures).ignore_then(contract_expr_p()).repeated().collect::<Vec<_>>())
        .then_ignore(just(TokenKind::Assign))
        .then(expr_p())
        .map(move |(((((((s, (n, _)), prms), ret), efx), req), ens), body): (((((((Span, (String, Span)), Vec<Param>), Option<TypeExpr>), Vec<EffectRef>), Vec<Expr>), Vec<Expr>), Expr)| {
            Item::Function(FnDef { name: n, params: prms, return_type: ret, effects: efx,
                requires: req, ensures: ens, body: body.clone(), is_pub, is_async, span: s.merge(body.span()) })
        }).boxed()
}

fn param_p<'t, I: ValueInput<'t, Token=TokenKind, Span=Span> + 't>()
    -> impl Parser<'t, I, Param, E<'t>> + Clone
{
    select! { TokenKind::Ident(n) = e => (n, e.span()) }
        .then(just(TokenKind::Colon).ignore_then(type_expr_p()).or_not())
        .map(|((n, s), ty): ((String, Span), Option<TypeExpr>)| {
            let end = ty.as_ref().map(|t| t.span()).unwrap_or(s);
            Param { name: n, ty, span: s.merge(end) }
        })
}

fn type_def_p<'t, I: ValueInput<'t, Token=TokenKind, Span=Span> + 't>(is_pub: bool)
    -> Boxed<'t, 't, I, Item, E<'t>>
{
    just(TokenKind::Type).map_with(|_: TokenKind, e| e.span())
        .then(select! { TokenKind::UpperIdent(n) = e => (n, e.span()) })
        .then(select! { TokenKind::Ident(n) => n }.repeated().collect::<Vec<_>>())
        .then_ignore(just(TokenKind::Assign))
        .then(type_body_p())
        .map_with(move |(((s, (n, _)), tps), kind): (((Span, (String, Span)), Vec<String>), TypeDefKind), e| {
            Item::TypeDef(TypeDef { name: n, type_params: tps, kind, is_pub, span: s.merge(e.span()) })
        }).boxed()
}

fn type_body_p<'t, I: ValueInput<'t, Token=TokenKind, Span=Span> + 't>()
    -> Boxed<'t, 't, I, TypeDefKind, E<'t>>
{
    choice((
        // Struct: { field: Type, ... }
        just(TokenKind::LBrace).ignore_then(
            select! { TokenKind::Ident(n) = e => (n, e.span()) }.then_ignore(just(TokenKind::Colon)).then(type_expr_p())
                .map(|((n, fs), ty): ((String, Span), TypeExpr)| Field { name: n, ty: ty.clone(), span: fs.merge(ty.span()) })
                .separated_by(just(TokenKind::Comma)).allow_trailing().collect::<Vec<_>>()
        ).then_ignore(just(TokenKind::RBrace)).map(TypeDefKind::Struct),
        // Refined: TypeExpr where constraint (must have `where`)
        type_expr_p().then_ignore(just(TokenKind::Where)).then(expr_p())
            .map(|(ty, c)| TypeDefKind::Refined { base_type: ty, constraint: c }),
        // Sum with pipe: Variant1 payload | Variant2 payload (multiple variants)
        sum_variants_p(),
        // Single-variant sum: UpperIdent [payloads] (non-primitive name)
        single_variant_sum_p(),
        // Alias (no `where`, no `|`): TypeExpr
        type_expr_p().map(TypeDefKind::Alias),
    )).boxed()
}

/// Helper: type atom for sum variant payloads.
/// Matches UpperIdent, lowercase ident (not followed by `:`), or `(type_expr)`.
fn variant_type_atom<'t, I: ValueInput<'t, Token=TokenKind, Span=Span> + 't>()
    -> impl Parser<'t, I, TypeExpr, E<'t>> + Clone
{
    choice((
        select! { TokenKind::UpperIdent(n) = e => TypeExpr::Named(n, e.span()) },
        // Lowercase ident as payload, but NOT if followed by `:` (type annotation boundary)
        select! { TokenKind::Ident(n) = e => (n, e.span()) }
            .then(any().rewind())
            .try_map(|((n, s), next): ((String, Span), TokenKind), span| {
                if matches!(next, TokenKind::Colon) {
                    Err(Rich::custom(span, "not a variant payload"))
                } else {
                    Ok(TypeExpr::Named(n, s))
                }
            }),
        just(TokenKind::LParen).ignore_then(type_expr_p()).then_ignore(just(TokenKind::RParen)),
    ))
}

fn sum_variants_p<'t, I: ValueInput<'t, Token=TokenKind, Span=Span> + 't>()
    -> impl Parser<'t, I, TypeDefKind, E<'t>> + Clone
{
    let ta = variant_type_atom::<I>();
    let variant = select! { TokenKind::UpperIdent(n) = e => (n, e.span()) }
        .then(ta.repeated().collect::<Vec<_>>())
        .map(|((n, vs), fs): ((String, Span), Vec<TypeExpr>)| {
            let ve = fs.last().map(|f| f.span()).unwrap_or(vs);
            Variant { name: n, fields: fs, span: vs.merge(ve) }
        });
    // Multi-variant sum (at least one `|`)
    variant.clone()
        .then(just(TokenKind::Pipe).ignore_then(variant).repeated().at_least(1).collect::<Vec<_>>())
        .map(|(first, rest): (Variant, Vec<Variant>)| {
            let mut vs = vec![first];
            vs.extend(rest);
            TypeDefKind::Sum(vs)
        })
}

/// Single-variant sum type (e.g., `type IoError = IoFailed String`).
/// Only matches when the UpperIdent is NOT a known primitive type.
fn single_variant_sum_p<'t, I: ValueInput<'t, Token=TokenKind, Span=Span> + 't>()
    -> impl Parser<'t, I, TypeDefKind, E<'t>> + Clone
{
    let ta = variant_type_atom::<I>();
    select! { TokenKind::UpperIdent(n) = e => (n, e.span()) }
        .try_map(|(n, s): (String, Span), span| {
            // Known primitive type names are aliases, not sum variants
            let known = matches!(n.as_str(),
                "Int" | "Int8" | "Int16" | "Int32" | "Int64"
                | "UInt" | "UInt8" | "UInt16" | "UInt32" | "UInt64"
                | "Float32" | "Float64" | "Decimal" | "BigDecimal"
                | "Bool" | "Char" | "String" | "Unit"
                | "List" | "Option" | "Result" | "Map" | "Set"
            );
            if known { Err(Rich::custom(span, "known type name, not a variant")) }
            else { Ok((n, s)) }
        })
        .then(ta.repeated().collect::<Vec<_>>())
        .map(|((n, vs), fs): ((String, Span), Vec<TypeExpr>)| {
            let ve = fs.last().map(|f| f.span()).unwrap_or(vs);
            TypeDefKind::Sum(vec![Variant { name: n, fields: fs, span: vs.merge(ve) }])
        })
}

fn concept_def_p<'t, I: ValueInput<'t, Token=TokenKind, Span=Span> + 't>(is_pub: bool)
    -> Boxed<'t, 't, I, Item, E<'t>>
{
    just(TokenKind::Concept).map_with(|_: TokenKind, e| -> Span { e.span() })
        .then(select! { TokenKind::Ident(n) = e => (n, e.span()), TokenKind::UpperIdent(n) = e => (n, e.span()) }.map(|(n, _): (String, Span)| n))
        .then(just(TokenKind::LParen).ignore_then(
            any().and_is(just(TokenKind::RParen).not()).repeated().collect::<Vec<TokenKind>>()
        ).then_ignore(just(TokenKind::RParen)).or_not())
        .then(just(TokenKind::Colon).ignore_then(
            select! { TokenKind::Ident(n) => n, TokenKind::UpperIdent(n) => n }
                .separated_by(just(TokenKind::Comma)).collect::<Vec<_>>()
        ).or_not().map(|o: Option<Vec<String>>| o.unwrap_or_default()))
        .boxed()
        .then_ignore(just(TokenKind::LBrace))
        .then(concept_body_p())
        .then_ignore(just(TokenKind::RBrace))
        .map_with(move |((((s, n), _), supers), (at, ms)): ((((Span, String), Option<Vec<TokenKind>>), Vec<String>), (Vec<AssocTypeDecl>, Vec<ConceptMethodSig>)), e| {
            Item::ConceptDef(ConceptDef { name: n, supers, assoc_types: at, methods: ms, is_pub, span: s.merge(e.span()) })
        }).boxed()
}

fn concept_body_p<'t, I: ValueInput<'t, Token=TokenKind, Span=Span> + 't>()
    -> Boxed<'t, 't, I, (Vec<AssocTypeDecl>, Vec<ConceptMethodSig>), E<'t>>
{
    let at = just(TokenKind::Type).map_with(|_: TokenKind, e| e.span())
        .then(select! { TokenKind::Ident(n) = e => (n, e.span()), TokenKind::UpperIdent(n) = e => (n, e.span()) })
        .then(just(TokenKind::Assign).ignore_then(type_expr_p()).or_not())
        .map(|((s, (n, _)), def): ((Span, (String, Span)), Option<TypeExpr>)| {
            let end = def.as_ref().map(|t| t.span()).unwrap_or(s);
            AssocTypeDecl { name: n, default: def, span: s.merge(end) }
        });
    let ms = just(TokenKind::Def).map_with(|_: TokenKind, e| e.span())
        .then(select! { TokenKind::Ident(n) = e => (n, e.span()) })
        .then_ignore(just(TokenKind::LParen))
        .then(param_p().separated_by(just(TokenKind::Comma)).allow_trailing().collect::<Vec<_>>())
        .then_ignore(just(TokenKind::RParen))
        .then(just(TokenKind::Arrow).ignore_then(type_expr_p()).or_not())
        .then(just(TokenKind::Assign).ignore_then(expr_p()).or_not())
        .map(|((((s, (n, _)), prms), ret), db): ((((Span, (String, Span)), Vec<Param>), Option<TypeExpr>), Option<Expr>)| {
            let end = db.as_ref().map(|e| e.span()).or_else(|| ret.as_ref().map(|t| t.span())).unwrap_or(s);
            ConceptMethodSig { name: n, params: prms, return_type: ret, default_body: db, span: s.merge(end) }
        });
    #[derive(Clone)] enum CI { A(AssocTypeDecl), M(ConceptMethodSig) }
    choice((at.map(CI::A), ms.map(CI::M)))
        .then_ignore(just(TokenKind::Semicolon).or(just(TokenKind::Comma)).or_not())
        .repeated().collect::<Vec<_>>()
        .map(|items| {
            let (mut a, mut m) = (Vec::new(), Vec::new());
            for i in items { match i { CI::A(x) => a.push(x), CI::M(x) => m.push(x) } }
            (a, m)
        }).boxed()
}

fn instance_def_p<'t, I: ValueInput<'t, Token=TokenKind, Span=Span> + 't>(is_pub: bool)
    -> Boxed<'t, 't, I, Item, E<'t>>
{
    just(TokenKind::Instance).map_with(|_: TokenKind, e| -> Span { e.span() })
        .then(choice((
            select! { TokenKind::Ident(n) => n, TokenKind::UpperIdent(n) => n }
                .then_ignore(just(TokenKind::For)).then(type_expr_p())
                .map(|(c, ft)| InstanceKind::Concept { concept: c, for_type: ft }),
            type_expr_p().map(InstanceKind::Inherent),
        )))
        .then_ignore(just(TokenKind::LBrace))
        .then(instance_body_p())
        .then_ignore(just(TokenKind::RBrace))
        .map_with(move |((s, kind), (at, ms)): ((Span, InstanceKind), (Vec<AssocTypeBinding>, Vec<MethodDef>)), e| {
            Item::InstanceDef(InstanceDef { kind, assoc_types: at, methods: ms, is_pub, span: s.merge(e.span()) })
        }).boxed()
}

fn instance_body_p<'t, I: ValueInput<'t, Token=TokenKind, Span=Span> + 't>()
    -> Boxed<'t, 't, I, (Vec<AssocTypeBinding>, Vec<MethodDef>), E<'t>>
{
    let atb = just(TokenKind::Type).map_with(|_: TokenKind, e| e.span())
        .then(select! { TokenKind::Ident(n) = e => (n, e.span()), TokenKind::UpperIdent(n) = e => (n, e.span()) })
        .then_ignore(just(TokenKind::Assign)).then(type_expr_p())
        .map(|((s, (n, _)), ty): ((Span, (String, Span)), TypeExpr)| {
            AssocTypeBinding { name: n, ty: ty.clone(), span: s.merge(ty.span()) }
        });
    let md = just(TokenKind::Def).map_with(|_: TokenKind, e| e.span())
        .then(select! { TokenKind::Ident(n) = e => (n, e.span()) })
        .then_ignore(just(TokenKind::LParen))
        .then(param_p().separated_by(just(TokenKind::Comma)).allow_trailing().collect::<Vec<_>>())
        .then_ignore(just(TokenKind::RParen))
        .then(just(TokenKind::Arrow).ignore_then(type_expr_p()).or_not())
        .then_ignore(just(TokenKind::Assign)).then(expr_p())
        .map(|((((s, (n, _)), prms), ret), body): ((((Span, (String, Span)), Vec<Param>), Option<TypeExpr>), Expr)| {
            MethodDef { name: n, params: prms, return_type: ret, body: body.clone(), span: s.merge(body.span()) }
        });
    #[derive(Clone)] enum II { A(AssocTypeBinding), M(MethodDef) }
    choice((atb.map(II::A), md.map(II::M)))
        .then_ignore(just(TokenKind::Semicolon).or(just(TokenKind::Comma)).or_not())
        .repeated().collect::<Vec<_>>()
        .map(|items| {
            let (mut a, mut m) = (Vec::new(), Vec::new());
            for i in items { match i { II::A(x) => a.push(x), II::M(x) => m.push(x) } }
            (a, m)
        }).boxed()
}

fn use_decl_p<'t, I: ValueInput<'t, Token=TokenKind, Span=Span>>()
    -> impl Parser<'t, I, Item, E<'t>> + Clone
{
    #[derive(Clone)] enum US { S(String), G(Vec<String>) }
    just(TokenKind::Use).map_with(|_: TokenKind, e| e.span())
        .then(select! { TokenKind::Ident(n) => n, TokenKind::UpperIdent(n) => n })
        .then(just(TokenKind::ColonColon).ignore_then(choice((
            just(TokenKind::LBrace).ignore_then(
                select! { TokenKind::Ident(n) => n, TokenKind::UpperIdent(n) => n }
                    .separated_by(just(TokenKind::Comma)).collect::<Vec<_>>()
            ).then_ignore(just(TokenKind::RBrace)).map(US::G),
            select! { TokenKind::Ident(n) => n, TokenKind::UpperIdent(n) => n }.map(US::S),
        ))).repeated().collect::<Vec<_>>())
        .map_with(|((s, first), segs): ((Span, String), Vec<US>), e| {
            let mut path = vec![first]; let mut items = None;
            for seg in segs { match seg { US::S(n) => path.push(n), US::G(ns) => { items = Some(ns); break; } } }
            Item::Use(UseDecl { path, items, span: s.merge(e.span()) })
        })
}

fn module_decl_p<'t, I: ValueInput<'t, Token=TokenKind, Span=Span>>()
    -> impl Parser<'t, I, Item, E<'t>> + Clone
{
    just(TokenKind::Module).map_with(|_: TokenKind, e| e.span())
        .then(select! { TokenKind::Ident(n) = e => (n, e.span()) })
        .map_with(|(s, (n, _)): (Span, (String, Span)), e| {
            Item::ModuleDecl(ModuleDecl { name: n, span: s.merge(e.span()) })
        })
}

fn type_ann_p<'t, I: ValueInput<'t, Token=TokenKind, Span=Span> + 't>()
    -> impl Parser<'t, I, Item, E<'t>> + Clone
{
    select! { TokenKind::Ident(n) = e => (n, e.span()) }
        .then_ignore(just(TokenKind::Colon)).then(type_expr_p())
        .map(|((n, s), ty): ((String, Span), TypeExpr)| {
            Item::TypeAnnotation(TypeAnnotation { name: n, ty: ty.clone(), span: s.merge(ty.span()) })
        })
}

fn item_p<'t, I: ValueInput<'t, Token=TokenKind, Span=Span> + 't>()
    -> impl Parser<'t, I, Item, E<'t>> + Clone
{
    choice((
        just(TokenKind::Pub).ignore_then(just(TokenKind::Async)).ignore_then(fn_def_p(true, true)),
        just(TokenKind::Pub).ignore_then(fn_def_p(true, false)),
        just(TokenKind::Pub).ignore_then(type_def_p(true)),
        just(TokenKind::Pub).ignore_then(concept_def_p(true)),
        just(TokenKind::Pub).ignore_then(instance_def_p(true)),
        just(TokenKind::Async).ignore_then(fn_def_p(false, true)),
        fn_def_p(false, false),
        type_def_p(false),
        concept_def_p(false),
        instance_def_p(false),
        use_decl_p().boxed(),
        module_decl_p().boxed(),
        type_ann_p().boxed(),
    ))
}

fn module_p<'t, I: ValueInput<'t, Token=TokenKind, Span=Span> + 't>()
    -> impl Parser<'t, I, Module, E<'t>>
{
    item_p().repeated().collect::<Vec<_>>()
        .map_with(|items: Vec<Item>, e| Module { name: None, items, span: e.span() })
}

fn expr_contains_try(e: &Expr) -> bool {
    match e {
        Expr::Try(_, _) => true,
        Expr::Binary(l, _, r, _) | Expr::Pipeline(l, r, _) => expr_contains_try(l) || expr_contains_try(r),
        Expr::Unary(_, i, _) | Expr::FieldAccess(i, _, _) => expr_contains_try(i),
        Expr::Block(es, _) => es.iter().any(expr_contains_try),
        Expr::Call(c, a, _) | Expr::MethodCall(c, _, a, _) => expr_contains_try(c) || a.iter().any(expr_contains_try),
        Expr::Lambda(_, _, b, _) => expr_contains_try(b),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::*;

    fn parse_expr_str(input: &str) -> Expr {
        let wrapped = format!("def __test__() -> Int = {input}");
        let module = super::parse(&wrapped, 0).unwrap();
        match &module.items[0] {
            Item::Function(f) => f.body.clone(),
            _ => panic!("expected function"),
        }
    }

    fn parse_module_str(input: &str) -> Module {
        super::parse(input, 0).unwrap()
    }

    #[test]
    fn test_parse_int_literal() {
        let expr = parse_expr_str("42");
        assert!(matches!(expr, Expr::IntLit(42, _)));
    }

    #[test]
    fn test_parse_binary_ops() {
        let expr = parse_expr_str("1 + 2 * 3");
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
}
