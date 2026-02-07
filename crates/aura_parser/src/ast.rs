use aura_common::Span;

/// Unique identifier for definitions
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(pub u32);

/// A complete module (source file)
#[derive(Debug, Clone)]
pub struct Module {
    pub name: Option<String>,
    pub items: Vec<Item>,
    pub span: Span,
}

/// Top-level items
#[derive(Debug, Clone)]
pub enum Item {
    Function(FnDef),
    TypeDef(TypeDef),
    ConceptDef(ConceptDef),
    InstanceDef(InstanceDef),
    Use(UseDecl),
    ModuleDecl(ModuleDecl),
    TypeAnnotation(TypeAnnotation),
}

/// Standalone type annotation: `name: Type`
#[derive(Debug, Clone)]
pub struct TypeAnnotation {
    pub name: String,
    pub ty: TypeExpr,
    pub span: Span,
}

/// Function definition
#[derive(Debug, Clone)]
pub struct FnDef {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Option<TypeExpr>,
    pub body: Expr,
    pub is_pub: bool,
    pub is_async: bool,
    pub span: Span,
}

/// Function parameter
#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub ty: Option<TypeExpr>,
    pub span: Span,
}

/// Type definition: sum types and structs
#[derive(Debug, Clone)]
pub struct TypeDef {
    pub name: String,
    pub type_params: Vec<String>,
    pub kind: TypeDefKind,
    pub is_pub: bool,
    pub span: Span,
}

/// Concept definition
#[derive(Debug, Clone)]
pub struct ConceptDef {
    pub name: String,
    pub supers: Vec<String>,
    pub assoc_types: Vec<AssocTypeDecl>,
    pub methods: Vec<ConceptMethodSig>,
    pub is_pub: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct AssocTypeDecl {
    pub name: String,
    pub default: Option<TypeExpr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ConceptMethodSig {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Option<TypeExpr>,
    pub default_body: Option<Expr>,
    pub span: Span,
}

/// Instance definition
#[derive(Debug, Clone)]
pub struct InstanceDef {
    pub kind: InstanceKind,
    pub assoc_types: Vec<AssocTypeBinding>,
    pub methods: Vec<MethodDef>,
    pub is_pub: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum InstanceKind {
    Inherent(TypeExpr),
    Concept { concept: String, for_type: TypeExpr },
}

#[derive(Debug, Clone)]
pub struct AssocTypeBinding {
    pub name: String,
    pub ty: TypeExpr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MethodDef {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Option<TypeExpr>,
    pub body: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TypeDefKind {
    Sum(Vec<Variant>),
    Struct(Vec<Field>),
    Refined {
        base_type: TypeExpr,
        constraint: Expr,
    },
}

#[derive(Debug, Clone)]
pub struct Variant {
    pub name: String,
    pub fields: Vec<TypeExpr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: String,
    pub ty: TypeExpr,
    pub span: Span,
}

/// Use declaration
#[derive(Debug, Clone)]
pub struct UseDecl {
    pub path: Vec<String>,
    pub items: Option<Vec<String>>,
    pub span: Span,
}

/// Module declaration
#[derive(Debug, Clone)]
pub struct ModuleDecl {
    pub name: String,
    pub span: Span,
}

/// Type expressions
#[derive(Debug, Clone)]
pub enum TypeExpr {
    Named(String, Span),
    App(Box<TypeExpr>, Vec<TypeExpr>, Span),
    Product(Vec<TypeExpr>, Span),
    Function(Vec<TypeExpr>, Box<TypeExpr>, Span),
    Forall(Vec<ConceptConstraint>, Box<TypeExpr>, Span),
    Unit(Span),
}

#[derive(Debug, Clone)]
pub struct ConceptConstraint {
    pub concept: String,
    pub ty_var: String,
    pub span: Span,
}

impl TypeExpr {
    pub fn span(&self) -> Span {
        match self {
            TypeExpr::Named(_, s) => *s,
            TypeExpr::App(_, _, s) => *s,
            TypeExpr::Product(_, s) => *s,
            TypeExpr::Function(_, _, s) => *s,
            TypeExpr::Forall(_, _, s) => *s,
            TypeExpr::Unit(s) => *s,
        }
    }
}

/// Expressions
#[derive(Debug, Clone)]
pub enum Expr {
    IntLit(i64, Span),
    FloatLit(f64, Span),
    StringLit(String, Span),
    StringInterp(Vec<StringPart>, Span),
    BoolLit(bool, Span),
    Ident(String, Span),
    QualifiedIdent(String, String, Span),

    Binary(Box<Expr>, BinOp, Box<Expr>, Span),
    Unary(UnaryOp, Box<Expr>, Span),
    Pipeline(Box<Expr>, Box<Expr>, Span),

    Block(Vec<Expr>, Span),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>, Span),
    Match(Box<Expr>, Vec<MatchArm>, Span),
    For(String, Box<Expr>, Box<Expr>, Span),
    ForPattern(Pattern, Box<Expr>, Box<Expr>, Span),
    While(Box<Expr>, Box<Expr>, Span),

    Let(String, bool, Option<TypeExpr>, Box<Expr>, Span),
    LetPattern(Pattern, bool, Option<TypeExpr>, Box<Expr>, Span),
    Assign(Box<Expr>, Box<Expr>, Span),
    Return(Option<Box<Expr>>, Span),
    Break(Span),
    Continue(Span),

    Call(Box<Expr>, Vec<Expr>, Span),
    MethodCall(Box<Expr>, String, Vec<Expr>, Span),
    FieldAccess(Box<Expr>, String, Span),
    Lambda(Vec<Param>, Option<TypeExpr>, Box<Expr>, Span),

    StructLit(String, Vec<(String, Expr)>, Span),
    With(Box<Expr>, Vec<(String, Expr)>, Span),
    ListLit(Vec<Expr>, Span),
    TupleLit(Vec<Expr>, Span),
    Try(Box<Expr>, Span),
    Range(Box<Expr>, Box<Expr>, bool, Span), // inclusive flag

    Unit(Span),
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::IntLit(_, s) => *s,
            Expr::FloatLit(_, s) => *s,
            Expr::StringLit(_, s) => *s,
            Expr::StringInterp(_, s) => *s,
            Expr::BoolLit(_, s) => *s,
            Expr::Ident(_, s) => *s,
            Expr::QualifiedIdent(_, _, s) => *s,
            Expr::Binary(_, _, _, s) => *s,
            Expr::Unary(_, _, s) => *s,
            Expr::Pipeline(_, _, s) => *s,
            Expr::Block(_, s) => *s,
            Expr::If(_, _, _, s) => *s,
            Expr::Match(_, _, s) => *s,
            Expr::For(_, _, _, s) => *s,
            Expr::ForPattern(_, _, _, s) => *s,
            Expr::While(_, _, s) => *s,
            Expr::Let(_, _, _, _, s) => *s,
            Expr::LetPattern(_, _, _, _, s) => *s,
            Expr::Assign(_, _, s) => *s,
            Expr::Return(_, s) => *s,
            Expr::Break(s) => *s,
            Expr::Continue(s) => *s,
            Expr::Call(_, _, s) => *s,
            Expr::MethodCall(_, _, _, s) => *s,
            Expr::FieldAccess(_, _, s) => *s,
            Expr::Lambda(_, _, _, s) => *s,
            Expr::StructLit(_, _, s) => *s,
            Expr::With(_, _, s) => *s,
            Expr::ListLit(_, s) => *s,
            Expr::TupleLit(_, s) => *s,
            Expr::Try(_, s) => *s,
            Expr::Range(_, _, _, s) => *s,
            Expr::Unit(s) => *s,
        }
    }
}

#[derive(Debug, Clone)]
pub struct StringPart {
    pub kind: StringPartKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum StringPartKind {
    Literal(String),
    Expr(Expr),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    NotEq,
    Lt,
    Gt,
    LtEq,
    GtEq,
    And,
    Or,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<Expr>,
    pub body: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Wildcard(Span),
    Ident(String, Span),
    Literal(LitPattern, Span),
    Constructor(String, Vec<Pattern>, Span),
    Tuple(Vec<Pattern>, Span),
    Struct(String, Vec<FieldPattern>, bool, Span),
    Or(Vec<Pattern>, Span),
}

#[derive(Debug, Clone)]
pub struct FieldPattern {
    pub name: String,
    pub pattern: Pattern,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum LitPattern {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
}
