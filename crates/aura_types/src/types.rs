use std::fmt;

/// Unique identifier for type variables.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVarId(pub u32);

/// Core type representation for Aura.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    // Primitives
    Int,
    Int8,
    Int16,
    Int32,
    Int64,
    UInt,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Float32,
    Float64,
    Decimal,
    BigDecimal,
    Bool,
    Char,
    String,
    Unit,

    /// Type variable (unknown, to be solved by inference)
    Var(TypeVarId),

    /// Named user type (e.g., User, Status) with type args
    Named(String, Vec<Type>),

    /// Product type: A * B * C
    Product(Vec<Type>),

    /// Function type: (params) -> return
    Function(Vec<Type>, Box<Type>),

    /// Error sentinel for recovery
    Error,
}

impl Type {
    pub fn is_numeric(&self) -> bool {
        matches!(
            self,
            Type::Int
                | Type::Int8
                | Type::Int16
                | Type::Int32
                | Type::Int64
                | Type::UInt
                | Type::UInt8
                | Type::UInt16
                | Type::UInt32
                | Type::UInt64
                | Type::Float32
                | Type::Float64
                | Type::Decimal
                | Type::BigDecimal
        )
    }

    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            Type::Int
                | Type::Int8
                | Type::Int16
                | Type::Int32
                | Type::Int64
                | Type::UInt
                | Type::UInt8
                | Type::UInt16
                | Type::UInt32
                | Type::UInt64
        )
    }

    pub fn is_float(&self) -> bool {
        matches!(
            self,
            Type::Float32 | Type::Float64 | Type::Decimal | Type::BigDecimal
        )
    }

    /// Resolve a type name to its primitive Type, if applicable.
    pub fn from_name(name: &str) -> Option<Type> {
        match name {
            "Int" => Some(Type::Int),
            "Int8" => Some(Type::Int8),
            "Int16" => Some(Type::Int16),
            "Int32" => Some(Type::Int32),
            "Int64" => Some(Type::Int64),
            "UInt" => Some(Type::UInt),
            "UInt8" => Some(Type::UInt8),
            "UInt16" => Some(Type::UInt16),
            "UInt32" => Some(Type::UInt32),
            "UInt64" => Some(Type::UInt64),
            "Float32" => Some(Type::Float32),
            "Float64" => Some(Type::Float64),
            "Decimal" => Some(Type::Decimal),
            "BigDecimal" => Some(Type::BigDecimal),
            "Bool" => Some(Type::Bool),
            "Char" => Some(Type::Char),
            "String" => Some(Type::String),
            "Unit" => Some(Type::Unit),
            _ => None,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int => write!(f, "Int"),
            Type::Int8 => write!(f, "Int8"),
            Type::Int16 => write!(f, "Int16"),
            Type::Int32 => write!(f, "Int32"),
            Type::Int64 => write!(f, "Int64"),
            Type::UInt => write!(f, "UInt"),
            Type::UInt8 => write!(f, "UInt8"),
            Type::UInt16 => write!(f, "UInt16"),
            Type::UInt32 => write!(f, "UInt32"),
            Type::UInt64 => write!(f, "UInt64"),
            Type::Float32 => write!(f, "Float32"),
            Type::Float64 => write!(f, "Float64"),
            Type::Decimal => write!(f, "Decimal"),
            Type::BigDecimal => write!(f, "BigDecimal"),
            Type::Bool => write!(f, "Bool"),
            Type::Char => write!(f, "Char"),
            Type::String => write!(f, "String"),
            Type::Unit => write!(f, "()"),
            Type::Var(id) => write!(f, "?{}", id.0),
            Type::Named(name, args) => {
                write!(f, "{name}")?;
                for arg in args {
                    write!(f, " {arg}")?;
                }
                Ok(())
            }
            Type::Product(types) => {
                for (i, t) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, " * ")?;
                    }
                    write!(f, "{t}")?;
                }
                Ok(())
            }
            Type::Function(params, ret) => {
                if params.len() == 1 {
                    write!(f, "{}", params[0])?;
                } else {
                    write!(f, "(")?;
                    for (i, p) in params.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{p}")?;
                    }
                    write!(f, ")")?;
                }
                write!(f, " -> {ret}")
            }
            Type::Error => write!(f, "<error>"),
        }
    }
}
