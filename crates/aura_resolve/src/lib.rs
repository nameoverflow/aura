pub mod resolver;
pub mod scope;

pub use resolver::{DefId, DefInfo, DefKind, ResolveError, ResolvedModule, Resolver};
pub use scope::Scope;
