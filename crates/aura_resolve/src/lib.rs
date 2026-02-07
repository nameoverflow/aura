pub mod resolver;
pub mod scope;

pub use resolver::{DefId, DefKind, DefInfo, ResolveError, ResolvedModule, Resolver};
pub use scope::Scope;
