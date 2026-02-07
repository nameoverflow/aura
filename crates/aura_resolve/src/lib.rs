pub mod resolve;
pub mod scope;

pub use resolve::{DefId, DefKind, ResolveError, ResolvedModule, Resolver};
pub use scope::Scope;
