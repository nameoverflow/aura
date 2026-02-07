pub mod resolve;
pub mod scope;

pub use resolve::{Resolver, ResolveError, ResolvedModule, DefId, DefKind};
pub use scope::Scope;
