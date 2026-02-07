use std::collections::HashMap;

/// A unique identifier for a definition in the program.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefId(pub u32);

/// Lexical scope tracking.
/// Supports nested scopes with shadowing.
#[derive(Debug)]
pub struct Scope {
    /// Stack of scope frames. Each frame maps names to DefIds.
    frames: Vec<HashMap<String, DefId>>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            frames: vec![HashMap::new()], // global scope
        }
    }

    pub fn push(&mut self) {
        self.frames.push(HashMap::new());
    }

    pub fn pop(&mut self) {
        assert!(self.frames.len() > 1, "cannot pop global scope");
        self.frames.pop();
    }

    pub fn define(&mut self, name: String, id: DefId) -> Option<DefId> {
        self.frames.last_mut().unwrap().insert(name, id)
    }

    pub fn lookup(&self, name: &str) -> Option<DefId> {
        // Search from innermost to outermost
        for frame in self.frames.iter().rev() {
            if let Some(&id) = frame.get(name) {
                return Some(id);
            }
        }
        None
    }

    pub fn depth(&self) -> usize {
        self.frames.len()
    }
}

impl Default for Scope {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scope_basic() {
        let mut scope = Scope::new();
        scope.define("x".into(), DefId(0));
        assert_eq!(scope.lookup("x"), Some(DefId(0)));
        assert_eq!(scope.lookup("y"), None);
    }

    #[test]
    fn test_scope_shadowing() {
        let mut scope = Scope::new();
        scope.define("x".into(), DefId(0));
        scope.push();
        scope.define("x".into(), DefId(1));
        assert_eq!(scope.lookup("x"), Some(DefId(1)));
        scope.pop();
        assert_eq!(scope.lookup("x"), Some(DefId(0)));
    }

    #[test]
    fn test_scope_nested() {
        let mut scope = Scope::new();
        scope.define("a".into(), DefId(0));
        scope.push();
        scope.define("b".into(), DefId(1));
        scope.push();
        scope.define("c".into(), DefId(2));
        assert_eq!(scope.lookup("a"), Some(DefId(0)));
        assert_eq!(scope.lookup("b"), Some(DefId(1)));
        assert_eq!(scope.lookup("c"), Some(DefId(2)));
        scope.pop();
        assert_eq!(scope.lookup("c"), None);
        scope.pop();
        assert_eq!(scope.lookup("b"), None);
    }
}
