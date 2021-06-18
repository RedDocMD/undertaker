use std::collections::HashMap;
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

use syn::Expr;
use uuid::Uuid;

use crate::discover::Object;
use crate::path;
use crate::uses::{extend_path_once, UsePath};

type BlockUsePaths = Vec<UsePath>;

pub struct Context {
    use_paths: Vec<BlockUsePaths>,
    env: Environment,
    expr_uuid_store: ExprUuidStore,
}

fn prelude_paths() -> Vec<UsePath> {
    vec![
        path!["std", "marker", "Copy"],
        path!["std", "marker", "Send"],
        path!["std", "marker", "Sized"],
        path!["std", "marker", "Sync"],
        path!["std", "marker", "Unpin"],
        path!["std", "ops", "Drop"],
        path!["std", "ops", "Fn"],
        path!["std", "ops", "FnMut"],
        path!["std", "ops", "FnOnce"],
        path!["std", "mem", "drop"],
        path!["std", "boxed", "Box"],
        path!["std", "borrow", "ToOwned"],
        path!["std", "clone", "Clone"],
        path!["std", "cmp", "PartialEq"],
        path!["std", "cmp", "PartialOrd"],
        path!["std", "cmp", "Eq"],
        path!["std", "cmp", "Ord"],
        path!["std", "convert", "AsRef"],
        path!["std", "convert", "AsMut"],
        path!["std", "convert", "Into"],
        path!["std", "convert", "From"],
        path!["std", "default", "Default"],
        path!["std", "iter", "Iterator"],
        path!["std", "iter", "Extend"],
        path!["std", "iter", "IntoIterator"],
        path!["std", "iter", "DoubleEndedIterator"],
        path!["std", "iter", "ExactSizeIterator"],
        path!["std", "option", "Option"],
        path!["std", "option", "Option", "Some"],
        path!["std", "option", "Option", "None"],
        path!["std", "result", "Result"],
        path!["std", "result", "Result", "Ok"],
        path!["std", "result", "Result", "Err"],
        path!["std", "string", "String"],
        path!["std", "string", "ToString"],
        path!["std", "vec", "Vec"],
    ]
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

impl Context {
    pub fn new() -> Self {
        Self {
            use_paths: vec![prelude_paths()],
            env: Environment::new(),
            expr_uuid_store: ExprUuidStore::new(),
        }
    }

    pub fn enter_block(&mut self) {
        self.use_paths.push(Vec::new());
        self.env.enter_block();
        self.expr_uuid_store.enter_block();
    }

    pub fn exit_block(&mut self) {
        self.use_paths.pop();
        self.env.exit_block();
        self.expr_uuid_store.exit_block();
    }

    /// Adds paths to the current block.
    pub fn add_use_paths(&mut self, paths: Vec<UsePath>) {
        // Safe since there is atleast the global scope.
        let mut paths = paths;
        paths = extend_path_once(self.iter(), &paths).unwrap();
        let last = self.use_paths.last_mut().unwrap();
        last.append(&mut paths);
    }

    pub fn iter(&self) -> PathIter<'_> {
        PathIter::new(&self)
    }

    pub fn add_binding(&mut self, id: String, ob: Object) {
        self.env.add_binding(id, ob);
    }

    pub fn get_binding(&self, id: &str) -> Option<&Object> {
        self.env.get_binding(id)
    }

    pub fn tot_binding_cnt(&self) -> usize {
        self.env.tot_binding_cnt()
    }

    pub fn add_expr_uuid(&mut self, expr: Expr, idx: usize, uuid: Uuid) {
        let idx_expr = IndexedExpr { expr, idx };
        self.expr_uuid_store.add_binding(idx_expr, uuid);
    }

    pub fn get_expr_uuid(&self, expr: Expr, idx: usize) -> Option<&Uuid> {
        let idx_expr = IndexedExpr { expr, idx };
        self.expr_uuid_store.get_binding(&idx_expr)
    }
}

impl Display for Context {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "Use paths:")?;
        for path in self.iter() {
            writeln!(f, "  {}", path)?;
        }
        writeln!(f, "Bindings:")?;
        write!(f, "{}", self.env)
    }
}

#[derive(Clone)]
pub struct PathIter<'ctx> {
    block_idx: usize,
    path_idx: usize,
    ctx: &'ctx Context,
}

impl<'ctx> PathIter<'ctx> {
    fn new(ctx: &'ctx Context) -> Self {
        Self {
            block_idx: 0,
            path_idx: 0,
            ctx,
        }
    }
}

impl PathIter<'_> {
    fn advance(&mut self) -> bool {
        let block = self.ctx.use_paths.get(self.block_idx);
        if let Some(block) = block {
            self.path_idx += 1;
            let mut block = Some(block);
            while block.is_some() && self.path_idx == block.unwrap().len() {
                self.block_idx += 1;
                self.path_idx = 0;
                block = self.ctx.use_paths.get(self.block_idx);
            }
            block.is_some()
        } else {
            false
        }
    }
}

impl<'ctx> Iterator for PathIter<'ctx> {
    type Item = &'ctx UsePath;

    fn next(&mut self) -> Option<Self::Item> {
        let item = {
            let block = self.ctx.use_paths.get(self.block_idx);
            if let Some(block) = block {
                block.get(self.path_idx)
            } else {
                None
            }
        };
        self.advance();
        item
    }
}

struct Environment {
    block_envs: Vec<BlockEnvironment>,
}

struct BlockEnvironment {
    id_map: HashMap<String, Object>,
}

impl Environment {
    fn new() -> Self {
        // For the global scope
        Self {
            block_envs: vec![BlockEnvironment::new()],
        }
    }

    fn enter_block(&mut self) {
        self.block_envs.push(BlockEnvironment::new());
    }

    fn exit_block(&mut self) {
        self.block_envs.pop();
    }

    fn add_binding(&mut self, id: String, ob: Object) {
        // Safe to unwrap because we have at least the global block
        let block = self.block_envs.last_mut().unwrap();
        block.add_binding(id, ob);
    }

    fn get_binding(&self, id: &str) -> Option<&Object> {
        for block in self.block_envs.iter().rev() {
            let binding = block.get_binding(id);
            if binding.is_some() {
                return binding;
            }
        }
        None
    }

    fn tot_binding_cnt(&self) -> usize {
        self.block_envs
            .iter()
            .fold(0, |acc, el| acc + el.id_map.keys().len())
    }
}

impl Display for Environment {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut padding = String::new();
        for block in &self.block_envs {
            padding += "  ";
            for ob in block.id_map.values() {
                writeln!(f, "{}{}", padding, ob)?;
            }
        }
        Ok(())
    }
}

impl BlockEnvironment {
    fn new() -> Self {
        Self {
            id_map: HashMap::new(),
        }
    }

    fn add_binding(&mut self, id: String, ob: Object) {
        self.id_map.insert(id, ob);
    }

    fn get_binding(&self, id: &str) -> Option<&Object> {
        self.id_map.get(id)
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct IndexedExpr {
    expr: Expr,
    idx: usize,
}

struct BlockUuidExprMap {
    map: HashMap<IndexedExpr, Uuid>,
}

impl BlockUuidExprMap {
    fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    fn add_binding(&mut self, indexed_expr: IndexedExpr, uuid: Uuid) {
        self.map.insert(indexed_expr, uuid);
    }

    fn get_binding(&self, indexed_expr: &IndexedExpr) -> Option<&Uuid> {
        self.map.get(indexed_expr)
    }
}

struct ExprUuidStore {
    store: Vec<BlockUuidExprMap>,
}

impl ExprUuidStore {
    fn new() -> Self {
        Self {
            store: vec![BlockUuidExprMap::new()],
        }
    }

    fn add_binding(&mut self, indexed_expr: IndexedExpr, uuid: Uuid) {
        let map = self.store.last_mut().unwrap();
        map.add_binding(indexed_expr, uuid);
    }

    fn get_binding(&self, indexed_expr: &IndexedExpr) -> Option<&Uuid> {
        let map = self.store.last().unwrap();
        map.get_binding(indexed_expr)
    }

    fn enter_block(&mut self) {
        self.store.push(BlockUuidExprMap::new());
    }

    fn exit_block(&mut self) {
        self.store.pop();
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::path;

    #[test]
    fn test_empty_ctx() {
        let ctx = Context::new();
        assert_eq!(ctx.iter().next(), None);
    }

    #[test]
    fn test_ctx_with_one_block() {
        let mut ctx = Context::new();
        ctx.add_use_paths(vec![
            path!["std", "fs", "File"],
            path!["std", "path", "Path"],
        ]);
        let paths: Vec<&UsePath> = ctx.iter().collect();
        assert_eq!(paths.len(), 2);
        assert_eq!(paths[0], &path!["std", "fs", "File"]);
        assert_eq!(paths[1], &path!["std", "path", "Path"]);
    }

    #[test]
    fn test_ctx_with_two_blocks() {
        let mut ctx = Context::new();
        ctx.add_use_paths(vec![
            path!["std", "fs", "File"],
            path!["std", "path", "Path"],
        ]);
        ctx.enter_block();
        ctx.add_use_paths(vec![
            path!["std", "path", "PathBuf"],
            path!["std", "collections", "HashMap"],
        ]);
        let paths: Vec<&UsePath> = ctx.iter().collect();
        assert_eq!(paths.len(), 4);
        assert_eq!(paths[0], &path!["std", "fs", "File"]);
        assert_eq!(paths[1], &path!["std", "path", "Path"]);
        assert_eq!(paths[2], &path!["std", "path", "PathBuf"]);
        assert_eq!(paths[3], &path!["std", "collections", "HashMap"]);
    }

    #[test]
    fn test_ctx_with_empty_blocks() {
        let mut ctx = Context::new();
        ctx.add_use_paths(vec![
            path!["std", "fs", "File"],
            path!["std", "path", "Path"],
        ]);
        ctx.enter_block();
        ctx.enter_block();
        ctx.enter_block();
        ctx.enter_block();
        ctx.add_use_paths(vec![
            path!["std", "path", "PathBuf"],
            path!["std", "collections", "HashMap"],
        ]);
        ctx.enter_block();
        ctx.enter_block();
        let paths: Vec<&UsePath> = ctx.iter().collect();
        assert_eq!(paths.len(), 4);
        assert_eq!(paths[0], &path!["std", "fs", "File"]);
        assert_eq!(paths[1], &path!["std", "path", "Path"]);
        assert_eq!(paths[2], &path!["std", "path", "PathBuf"]);
        assert_eq!(paths[3], &path!["std", "collections", "HashMap"]);
    }
}
