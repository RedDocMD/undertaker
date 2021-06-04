use std::collections::HashMap;

use crate::resource::Object;
use crate::uses::{extend_path_once, UsePath};

type BlockUsePaths = Vec<UsePath>;

pub struct Context {
    use_paths: Vec<BlockUsePaths>,
    env: Environment,
}

impl Context {
    pub fn new() -> Self {
        let mut paths = Vec::new();
        paths.push(Vec::new()); // For the global scope
        Self {
            use_paths: paths,
            env: Environment::new(),
        }
    }

    pub fn enter_block(&mut self) {
        self.use_paths.push(Vec::new());
        self.env.enter_block();
    }

    pub fn exit_block(&mut self) {
        self.use_paths.pop();
        self.env.exit_block();
    }

    /// Adds paths to the current block.
    pub fn add_use_paths(&mut self, paths: Vec<UsePath>) {
        // Safe since there is atleast the global scope.
        let parents: Vec<UsePath> = self.iter().cloned().collect();
        let mut paths = paths;
        paths = extend_path_once(&parents, &paths).unwrap();
        let last = self.use_paths.last_mut().unwrap();
        last.append(&mut paths);
    }

    pub fn iter(&self) -> PathIter<'_> {
        PathIter::new(&self)
    }
}

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
        let mut block_envs = Vec::new();
        block_envs.push(BlockEnvironment::new()); // For the global scope
        Self { block_envs }
    }

    fn enter_block(&mut self) {
        self.block_envs.push(BlockEnvironment::new());
    }

    fn exit_block(&mut self) {
        self.block_envs.pop();
    }
}

impl BlockEnvironment {
    fn new() -> Self {
        Self {
            id_map: HashMap::new(),
        }
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
