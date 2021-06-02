use std::collections::HashMap;

use crate::resource::Object;
use crate::uses::UsePath;

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
        // self.env.enter_block();
    }

    pub fn exit_block(&mut self) {
        self.use_paths.pop();
        // self.env.exit_block();
    }

    /// Adds paths to the current block.
    pub fn add_use_paths(&mut self, paths: Vec<UsePath>) {
        // Safe since there is atleast the global scope.
        let last = self.use_paths.last_mut().unwrap();
        let mut paths = paths;
        last.append(&mut paths);
    }
}

pub struct Environment {
    id_map: HashMap<String, Vec<Object>>,
}

impl Environment {
    fn new() -> Self {
        Self {
            id_map: HashMap::new(),
        }
    }
}
