use std::{
    fmt::{self, Display, Formatter},
    rc::{Rc, Weak},
};

use colored::*;
use syn::{Block, Expr, Stmt};
use uuid::Uuid;

struct DepGraphNode {
    expr: Expr,
    uuid: Uuid,
    cross_deps: Vec<(Weak<DepGraph>, usize)>,
}

impl DepGraphNode {
    fn new(expr: Expr) -> Self {
        Self {
            expr,
            uuid: Uuid::new_v4(),
            cross_deps: Vec::new(),
        }
    }

    fn add_cross_dep(&mut self, graph: &Rc<DepGraph>, idx: usize) {
        self.cross_deps.push((Rc::downgrade(graph), idx));
    }

    fn from_stmt(stmt: Stmt) -> Option<Self> {
        use Stmt::*;
        match stmt {
            Local(local) => local.init.map(|(_, expr)| DepGraphNode::new(*expr)),
            Item(_) => None,
            Expr(expr) => Some(DepGraphNode::new(expr)),
            Semi(expr, _) => Some(DepGraphNode::new(expr)),
        }
    }
}

impl Display for DepGraphNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {:?}", self.uuid.to_string().yellow(), self.expr)
    }
}

/// DepGraph is a *linear* chain of DepGraphNode.
/// So, it represents the temporal dependency in a block.
pub struct DepGraph {
    uuid: Uuid,
    nodes: Vec<DepGraphNode>,
}

impl DepGraph {
    fn new() -> Rc<Self> {
        Rc::new(Self {
            uuid: Uuid::new_v4(),
            nodes: Vec::new(),
        })
    }

    pub fn from_block(block: &Block) -> Rc<Self> {
        let nodes: Vec<DepGraphNode> = block
            .stmts
            .iter()
            .map(|stmt| DepGraphNode::from_stmt(stmt.clone()))
            .filter(Option::is_some)
            .flatten()
            .collect();
        Rc::new(Self {
            uuid: Uuid::new_v4(),
            nodes,
        })
    }

    pub fn from_expr(expr: Expr) -> Rc<Self> {
        Rc::new(Self {
            uuid: Uuid::new_v4(),
            nodes: vec![DepGraphNode::new(expr)],
        })
    }
}

impl Display for DepGraph {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for node in &self.nodes {
            writeln!(f, "{}", node)?;
        }
        Ok(())
    }
}
