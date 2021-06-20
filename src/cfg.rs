use std::{
    cell::RefCell,
    collections::VecDeque,
    rc::{Rc, Weak},
};

use syn::{Block, Expr, Item, Pat, Stmt};

type CFGNodeStrongPtr<'ast> = Rc<RefCell<CFGNode<'ast>>>;
type CFGNodeWeakPtr<'ast> = Weak<RefCell<CFGNode<'ast>>>;

enum CFGExpr<'ast> {
    Expr(&'ast Expr),
    Item(&'ast Item),
    ForGuard(&'ast Pat, &'ast Expr),
    WhileGuard(&'ast Expr),
    IfGuard(&'ast Expr),
    Phantom,
}

enum CFGNodePtr<'ast> {
    Strong(CFGNodeStrongPtr<'ast>),
    Weak(CFGNodeWeakPtr<'ast>),
}

struct CFGNode<'ast> {
    expr: CFGExpr<'ast>,
    pred: Vec<CFGNodeWeakPtr<'ast>>,
    succ: Vec<CFGNodePtr<'ast>>,
}

impl<'ast> CFGNode<'ast> {
    fn new(expr: CFGExpr<'ast>) -> CFGNodeStrongPtr<'ast> {
        Rc::new(RefCell::new(Self {
            expr,
            pred: Vec::new(),
            succ: Vec::new(),
        }))
    }
}

pub struct CFGBlock<'ast> {
    head: CFGNodeStrongPtr<'ast>,
    tail: CFGNodeStrongPtr<'ast>,
}

impl<'ast> CFGBlock<'ast> {
    fn from_stmt(stmt: &'ast Stmt) -> Option<CFGBlock<'ast>> {
        match stmt {
            Stmt::Local(local) => local
                .init
                .as_ref()
                .and_then(|(_, expr)| Self::from_expr(expr.as_ref())),
            Stmt::Item(item) => {
                let node = CFGNode::new(CFGExpr::Item(item));
                Some(CFGBlock {
                    head: Rc::clone(&node),
                    tail: node,
                })
            }
            Stmt::Expr(expr) => CFGBlock::from_expr(&expr),
            Stmt::Semi(expr, _) => CFGBlock::from_expr(&expr),
        }
    }

    pub fn from_expr(expr: &'ast Expr) -> Option<CFGBlock<'ast>> {
        fn handle_loop_with_cond<'b>(
            cond: CFGNodeStrongPtr<'b>,
            body: &'b Block,
        ) -> Option<CFGBlock<'b>> {
            let body = CFGBlock::from_block(body);
            if let Some(body) = body {
                (*cond)
                    .borrow_mut()
                    .succ
                    .push(CFGNodePtr::Strong(Rc::clone(&body.head)));
                (*body.head).borrow_mut().pred.push(Rc::downgrade(&cond));

                (*body.tail)
                    .borrow_mut()
                    .succ
                    .push(CFGNodePtr::Weak(Rc::downgrade(&cond)));
                (*cond).borrow_mut().pred.push(Rc::downgrade(&body.tail));
            }
            Some(CFGBlock {
                head: Rc::clone(&cond),
                tail: cond,
            })
        }

        match expr {
            Expr::Async(expr) => Self::from_block(&expr.block),
            Expr::Block(expr) => Self::from_block(&expr.block),
            Expr::ForLoop(expr) => {
                let for_guard = CFGNode::new(CFGExpr::ForGuard(&expr.pat, &expr.expr.as_ref()));
                handle_loop_with_cond(for_guard, &expr.body)
            }
            Expr::If(expr) => {
                let cond_node = CFGNode::new(CFGExpr::IfGuard(expr.cond.as_ref()));
                let then_block = Self::from_block(&expr.then_branch);
                let else_block = expr
                    .else_branch
                    .as_ref()
                    .and_then(|(_, expr)| Self::from_expr(expr.as_ref()));
                let exit_node = CFGNode::new(CFGExpr::Phantom);
                if let Some(then_block) = &then_block {
                    (*cond_node)
                        .borrow_mut()
                        .succ
                        .push(CFGNodePtr::Strong(Rc::clone(&then_block.head)));
                    (*then_block.head)
                        .borrow_mut()
                        .pred
                        .push(Rc::downgrade(&cond_node));
                    (*then_block.tail)
                        .borrow_mut()
                        .succ
                        .push(CFGNodePtr::Strong(Rc::clone(&exit_node)));
                    (*exit_node)
                        .borrow_mut()
                        .pred
                        .push(Rc::downgrade(&then_block.tail));
                }
                if let Some(else_block) = &else_block {
                    (*cond_node)
                        .borrow_mut()
                        .succ
                        .push(CFGNodePtr::Strong(Rc::clone(&else_block.head)));
                    (*else_block.head)
                        .borrow_mut()
                        .pred
                        .push(Rc::downgrade(&cond_node));
                    (*else_block.tail)
                        .borrow_mut()
                        .succ
                        .push(CFGNodePtr::Strong(Rc::clone(&exit_node)));
                    (*exit_node)
                        .borrow_mut()
                        .pred
                        .push(Rc::downgrade(&else_block.tail));
                }
                if then_block.is_none() || else_block.is_none() {
                    (*cond_node)
                        .borrow_mut()
                        .succ
                        .push(CFGNodePtr::Strong(Rc::clone(&exit_node)));
                    (*exit_node)
                        .borrow_mut()
                        .pred
                        .push(Rc::downgrade(&cond_node));
                }
                Some(CFGBlock {
                    head: cond_node,
                    tail: exit_node,
                })
            }
            Expr::Loop(loop_expr) => {
                let body = Self::from_block(&loop_expr.body);
                if let Some(body) = body {
                    (*body.tail)
                        .borrow_mut()
                        .succ
                        .push(CFGNodePtr::Weak(Rc::downgrade(&body.head)));
                    (*body.head)
                        .borrow_mut()
                        .pred
                        .push(Rc::downgrade(&body.tail));
                    Some(body)
                } else {
                    let node = CFGNode::new(CFGExpr::Expr(expr));
                    Some(CFGBlock {
                        head: Rc::clone(&node),
                        tail: node,
                    })
                }
            }
            Expr::While(expr) => {
                let while_guard = CFGNode::new(CFGExpr::WhileGuard(expr.cond.as_ref()));
                handle_loop_with_cond(while_guard, &expr.body)
            }
            _ => {
                let node = CFGNode::new(CFGExpr::Expr(expr));
                Some(CFGBlock {
                    head: Rc::clone(&node),
                    tail: node,
                })
            }
        }
    }

    pub fn from_block(block: &'ast Block) -> Option<CFGBlock<'ast>> {
        let mut stmt_blocks: VecDeque<CFGBlock> = block
            .stmts
            .iter()
            .map(Self::from_stmt)
            .filter(Option::is_some)
            .flatten()
            .collect();
        if stmt_blocks.is_empty() {
            None
        } else {
            let first = stmt_blocks.pop_front().unwrap();
            let mut ptr = Rc::clone(&first.tail);
            for stmt_block in stmt_blocks {
                (*ptr)
                    .borrow_mut()
                    .succ
                    .push(CFGNodePtr::Strong(Rc::clone(&stmt_block.head)));
                (*stmt_block.head)
                    .borrow_mut()
                    .pred
                    .push(Rc::downgrade(&ptr));
                ptr = Rc::clone(&stmt_block.tail);
            }
            Some(CFGBlock {
                head: first.head,
                tail: ptr,
            })
        }
    }
}
