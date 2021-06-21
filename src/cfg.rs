use std::{
    cell::RefCell,
    collections::{HashSet, VecDeque},
    fmt::{self, Display, Formatter, Write},
    rc::{Rc, Weak},
};

use syn::{Block, Expr, Item, Pat, Stmt};
use uuid::{adapter::SimpleRef, Uuid};

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
    uuid: Uuid,
}

impl<'ast> CFGNode<'ast> {
    fn new(expr: CFGExpr<'ast>) -> CFGNodeStrongPtr<'ast> {
        Rc::new(RefCell::new(Self {
            expr,
            pred: Vec::new(),
            succ: Vec::new(),
            uuid: Uuid::new_v4(),
        }))
    }

    fn node_label(&self) -> String {
        let simp = self.uuid.to_simple_ref();
        let mut buf = [0_u8; SimpleRef::LENGTH];
        let rep = String::from(simp.encode_lower(&mut buf));
        String::from("n") + &rep
    }

    fn node_info(&self) -> String {
        let name = match self.expr {
            CFGExpr::Expr(expr) => expr_name(expr),
            CFGExpr::Item(item) => item_name(item),
            CFGExpr::ForGuard(_, expr) => format!("for_guard: {}", expr_name(expr)),
            CFGExpr::WhileGuard(expr) => format!("while_cond: {}", expr_name(expr)),
            CFGExpr::IfGuard(expr) => format!("if_cond: {}", expr_name(expr)),
            CFGExpr::Phantom => String::from("No-Op"),
        };
        format!("{} [label=\"{}\"];", self.node_label(), name)
    }
}

impl PartialEq for CFGNode<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.uuid == other.uuid
    }
}

impl Eq for CFGNode<'_> {}

fn expr_name(expr: &Expr) -> String {
    String::from(match expr {
        Expr::Array(_) => "array slice",
        Expr::Assign(_) => "assign expr",
        Expr::AssignOp(_) => "assign-op expr",
        Expr::Async(_) => unreachable!("async block"),
        Expr::Await(_) => "await expr",
        Expr::Binary(_) => "binary-op expr",
        Expr::Block(_) => unreachable!("block"),
        Expr::Box(_) => "box expr",
        Expr::Break(_) => "break",
        Expr::Call(_) => "function call",
        Expr::Cast(_) => "cast expr",
        Expr::Closure(_) => "closure expr",
        Expr::Continue(_) => "continue",
        Expr::Field(_) => "field access",
        Expr::ForLoop(_) => unreachable!("for loop"),
        Expr::Group(_) => "group ??",
        Expr::If(_) => unreachable!("if stmt"),
        Expr::Index(_) => "index expr",
        Expr::Let(_) => "let binding",
        Expr::Lit(_) => "literal",
        Expr::Loop(_) => unreachable!("loop"),
        Expr::Macro(_) => "macro invocation",
        Expr::Match(_) => "match expr",
        Expr::MethodCall(_) => "method call",
        Expr::Paren(_) => "parenthesized expr",
        Expr::Path(_) => "path expr",
        Expr::Range(_) => "range expr",
        Expr::Reference(_) => "ref expr",
        Expr::Repeat(_) => "repeat expr",
        Expr::Return(_) => "return expr",
        Expr::Struct(_) => "struct expr",
        Expr::Try(_) => "try expr?",
        Expr::TryBlock(_) => "try block?",
        Expr::Tuple(_) => "tuple expr",
        Expr::Type(_) => "type expr",
        Expr::Unary(_) => "unary op expr",
        Expr::Unsafe(_) => "unsafe expr",
        Expr::Verbatim(_) => todo!("what is verbatim doing here?"),
        Expr::While(_) => unreachable!("while loop"),
        Expr::Yield(_) => "yield expr",
        Expr::__TestExhaustive(_) => todo!("what is a __TestExhaustive?"),
    })
}

fn item_name(item: &Item) -> String {
    String::from(match item {
        Item::Const(_) => "const decl",
        Item::Enum(_) => "enum decl",
        Item::ExternCrate(_) => "extern crate decl",
        Item::Fn(_) => "function decl",
        Item::ForeignMod(_) => "foreign mod",
        Item::Impl(_) => "impl decl",
        Item::Macro(_) => "macro decl",
        Item::Macro2(_) => "macro2 decl",
        Item::Mod(_) => "mod decl",
        Item::Static(_) => "static decl",
        Item::Struct(_) => "struct decl",
        Item::Trait(_) => "trait decl",
        Item::TraitAlias(_) => "trai alias decl",
        Item::Type(_) => "type decl",
        Item::Union(_) => "union decl",
        Item::Use(_) => "use stmt",
        Item::Verbatim(_) => todo!("what is a verbatim doing here?"),
        Item::__TestExhaustive(_) => todo!("what is a __TestExhaustive?"),
    })
}

impl Display for CFGExpr<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            CFGExpr::Expr(expr) => write!(f, "{}", expr_name(expr)),
            CFGExpr::Item(_) => todo!(),
            CFGExpr::ForGuard(_, _) => todo!(),
            CFGExpr::WhileGuard(_) => todo!(),
            CFGExpr::IfGuard(_) => todo!(),
            CFGExpr::Phantom => todo!(),
        }
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

    pub fn dot_description(&self) -> String {
        let mut node_desc = String::new();
        let mut edge_desc = String::new();
        let mut stack = Vec::new();
        let mut done = HashSet::new();

        stack.push(Rc::clone(&self.head));
        while !stack.is_empty() {
            let node = stack.pop().unwrap();
            let node = node.as_ref().borrow();
            if done.contains(&node.uuid) {
                continue;
            }
            done.insert(node.uuid);

            writeln!(&mut node_desc, "\t{}", node.node_info())
                .expect("expected to be able write to string");

            for s in &node.succ {
                match s {
                    CFGNodePtr::Strong(strong) => {
                        writeln!(
                            &mut edge_desc,
                            "\t{} -> {};",
                            node.node_label(),
                            strong.as_ref().borrow().node_label()
                        )
                        .expect("expected to be able to write to string");
                    }
                    CFGNodePtr::Weak(weak) => {
                        if let Some(weak) = Weak::upgrade(weak) {
                            writeln!(
                                &mut edge_desc,
                                "\t{} -> {};",
                                node.node_label(),
                                weak.as_ref().borrow().node_label()
                            )
                            .expect("expected to be able to write to string");
                        }
                    }
                }

                if let CFGNodePtr::Strong(s) = s {
                    stack.push(Rc::clone(s));
                }
            }
        }
        format!("digraph G {{\n{}{}}}\n", edge_desc, node_desc)
    }
}
