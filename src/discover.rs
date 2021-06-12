use std::fmt::{self, Display, Formatter};

use syn::{Block, Expr, Path, Stmt};

use crate::{
    context::Context,
    resource::ResourceID,
    types::{trim_common, Callable, CallableType, Resource, ResourceFile},
    uses::UsePathComponent,
};

use colored::*;

fn trim_id_by_ctxt(id: &ResourceID, ctx: &Context) -> ResourceID {
    for path in ctx.iter() {
        if let Some(id) = trim_common(id, path) {
            return id;
        }
    }
    id.clone()
}

fn match_expr_path(res: &ResourceID, expr_path: &Path) -> bool {
    res.components()
        .iter()
        .zip(expr_path.segments.iter())
        .all(|(res_seg, expr_seg)| {
            if let UsePathComponent::Name(res_seg) = res_seg {
                res_seg == &expr_seg.ident.to_string()
            } else {
                false
            }
        })
}

pub fn callable_from_expr(expr: &Expr, callable: &Callable, ctx: &Context, info: &ResourceFile) {
    let trimmed_id = trim_id_by_ctxt(callable.id(), ctx);
    match expr {
        Expr::Call(expr) => {
            if callable.ctype() != CallableType::Function {
                return;
            }
            match expr.func.as_ref() {
                Expr::Path(func) => {
                    if match_expr_path(&trimmed_id, &func.path) {
                        let mut args_and_types = expr.args.iter().zip(callable.args().iter());
                        let args_valid = args_and_types.all(|(expr, res)| {
                            if let Some(expr_res) = get_expr_type(expr, ctx, info) {
                                // TODO: Fix this hack
                                if let Some(first) = expr_res.first() {
                                    return first == res;
                                }
                            }
                            false
                        });
                        if args_valid {
                            println!("Found {}", format!("{}", callable).green());
                        }
                    }
                }
                _ => println!("ignored fn call"),
            }
        }
        _ => println!("ignored"),
    }
}

pub fn callable_from_block(block: &Block, callable: &Callable, ctx: &Context, info: &ResourceFile) {
    for stmt in &block.stmts {
        match stmt {
            Stmt::Local(stmt) => {
                if let Some((_, init)) = &stmt.init {
                    callable_from_expr(init.as_ref(), callable, ctx, info);
                }
            }
            _ => println!("ignored stmt"),
        }
    }
}

fn get_expr_type(expr: &Expr, ctx: &Context, info: &ResourceFile) -> Option<Vec<Resource>> {
    match expr {
        Expr::Path(expr) => {
            let segments: Vec<String> = expr
                .path
                .segments
                .iter()
                .map(|seg| seg.ident.to_string())
                .collect();
            if segments.len() == 1 {
                let name = &segments[0];
                if let Some(ob) = ctx.get_binding(name) {
                    return Some(vec![ob.res.clone()]);
                }
            }
        }
        Expr::Call(expr) => {
            for callable in info.callables() {
                if callable.ctype() != CallableType::Function {
                    return None;
                }
                let trimmed_id = trim_id_by_ctxt(callable.id(), ctx);
                match expr.func.as_ref() {
                    Expr::Path(func) => {
                        if match_expr_path(&trimmed_id, &func.path) {
                            let mut args_and_types = expr.args.iter().zip(callable.args().iter());
                            let args_valid = args_and_types.all(|(expr, res)| {
                                if let Some(expr_res) = get_expr_type(expr, ctx, info) {
                                    // TODO: Fix this hack
                                    if let Some(first) = expr_res.first() {
                                        return first == res;
                                    }
                                }
                                false
                            });
                            if args_valid {
                                return Some(callable.ret().rets().clone());
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        _ => {}
    }
    None
}

pub struct Object {
    name: String,
    res: Resource,
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.res)
    }
}
