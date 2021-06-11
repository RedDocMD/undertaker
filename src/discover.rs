use syn::{Block, Expr, Path, Stmt};

use crate::{
    context::Context,
    resource::ResourceID,
    types::{trim_common, Callable, CallableType},
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

pub fn callable_from_expr(expr: &Expr, callable: &Callable, ctx: &Context) {
    let trimmed_id = trim_id_by_ctxt(callable.id(), ctx);
    match expr {
        Expr::Call(expr) => {
            if callable.ctype() != CallableType::Function {
                return;
            }
            match expr.func.as_ref() {
                Expr::Path(func) => {
                    if match_expr_path(&trimmed_id, &func.path) {
                        println!("Found {}", format!("{}", callable).green());
                        // Check args for types
                    }
                }
                _ => println!("ignored fn call"),
            }
        }
        _ => println!("ignored"),
    }
}

pub fn callable_from_block(block: &Block, callable: &Callable, ctx: &Context) {
    for stmt in &block.stmts {
        match stmt {
            Stmt::Local(stmt) => {
                if let Some((_, init)) = &stmt.init {
                    callable_from_expr(init.as_ref(), callable, ctx);
                }
            }
            _ => println!("ignored stmt"),
        }
    }
}
