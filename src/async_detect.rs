use log::info;
use syn::{Block, Expr, ImplItem, Item};

fn async_in_item(item: &Item) {
    match item {
        syn::Item::Fn(item) => async_in_block(item.block.as_ref()),
        syn::Item::Impl(item) => {
            for impl_item in &item.items {
                if let ImplItem::Method(method) = impl_item {
                    async_in_block(&method.block);
                }
            }
        }
        syn::Item::Mod(module) => {
            if let Some((_, items)) = &module.content {
                for item in items {
                    async_in_item(item);
                }
            }
        }
        _ => {}
    }
}

pub fn async_in_block(block: &Block) {
    for stmt in &block.stmts {
        match stmt {
            syn::Stmt::Local(expr) => {
                if let Some((_, init)) = &expr.init {
                    async_in_expr(init.as_ref());
                }
            }
            syn::Stmt::Item(item) => async_in_item(item),
            syn::Stmt::Expr(expr) => async_in_expr(expr),
            syn::Stmt::Semi(expr, _) => async_in_expr(expr),
        }
    }
}

fn async_in_expr(expr: &Expr) {
    match expr {
        Expr::Assign(expr) => async_in_expr(expr.right.as_ref()),
        Expr::AssignOp(expr) => async_in_expr(expr.right.as_ref()),
        Expr::Async(_) => {
            info!("Found async block")
        }
        Expr::Block(expr) => async_in_block(&expr.block),
        Expr::Call(expr) => {
            for arg in &expr.args {
                async_in_expr(arg)
            }
        }
        Expr::Closure(expr) => {
            if expr.asyncness.is_some() {
                info!("Found async block (closure)");
            }
        }
        Expr::MethodCall(expr) => {
            for arg in &expr.args {
                async_in_expr(arg);
            }
        }
        _ => {}
    }
}
