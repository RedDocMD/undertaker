use log::info;
use syn::{Block, Expr, ExprAsync, ExprClosure, ImplItem, Item};

fn async_in_item(item: &Item) -> Vec<AsyncCode> {
    match item {
        syn::Item::Fn(item) => async_in_block(item.block.as_ref()),
        syn::Item::Impl(item) => {
            let mut codes = Vec::new();
            for impl_item in &item.items {
                if let ImplItem::Method(method) = impl_item {
                    codes.append(&mut async_in_block(&method.block));
                }
            }
            codes
        }
        syn::Item::Mod(module) => {
            let mut codes = Vec::new();
            if let Some((_, items)) = &module.content {
                for item in items {
                    codes.append(&mut async_in_item(item));
                }
            }
            codes
        }
        _ => vec![],
    }
}

pub fn async_in_block(block: &Block) -> Vec<AsyncCode> {
    let mut codes = Vec::new();
    for stmt in &block.stmts {
        let mut code = match stmt {
            syn::Stmt::Local(expr) => {
                if let Some((_, init)) = &expr.init {
                    async_in_expr(init.as_ref())
                } else {
                    vec![]
                }
            }
            syn::Stmt::Item(item) => async_in_item(item),
            syn::Stmt::Expr(expr) => async_in_expr(expr),
            syn::Stmt::Semi(expr, _) => async_in_expr(expr),
        };
        codes.append(&mut code);
    }
    codes
}

fn async_in_expr(expr: &Expr) -> Vec<AsyncCode> {
    match expr {
        Expr::Assign(expr) => async_in_expr(expr.right.as_ref()),
        Expr::AssignOp(expr) => async_in_expr(expr.right.as_ref()),
        Expr::Async(expr) => {
            info!("Found async block");
            vec![AsyncCode::Block(expr.clone())]
        }
        Expr::Block(expr) => async_in_block(&expr.block),
        Expr::Call(expr) => {
            let mut codes = Vec::new();
            for arg in &expr.args {
                codes.append(&mut async_in_expr(arg));
            }
            codes
        }
        Expr::Closure(expr) => {
            if expr.asyncness.is_some() {
                info!("Found async block (closure)");
                vec![AsyncCode::Closure(expr.clone())]
            } else {
                vec![]
            }
        }
        Expr::MethodCall(expr) => {
            let mut codes = Vec::new();
            for arg in &expr.args {
                codes.append(&mut async_in_expr(arg));
            }
            codes
        }
        _ => vec![],
    }
}

#[derive(Debug)]
pub enum AsyncCode {
    Block(ExprAsync),
    Closure(ExprClosure),
}
