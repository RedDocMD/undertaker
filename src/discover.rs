use std::fmt::{self, Display, Formatter};

use syn::{Block, Expr, Pat, Path, Stmt};

use crate::{
    context::Context,
    resource::ResourceID,
    types::{trim_common, Arg, Callable, CallableType, Resource, ResourceFile, Return},
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

pub fn callable_from_expr(
    expr: &Expr,
    callable: &Callable,
    ctx: &Context,
    info: &ResourceFile,
) -> bool {
    let trimmed_id = trim_id_by_ctxt(callable.id(), ctx);
    match expr {
        Expr::Call(expr) => {
            if callable.ctype() != CallableType::Function {
                return false;
            }
            match expr.func.as_ref() {
                Expr::Path(func) => {
                    if match_expr_path(&trimmed_id, &func.path) {
                        let mut args_and_types = expr.args.iter().zip(callable.args().iter());
                        let args_valid = args_and_types.all(|(expr, res)| {
                            if let Some(expr_res) = get_expr_type(expr, ctx, info) {
                                &expr_res == res
                            } else {
                                false
                            }
                        });
                        if args_valid {
                            println!("Found {}", format!("{}", callable).green());
                            return true;
                        }
                    }
                }
                _ => println!("ignored fn call"),
            }
        }
        _ => println!("ignored"),
    }
    false
}

pub fn creator_from_block(
    block: &Block,
    creator: &Callable,
    resource: &Resource,
    ctx: &mut Context,
    info: &ResourceFile,
) {
    for stmt in &block.stmts {
        match stmt {
            Stmt::Local(stmt) => {
                if let Some((_, init)) = &stmt.init {
                    if callable_from_expr(init.as_ref(), creator, ctx, info) {
                        let ret = creator.ret();
                        let pat = &stmt.pat;
                        let name = match_arg_to_pat(resource, ret, pat).unwrap();
                        let ob = Object::new(name, resource.clone());
                        ctx.add_binding(ob.name.clone(), ob);
                    }
                }
            }
            _ => println!("ignored stmt"),
        }
    }
}

fn match_arg_to_pat(res: &Resource, arg: &Arg, pat: &Pat) -> Option<String> {
    match arg {
        Arg::Res(arg_res) => {
            if arg_res != res {
                return None;
            }
            match pat {
                Pat::Ident(pat) => Some(pat.ident.to_string()),
                Pat::Path(pat) => Some(path_to_string(&pat.path)),
                Pat::Type(pat) => match_arg_to_pat(res, arg, pat.pat.as_ref()),
                _ => None,
            }
        }
        Arg::Tuple(arg_vec) => {
            if let Pat::Tuple(pat) = &pat {
                for (idx, pat) in pat.elems.iter().enumerate() {
                    let s = match_arg_to_pat(res, &arg_vec[idx], pat);
                    if s.is_some() {
                        return s;
                    }
                }
            }
            None
        }
    }
}

fn path_to_string(path: &Path) -> String {
    let comp_strs: Vec<String> = path
        .segments
        .iter()
        .map(|item| item.ident.to_string())
        .collect();
    comp_strs.join("::")
}

fn get_expr_type(expr: &Expr, ctx: &Context, info: &ResourceFile) -> Option<Return> {
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
                    return Some(Return::Res(ob.res.clone()));
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
                                    &expr_res == res
                                } else {
                                    false
                                }
                            });
                            if args_valid {
                                return Some(callable.ret().clone());
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

impl Object {
    fn new(name: String, res: Resource) -> Self {
        Self { name, res }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.res)
    }
}