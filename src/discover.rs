use std::fmt::{self, Display, Formatter};

use syn::{Block, Expr, Lit, Pat, Path, Stmt};

use log::{debug, info, trace, warn};
use uuid::Uuid;

use crate::{
    context::Context,
    resource::ResourceID,
    types::{
        trim_common, Arg, Callable, CallableType, Resource, ResourceFile, Return, UUIDPropagation,
    },
    uses::{UsePath, UsePathComponent},
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
    // Precondition: If there are nested method calls, outermost one is the right one
    let trimmed_id = trim_id_by_ctxt(callable.id(), ctx);
    match expr {
        Expr::Call(expr) => {
            if callable.ctype() != CallableType::Function {
                return false;
            }
            match expr.func.as_ref() {
                Expr::Path(func) => {
                    trace!("{}, {:?}", trimmed_id, func.path);
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
                            info!("Found {}", format!("{}", callable).green());
                            return true;
                        }
                    }
                }
                _ => debug!("ignored fn call"),
            }
        }
        Expr::MethodCall(expr) => {
            if callable.ctype() != CallableType::Method {
                return false;
            }
            let cid = callable.id();
            let cid_parts = cid.components();
            let base_res_id = UsePath::from(&cid_parts[0..cid_parts.len() - 1].to_vec());
            if let UsePathComponent::Name(method_name) = cid_parts.last().unwrap() {
                if method_name == &expr.method.to_string() {
                    if let Some(Return::NonVoid(Arg::Res(reciever_res))) =
                        get_expr_type(expr.receiver.as_ref(), ctx, info)
                    {
                        let id = if reciever_res.is_deref() {
                            let type_map = reciever_res.type_map();
                            if type_map.len() != 1 {
                                todo!(
                                    "yet to handle deref trait for types with {} type params",
                                    type_map.len()
                                );
                            }
                            type_map.values().next().unwrap().id()
                        } else {
                            trace!("not deref");
                            reciever_res.id()
                        };
                        trace!("-- {:?} {:?} --", id, base_res_id);
                        if id == &base_res_id {
                            trace!("ok reciver");
                            let mut args_and_types = expr.args.iter().zip(callable.args().iter());
                            let args_valid = args_and_types.all(|(expr, res)| {
                                if let Some(expr_res) = get_expr_type(expr, ctx, info) {
                                    &expr_res == res
                                } else {
                                    false
                                }
                            });
                            if args_valid {
                                info!("Found {}", format!("{}", callable).green());
                                return true;
                            }
                        }
                    }
                }
            }
        }
        Expr::Assign(expr) => {
            if callable_from_expr(expr.left.as_ref(), callable, ctx, info) {
                return true;
            }
            if callable_from_expr(expr.right.as_ref(), callable, ctx, info) {
                return true;
            }
        }
        Expr::AssignOp(expr) => {
            if callable_from_expr(expr.left.as_ref(), callable, ctx, info) {
                return true;
            }
            if callable_from_expr(expr.right.as_ref(), callable, ctx, info) {
                return true;
            }
        }
        Expr::Await(expr) => {
            return callable_from_expr(expr.base.as_ref(), callable, ctx, info);
        }
        Expr::Let(expr) => {
            trace!("let expr");
            return callable_from_expr(expr.expr.as_ref(), callable, ctx, info);
        }
        _ => debug!("ignored"),
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
    assert!(!creator.ret().is_void());
    for (stmt_idx, stmt) in block.stmts.iter().enumerate() {
        match stmt {
            Stmt::Local(stmt) => {
                if let Some((_, init)) = &stmt.init {
                    if callable_from_expr(init.as_ref(), creator, ctx, info) {
                        let ret = creator.ret();
                        let pat = &stmt.pat;
                        let name = match_arg_to_pat(resource, ret.as_non_void(), pat).unwrap();
                        if ctx.get_binding(&name).is_some() {
                            continue;
                        }
                        let uuid = match creator.prop() {
                            UUIDPropagation::Copy(idx) => uuid_from_call_expr(
                                init.as_ref(),
                                stmt_idx,
                                idx,
                                creator.ctype(),
                                ctx,
                            ),
                            UUIDPropagation::NoCopy => {
                                if let Some(uuid) =
                                    ctx.get_expr_uuid(init.as_ref().clone(), stmt_idx)
                                {
                                    *uuid
                                } else {
                                    let uuid = Uuid::new_v4();
                                    ctx.add_expr_uuid(init.as_ref().clone(), stmt_idx, uuid);
                                    uuid
                                }
                            }
                            UUIDPropagation::DontCare => {
                                warn!(
                                    "{} is a creator, yet has propagation type DontCare",
                                    creator
                                );
                                Uuid::new_v4()
                            }
                        };
                        let ob = Object::new(name, resource.clone(), uuid);
                        ctx.add_binding(ob.name.clone(), ob);
                    }
                }
            }
            _ => debug!("ignored stmt"),
        }
    }
}

fn uuid_from_call_expr(
    expr: &Expr,
    expr_idx: usize,
    arg_idx: usize,
    ctype: CallableType,
    ctx: &mut Context,
) -> Uuid {
    fn uuid_from_expr(expr: &Expr, idx: usize, ctx: &mut Context) -> Uuid {
        if let Some(uuid) = ctx.get_expr_uuid(expr.clone(), idx) {
            *uuid
        } else {
            match expr {
                Expr::Path(expr) => {
                    let segs = &expr.path.segments;
                    if segs.len() == 1 {
                        let name = segs[0].ident.to_string();
                        if let Some(ob) = ctx.get_binding(&name) {
                            return ob.internal_uuid;
                        }
                    }
                }
                Expr::Reference(expr) => {
                    return uuid_from_expr(expr.expr.as_ref(), idx, ctx);
                }
                _ => {}
            };
            let uuid = Uuid::new_v4();
            ctx.add_expr_uuid(expr.clone(), idx, uuid);
            uuid
        }
    }

    match expr {
        Expr::Call(expr) => {
            assert_eq!(
                ctype,
                CallableType::Function,
                "ctype must be Function to retrieve uuid from function call"
            );
            assert!(
                arg_idx > 0,
                "idx is {}, but expected non-zero index for function call",
                arg_idx
            );
            assert!(
                arg_idx <= expr.args.len(),
                "idx is {} but no. of args is {}",
                arg_idx,
                expr.args.len()
            );
            let arg = &expr.args[arg_idx - 1];
            uuid_from_expr(arg, expr_idx, ctx)
        }
        Expr::MethodCall(expr) => {
            assert_eq!(
                ctype,
                CallableType::Method,
                "ctype must be Method to retrieve uuid from function call"
            );
            assert!(
                arg_idx <= expr.args.len(),
                "idx is {} but no. of args is {}",
                arg_idx,
                expr.args.len()
            );
            if arg_idx == 0 {
                let recv = expr.receiver.as_ref();
                uuid_from_expr(recv, expr_idx, ctx)
            } else {
                let arg = &expr.args[arg_idx - 1];
                uuid_from_expr(arg, expr_idx, ctx)
            }
        }
        _ => unreachable!("{:?} is not a call expression", expr),
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
                    return Some(Return::NonVoid(Arg::Res(ob.res.clone())));
                }
            }
        }
        Expr::Reference(expr) => {
            return get_expr_type(expr.expr.as_ref(), ctx, info);
        }
        Expr::Call(expr) => {
            for callable in info.callables() {
                if callable.ctype() != CallableType::Function {
                    continue;
                }
                let trimmed_id = trim_id_by_ctxt(callable.id(), ctx);
                if let Expr::Path(func) = expr.func.as_ref() {
                    trace!("{}, {:?}", trimmed_id, func.path);
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
            }
        }
        Expr::MethodCall(expr) => {
            for callable in info.callables() {
                if callable.ctype() != CallableType::Method {
                    continue;
                }
                let cid = callable.id();
                let cid_parts = cid.components();
                let base_res_id = UsePath::from(&cid_parts[0..cid_parts.len() - 1].to_vec());
                if let UsePathComponent::Name(method_name) = cid_parts.last().unwrap() {
                    if method_name == &expr.method.to_string() {
                        if let Some(Return::NonVoid(Arg::Res(reciever_res))) =
                            get_expr_type(expr.receiver.as_ref(), ctx, info)
                        {
                            if reciever_res.id() == &base_res_id {
                                let mut args_and_types =
                                    expr.args.iter().zip(callable.args().iter());
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
                    }
                }
            }
        }
        Expr::Lit(expr) => {
            let primitives = ctx.primitives();
            match &expr.lit {
                Lit::Str(_) => return Some(Return::NonVoid(Arg::Res(primitives["str"].clone()))),
                Lit::ByteStr(_) => todo!("cannot handle byte-str"),
                Lit::Byte(_) => return Some(Return::NonVoid(Arg::Res(primitives["u8"].clone()))),
                Lit::Char(_) => return Some(Return::NonVoid(Arg::Res(primitives["char"].clone()))),
                Lit::Int(lit) => match lit.suffix() {
                    "u8" => return Some(Return::NonVoid(Arg::Res(primitives["u8"].clone()))),
                    "u16" => return Some(Return::NonVoid(Arg::Res(primitives["u16"].clone()))),
                    "u32" => return Some(Return::NonVoid(Arg::Res(primitives["u32"].clone()))),
                    "u64" => return Some(Return::NonVoid(Arg::Res(primitives["u64"].clone()))),
                    "u128" => return Some(Return::NonVoid(Arg::Res(primitives["u128"].clone()))),
                    "usize" => return Some(Return::NonVoid(Arg::Res(primitives["usize"].clone()))),
                    "i8" => return Some(Return::NonVoid(Arg::Res(primitives["i8"].clone()))),
                    "i16" => return Some(Return::NonVoid(Arg::Res(primitives["i16"].clone()))),
                    "i32" => return Some(Return::NonVoid(Arg::Res(primitives["i32"].clone()))),
                    "i64" => return Some(Return::NonVoid(Arg::Res(primitives["i64"].clone()))),
                    "i128" => return Some(Return::NonVoid(Arg::Res(primitives["i128"].clone()))),
                    "isize" => return Some(Return::NonVoid(Arg::Res(primitives["isize"].clone()))),
                    "" => return Some(Return::NonVoid(Arg::Res(primitives["integral"].clone()))),
                    _ => unreachable!("invalid int suffix: \"{}\"", lit.suffix()),
                },
                Lit::Float(lit) => match lit.suffix() {
                    "f32" => return Some(Return::NonVoid(Arg::Res(primitives["f32"].clone()))),
                    "f64" => return Some(Return::NonVoid(Arg::Res(primitives["f64"].clone()))),
                    "" => return Some(Return::NonVoid(Arg::Res(primitives["floating"].clone()))),
                    _ => unreachable!("invalid float suffix: \"{}\""),
                },
                Lit::Bool(_) => return Some(Return::NonVoid(Arg::Res(primitives["bool"].clone()))),
                Lit::Verbatim(_) => todo!("what is verbatim doing here"),
            }
        }
        _ => {}
    }
    None
}

#[derive(Clone)]
pub struct Object {
    name: String,
    res: Resource,
    internal_uuid: Uuid,
}

impl Object {
    fn new(name: String, res: Resource, internal_uuid: Uuid) -> Self {
        Self {
            name,
            res,
            internal_uuid,
        }
    }

    pub fn internal_uuid(&self) -> Uuid {
        self.internal_uuid
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}, {}", self.name, self.res, self.internal_uuid)
    }
}
