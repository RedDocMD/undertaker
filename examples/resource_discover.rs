use std::{
    collections::HashMap,
    env,
    error::Error,
    fs::File,
    io::prelude::*,
    process::{Command, Stdio},
    rc::Rc,
};

use colored::*;
use log::{debug, info, warn};
use syn::{Expr, Item};
use undertaker::{
    async_detect::{async_in_block, AsyncCode},
    cfg::{CFGBlock, CFGNodePtr, CFGNodeStrongPtr},
    context::Context,
    discover::{callable_from_expr, creator_from_block, Object},
    types::{parse_resource_file, Callable, CallableType, Monomorphisable, ResourceFile},
    uses,
};
use uuid::Uuid;

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::builder()
        .format(|buf, rec| {
            let line = rec
                .line()
                .map_or(String::new(), |line| format!(":{}", line));
            let file = rec
                .file()
                .map_or(String::new(), |file| format!(" {}", file));
            let prelude = format!("[{}{}{}]", rec.level(), file, line);
            writeln!(buf, "{} {}", prelude.cyan(), rec.args())
        })
        .write_style(env_logger::WriteStyle::Always)
        .init();

    let args: Vec<String> = env::args().collect();

    let info_filename = &args[1];
    let info = parse_resource_file(info_filename)?;

    println!("{}", "Callables".magenta());
    for callable in info.callables() {
        println!("{} {}", "⇒".blue(), callable);
    }
    println!();

    let code_filename = &args[2];
    let mut file = File::open(code_filename)?;
    let mut src = String::new();
    file.read_to_string(&mut src)?;

    let ast = syn::parse_file(&src)?;

    let top_uses = uses::extract_global_uses(&ast);
    let mut ctx = Context::new();
    ctx.add_use_paths(top_uses);

    for item in ast.items {
        if let Item::Fn(func) = item {
            let sig = &func.sig;
            if sig.ident == "main" {
                ctx.enter_block();
                let block_uses = uses::extract_block_uses(func.block.as_ref());
                ctx.add_use_paths(block_uses);

                let mut binding_cnt = 0;
                loop {
                    for res in info.specializations().values() {
                        let gen_creators_map = info.gen_creators();
                        let creator_ids = &gen_creators_map[res.id()];
                        let gen_callables = info.gen_callables();
                        for creator_id in creator_ids {
                            let gen_creator = &gen_callables[creator_id];
                            let creator = gen_creator.monomorphise(res.type_map().clone()).unwrap();
                            debug!("{}", creator.to_string().red());
                            creator_from_block(
                                func.block.as_ref(),
                                &creator,
                                &res,
                                &mut ctx,
                                &info,
                            );
                        }
                    }
                    let new_binding_cnt = ctx.tot_binding_cnt();
                    if new_binding_cnt == binding_cnt {
                        break;
                    } else {
                        binding_cnt = new_binding_cnt;
                    }
                }
                println!("\n{}:\n{}", "Context".yellow(), ctx);
                let blocks = async_in_block(&func.block);
                println!("Found {} blocks", blocks.len().to_string().yellow());

                // let dep_graphs: Vec<Rc<DepGraph>> = blocks
                //     .into_iter()
                //     .map(|code| match code {
                //         AsyncCode::Block(block) => DepGraph::from_block(&block.block),
                //         AsyncCode::Closure(closure) => DepGraph::from_expr(*closure.body),
                //     })
                //     .collect();
                // println!("\n{}", "DepGraphs:".cyan());
                // for dep_graph in &dep_graphs {
                //     println!("{}", dep_graph);
                // }
                let cfgs: Vec<CFGBlock<'_>> = blocks
                    .iter()
                    .map(|code| match code {
                        AsyncCode::Block(block) => CFGBlock::from_block(&block.block),
                        AsyncCode::Closure(closure) => CFGBlock::from_expr(closure.body.as_ref()),
                    })
                    .filter(Option::is_some)
                    .flatten()
                    .collect();
                assert_eq!(cfgs.len(), 2);
                for (idx, cfg) in cfgs.iter().enumerate() {
                    let path = format!("dot/cfg{}.pdf", idx);
                    create_cfg_pdf(cfg, &path);
                }

                let mut cfg_blockers = Vec::new();

                for gen_blockers in info.gen_blockers().values() {
                    for gen_blocker in gen_blockers {
                        for callable in info.callables() {
                            if callable.id() == gen_blocker {
                                debug!("Trying blocker {}", callable);
                                for cfg in &cfgs {
                                    let obs = callable_from_cfg(cfg, &callable, &ctx, &info);
                                    cfg_blockers.push(obs);
                                }
                            }
                        }
                    }
                }

                ctx.exit_block();
                break;
            }
        }
    }

    Ok(())
}

fn create_cfg_pdf(cfg: &CFGBlock<'_>, path: &str) {
    let mut proc = Command::new("dot")
        .args(&["-Tpdf", "-o", path])
        .stdin(Stdio::piped())
        .spawn()
        .expect("failed to get dot command");
    let dot_string = cfg.dot_description();
    let mut stdin = proc.stdin.take().expect("failed to open stdin");
    std::thread::spawn(move || {
        stdin
            .write_all(dot_string.as_bytes())
            .expect("failed to write to stdin");
    });
    let exit = proc.wait().unwrap();
    if exit.success() {
        info!("Created DOT graph in {}", path);
    } else {
        warn!("Failed to create DOT graph");
    }
}

fn callable_from_cfg<'ast>(
    cfg: &CFGBlock<'ast>,
    callable: &Callable,
    ctx: &Context,
    info: &ResourceFile,
) -> HashMap<Uuid, Vec<(Object, CFGNodeStrongPtr<'ast>)>> {
    let mut stack = Vec::new();
    stack.push(Rc::clone(cfg.head()));
    let mut calls: HashMap<Uuid, Vec<(Object, CFGNodeStrongPtr<'ast>)>> = HashMap::new();
    while !stack.is_empty() {
        let node = stack.pop().unwrap();
        let node_cpy = Rc::clone(&node);
        let node = node.borrow();
        for succ in node.succ() {
            if let CFGNodePtr::Strong(succ) = succ {
                stack.push(Rc::clone(succ));
            }
        }
        let expr = match node.expr() {
            undertaker::cfg::CFGExpr::Expr(expr) => *expr,
            undertaker::cfg::CFGExpr::Item(_) => continue,
            undertaker::cfg::CFGExpr::ForGuard(_, expr) => *expr,
            undertaker::cfg::CFGExpr::WhileGuard(expr) => *expr,
            undertaker::cfg::CFGExpr::IfGuard(expr) => *expr,
            undertaker::cfg::CFGExpr::Phantom => continue,
        };
        if callable_from_expr(expr, callable, ctx, info) {
            let ob = object_from_call(expr, callable, ctx).expect("expected to find object here");
            let uuid = ob.internal_uuid();
            if calls.contains_key(&uuid) {
                let obs = calls.get_mut(&uuid).unwrap();
                obs.push((ob, node_cpy));
            } else {
                calls.insert(uuid, vec![(ob, node_cpy)]);
            }
        }
    }
    calls
}

fn object_from_call(expr: &Expr, callable: &Callable, ctx: &Context) -> Option<Object> {
    // Precondition: expr actually contains a call to callable.
    // Precondition: callable is a method.
    // Precondition: If there are nested method calls, outermost one is the right one
    // Precondition: There is only one method call
    if callable.ctype() != CallableType::Method {
        todo!("object_from_call only supports method calls");
    }
    match expr {
        Expr::Assign(expr) => {
            let ob = object_from_call(expr.left.as_ref(), callable, ctx);
            if ob.is_some() {
                return ob;
            } else {
                return object_from_call(expr.right.as_ref(), callable, ctx);
            }
        }
        Expr::AssignOp(expr) => {
            let ob = object_from_call(expr.left.as_ref(), callable, ctx);
            if ob.is_some() {
                ob
            } else {
                object_from_call(expr.right.as_ref(), callable, ctx)
            }
        }
        Expr::Await(expr) => object_from_call(expr.base.as_ref(), callable, ctx),
        Expr::Let(expr) => object_from_call(expr.expr.as_ref(), callable, ctx),
        Expr::MethodCall(expr) => {
            let reciever = expr.receiver.as_ref();
            let receiver = match reciever {
                Expr::Path(_) => reciever,
                Expr::Reference(expr) => expr.expr.as_ref(),
                _ => todo!("Supports only direct method calls"),
            };
            if let Expr::Path(expr) = receiver {
                let segments: Vec<String> = expr
                    .path
                    .segments
                    .iter()
                    .map(|seg| seg.ident.to_string())
                    .collect();
                if segments.len() == 1 {
                    let name = &segments[0];
                    ctx.get_binding(name).cloned()
                } else {
                    todo!("only support literal names")
                }
            } else {
                todo!("Supports only direct method calls")
            }
        }
        _ => None,
    }
}
