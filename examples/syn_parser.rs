use std::hash::Hash;
use std::io::prelude::*;
use std::{env, fs::File, process};

use syn::{Expr, Item, ItemFn, Stmt};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        process::exit(1);
    }
    let filename = &args[1];
    let mut file = File::open(filename).expect(&format!("Failed to open {}", filename));
    let mut src = String::new();
    file.read_to_string(&mut src)
        .expect(&format!("Failed to read {}", filename));

    let ast = syn::parse_file(&src).expect(&format!("Failed to parse {}", filename));

    for item in ast.items {
        if let Item::Fn(func) = item {
            let sig = &func.sig;
            if sig.ident.to_string() == String::from("main") {
                handle_main(&func);
                break;
            }
        }
    }

    let new_src = "
fn foo() {
    let (tx1, rx1) = oneshot::channel::<i64>();
    let (tx1, rx1) = oneshot::channel::<i64>();
}
    ";
    let ast = syn::parse_file(&new_src).unwrap();
    let mut exprs = Vec::new();
    for item in &ast.items {
        if let Item::Fn(func) = item {
            for stmt in &func.block.stmts {
                if let Stmt::Local(stmt) = stmt {
                    if let Some((_, expr)) = &stmt.init {
                        exprs.push(expr.as_ref());
                    }
                }
            }
        }
    }
    assert_eq!(exprs.len(), 2);
    assert_ne!(exprs[0], exprs[1]);
}

fn handle_main(func: &ItemFn) {
    let stmts = &func.block.stmts;
    // Trying to find statements of the form "let <iden> = <struct_name>::new(...);"
    for stmt in stmts {
        if let Stmt::Local(local) = stmt {
            if let Some((_, expr)) = &local.init {
                if let Expr::Call(call_expr) = expr.as_ref() {
                    if let Expr::Path(call_path) = call_expr.func.as_ref() {
                        let path = &call_path.path;
                        if path.leading_colon.is_none() {
                            let segs: Vec<String> = path
                                .segments
                                .iter()
                                .map(|seg| seg.ident.to_string())
                                .collect();
                            if segs == vec!["oneshot", "channel"] {
                                println!("Oneshot channel");
                                use syn::Pat::*;
                                match &local.pat {
                                    Tuple(tup) => {
                                        for part in tup.elems.iter() {
                                            if let Ident(ident) = part {
                                                println!("{}", ident.ident);
                                            }
                                        }
                                    }
                                    _ => println!("{:?}", local.pat),
                                }
                            } else if segs == vec!["Arc", "new"] || segs == vec!["Arc", "clone"] {
                                println!("Arc:");
                                use syn::Pat::*;
                                match &local.pat {
                                    Ident(ident) => {
                                        println!("{}", ident.ident);
                                    }
                                    _ => println!("{:?}", local.pat),
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
