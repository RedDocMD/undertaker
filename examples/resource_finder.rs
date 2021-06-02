use std::io::prelude::*;
use std::{env, fs::File, process};

use syn::Item;
use undertaker::resource;
use undertaker::resource::{Creator, DirectCreator, Resource, TupleCreator};
use undertaker::uses;

#[macro_use]
extern crate undertaker;

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
    let reciever_res = Resource::single(
        path!["tokio", "sync", "oneshot", "Reciever"],
        vec![Creator::Tuple(TupleCreator::new(
            path!["tokio", "sync", "oneshot", "channel"],
            vec![],
            1,
        ))],
    );
    let notify_res = Resource::single(
        path!["tokio", "sync", "Notify"],
        vec![Creator::Direct(DirectCreator::new(
            path!["tokio", "sync", "Notify", "new"],
            vec![],
        ))],
    );
    let notify = notify_res.as_single().unwrap().clone();
    let arc_notify = Resource::single(
        path!["std", "sync", "Arc"],
        vec![
            Creator::Direct(DirectCreator::new(
                path!["std", "sync", "Arc", "new"],
                vec![],
            )),
            Creator::Direct(DirectCreator::new(
                path!["std", "sync", "Arc", "clone"],
                vec![],
            )),
        ],
    )
    .nest(notify)
    .to_owned();

    let top_uses = uses::extract_global_uses(&ast);

    for item in ast.items {
        if let Item::Fn(func) = item {
            let sig = &func.sig;
            if sig.ident.to_string() == String::from("main") {
                let obj = resource::resource_creation_from_block(
                    func.block.as_ref(),
                    &reciever_res,
                    &top_uses,
                )
                .unwrap();
                println!("{}", obj);
                let obj = resource::resource_creation_from_block(
                    func.block.as_ref(),
                    &notify_res,
                    &top_uses,
                )
                .unwrap();
                println!("{}", obj);
                let obj = resource::resource_creation_from_block(
                    func.block.as_ref(),
                    &arc_notify,
                    &top_uses,
                )
                .unwrap();
                println!("{}", obj);
                break;
            }
        }
    }
}
