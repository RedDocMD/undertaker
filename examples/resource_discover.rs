use std::{env, error::Error, fs::File, io::prelude::*};

use colored::*;
use syn::Item;
use undertaker::{
    context::Context,
    discover::creator_from_block,
    types::{parse_resource_file, Monomorphisable},
    uses,
};

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();

    let info_filename = &args[1];
    let info = parse_resource_file(info_filename)?;

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
            if sig.ident.to_string() == String::from("main") {
                ctx.enter_block();
                let block_uses = uses::extract_block_uses(func.block.as_ref());
                ctx.add_use_paths(block_uses);

                for (_, res) in info.specializations() {
                    let gen_creators_map = info.gen_creators();
                    let creator_ids = &gen_creators_map[res.id()];
                    let gen_callables = info.gen_callables();
                    for creator_id in creator_ids {
                        let gen_creator = &gen_callables[creator_id];
                        let creator = gen_creator.monomorphise(res.type_map().clone()).unwrap();
                        creator_from_block(func.block.as_ref(), &creator, &res, &mut ctx, &info);
                    }
                }

                println!("\n{}:\n{}", "Context".yellow(), ctx);
                ctx.exit_block();
                break;
            }
        }
    }

    Ok(())
}