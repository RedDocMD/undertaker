use std::{env, error::Error, fs::File, io::prelude::*};

use colored::*;
use syn::Item;
use undertaker::{
    context::Context,
    discover::callable_from_block,
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

                for (key, res) in &info.specializations {
                    let gen_res_name = if let Some(type_idx) = key.find("<") {
                        &key.as_str()[0..type_idx]
                    } else {
                        key.as_str()
                    };
                    for creator_name in &info.gen_creators[gen_res_name] {
                        let gen_creator = &info.gen_callables[creator_name];
                        let creator = gen_creator.monomorphise(res.type_map().clone()).unwrap();

                        callable_from_block(func.block.as_ref(), &creator, &ctx);
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
