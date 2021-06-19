use std::{env, error::Error, fs::File, io::prelude::*, rc::Rc};

use colored::*;
use log::debug;
use syn::Item;
use undertaker::{
    async_detect::{async_in_block, AsyncCode},
    context::Context,
    discover::creator_from_block,
    graph::DepGraph,
    types::{parse_resource_file, Monomorphisable},
    uses,
};

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::builder()
        .filter_level(log::LevelFilter::Info)
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

                let dep_graphs: Vec<Rc<DepGraph>> = blocks
                    .into_iter()
                    .map(|code| match code {
                        AsyncCode::Block(block) => DepGraph::from_block(&block.block),
                        AsyncCode::Closure(closure) => DepGraph::from_expr(*closure.body),
                    })
                    .collect();
                println!("\n{}", "DepGraphs:".cyan());
                for dep_graph in &dep_graphs {
                    println!("{}", dep_graph);
                }

                ctx.exit_block();
                break;
            }
        }
    }

    Ok(())
}
