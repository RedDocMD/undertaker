use std::{env, error::Error};

use colored::*;
use undertaker::types::parse_resource_file;

fn main() -> Result<(), Box<dyn Error>> {
    let filename = env::args()
        .skip(1)
        .next()
        .expect("First arg must be filename");
    let info = parse_resource_file(filename)?;
    println!("{}", "Generic Resources:".yellow());
    for (name, gen_res) in info.gen_resources() {
        println!("{} => {}", name, gen_res);
    }
    println!("\n{}", "Generic Callables:".yellow());
    for (name, gen_call) in info.gen_callables() {
        println!("{} => {}", name, gen_call);
    }
    println!("\n{}", "Generic Creators:".yellow());
    for (res, creators) in info.gen_creators() {
        let res = &info.gen_resources()[res];
        print!("{} => ", res);
        for (idx, creator) in creators.iter().enumerate() {
            let creator = &info.gen_callables()[creator];
            print!("{}", creator);
            if idx != creators.len() - 1 {
                print!(", ");
            }
        }
        println!();
    }
    println!("\n{}", "Generic Blockers:".yellow());
    for (res, blockers) in info.gen_blockers() {
        let res = &info.gen_resources()[res];
        print!("{} => ", res);
        for (idx, blocker) in blockers.iter().enumerate() {
            let blocker = &info.gen_callables()[blocker];
            print!("{}", blocker);
            if idx != blockers.len() - 1 {
                print!(", ");
            }
        }
        println!();
    }
    println!("\n{}", "Resources:".yellow());
    for (name, res) in info.specializations() {
        println!("{} => {}", name, res);
    }
    Ok(())
}
