use std::io::prelude::*;
use std::{env, fs::File, process};

use syn::Item;

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
            let sig = func.sig;
            if sig.ident.to_string() == String::from("main") {
                println!("Found main");
            }
        }
    }
}
