# Undertaker
This project has an archetypal deadlock example in `data/simple-deadlock.rs`.
To run a sample detector from the project, the following tools are needed:
- Rust toolchain (`cargo` and `rustup`). Installation instructions are [here](https://www.rust-lang.org/tools/install).
- Graphviz and the `dot` binary.

Then execute the following, from the project directory:
```
git clone https://github.com/RedDocMD/undertaker
cd undertaker
mkdir dot
RUST_LOG=Off cargo run --example resource_discover data/resources.yml data/simple-deadlock.rs
```
Apart from printing the fact that a deadlock has been detected, the `dot` directory will contain PDF's of the CFG of the async blocks.
