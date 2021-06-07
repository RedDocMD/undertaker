use std::{env, error::Error};

use undertaker::types::parse_resource_file;

fn main() -> Result<(), Box<dyn Error>> {
    let filename = env::args()
        .skip(1)
        .next()
        .expect("First arg must be filename");
    parse_resource_file(filename)?;
    Ok(())
}
