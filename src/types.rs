use std::{
    collections::HashMap,
    error::Error,
    fmt::{self, Display, Formatter},
    fs::File,
    io::Read,
    path,
};

use yaml_rust::{Yaml, YamlLoader};

use crate::uses::{self, UsePath};

pub type ResourceID = UsePath;

#[derive(Debug)]
pub struct GenericResource {
    id: ResourceID,
    type_params: Vec<TypeParam>,
}

#[derive(Debug)]
pub struct TypeParam {
    name: String,
}

pub struct GenericCallable {
    id: ResourceID,
    type_params: Vec<TypeParam>,
    ctype: CallableType,
    args: Vec<GenericArg>,
    ret: GenericReturn,
}

pub enum CallableType {
    Function,
    Method,
}

pub enum GenericArg {
    Type(TypeParam),
    Res(GenericResource),
}

pub enum GenericReturn {
    Void,
    One(GenericArg),
    Pair(GenericArg, GenericArg),
}

pub fn parse_resource_file<T: AsRef<path::Path>>(filepath: T) -> Result<(), Box<dyn Error>> {
    let mut file = File::open(filepath.as_ref())?;
    let mut file_content = String::new();
    file.read_to_string(&mut file_content)?;
    let docs = YamlLoader::load_from_str(&file_content)?;
    assert!(docs.len() == 1);
    let doc = docs[0].as_hash().unwrap();

    // Parse resouces
    let resources_yaml = doc
        .get(&Yaml::from_str("resources"))
        .ok_or(InvalidResourceFile(String::from("no resources key")))?
        .as_vec()
        .ok_or(InvalidResourceFile(String::from(
            "expected resources to be an array",
        )))?;

    let mut resources = HashMap::new();
    for yaml in resources_yaml {
        let (name, resource) = parse_resource_from_yaml(yaml)?;
        println!("{} => {:?}", name, resource);
        resources.insert(name, resource);
    }

    // Parse callables
    Ok(())
}

fn parse_resource_from_yaml(yaml: &Yaml) -> Result<(String, GenericResource), Box<dyn Error>> {
    let resource_yaml = yaml
        .as_hash()
        .ok_or(InvalidResourceFile(String::from("invalid resource format")))?;
    if resource_yaml.len() != 1 {
        return Err(Box::new(InvalidResourceFile(String::from(
            "invalid resource format",
        ))));
    }
    for (key, value) in resource_yaml {
        let name = key.as_str().unwrap();
        let value = value
            .as_hash()
            .ok_or(InvalidResourceFile(String::from("invalid resource format")))?;
        let id_str = value
            .get(&Yaml::from_str("id"))
            .ok_or(InvalidResourceFile(String::from("no id param of resource")))?
            .as_str()
            .ok_or(InvalidResourceFile(String::from("id must be a string")))?;
        let type_params_yaml = value
            .get(&Yaml::from_str("type_params"))
            .ok_or("no type_params param of resource")?
            .as_vec()
            .ok_or(InvalidResourceFile(String::from(
                "type_params must be a list",
            )))?;

        let id = uses::convert_to_path(&id_str.split("::").collect())
            .ok_or(InvalidResourceFile(format!("invalid id: {}", id_str)))?;
        let type_params: Vec<TypeParam> = type_params_yaml
            .into_iter()
            .map(|param| TypeParam {
                name: String::from(param.as_str().expect("type param must be a string")),
            })
            .collect();

        return Ok((name.to_string(), GenericResource { id, type_params }));
    }
    unreachable!();
}

#[derive(Debug)]
pub struct InvalidResourceFile(String);

impl Display for InvalidResourceFile {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Error for InvalidResourceFile {}
