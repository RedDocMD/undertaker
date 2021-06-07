use std::{
    collections::HashMap,
    error::Error,
    fmt::{self, Display, Formatter},
    fs::File,
    io::Read,
    path,
};

use linked_hash_map::LinkedHashMap;
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

impl TypeParam {
    fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
        }
    }
}

impl PartialEq for TypeParam {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl GenericResource {
    fn substitute_type_params(&self, new_params: Vec<TypeParam>) -> Option<Self> {
        if new_params.len() != self.type_params.len() {
            return None;
        }
        Some(Self {
            id: self.id.clone(),
            type_params: new_params,
        })
    }
}

#[derive(Debug)]
pub struct GenericCallable {
    id: ResourceID,
    type_params: Vec<TypeParam>,
    ctype: CallableType,
    args: Vec<GenericArg>,
    ret: GenericReturn,
}

#[derive(Debug)]
pub enum CallableType {
    Function,
    Method,
}

impl CallableType {
    fn from_str(s: &str) -> Option<Self> {
        if s == "Function" {
            Some(Self::Function)
        } else if s == "Method" {
            Some(Self::Method)
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub enum GenericArg {
    Type(TypeParam),
    Res(GenericResource),
}

#[derive(Debug)]
pub struct GenericReturn {
    rets: Vec<GenericArg>,
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
        .ok_or(ParseErr::new("no resources key"))?
        .as_vec()
        .ok_or(ParseErr::new("expected resources to be an array"))?;

    let mut resources = HashMap::new();
    for yaml in resources_yaml {
        let (name, resource) = parse_resource_from_yaml(yaml)?;
        println!("{} => {:?}", name, resource);
        resources.insert(name, resource);
    }

    // Parse callables
    let callables_yaml = doc
        .get(&Yaml::from_str("callables"))
        .ok_or(ParseErr::new("no callables key"))?
        .as_vec()
        .ok_or(ParseErr::new("expected callables to be an array"))?;

    let mut callables = HashMap::new();
    for yaml in callables_yaml {
        let (name, callable) = parse_callable_from_yaml(yaml, &resources)?;
        println!("{} => {:?}", name, callable);
        callables.insert(name, callable);
    }
    Ok(())
}

fn parse_resource_from_yaml(yaml: &Yaml) -> Result<(String, GenericResource), Box<dyn Error>> {
    let resource_yaml = yaml
        .as_hash()
        .ok_or(ParseErr::new("invalid resource format"))?;
    if resource_yaml.len() != 1 {
        return Err(Box::new(ParseErr::new("invalid resource format")));
    }
    for (key, value) in resource_yaml {
        let name = key.as_str().unwrap();
        let value = value
            .as_hash()
            .ok_or(ParseErr::new("invalid resource format"))?;
        let id_str = value
            .get(&Yaml::from_str("id"))
            .ok_or(ParseErr::new("no id param of resource"))?
            .as_str()
            .ok_or(ParseErr::new("id must be a string"))?;
        let type_params_yaml = value
            .get(&Yaml::from_str("type_params"))
            .ok_or("no type_params param of resource")?
            .as_vec()
            .ok_or(ParseErr::new("type_params must be a list"))?;

        let id = uses::convert_to_path(&id_str.split("::").collect())
            .ok_or(ParseErr(format!("invalid id: {}", id_str)))?;
        let type_params: Vec<TypeParam> = type_params_yaml
            .into_iter()
            .map(|param| TypeParam::new(param.as_str().expect("type param must be a string")))
            .collect();

        return Ok((name.to_string(), GenericResource { id, type_params }));
    }
    unreachable!();
}

fn parse_callable_from_yaml(
    yaml: &Yaml,
    resources_map: &HashMap<String, GenericResource>,
) -> Result<(String, GenericCallable), Box<dyn Error>> {
    fn generic_resource_from_yaml(
        hash: &LinkedHashMap<Yaml, Yaml>,
        resources_map: &HashMap<String, GenericResource>,
    ) -> Result<GenericResource, Box<dyn Error>> {
        let name = hash
            .get(&Yaml::from_str("name"))
            .ok_or(ParseErr::new("name must be provided for res"))?
            .as_str()
            .ok_or(ParseErr::new("name must be a string"))?;
        let resource = resources_map
            .get(&name.to_string())
            .ok_or(ParseErr(format!("resource {} not found", name)))?;
        let type_params_vec = hash
            .get(&Yaml::from_str("type_params"))
            .ok_or(ParseErr::new("type_params must be provided for res"))?
            .as_vec()
            .ok_or(ParseErr::new("type_params must be a vector"))?;
        let type_params: Vec<TypeParam> = type_params_vec
            .iter()
            .map(|param| TypeParam::new(param.as_str().expect("type param must be a string")))
            .collect();
        let new_resource = resource
            .substitute_type_params(type_params)
            .ok_or(ParseErr(format!(
                "invalid number of type params for {}",
                name
            )))?;
        Ok(new_resource)
    }

    fn arg_vec_from_yaml(
        args_vec: &Vec<Yaml>,
        resources_map: &HashMap<String, GenericResource>,
        type_params: &Vec<TypeParam>,
    ) -> Result<Vec<GenericArg>, Box<dyn Error>> {
        let mut args = Vec::new();
        for arg_hash in args_vec {
            let arg_hash = arg_hash
                .as_hash()
                .ok_or(ParseErr::new("args must contain hash"))?;
            let cat = arg_hash
                .get(&Yaml::from_str("cat"))
                .ok_or(ParseErr::new("arg hash must have a cat"))?
                .as_str()
                .ok_or(ParseErr::new("cat must be a string"))?;

            if cat == "Res" {
                let arg = generic_resource_from_yaml(arg_hash, resources_map)?;
                for type_param in &arg.type_params {
                    if !type_params.contains(type_param) {
                        return Err(Box::new(ParseErr(format!(
                            "{} is not a type param defined above",
                            type_param.name
                        ))));
                    }
                }
                args.push(GenericArg::Res(arg));
            } else if cat == "Type" {
                let type_name = arg_hash
                    .get(&Yaml::from_str("type_param"))
                    .ok_or(ParseErr::new("type_param must be specified for Type arg"))?
                    .as_str()
                    .ok_or(ParseErr::new("type_param must be a string"))?;
                let type_param = TypeParam::new(type_name);
                if !type_params.contains(&type_param) {
                    return Err(Box::new(ParseErr(format!(
                        "{} is not a type param defined above",
                        type_name
                    ))));
                }
                args.push(GenericArg::Type(type_param));
            } else {
                return Err(Box::new(ParseErr(format!(
                    "{} isn't a valid type of arg",
                    cat
                ))));
            }
        }
        Ok(args)
    }

    let callable_yaml = yaml.as_hash().ok_or(ParseErr(String::from(
        "expected callable to be specified as hash",
    )))?;
    if callable_yaml.len() != 1 {
        return Err(Box::new(ParseErr::new("invalid callable format")));
    }
    for (key, value) in callable_yaml {
        let name = key.as_str().unwrap();
        let value = value
            .as_hash()
            .ok_or(ParseErr::new("invalid callable format"))?;
        let id_str = value
            .get(&Yaml::from_str("id"))
            .ok_or(ParseErr::new("no id param of callable"))?
            .as_str()
            .ok_or(ParseErr::new("id must be a string"))?;
        let type_params_yaml = value
            .get(&Yaml::from_str("type_params"))
            .ok_or("no type_params param of callable")?
            .as_vec()
            .ok_or(ParseErr::new("type_params must be a list"))?;
        let ctype_str = value
            .get(&Yaml::from_str("ctype"))
            .ok_or(ParseErr::new("no ctype param of callable"))?
            .as_str()
            .ok_or(ParseErr::new("ctype must be a string"))?;
        let args_vec = value
            .get(&Yaml::from_str("args"))
            .ok_or(ParseErr::new("no args param of callable"))?
            .as_vec()
            .ok_or(ParseErr::new("args must be a list"))?;
        let ret_vec = value
            .get(&Yaml::from_str("ret"))
            .ok_or(ParseErr::new("no ret param of callable"))?
            .as_vec()
            .ok_or(ParseErr::new("ret must be a list"))?;

        let id = uses::convert_to_path(&id_str.split("::").collect())
            .ok_or(ParseErr(format!("invalid id: {}", id_str)))?;
        let type_params: Vec<TypeParam> = type_params_yaml
            .into_iter()
            .map(|param| TypeParam::new(param.as_str().expect("type param must be a string")))
            .collect();
        let ctype = CallableType::from_str(ctype_str)
            .ok_or(ParseErr(format!("invalid ctype: {}", ctype_str)))?;
        let args = arg_vec_from_yaml(args_vec, resources_map, &type_params)?;
        let ret = GenericReturn {
            rets: arg_vec_from_yaml(ret_vec, resources_map, &type_params)?,
        };

        let callable = GenericCallable {
            id,
            type_params,
            ctype,
            args,
            ret,
        };
        return Ok((name.to_string(), callable));
    }
    unreachable!();
}

#[derive(Debug)]
pub struct ParseErr(String);

impl Display for ParseErr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Error for ParseErr {}

impl ParseErr {
    fn new(s: &str) -> Self {
        Self(s.to_string())
    }
}
