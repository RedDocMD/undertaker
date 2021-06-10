use std::{
    collections::HashMap,
    error::Error,
    fmt::{self, Display, Formatter, Write},
    fs::File,
    hash::{Hash, Hasher},
    io::Read,
    path,
};

use linked_hash_map::LinkedHashMap;
use yaml_rust::{Yaml, YamlLoader};

use crate::uses::{self, UsePath};

pub type ResourceID = UsePath;

#[derive(Debug, Eq)]
pub struct GenericResource {
    id: ResourceID,
    type_params: Vec<TypeParam>,
}

#[derive(Debug, Eq, Clone)]
pub struct TypeParam {
    name: String,
}

pub trait Monomorphisable<T> {
    fn monomorphise(&self, type_map: HashMap<TypeParam, Resource>) -> Option<T>;

    fn is_auto_monomorphisable(&self) -> bool {
        self.monomorphise(HashMap::new()).is_some()
    }

    fn auto_monomorphise(&self) -> Option<T> {
        self.monomorphise(HashMap::new())
    }
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

impl Hash for TypeParam {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl Display for TypeParam {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
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

    fn new_without_types(id: ResourceID) -> Self {
        Self {
            id,
            type_params: vec![],
        }
    }
}

impl PartialEq for GenericResource {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Hash for GenericResource {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.to_string().hash(state);
    }
}

impl Monomorphisable<Resource> for GenericResource {
    fn monomorphise(&self, type_map: HashMap<TypeParam, Resource>) -> Option<Resource> {
        if type_map.len() != self.type_params.len() {
            return None;
        }
        let valid = self
            .type_params
            .iter()
            .all(|type_param| type_map.contains_key(type_param));
        if !valid {
            return None;
        }
        Some(Resource {
            id: self.id.clone(),
            type_map,
        })
    }
}

impl Display for GenericResource {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let params: Vec<String> = self
            .type_params
            .iter()
            .map(|item| item.name.clone())
            .collect();
        let params_str = params.join(", ");
        write!(f, "{}", self.id)?;
        if params.len() != 0 {
            write!(f, "<{}>", params_str)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct Resource {
    id: ResourceID,
    type_map: HashMap<TypeParam, Resource>,
}

impl Display for Resource {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.id)?;
        if self.type_map.len() != 0 {
            let reso_strs: Vec<String> = self
                .type_map
                .iter()
                .map(|(name, res)| {
                    let mut str = String::new();
                    write!(&mut str, "{} = {}", name, res)
                        .expect("expected to be able to write to a string");
                    str
                })
                .collect();
            write!(f, "<{}>", reso_strs.join(", "))?;
        }
        Ok(())
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
pub struct Callable {
    id: ResourceID,
    ctype: CallableType,
    args: Vec<Resource>,
    ret: Return,
}

impl Display for GenericCallable {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let args: Vec<String> = self
            .args
            .iter()
            .map(|item| {
                let mut output = String::new();
                write!(&mut output, "{}", item).expect("failed to string");
                output
            })
            .collect();
        let args_str = args.join(", ");
        let params: Vec<String> = self
            .type_params
            .iter()
            .map(|item| item.name.clone())
            .collect();
        let params_str = params.join(", ");
        write!(f, "{}", self.id)?;
        if params.len() != 0 {
            write!(f, "<{}>", params_str)?;
        }
        write!(f, "({}) {}", args_str, self.ret)
    }
}

impl Monomorphisable<Callable> for GenericCallable {
    fn monomorphise(&self, type_map: HashMap<TypeParam, Resource>) -> Option<Callable> {
        let mut args = Vec::new();
        for gen_arg in &self.args {
            if let Some(arg) = gen_arg.monomorphise(type_map.clone()) {
                args.push(arg);
            } else {
                return None;
            }
        }
        if let Some(ret) = self.ret.monomorphise(type_map) {
            Some(Callable {
                id: self.id.clone(),
                ctype: self.ctype,
                args,
                ret,
            })
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy)]
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

impl Display for CallableType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Function => write!(f, "Function"),
            Self::Method => write!(f, "Method"),
        }
    }
}

#[derive(Debug)]
pub enum GenericArg {
    Type(TypeParam),
    Res(GenericResource),
}

impl Display for GenericArg {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Type(param) => write!(f, "{}", param.name),
            Self::Res(res) => write!(f, "{}", res),
        }
    }
}

impl Monomorphisable<Resource> for GenericArg {
    fn monomorphise(&self, type_map: HashMap<TypeParam, Resource>) -> Option<Resource> {
        match self {
            Self::Type(type_param) => type_map.get(type_param).cloned(),
            Self::Res(gen_res) => gen_res.monomorphise(type_map),
        }
    }
}

#[derive(Debug)]
pub struct GenericReturn {
    rets: Vec<GenericArg>,
}

#[derive(Debug)]
pub struct Return {
    rets: Vec<Resource>,
}

impl Display for GenericReturn {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let args: Vec<String> = self
            .rets
            .iter()
            .map(|item| {
                let mut output = String::new();
                write!(&mut output, "{}", item).expect("failed to string");
                output
            })
            .collect();
        let args_str = args.join(", ");
        if args.len() == 1 {
            write!(f, "-> {}", args_str)
        } else if args.len() > 1 {
            write!(f, "-> ({})", args_str)
        } else {
            Ok(())
        }
    }
}

impl Monomorphisable<Return> for GenericReturn {
    fn monomorphise(&self, type_map: HashMap<TypeParam, Resource>) -> Option<Return> {
        let mut rets = Vec::new();
        for gen_ret in &self.rets {
            if let Some(ret) = gen_ret.monomorphise(type_map.clone()) {
                rets.push(ret);
            } else {
                return None;
            }
        }
        Some(Return { rets })
    }
}

fn primitive_types_as_resources() -> HashMap<String, GenericResource> {
    let types = vec![
        "bool", "u8", "u16", "u32", "u64", "u128", "i8", "i16", "i32", "i64", "i128", "f32", "f64",
        "usize", "isize", "char", "str",
    ];

    types
        .iter()
        .map(|item| {
            let path = uses::convert_to_path(&vec![item.to_string()]).unwrap();
            (item.to_string(), GenericResource::new_without_types(path))
        })
        .collect()
}

pub struct ResourceFile {
    pub gen_resources: HashMap<String, GenericResource>,
    pub gen_callables: HashMap<String, GenericCallable>,
    pub gen_creators: HashMap<String, Vec<String>>,
    pub specializations: Vec<Resource>,
}

pub fn parse_resource_file<T: AsRef<path::Path>>(
    filepath: T,
) -> Result<ResourceFile, Box<dyn Error>> {
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

    let mut resources = primitive_types_as_resources();
    for yaml in resources_yaml {
        let (name, resource) = parse_resource_from_yaml(yaml)?;
        resources.insert(name, resource);
    }

    // Parse callables
    let mut callables = HashMap::new();
    let mut creators = HashMap::new();

    if let Some(callables_yaml) = doc.get(&Yaml::from_str("callables")) {
        let callables_yaml = callables_yaml
            .as_vec()
            .ok_or(ParseErr::new("expected callables to be an array"))?;

        for yaml in callables_yaml {
            let (name, callable) = parse_callable_from_yaml(yaml, &resources)?;
            callables.insert(name, callable);
        }

        if let Some(creators_yaml) = doc.get(&Yaml::from_str("creators")) {
            let creators_yaml = creators_yaml
                .as_vec()
                .ok_or(ParseErr::new("expected creators to be an array"))?;

            for creator_yaml in creators_yaml {
                let (res, creators_vec) =
                    parse_creator_from_yaml(creator_yaml, &resources, &callables)?;
                creators.insert(res, creators_vec);
            }
        }
    }

    let mut specializations = Vec::new();

    if let Some(specialize_yaml) = doc.get(&Yaml::from_str("specialize")) {
        let specialize_yaml = specialize_yaml
            .as_vec()
            .ok_or(ParseErr::new("expected specializartions to be an array"))?;
        for yaml in specialize_yaml {
            let (_, res) = parse_specialization_from_yaml(yaml, &resources)?;
            specializations.push(res);
        }
    }

    Ok(ResourceFile {
        gen_resources: resources,
        gen_callables: callables,
        gen_creators: creators,
        specializations,
    })
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

fn parse_creator_from_yaml(
    yaml: &Yaml,
    res_map: &HashMap<String, GenericResource>,
    call_map: &HashMap<String, GenericCallable>,
) -> Result<(String, Vec<String>), Box<dyn Error>> {
    let hash = yaml
        .as_hash()
        .ok_or(ParseErr::new("expected each creator to be a hash"))?;
    if hash.len() != 1 {
        return Err(Box::new(ParseErr::new("invalid creator format")));
    }
    for (key, value) in hash {
        let name = key
            .as_str()
            .ok_or(ParseErr::new("expected res name of creator to be a string"))?;
        if !res_map.contains_key(name) {
            return Err(Box::new(ParseErr(format!(
                "{} is not defined as a resource",
                name
            ))));
        }

        let creators_vec = value
            .as_vec()
            .ok_or(ParseErr::new("expected creator names to be a list"))?;
        let creators: Vec<String> = creators_vec
            .iter()
            .map(|item| {
                let call_name = item.as_str().expect("expected creator name to be a string");
                if !call_map.contains_key(call_name) {
                    panic!("{} is not defined as a callable", call_name);
                }
                call_name.to_string()
            })
            .collect();

        return Ok((name.to_string(), creators));
    }
    unreachable!()
}

fn parse_specialization_from_yaml(
    yaml: &Yaml,
    gen_res_map: &HashMap<String, GenericResource>,
) -> Result<(String, Resource), Box<dyn Error>> {
    let hash = yaml
        .as_hash()
        .ok_or(ParseErr::new("expected each specialization to be a hash"))?;
    if hash.len() != 1 {
        return Err(Box::new(ParseErr::new("invalid specialize format")));
    }
    for (key, value) in hash {
        let name = key.as_str().ok_or(ParseErr::new(
            "expected res name of specialization to be a string",
        ))?;
        let res = monomorphise_from_yaml(value, gen_res_map)?;
        return Ok((name.to_string(), res));
    }
    unreachable!()
}

fn monomorphise_from_yaml(
    yaml: &Yaml,
    gen_res_map: &HashMap<String, GenericResource>,
) -> Result<Resource, Box<dyn Error>> {
    let hash = yaml.as_hash().ok_or(ParseErr::new(
        "expected specialization to be a hash internally",
    ))?;
    let res_name = hash
        .get(&Yaml::from_str("res"))
        .ok_or(ParseErr::new("expected res key in specialize"))?
        .as_str()
        .ok_or(ParseErr::new("expected res key to have string value"))?;
    let gen_res = gen_res_map.get(res_name).ok_or(ParseErr(format!(
        "resource {} not defined before",
        res_name
    )))?;
    let type_list = hash
        .get(&Yaml::from_str("type_map"))
        .ok_or(ParseErr::new("expected type_map key in specialize"))?
        .as_vec()
        .ok_or(ParseErr::new("expected type_map key to be a list"))?;
    let mut type_map = HashMap::new();
    for yaml in type_list {
        let type_hash = yaml.as_hash().ok_or(ParseErr::new(
            "expected type_map to specify type subsitution as a hash",
        ))?;
        if type_hash.len() != 1 {
            return Err(Box::new(ParseErr::new("type_map incorrectly specified")));
        }
        for (key, value) in type_hash {
            let name = key
                .as_str()
                .ok_or(ParseErr::new("expected type-name to be a string"))?;
            let type_param = TypeParam::new(name);
            let resource = monomorphise_from_yaml(value, gen_res_map)?;
            type_map.insert(type_param, resource);
        }
    }
    match gen_res.monomorphise(type_map) {
        Some(res) => Ok(res),
        None => Err(Box::new(ParseErr(format!(
            "failed to monomorphise {}",
            res_name
        )))),
    }
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
