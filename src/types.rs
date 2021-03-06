use std::{
    collections::{hash_map::DefaultHasher, HashMap, HashSet},
    error::Error,
    fmt::{self, Display, Formatter, Write},
    fs::File,
    hash::{Hash, Hasher},
    io::Read,
    path,
};

use linked_hash_map::LinkedHashMap;
use yaml_rust::{Yaml, YamlLoader};

use crate::{
    uses::{self, UsePath, UsePathComponent},
    utils::specialize_callable,
};

pub type ResourceID = UsePath;

#[derive(Debug, Eq, Clone)]
pub struct GenericResource {
    id: ResourceID,
    type_params: Vec<TypeParam>,
    is_deref: bool,
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
            is_deref: self.is_deref,
        })
    }

    fn new_without_types(id: ResourceID) -> Self {
        Self {
            id,
            type_params: vec![],
            is_deref: false,
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

impl PartialEq for Resource {
    fn eq(&self, other: &Self) -> bool {
        if self.is_integral()
            && other.is_integral()
            && (self.to_string() == "integral" || other.to_string() == "integral")
        {
            return true;
        }
        if self.is_floating()
            && other.is_floating()
            && (self.to_string() == "floating" || other.to_string() == "floating")
        {
            return true;
        }
        self.id == other.id && self.type_map == other.type_map
    }
}

impl Hash for Resource {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.to_string().hash(state);
        for (key, value) in &self.type_map {
            let mut def_hasher = DefaultHasher::new();
            key.hash(&mut def_hasher);
            let key_hash = def_hasher.finish();
            let mut def_hasher = DefaultHasher::new();
            value.hash(&mut def_hasher);
            let value_hash = def_hasher.finish();
            state.write_u64(key_hash ^ value_hash);
        }
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
            is_deref: self.is_deref,
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
        if !params.is_empty() {
            write!(f, "<{}>", params_str)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Eq)]
pub struct Resource {
    id: ResourceID,
    type_map: HashMap<TypeParam, Resource>,
    is_deref: bool,
}

impl Resource {
    pub fn id(&self) -> &ResourceID {
        &self.id
    }

    pub fn type_map(&self) -> &HashMap<TypeParam, Resource> {
        &self.type_map
    }

    pub fn is_deref(&self) -> bool {
        self.is_deref
    }

    fn is_integral(&self) -> bool {
        let disp = self.to_string();
        disp == "u8"
            || disp == "u16"
            || disp == "u32"
            || disp == "u64"
            || disp == "u128"
            || disp == "i8"
            || disp == "i16"
            || disp == "i32"
            || disp == "i64"
            || disp == "i128"
            || disp == "usize"
            || disp == "isize"
            || disp == "integral"
    }

    fn is_floating(&self) -> bool {
        let disp = self.to_string();
        disp == "f32" || disp == "f64" || disp == "floating"
    }
}

impl Display for Resource {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.id)?;
        if !self.type_map.is_empty() {
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
    prop: UUIDPropagation,
    is_async: bool,
}

impl GenericCallable {
    pub fn id(&self) -> &ResourceID {
        &self.id
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum UUIDPropagation {
    Copy(usize),
    NoCopy,
    DontCare,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Callable {
    id: ResourceID,
    ctype: CallableType,
    args: Vec<Arg>,
    ret: Return,
    prop: UUIDPropagation,
    is_async: bool,
}

impl Callable {
    pub fn id(&self) -> &ResourceID {
        &self.id
    }

    pub fn ctype(&self) -> CallableType {
        self.ctype
    }

    pub fn args(&self) -> &Vec<Arg> {
        &self.args
    }

    pub fn ret(&self) -> &Return {
        &self.ret
    }

    pub fn prop(&self) -> UUIDPropagation {
        self.prop
    }
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
        if self.is_async {
            write!(f, "async ")?;
        }
        write!(f, "fn {}", self.id)?;
        if !params.is_empty() {
            write!(f, "<{}>", params_str)?;
        }
        write!(f, "({})", args_str)?;
        if !self.ret.is_void() {
            write!(f, " -> {}", self.ret)?;
        }
        Ok(())
    }
}

impl Display for Callable {
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
        if self.is_async {
            write!(f, "async ")?;
        }
        write!(f, "fn {}({})", self.id, args_str)?;
        if !self.ret.is_void() {
            write!(f, " -> {}", self.ret)?;
        }
        Ok(())
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
            return Some(Callable {
                id: self.id.clone(),
                ctype: self.ctype,
                args,
                ret,
                prop: self.prop,
                is_async: self.is_async,
            });
        }
        None
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum GenericArg {
    Type(TypeParam),
    Res(GenericResource),
    Tuple(Vec<GenericArg>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Arg {
    Res(Resource),
    Tuple(Vec<Arg>),
}

impl Display for GenericArg {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Type(param) => write!(f, "{}", param.name),
            Self::Res(res) => write!(f, "{}", res),
            Self::Tuple(tuple) => {
                let arg_strs: Vec<String> = tuple
                    .iter()
                    .map(|arg| {
                        let mut s = String::new();
                        write!(&mut s, "{}", arg).expect("Expected to be able to write to string");
                        s
                    })
                    .collect();
                write!(f, "({})", arg_strs.join(", "))
            }
        }
    }
}

impl Display for Arg {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Res(res) => write!(f, "{}", res),
            Self::Tuple(tuple) => {
                let arg_strs: Vec<String> = tuple
                    .iter()
                    .map(|arg| {
                        let mut s = String::new();
                        write!(&mut s, "{}", arg).expect("Expected to be able to write to string");
                        s
                    })
                    .collect();
                write!(f, "({})", arg_strs.join(", "))
            }
        }
    }
}

impl Monomorphisable<Arg> for GenericArg {
    fn monomorphise(&self, type_map: HashMap<TypeParam, Resource>) -> Option<Arg> {
        match self {
            Self::Type(type_param) => type_map.get(type_param).cloned().map(Arg::Res),
            Self::Res(gen_res) => gen_res.monomorphise(type_map).map(Arg::Res),
            Self::Tuple(tuple) => {
                let args = tuple.iter().map(|arg| arg.monomorphise(type_map.clone()));
                let valid = args.clone().all(|item| item.is_some());
                if valid {
                    let args: Vec<Arg> = args.map(|item| item.unwrap()).collect();
                    Some(Arg::Tuple(args))
                } else {
                    None
                }
            }
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum GenericReturn {
    Void,
    NonVoid(GenericArg),
}

impl GenericReturn {
    pub fn is_void(&self) -> bool {
        match self {
            Self::Void => true,
            Self::NonVoid(_) => false,
        }
    }
}

impl Display for GenericReturn {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Self::NonVoid(arg) = self {
            write!(f, "{}", arg)?;
        }
        Ok(())
    }
}

impl Monomorphisable<Return> for GenericReturn {
    fn monomorphise(&self, type_map: HashMap<TypeParam, Resource>) -> Option<Return> {
        match self {
            Self::Void => Some(Return::Void),
            Self::NonVoid(arg) => arg.monomorphise(type_map).map(Return::NonVoid),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum Return {
    Void,
    NonVoid(Arg),
}

impl Return {
    pub fn is_void(&self) -> bool {
        match self {
            Self::Void => true,
            Self::NonVoid(_) => false,
        }
    }

    pub fn as_non_void(&self) -> &Arg {
        match self {
            Self::Void => panic!("Failed to convert void Return to non-void"),
            Self::NonVoid(arg) => arg,
        }
    }
}

impl PartialEq<Arg> for Return {
    fn eq(&self, rhs: &Arg) -> bool {
        match self {
            Self::Void => false,
            Self::NonVoid(arg) => arg == rhs,
        }
    }
}

impl PartialEq<Return> for Arg {
    fn eq(&self, rhs: &Return) -> bool {
        rhs == self
    }
}

impl Display for Return {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Self::NonVoid(arg) = self {
            write!(f, "{}", arg)?;
        }
        Ok(())
    }
}

pub fn primitive_types_as_resources() -> HashMap<String, GenericResource> {
    let types = vec![
        "bool", "u8", "u16", "u32", "u64", "u128", "i8", "i16", "i32", "i64", "i128", "f32", "f64",
        "usize", "isize", "char", "str", "integral", "floating",
    ];
    // The last two aren't real types - placeholder types for integral and floating literals.

    types
        .iter()
        .map(|item| {
            let path = uses::convert_to_path(&[item.to_string()]).unwrap();
            (item.to_string(), GenericResource::new_without_types(path))
        })
        .collect()
}

pub struct ResourceFile {
    gen_resources: HashMap<ResourceID, GenericResource>,
    gen_callables: HashMap<ResourceID, GenericCallable>,
    gen_creators: HashMap<ResourceID, Vec<ResourceID>>,
    gen_blockers: HashMap<ResourceID, Vec<ResourceID>>,
    gen_releasers: HashMap<ResourceID, Vec<ResourceID>>,
    specializations: HashMap<String, Resource>,
    callables: HashSet<Callable>,
}

impl ResourceFile {
    fn generate_callables(&mut self) {
        for gen_callable in self.gen_callables.values() {
            for res in self.specializations.values() {
                if let Some(callable) = specialize_callable(gen_callable, res) {
                    self.callables.insert(callable);
                }
            }
        }
    }

    pub fn callables(&self) -> &HashSet<Callable> {
        &self.callables
    }

    pub fn specializations(&self) -> &HashMap<String, Resource> {
        &self.specializations
    }

    pub fn gen_creators(&self) -> &HashMap<ResourceID, Vec<ResourceID>> {
        &self.gen_creators
    }

    pub fn gen_callables(&self) -> &HashMap<ResourceID, GenericCallable> {
        &self.gen_callables
    }

    pub fn gen_blockers(&self) -> &HashMap<ResourceID, Vec<ResourceID>> {
        &self.gen_blockers
    }

    pub fn gen_releasers(&self) -> &HashMap<ResourceID, Vec<ResourceID>> {
        &self.gen_releasers
    }

    pub fn gen_resources(&self) -> &HashMap<ResourceID, GenericResource> {
        &self.gen_resources
    }
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
        .ok_or_else(|| ParseErr::new("no resources key"))?
        .as_vec()
        .ok_or_else(|| ParseErr::new("expected resources to be an array"))?;

    let mut resources = primitive_types_as_resources();
    for yaml in resources_yaml {
        let (name, resource) = parse_resource_from_yaml(yaml)?;
        resources.insert(name, resource);
    }

    // Parse callables
    let mut callables = HashMap::new();
    let mut creators = HashMap::new();
    let mut blockers = HashMap::new();
    let mut releasers = HashMap::new();

    if let Some(callables_yaml) = doc.get(&Yaml::from_str("callables")) {
        let callables_yaml = callables_yaml
            .as_vec()
            .ok_or_else(|| ParseErr::new("expected callables to be an array"))?;

        for yaml in callables_yaml {
            let (name, callable) = parse_callable_from_yaml(yaml, &resources)?;
            callables.insert(name, callable);
        }

        if let Some(creators_yaml) = doc.get(&Yaml::from_str("creators")) {
            let creators_yaml = creators_yaml
                .as_vec()
                .ok_or_else(|| ParseErr::new("expected creators to be an array"))?;

            for creator_yaml in creators_yaml {
                let (res, creators_vec) =
                    parse_creator_from_yaml(creator_yaml, &resources, &callables)?;
                creators.insert(res, creators_vec);
            }
        }

        if let Some(blockers_yaml) = doc.get(&Yaml::from_str("blockers")) {
            let blockers_yaml = blockers_yaml
                .as_vec()
                .ok_or_else(|| ParseErr::new("expected blockers to be an array"))?;

            for blocker_yaml in blockers_yaml {
                let (res, blocker_vec) =
                    parse_blocker_from_yaml(blocker_yaml, &resources, &callables)?;
                blockers.insert(res, blocker_vec);
            }
        }

        if let Some(releasers_yaml) = doc.get(&Yaml::from_str("releasers")) {
            let releasers_yaml = releasers_yaml
                .as_vec()
                .ok_or_else(|| ParseErr::new("expected releasers to be an array"))?;

            for releaser_yaml in releasers_yaml {
                let (res, releaser_vec) =
                    parse_releaser_from_yaml(releaser_yaml, &resources, &callables)?;
                releasers.insert(res, releaser_vec);
            }
        }
    }

    let mut specializations = HashMap::new();

    if let Some(specialize_yaml) = doc.get(&Yaml::from_str("specialize")) {
        let specialize_yaml = specialize_yaml
            .as_vec()
            .ok_or_else(|| ParseErr::new("expected specializartions to be an array"))?;
        for yaml in specialize_yaml {
            let (name, res) = parse_specialization_from_yaml(yaml, &resources)?;
            specializations.insert(name, res);
        }
    }

    let mut gen_resources = HashMap::new();
    for (_, res) in resources {
        gen_resources.insert(res.id.clone(), res);
    }

    let mut gen_callables = HashMap::new();
    for (_, call) in callables {
        gen_callables.insert(call.id.clone(), call);
    }

    let mut info = ResourceFile {
        gen_resources,
        gen_callables,
        gen_creators: creators,
        gen_blockers: blockers,
        gen_releasers: releasers,
        specializations,
        callables: HashSet::new(),
    };
    info.generate_callables();

    Ok(info)
}

fn parse_resource_from_yaml(yaml: &Yaml) -> Result<(String, GenericResource), Box<dyn Error>> {
    let resource_yaml = yaml
        .as_hash()
        .ok_or_else(|| ParseErr::new("invalid resource format"))?;
    if resource_yaml.len() != 1 {
        return Err(Box::new(ParseErr::new("invalid resource format")));
    }
    let key = resource_yaml.keys().next().unwrap();
    let value = &resource_yaml[key];
    let name = key.as_str().unwrap();
    let value = value
        .as_hash()
        .ok_or_else(|| ParseErr::new("invalid resource format"))?;
    let id_str = value
        .get(&Yaml::from_str("id"))
        .ok_or_else(|| ParseErr::new("no id param of resource"))?
        .as_str()
        .ok_or_else(|| ParseErr::new("id must be a string"))?;
    let type_params_yaml = value
        .get(&Yaml::from_str("type_params"))
        .ok_or_else(|| ParseErr::new("no type_params param of resource"))?
        .as_vec()
        .ok_or_else(|| ParseErr::new("type_params must be a list"))?;
    let is_deref = value
        .get(&Yaml::from_str("is_deref"))
        .ok_or_else(|| ParseErr::new("no is_deref param of resource"))?
        .as_bool()
        .ok_or_else(|| ParseErr::new("expected is_deref to be a bool"))?;

    let id = uses::convert_to_path(&id_str.split("::").collect::<Vec<&str>>())
        .ok_or_else(|| ParseErr(format!("invalid id: {}", id_str)))?;
    let type_params: Vec<TypeParam> = type_params_yaml
        .iter()
        .map(|param| TypeParam::new(param.as_str().expect("type param must be a string")))
        .collect();

    Ok((
        name.to_string(),
        GenericResource {
            id,
            type_params,
            is_deref,
        },
    ))
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
            .ok_or_else(|| ParseErr::new("name must be provided for res"))?
            .as_str()
            .ok_or_else(|| ParseErr::new("name must be a string"))?;
        let resource = resources_map
            .get(&name.to_string())
            .ok_or_else(|| ParseErr(format!("resource {} not found", name)))?;
        let type_params_vec = hash
            .get(&Yaml::from_str("type_params"))
            .ok_or_else(|| ParseErr::new("type_params must be provided for res"))?
            .as_vec()
            .ok_or_else(|| ParseErr::new("type_params must be a vector"))?;
        let type_params: Vec<TypeParam> = type_params_vec
            .iter()
            .map(|param| TypeParam::new(param.as_str().expect("type param must be a string")))
            .collect();
        let new_resource = resource
            .substitute_type_params(type_params)
            .ok_or_else(|| ParseErr(format!("invalid number of type params for {}", name)))?;
        Ok(new_resource)
    }

    fn arg_vec_from_yaml(
        args_vec: &[Yaml],
        resources_map: &HashMap<String, GenericResource>,
        type_params: &[TypeParam],
    ) -> Result<Vec<GenericArg>, Box<dyn Error>> {
        let mut args = Vec::new();
        for arg_hash in args_vec {
            let arg_hash = arg_hash
                .as_hash()
                .ok_or_else(|| ParseErr::new("args must contain hash"))?;
            let cat = arg_hash
                .get(&Yaml::from_str("cat"))
                .ok_or_else(|| ParseErr::new("arg hash must have a cat"))?
                .as_str()
                .ok_or_else(|| ParseErr::new("cat must be a string"))?;

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
                    .ok_or_else(|| ParseErr::new("type_param must be specified for Type arg"))?
                    .as_str()
                    .ok_or_else(|| ParseErr::new("type_param must be a string"))?;
                let type_param = TypeParam::new(type_name);
                if !type_params.contains(&type_param) {
                    return Err(Box::new(ParseErr(format!(
                        "{} is not a type param defined above",
                        type_name
                    ))));
                }
                args.push(GenericArg::Type(type_param));
            } else if cat == "Tuple" {
                let members_list = arg_hash
                    .get(&Yaml::from_str("members"))
                    .ok_or_else(|| ParseErr::new("tuple type must have members"))?
                    .as_vec()
                    .ok_or_else(|| ParseErr::new("expected members to be a list"))?;
                let sub_args = arg_vec_from_yaml(members_list, resources_map, type_params)?;
                args.push(GenericArg::Tuple(sub_args));
            } else {
                return Err(Box::new(ParseErr(format!(
                    "{} isn't a valid type of arg",
                    cat
                ))));
            }
        }
        Ok(args)
    }

    let callable_yaml = yaml
        .as_hash()
        .ok_or_else(|| ParseErr(String::from("expected callable to be specified as hash")))?;
    if callable_yaml.len() != 1 {
        return Err(Box::new(ParseErr::new("invalid callable format")));
    }
    let key = callable_yaml.keys().next().unwrap();
    let value = &callable_yaml[key];
    let name = key.as_str().unwrap();
    let value = value
        .as_hash()
        .ok_or_else(|| ParseErr::new("invalid callable format"))?;
    let id_str = value
        .get(&Yaml::from_str("id"))
        .ok_or_else(|| ParseErr::new("no id param of callable"))?
        .as_str()
        .ok_or_else(|| ParseErr::new("id must be a string"))?;
    let type_params_yaml = value
        .get(&Yaml::from_str("type_params"))
        .ok_or_else(|| ParseErr::new("no type_params param of callable"))?
        .as_vec()
        .ok_or_else(|| ParseErr::new("type_params must be a list"))?;
    let ctype_str = value
        .get(&Yaml::from_str("ctype"))
        .ok_or_else(|| ParseErr::new("no ctype param of callable"))?
        .as_str()
        .ok_or_else(|| ParseErr::new("ctype must be a string"))?;
    let args_vec = value
        .get(&Yaml::from_str("args"))
        .ok_or_else(|| ParseErr::new("no args param of callable"))?
        .as_vec()
        .ok_or_else(|| ParseErr::new("args must be a list"))?;
    let ret_vec = value
        .get(&Yaml::from_str("ret"))
        .ok_or_else(|| ParseErr::new("no ret param of callable"))?
        .as_vec()
        .ok_or_else(|| ParseErr::new("ret must be a list"))?;
    let prop_hash = value
        .get(&Yaml::from_str("prop"))
        .ok_or_else(|| ParseErr::new("expected prop param of callable"))?
        .as_hash()
        .ok_or_else(|| ParseErr::new("expected prop param to provide a map"))?;
    let is_async = value
        .get(&Yaml::from_str("async"))
        .ok_or_else(|| ParseErr::new("expected async param of callable"))?
        .as_bool()
        .ok_or_else(|| ParseErr::new("expected async param to be a boolean"))?;

    let id = uses::convert_to_path(&id_str.split("::").collect::<Vec<&str>>())
        .ok_or_else(|| ParseErr(format!("invalid id: {}", id_str)))?;
    let type_params: Vec<TypeParam> = type_params_yaml
        .iter()
        .map(|param| TypeParam::new(param.as_str().expect("type param must be a string")))
        .collect();
    let ctype = CallableType::from_str(ctype_str)
        .ok_or_else(|| ParseErr(format!("invalid ctype: {}", ctype_str)))?;
    let args = arg_vec_from_yaml(args_vec, resources_map, &type_params)?;
    let ret_vec = arg_vec_from_yaml(ret_vec, resources_map, &type_params)?;
    let mut ret = GenericReturn::Void;
    if ret_vec.len() == 1 {
        ret = GenericReturn::NonVoid(ret_vec.into_iter().next().unwrap());
    } else if !ret_vec.is_empty() {
        return Err(Box::new(ParseErr::new(
            "expected return to be a single arg or void",
        )));
    }
    let prop_type = prop_hash
        .get(&Yaml::from_str("type"))
        .ok_or_else(|| ParseErr::new("prop map must contain type key"))?
        .as_str()
        .ok_or_else(|| ParseErr::new("expected type key of prop to be a string"))?;
    let prop = if prop_type == "DontCare" {
        UUIDPropagation::DontCare
    } else if prop_type == "NoCopy" {
        UUIDPropagation::NoCopy
    } else if prop_type == "Copy" {
        let prop_idx = prop_hash
            .get(&Yaml::from_str("idx"))
            .ok_or_else(|| ParseErr::new("prop map of type Copy must contain idx key"))?
            .as_i64()
            .ok_or_else(|| ParseErr::new("expected idx key of prop to be an integer"))?;
        if prop_idx < 0 {
            return Err(Box::new(ParseErr(format!(
                "{} is not a valid idx since it must be non-negative",
                prop_idx
            ))));
        }
        let prop_idx = prop_idx as usize;
        if prop_idx == 0 && ctype != CallableType::Method {
            return Err(Box::new(ParseErr::new(
                "idx is 1-indexed. 0 is a special index for method reciever",
            )));
        }
        if prop_idx > args.len() {
            return Err(Box::new(ParseErr(format!(
                "{} is an out-of-bounds idx",
                prop_idx
            ))));
        }
        UUIDPropagation::Copy(prop_idx)
    } else {
        return Err(Box::new(ParseErr(format!(
            "{} is not a valid prop type",
            prop_type
        ))));
    };

    let callable = GenericCallable {
        id,
        type_params,
        ctype,
        args,
        ret,
        prop,
        is_async,
    };
    Ok((name.to_string(), callable))
}

fn parse_creator_from_yaml(
    yaml: &Yaml,
    res_map: &HashMap<String, GenericResource>,
    call_map: &HashMap<String, GenericCallable>,
) -> Result<(ResourceID, Vec<ResourceID>), Box<dyn Error>> {
    let hash = yaml
        .as_hash()
        .ok_or_else(|| ParseErr::new("expected each creator to be a hash"))?;
    if hash.len() != 1 {
        return Err(Box::new(ParseErr::new("invalid creator format")));
    }
    let key = hash.keys().next().unwrap();
    let value = &hash[key];
    let name = key
        .as_str()
        .ok_or_else(|| ParseErr::new("expected res name of creator to be a string"))?;
    if !res_map.contains_key(name) {
        return Err(Box::new(ParseErr(format!(
            "{} is not defined as a resource",
            name
        ))));
    }
    let res_id = res_map[name].id.clone();

    let creators_vec = value
        .as_vec()
        .ok_or_else(|| ParseErr::new("expected creator names to be a list"))?;
    let creator_ids: Vec<ResourceID> = creators_vec
        .iter()
        .map(|item| {
            let call_name = item.as_str().expect("expected creator name to be a string");
            if !call_map.contains_key(call_name) {
                panic!("{} is not defined as a callable", call_name);
            }
            call_map[call_name].id.clone()
        })
        .collect();

    Ok((res_id, creator_ids))
}

fn parse_blocker_from_yaml(
    yaml: &Yaml,
    res_map: &HashMap<String, GenericResource>,
    call_map: &HashMap<String, GenericCallable>,
) -> Result<(ResourceID, Vec<ResourceID>), Box<dyn Error>> {
    let hash = yaml
        .as_hash()
        .ok_or_else(|| ParseErr::new("expected each blocker to be a hash"))?;
    if hash.len() != 1 {
        return Err(Box::new(ParseErr::new("invalid blocker format")));
    }
    let key = hash.keys().next().unwrap();
    let value = &hash[key];
    let name = key
        .as_str()
        .ok_or_else(|| ParseErr::new("expected res name of blocker to be a string"))?;
    if !res_map.contains_key(name) {
        return Err(Box::new(ParseErr(format!(
            "{} is not defined as a resource",
            name
        ))));
    }
    let res_id = res_map[name].id.clone();

    let blockers_vec = value
        .as_vec()
        .ok_or_else(|| ParseErr::new("expected blocker names to be a list"))?;
    let blocker_ids: Vec<ResourceID> = blockers_vec
        .iter()
        .map(|item| {
            let call_name = item.as_str().expect("expected blocker name to be a string");
            if !call_map.contains_key(call_name) {
                panic!("{} is not defined as a callable", call_name);
            }
            call_map[call_name].id.clone()
        })
        .collect();

    Ok((res_id, blocker_ids))
}

fn parse_releaser_from_yaml(
    yaml: &Yaml,
    res_map: &HashMap<String, GenericResource>,
    call_map: &HashMap<String, GenericCallable>,
) -> Result<(ResourceID, Vec<ResourceID>), Box<dyn Error>> {
    let hash = yaml
        .as_hash()
        .ok_or_else(|| ParseErr::new("expected each releaser to be a hash"))?;
    if hash.len() != 1 {
        return Err(Box::new(ParseErr::new("invalid releaser format")));
    }
    let key = hash.keys().next().unwrap();
    let value = &hash[key];
    let name = key
        .as_str()
        .ok_or_else(|| ParseErr::new("expected res name of releaser to be a string"))?;
    if !res_map.contains_key(name) {
        return Err(Box::new(ParseErr(format!(
            "{} is not defined as a resource",
            name
        ))));
    }
    let res_id = res_map[name].id.clone();

    let releasers_vec = value
        .as_vec()
        .ok_or_else(|| ParseErr::new("expected releaser names to be a list"))?;
    let releaser_ids: Vec<ResourceID> = releasers_vec
        .iter()
        .map(|item| {
            let call_name = item
                .as_str()
                .expect("expected releaser name to be a string");
            if !call_map.contains_key(call_name) {
                panic!("{} is not defined as a callable", call_name);
            }
            call_map[call_name].id.clone()
        })
        .collect();

    Ok((res_id, releaser_ids))
}

fn parse_specialization_from_yaml(
    yaml: &Yaml,
    gen_res_map: &HashMap<String, GenericResource>,
) -> Result<(String, Resource), Box<dyn Error>> {
    let hash = yaml
        .as_hash()
        .ok_or_else(|| ParseErr::new("expected each specialization to be a hash"))?;
    if hash.len() != 1 {
        return Err(Box::new(ParseErr::new("invalid specialize format")));
    }
    let key = hash.keys().next().unwrap();
    let value = &hash[key];
    let name = key
        .as_str()
        .ok_or_else(|| ParseErr::new("expected res name of specialization to be a string"))?;
    let res = monomorphise_from_yaml(value, gen_res_map)?;
    Ok((name.to_string(), res))
}

fn monomorphise_from_yaml(
    yaml: &Yaml,
    gen_res_map: &HashMap<String, GenericResource>,
) -> Result<Resource, Box<dyn Error>> {
    let hash = yaml
        .as_hash()
        .ok_or_else(|| ParseErr::new("expected specialization to be a hash internally"))?;
    let res_name = hash
        .get(&Yaml::from_str("res"))
        .ok_or_else(|| ParseErr::new("expected res key in specialize"))?
        .as_str()
        .ok_or_else(|| ParseErr::new("expected res key to have string value"))?;
    let gen_res = gen_res_map
        .get(res_name)
        .ok_or_else(|| ParseErr(format!("resource {} not defined before", res_name)))?;
    let type_list = hash
        .get(&Yaml::from_str("type_map"))
        .ok_or_else(|| ParseErr::new("expected type_map key in specialize"))?
        .as_vec()
        .ok_or_else(|| ParseErr::new("expected type_map key to be a list"))?;
    let mut type_map = HashMap::new();
    for yaml in type_list {
        let type_hash = yaml.as_hash().ok_or_else(|| {
            ParseErr::new("expected type_map to specify type subsitution as a hash")
        })?;
        if type_hash.len() != 1 {
            return Err(Box::new(ParseErr::new("type_map incorrectly specified")));
        }
        for (key, value) in type_hash {
            let name = key
                .as_str()
                .ok_or_else(|| ParseErr::new("expected type-name to be a string"))?;
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

/// Attempts to remove `path` from `id` if `path` is an acceptable prefix of `id`.
///
/// Criterion for acceptability: Assume that path is used as a use path in a Rust module.
/// If a function is directly a member of *that* module, and we attempt to use id, then
/// path is acceptable if it **shortens** the length of id that is required to be used.
///
/// *Example*: If we want to invoke `tokio::sync::oneshot::channel` (this is `id`), then
/// all the acceptable paths are: `tokio`, `tokio::*`, `tokio::sync`, `tokio::sync::*`,
/// `tokio::sync::oneshot`, `tokio::sync::oneshot::*`, `tokio::sync::oneshot::channel`.
pub fn trim_common(id: &ResourceID, path: &UsePath) -> Option<ResourceID> {
    let path_comps = path.components();
    let id_comps = id.components();
    if let Some(new_comps) = id_comps.strip_prefix(&path_comps[0..path_comps.len() - 1]) {
        let path_comps_last = path_comps.last().unwrap();
        match path_comps_last {
            &UsePathComponent::Glob => return Some(UsePath::from(&new_comps.to_vec())),
            &UsePathComponent::Name(_) => {
                if id_comps.starts_with(&path_comps) {
                    return Some(UsePath::from(&new_comps.to_vec()));
                }
            }
            UsePathComponent::Alias(alias) => {
                let (from, to) = alias.to_pair();
                let new_first = new_comps.first().unwrap();
                if let UsePathComponent::Name(name) = new_first {
                    if name == from {
                        let mut new_comps = new_comps.to_vec();
                        new_comps[0] = UsePathComponent::Name(to.to_string());
                        return Some(UsePath::from(&new_comps));
                    }
                }
            }
        };
    }
    None
}
