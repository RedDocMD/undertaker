use std::fmt::{self, Display, Formatter};

use syn::{Block, Expr, Pat, Path};

use crate::context::Context;
use crate::uses::*;

/// The fully-qualified path to a resource.
/// A resource may be a struct or a function.
pub type ResourceID = UsePath;

/// SingleResource refers to a single struct, which
/// we want to consider as a resource.
///
/// The `creators` field contains identifiers of functions
/// (aka methods) which create (or construct) such a resource.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SingleResource {
    id: ResourceID,
    creators: Vec<Creator>,
}

impl SingleResource {
    fn new(id: ResourceID, creators: Vec<Creator>) -> Self {
        Self { id, creators }
    }

    fn trim_by_use_paths<'path, I>(&self, paths: I) -> SingleResource
    where
        I: Iterator<Item = &'path UsePath> + Clone,
    {
        let mut new_id = self.id.clone();
        for path in paths.clone() {
            if let Some(id) = trim_common(&self.id, path) {
                new_id = id;
                break;
            }
        }
        let mut new_creators = Vec::new();
        for creator in &self.creators {
            let mut new_creator = creator.clone();
            for path in paths.clone() {
                match creator {
                    Creator::Tuple(creator) => {
                        if let Some(new_id) = trim_common(&creator.id, path) {
                            new_creator = Creator::Tuple(TupleCreator::new(
                                new_id,
                                creator.args.clone(),
                                creator.ret_idx,
                            ));
                            break;
                        }
                    }
                    Creator::Direct(creator) => {
                        if let Some(new_id) = trim_common(&creator.id, path) {
                            new_creator =
                                Creator::Direct(DirectCreator::new(new_id, creator.args.clone()));
                            break;
                        }
                    }
                }
            }
            new_creators.push(new_creator);
        }
        Self {
            id: new_id,
            creators: new_creators,
        }
    }
}

fn match_expr_path(res: &ResourceID, expr_path: &Path) -> bool {
    res.components()
        .iter()
        .zip(expr_path.segments.iter())
        .all(|(res_seg, expr_seg)| {
            if let UsePathComponent::Name(res_seg) = res_seg {
                res_seg == &expr_seg.ident.to_string()
            } else {
                false
            }
        })
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
fn trim_common(id: &ResourceID, path: &UsePath) -> Option<ResourceID> {
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
                        let mut new_comps = new_comps.to_vec().clone();
                        new_comps[0] = UsePathComponent::Name(to.to_string());
                        return Some(UsePath::from(&new_comps));
                    }
                }
            }
        };
    }
    None
}

/// Creator refers to functions which create a resource.
///
/// A `Direct` creator is one like `Arc::new()` or `Notify::new()`.
/// Such a function directly returns a resource.
///
/// A `Tuple` creator is one like `oneshot::channel()`. This returns
/// both the sender and the reciever as a tuple. Hence, the second arg
/// is the index of the tuple we are interested in.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Creator {
    Direct(DirectCreator),
    Tuple(TupleCreator),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DirectCreator {
    id: ResourceID,
    args: Vec<ResourceID>,
}

impl DirectCreator {
    pub fn new(id: ResourceID, args: Vec<ResourceID>) -> Self {
        Self { id, args }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TupleCreator {
    id: ResourceID,
    args: Vec<ResourceID>,
    ret_idx: usize,
}

impl TupleCreator {
    pub fn new(id: ResourceID, args: Vec<ResourceID>, ret_idx: usize) -> Self {
        Self { id, args, ret_idx }
    }
}

pub struct Object {
    ident: String,
    resource: Resource,
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.ident, self.resource)
    }
}

/// NestedResource refers to resources like Arc<Notify> or Mutex<Condvar>.
///
/// Individually, Arc, Notify, Mutex, Condvar are SingleResources. But to
/// satisfy the Rust type-system and our own concurrency needs, we need to nest
/// them.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Resource {
    single: SingleResource,
    rest: Option<Box<Resource>>,
}

impl Resource {
    /// Creates a non-nested resource.
    pub fn single(id: ResourceID, creators: Vec<Creator>) -> Self {
        Self {
            single: SingleResource::new(id, creators),
            rest: None,
        }
    }

    /// If the resource has no nesting, it returns the SingleResource
    /// it corresponds to. Otherwise returns None.
    pub fn as_single(&self) -> Option<&SingleResource> {
        if self.rest.is_none() {
            Some(&self.single)
        } else {
            None
        }
    }

    /// Nests a SingleResource inside of an existing resource.
    /// Meant to be used as a builder pattern.
    ///
    /// Thus D nested inside A<B<C>> becomes A<B<C<D>>>.
    pub fn nest(&mut self, single: SingleResource) -> &mut Self {
        if self.rest.is_none() {
            self.rest = Some(Box::new(Resource::from(single)));
        } else {
            let mut ptr = self.rest.as_mut().unwrap().as_mut();
            while ptr.rest.is_some() {
                ptr = ptr.rest.as_mut().unwrap().as_mut();
            }
            ptr.rest = Some(Box::new(Resource::from(single)));
        }
        self
    }
}

impl Display for Resource {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.single.id)?;
        if self.rest.is_some() {
            write!(f, "<{}>", self.rest.as_ref().unwrap().as_ref())?;
        }
        Ok(())
    }
}

impl From<SingleResource> for Resource {
    fn from(item: SingleResource) -> Self {
        Self {
            single: item,
            rest: None,
        }
    }
}

impl From<Vec<SingleResource>> for Resource {
    fn from(items: Vec<SingleResource>) -> Self {
        if items.len() == 1 {
            let mut items = items;
            Self {
                single: items.pop().unwrap(),
                rest: None,
            }
        } else {
            let mut items = items.into_iter();
            Self {
                single: items.next().unwrap(),
                rest: Some(Box::new(Resource::from(
                    items.collect::<Vec<SingleResource>>(),
                ))),
            }
        }
    }
}

pub fn resource_creation_from_block(
    block: &Block,
    resource: &Resource,
    ctxt: &mut Context,
) -> Option<Object> {
    // Adjust use paths to block
    let block_uses = extract_block_uses(block);
    ctxt.enter_block();
    ctxt.add_use_paths(block_uses);

    for stmt in &block.stmts {
        use syn::Stmt::*;
        match stmt {
            Local(local) => {
                if let Some(init) = &local.init {
                    let creator = resource_creation_from_expr(init.1.as_ref(), &resource, ctxt);
                    if let Some(creator) = creator {
                        let pat = &local.pat;
                        use Pat::*;
                        match pat {
                            Ident(pat) => {
                                return Some(Object {
                                    ident: pat.ident.to_string(),
                                    resource: resource.clone(),
                                });
                            }
                            Tuple(pat) => {
                                if let Creator::Tuple(TupleCreator { ret_idx, .. }) = creator {
                                    let pat = pat.elems.iter().skip(*ret_idx).next().unwrap();
                                    if let Ident(lit) = pat {
                                        return Some(Object {
                                            ident: lit.ident.to_string(),
                                            resource: resource.clone(),
                                        });
                                    } else {
                                        unreachable!("Did not expect anything other than a literal pattern here");
                                    }
                                } else {
                                    unreachable!("Cannot be a tuple let and not a tuple creator");
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }
            _ => {}
        };
    }
    // TODO: Shouldn't we call ctxt.exit_block() here?
    None
}

pub fn resource_creation_from_expr<'res>(
    expr: &Expr,
    resource: &'res Resource,
    ctxt: &mut Context,
) -> Option<&'res Creator> {
    use Expr::*;
    match expr {
        Call(expr) => {
            let trimmed_res = resource.single.trim_by_use_paths(ctxt.iter());
            // Check if the function call is appropriate.
            match expr.func.as_ref() {
                Path(path_expr) => {
                    for (idx, creator) in trimmed_res.creators.iter().enumerate() {
                        let (id, _) = match creator {
                            Creator::Direct(DirectCreator { id, args, .. }) => (id, args),
                            Creator::Tuple(TupleCreator { id, args, .. }) => (id, args),
                        };
                        if match_expr_path(id, &path_expr.path) {
                            println!("Found {}", id);
                            if let Some(rest) = &resource.rest {
                                for arg in &expr.args {
                                    if resource_creation_from_expr(arg, rest.as_ref(), ctxt)
                                        .is_some()
                                    {
                                        return Some(&resource.single.creators[idx]);
                                    }
                                }
                            } else {
                                return Some(&resource.single.creators[idx]);
                            }
                        }
                    }
                }
                _ => {}
            }
            // Otherwise check the args
            for arg in &expr.args {
                let creator = resource_creation_from_expr(arg, &resource, ctxt);
                if creator.is_some() {
                    return creator;
                }
            }
        }
        _ => {}
    };
    None
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::path;

    #[test]
    fn test_trim_path() {
        let id = path!["tokio", "sync", "oneshot", "channel"];
        let paths = vec![
            path!["tokio"],
            path!["tokio", "*"],
            path!["tokio", "sync"],
            path!["tokio", "sync", "*"],
            path!["tokio", "sync", "oneshot"],
            path!["tokio", "sync", "oneshot,onesie"],
            path!["tokio", "sync", "oneshot", "*"],
            path!["tokio", "sync", "oneshot", "channel"],
        ];
        let trimmed_id = vec![
            path!["tokio", "sync", "oneshot", "channel"],
            path!["sync", "oneshot", "channel"],
            path!["sync", "oneshot", "channel"],
            path!["oneshot", "channel"],
            path!["oneshot", "channel"],
            path!["onesie", "channel"],
            path!["channel"],
            path!["channel"],
        ];
        assert!(paths
            .iter()
            .zip(trimmed_id.iter())
            .map(|(path, trimmed)| trim_common(&id, path).unwrap() == *trimmed)
            .all(|x| x));
    }

    #[test]
    fn test_trim_by_use_path() {
        let reciever_res = SingleResource {
            id: path!["tokio", "sync", "oneshot", "Reciever"],
            creators: vec![Creator::Tuple(TupleCreator::new(
                path!["tokio", "sync", "oneshot", "channel"],
                vec![],
                1,
            ))],
        };
        let use_paths = vec![
            path!["std", "fs", "File"],
            path!["std", "io", "prelude", "*"],
            path!["tokio", "sync", "Notify"],
            path!["tokio", "sync", "oneshot,one"],
        ];
        let trimed_reciever_res = SingleResource {
            id: path!["one", "Reciever"],
            creators: vec![Creator::Tuple(TupleCreator::new(
                path!["one", "channel"],
                vec![],
                1,
            ))],
        };
        assert_eq!(
            reciever_res.trim_by_use_paths(use_paths.iter()),
            trimed_reciever_res
        );
    }
}
