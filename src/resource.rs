use std::fmt::{self, Display, Formatter};

use syn::{Block, Expr, Pat, Path};

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

    fn trim_by_use_paths(&self, paths: &Vec<UsePath>) -> SingleResource {
        let mut new_id = self.id.clone();
        for path in paths {
            if let Some(id) = trim_common(&self.id, path) {
                new_id = id;
                break;
            }
        }
        let mut new_creators = Vec::new();
        for creator in &self.creators {
            let mut new_creator = creator.clone();
            for path in paths {
                match creator {
                    Creator::Tuple(id, idx) => {
                        if let Some(new_id) = trim_common(id, path) {
                            new_creator = Creator::Tuple(new_id, *idx);
                            break;
                        }
                    }
                    Creator::Direct(id) => {
                        if let Some(new_id) = trim_common(id, path) {
                            new_creator = Creator::Direct(new_id);
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
    Direct(ResourceID),
    Tuple(ResourceID, usize),
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
    res: SingleResource,
    rest: Option<Box<Resource>>,
}

impl Resource {
    pub fn single(id: ResourceID, creators: Vec<Creator>) -> Self {
        Self {
            res: SingleResource::new(id, creators),
            rest: None,
        }
    }
}

impl Display for Resource {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.res.id)?;
        if self.rest.is_some() {
            write!(f, "<{}>", self.rest.as_ref().unwrap().as_ref())?;
        }
        Ok(())
    }
}

pub fn resource_creation_from_block(
    block: &Block,
    resource: &Resource,
    outer_uses: &Vec<UsePath>,
) -> Option<Object> {
    // Adjust use paths to block
    let mut block_uses = extract_block_uses(block);
    block_uses = extend_path_once(outer_uses, &block_uses).unwrap();
    block_uses.append(&mut outer_uses.clone());

    for stmt in &block.stmts {
        use syn::Stmt::*;
        match stmt {
            Local(local) => {
                if let Some(init) = &local.init {
                    let creator =
                        resource_creation_from_expr(init.1.as_ref(), &resource, &block_uses);
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
                                if let Creator::Tuple(_, idx) = creator {
                                    let pat = pat.elems.iter().skip(*idx).next().unwrap();
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
    None
}

pub fn resource_creation_from_expr<'res>(
    expr: &Expr,
    resource: &'res Resource,
    uses: &Vec<UsePath>,
) -> Option<&'res Creator> {
    use Expr::*;
    match expr {
        Call(expr) => {
            // FIXME: Check for the nested inner resource
            let trimmed_res = resource.res.trim_by_use_paths(uses);
            // Check if the function call is appropriate.
            match expr.func.as_ref() {
                Path(expr) => {
                    for (idx, creator) in trimmed_res.creators.iter().enumerate() {
                        let id = match creator {
                            Creator::Direct(id) => id,
                            Creator::Tuple(id, _) => id,
                        };
                        if match_expr_path(id, &expr.path) {
                            println!("Found {}", id);
                            return Some(&resource.res.creators[idx]);
                        }
                    }
                }
                _ => {}
            }
            // Otherwise check the args
            for arg in &expr.args {
                let creator = resource_creation_from_expr(arg, &resource, uses);
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
            creators: vec![Creator::Tuple(
                path!["tokio", "sync", "oneshot", "channel"],
                1,
            )],
        };
        let use_paths = vec![
            path!["std", "fs", "File"],
            path!["std", "io", "prelude", "*"],
            path!["tokio", "sync", "Notify"],
            path!["tokio", "sync", "oneshot,one"],
        ];
        let trimed_reciever_res = SingleResource {
            id: path!["one", "Reciever"],
            creators: vec![Creator::Tuple(path!["one", "channel"], 1)],
        };
        assert_eq!(
            reciever_res.trim_by_use_paths(&&use_paths),
            trimed_reciever_res
        );
    }
}
