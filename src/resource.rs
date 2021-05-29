use crate::uses::*;

/// The fully-qualified path to a resource.
/// A resource may be a struct or a function.
pub type ResourceID = UsePath;

/// SingleResource refers to a single struct, which
/// we want to consider as a resource.
///
/// The `creators` field contains identifiers of functions
/// (aka methods) which create (or construct) such a resource.
#[derive(Debug, PartialEq, Eq)]
pub struct SingleResource {
    id: ResourceID,
    creators: Vec<Creator>,
}

impl SingleResource {
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

/// NestedResource refers to resources like Arc<Notify> or Mutex<Condvar>.
///
/// Individually, Arc, Notify, Mutex, Condvar are SingleResources. But to
/// satisfy the Rust type-system and our own concurrency needs, we need to nest
/// them.
pub struct NestedResource {
    res: SingleResource,
    rest: Option<Box<NestedResource>>,
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
