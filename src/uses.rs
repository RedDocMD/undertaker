use syn::{File, Item, UseTree};

/// A full use path, referring to a single module, function, struct or const.
/// Since, sometimes use statements are written in a way that require use to know
/// the contents of a crate, those paths are left partly defined.
/// Eg: `use std::fs::*;`.
pub struct UsePath {
    components: Vec<UsePathComponent>,
}

// The parts of a use path.
// So, use std::fs::* will create 3 parts, Name("std"), Name("fs"), Glob.
// So, use std::fs::File as StdFile will create 3 parts, Name("std"), Name("fs"),
// Alias(UsePathComponentAlias {from: "File", to: "StdFile"}).
#[derive(Clone)]
enum UsePathComponent {
    Name(String),
    Alias(UsePathComponentAlias),
    Glob,
}

impl From<&Vec<UsePathComponent>> for UsePath {
    fn from(components: &Vec<UsePathComponent>) -> Self {
        Self {
            components: components.clone(),
        }
    }
}

impl From<&UsePathComponent> for UsePath {
    fn from(component: &UsePathComponent) -> Self {
        Self {
            components: vec![component.clone()],
        }
    }
}

#[derive(Clone)]
struct UsePathComponentAlias {
    from: String,
    to: String,
}

impl UsePathComponentAlias {
    fn from_pair<U: AsRef<str>, V: AsRef<str>>(from: U, to: V) -> Self {
        UsePathComponentAlias {
            from: String::from(from.as_ref()),
            to: String::from(to.as_ref()),
        }
    }
}

/// Extracts a list of global UsePath's from a file.
pub fn extract_global_uses(file: &File) -> Vec<UsePath> {
    file.items
        .iter()
        .filter_map(|item| {
            if let Item::Use(use_item) = item {
                Some(paths_from_use_tree(&use_item.tree))
            } else {
                None
            }
        })
        .flatten()
        .collect()
}

fn paths_from_use_tree(tree: &UseTree) -> Vec<UsePath> {
    use UseTree::*;
    match tree {
        Name(name) => vec![UsePath::from(&UsePathComponent::Name(
            name.ident.to_string(),
        ))],
        Rename(rnm) => vec![UsePath::from(&UsePathComponent::Alias(
            UsePathComponentAlias::from_pair(rnm.ident.to_string(), rnm.rename.to_string()),
        ))],
        Glob(_) => vec![UsePath::from(&UsePathComponent::Glob)],
        Path(use_path) => {
            let sub_paths = paths_from_use_tree(&use_path.tree.as_ref());
            let first = UsePathComponent::Name(use_path.ident.to_string());
            sub_paths
                .iter()
                .map(|path| {
                    let mut new_path = vec![first.clone()];
                    new_path.append(&mut path.components.clone());
                    UsePath::from(&new_path)
                })
                .collect()
        }
        Group(use_group) => use_group
            .items
            .iter()
            .map(|tree| paths_from_use_tree(tree))
            .flatten()
            .collect(),
    }
}
