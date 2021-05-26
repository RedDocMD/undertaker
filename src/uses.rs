use syn::{Block, File, Item, Stmt, UseTree};

/// A full use path, referring to a single module, function, struct or const.
/// Since, sometimes use statements are written in a way that require use to know
/// the contents of a crate, those paths are left partly defined.
/// Eg: `use std::fs::*;`.
#[derive(PartialEq, Eq, Debug)]
pub struct UsePath {
    components: Vec<UsePathComponent>,
}

// The parts of a use path.
// So, use std::fs::* will create 3 parts, Name("std"), Name("fs"), Glob.
// So, use std::fs::File as StdFile will create 3 parts, Name("std"), Name("fs"),
// Alias(UsePathComponentAlias {from: "File", to: "StdFile"}).
#[derive(Clone, PartialEq, Eq, Debug)]
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

#[derive(Clone, PartialEq, Eq, Debug)]
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

/// Extracts a list of top level uses in a Block.
/// So it doesn't recurse into sub-blocks, only the top level ones are
/// returned.
pub fn extract_block_uses(block: &Block) -> Vec<UsePath> {
    block
        .stmts
        .iter()
        .filter_map(|stmt| {
            if let Stmt::Item(item) = stmt {
                if let Item::Use(use_item) = item {
                    return Some(paths_from_use_tree(&use_item.tree));
                }
            }
            None
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

#[cfg(test)]
mod test {
    use super::*;

    fn convert_to_path<T: AsRef<str>>(comps: &Vec<T>) -> Option<UsePath> {
        let parts: Vec<Option<UsePathComponent>> = comps
            .iter()
            .map(|s| {
                if s.as_ref() == "*" {
                    Some(UsePathComponent::Glob)
                } else if s.as_ref().contains(",") {
                    let words: Vec<&str> = s.as_ref().split(",").map(|s| s.trim()).collect();
                    if words.len() != 2 {
                        None
                    } else {
                        Some(UsePathComponent::Alias(UsePathComponentAlias::from_pair(
                            words[0].clone(),
                            words[1].clone(),
                        )))
                    }
                } else if !s.as_ref().contains(" ") {
                    Some(UsePathComponent::Name(String::from(s.as_ref())))
                } else {
                    None
                }
            })
            .collect();
        if parts.iter().any(|item| item.is_none()) {
            None
        } else {
            Some(UsePath::from(
                &parts
                    .into_iter()
                    .map(|part| part.unwrap())
                    .collect::<Vec<UsePathComponent>>(),
            ))
        }
    }

    #[test]
    fn test_discrete_uses() {
        let source = "use bstr::io::BufReadExt;
use std::fs::File;
use std::path::Path;

use self::searcher::Searcher;
use uucore::fs::is_stdout_interactive;
use uucore::ranges::Range;
use uucore::InvalidEncodingHandling;
";
        let file = syn::parse_file(&source).unwrap();
        let use_paths = extract_global_uses(&file);
        let expected_use_paths: Vec<UsePath> = vec![
            vec!["bstr", "io", "BufReadExt"],
            vec!["std", "fs", "File"],
            vec!["std", "path", "Path"],
            vec!["self", "searcher", "Searcher"],
            vec!["uucore", "fs", "is_stdout_interactive"],
            vec!["uucore", "ranges", "Range"],
            vec!["uucore", "InvalidEncodingHandling"],
        ]
        .iter()
        .map(|x| convert_to_path(x).unwrap())
        .collect();
        assert_eq!(use_paths, expected_use_paths);
    }

    #[test]
    fn test_grouped_uses() {
        let source = "use clap::{App, Arg};
use std::io::{stdin, stdout, BufReader, BufWriter, Read, Write};
use std::fs::*;
";
        let file = syn::parse_file(&source).unwrap();
        let use_paths = extract_global_uses(&file);
        let expected_use_paths: Vec<UsePath> = vec![
            vec!["clap", "App"],
            vec!["clap", "Arg"],
            vec!["std", "io", "stdin"],
            vec!["std", "io", "stdout"],
            vec!["std", "io", "BufReader"],
            vec!["std", "io", "BufWriter"],
            vec!["std", "io", "Read"],
            vec!["std", "io", "Write"],
            vec!["std", "fs", "*"],
        ]
        .iter()
        .map(|x| convert_to_path(x).unwrap())
        .collect();
        assert_eq!(use_paths, expected_use_paths);
    }

    #[test]
    fn test_block_uses() {
        let source = "
use std::path::Path;
use std::fs;

fn foo<U: AsRef<Path>>(p: U) {
    use fs::*;
    let x = File::open(p.as_ref());
    use std::path::PathBuf;
}
";
        let file = syn::parse_file(&source).unwrap();
        if let Item::Fn(fn_item) = &file.items[2] {
            let use_paths = extract_block_uses(fn_item.block.as_ref());
            let expected_use_paths: Vec<UsePath> =
                vec![vec!["fs", "*"], vec!["std", "path", "PathBuf"]]
                    .iter()
                    .map(|x| convert_to_path(x).unwrap())
                    .collect();
            assert_eq!(use_paths, expected_use_paths);
        } else {
            panic!("Code parsed incorrectly");
        }
    }
}
