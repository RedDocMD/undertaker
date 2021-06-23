use std::{
    fmt::{Display, Formatter, Result as FmtResult},
    hash::{Hash, Hasher},
};

use syn::{Block, File, Item, Stmt, UseTree};

/// A full use path, referring to a single module, function, struct or const.
/// Since, sometimes use statements are written in a way that require use to know
/// the contents of a crate, those paths are left partly defined.
/// Eg: `use std::fs::*;`.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct UsePath {
    components: Vec<UsePathComponent>,
}

impl Display for UsePath {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(
            f,
            "{}",
            self.components
                .iter()
                .map(|x| format!("{}", x))
                .fold(String::new(), |old, x| if old.is_empty() {
                    x
                } else {
                    old + "::" + &x
                })
        )
    }
}

impl UsePath {
    pub fn components(&self) -> &Vec<UsePathComponent> {
        &self.components
    }
}

impl Hash for UsePath {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.to_string().hash(state);
    }
}

/// The parts of a use path.
/// So, use std::fs::* will create 3 parts, Name("std"), Name("fs"), Glob.
/// So, use std::fs::File as StdFile will create 3 parts, Name("std"), Name("fs"),
/// Alias(UsePathComponentAlias {from: "File", to: "StdFile"}).
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum UsePathComponent {
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

impl Display for UsePathComponent {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        use UsePathComponent::*;
        match self {
            Name(name) => write!(f, "{}", name),
            Alias(alias) => write!(f, "{} as {}", alias.from, alias.to),
            Glob => write!(f, "*"),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct UsePathComponentAlias {
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

    pub fn to_pair(&self) -> (&str, &str) {
        (self.from.as_str(), self.to.as_str())
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
            if let Stmt::Item(Item::Use(use_item)) = stmt {
                return Some(paths_from_use_tree(&use_item.tree));
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

/// Qualifies the paths in the child using the paths in the parent.
///
/// Consider the following code (in a file):
///
/// ```no_run
/// use std::fs;
/// use std::path::Path;
///
/// fn foo<U: AsRef<Path>>(path: U) {
///     use fs::File;
///     let file = File::open(path);
/// }
/// ```
///
/// The use statement in the function `foo()` works only because
/// the global `use std::fs`. However, the global and the block
/// uses are extracted separately. This function will combine the
/// local uses with the global uses to convert `use fs::File` to
/// `use std::fs::File`.
///
/// This function performs this step for only consecutive blocks,
/// so if there is another level of scoping, the function needs to be run
/// twice.
///
/// Also, this function expands *only* the paths in `child`.
///
/// **Ambiguities**: Consider the following code:
///
/// ```no_run
/// use std::fs::*;
/// use std::path::*;
///
/// fn bar() {
///     use Component::*;
/// }
/// ```
///
/// If we knew the contents of `std::fs` and `std::path` then we would
/// immediately know that Component belongs to `std::path` and not `std::fs`.
/// This is how the compiler resolves things. However, we only have syntactic knowledge.
/// So, we have an *ambiguity*, ie, `use Component::*` will resolve to either
/// `std::fs::Component::*` or `std::path::Component::*`. This is *not* a problem
/// since we are guaranteed that there are no actual amiguities, and so while using the `UsePath`,
/// we will have no problem.
pub fn extend_path_once<'par, I>(
    parents: I,
    children: &[UsePath],
) -> Result<Vec<UsePath>, PathExtendFailed>
where
    I: Iterator<Item = &'par UsePath> + Clone,
{
    fn is_exact_parent_of(parent: &UsePath, child: &UsePath) -> bool {
        return parent.components.last() == child.components.first();
    }

    fn join_paths(parent: &UsePath, child: &UsePath) -> UsePath {
        UsePath::from(
            &parent.components[0..parent.components.len() - 1]
                .iter()
                .chain(child.components.iter())
                .cloned()
                .collect::<Vec<UsePathComponent>>(),
        )
    }

    let mut new_children = Vec::new();
    for child in children {
        // First, check for exact match.
        let mut found_match = false;
        for parent in parents.clone() {
            if is_exact_parent_of(parent, child) {
                if !found_match {
                    found_match = true;
                    new_children.push(join_paths(parent, child));
                } else {
                    return Err(PathExtendFailed(format!(
                        "Duplicate exact parents found for {}",
                        child
                    )));
                }
            }
        }
        if !found_match {
            // Then try adding in all the globs

            // FIXME: This doesn't work!
            // We can have something like:
            //
            // use std::path::*;
            // fn foo() {
            //     use std::fs::File;
            //     ...
            // }
            //
            // The following code will try try to construct the following:
            // use std::path::std::fs::File, which is garbage.
            parents
                .clone()
                .filter(|x| *x.components.last().unwrap() == UsePathComponent::Glob)
                .for_each(|x| {
                    new_children.push(join_paths(x, child));
                });
            // FIXME: This is a hack for the above problem.
            new_children.push(child.clone());
        }
    }
    Ok(new_children)
}

#[derive(Debug)]
pub struct PathExtendFailed(String);

pub fn convert_to_path<T: AsRef<str>>(comps: &[T]) -> Option<UsePath> {
    let parts: Vec<Option<UsePathComponent>> = comps
        .iter()
        .map(|s| {
            if s.as_ref() == "*" {
                Some(UsePathComponent::Glob)
            } else if s.as_ref().contains(',') {
                let words: Vec<&str> = s.as_ref().split(',').map(|s| s.trim()).collect();
                if words.len() != 2 {
                    None
                } else {
                    Some(UsePathComponent::Alias(UsePathComponentAlias::from_pair(
                        words[0].to_string(),
                        words[1].to_string(),
                    )))
                }
            } else if !s.as_ref().contains(' ') {
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

// This is a macro create a path using convert_to_path()
#[macro_export]
macro_rules! path {
    ($($elem:expr),+) => {{
        crate::uses::convert_to_path(&[$($elem,)*]).unwrap()
    }};
}

#[cfg(test)]
mod test {
    use super::*;

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
        let expected_use_paths = vec![
            path!["bstr", "io", "BufReadExt"],
            path!["std", "fs", "File"],
            path!["std", "path", "Path"],
            path!["self", "searcher", "Searcher"],
            path!["uucore", "fs", "is_stdout_interactive"],
            path!["uucore", "ranges", "Range"],
            path!["uucore", "InvalidEncodingHandling"],
        ];
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
        let expected_use_paths = vec![
            path!["clap", "App"],
            path!["clap", "Arg"],
            path!["std", "io", "stdin"],
            path!["std", "io", "stdout"],
            path!["std", "io", "BufReader"],
            path!["std", "io", "BufWriter"],
            path!["std", "io", "Read"],
            path!["std", "io", "Write"],
            path!["std", "fs", "*"],
        ];
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
            let expected_use_paths = vec![path!["fs", "*"], path!["std", "path", "PathBuf"]];
            assert_eq!(use_paths, expected_use_paths);
        } else {
            panic!("Code parsed incorrectly");
        }
    }

    #[test]
    fn test_extend_paths_unambiguous() {
        let parents = vec![path!["std", "fs", "*"], path!["std", "path", "Component"]];
        let children = vec![path!["File,Dumb"], path!["Component", "*"]];
        let extended = extend_path_once(parents.iter(), &children).unwrap();
        let extended_expected = vec![
            path!["std", "fs", "File,Dumb"],
            path!["File,Dumb"],
            path!["std", "path", "Component", "*"],
        ];
        assert_eq!(extended, extended_expected);
    }

    #[test]
    fn test_extend_paths_ambiguous() {
        let parents = vec![path!["std", "fs", "*"], path!["std", "path", "*"]];
        let children = vec![path!["File,Dumb"], path!["Component", "*"]];
        let extended = extend_path_once(parents.iter(), &children).unwrap();
        let extended_expected = vec![
            path!["std", "fs", "File,Dumb"],
            path!["std", "path", "File,Dumb"],
            path!["File,Dumb"],
            path!["std", "fs", "Component", "*"],
            path!["std", "path", "Component", "*"],
            path!["Component", "*"],
        ];
        assert_eq!(extended, extended_expected);
    }
}
