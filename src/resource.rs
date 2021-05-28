use crate::uses::*;

/// The fully-qualified path to a resource.
/// A resource may be a struct or a function.
pub type ResourceID = UsePath;

/// SingleResource refers to a single struct, which
/// we want to consider as a resource.
///
/// The `creators` field contains identifiers of functions
/// (aka methods) which create (or construct) such a resource.
pub struct SingleResource {
    id: ResourceID,
    creators: Vec<Creator>,
}

/// Creator refers to functions which create a resource.
///
/// A `Direct` creator is one like `Arc::new()` or `Notify::new()`.
/// Such a function directly returns a resource.
///
/// A `Tuple` creator is one like `oneshot::channel()`. This returns
/// both the sender and the reciever as a tuple. Hence, the second arg
/// is the index of the tuple we are interested in.
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
