use crate::types::{Callable, GenericCallable, Monomorphisable, Resource};

pub fn specialize_callable(
    gen_callable: &GenericCallable,
    resource: &Resource,
) -> Option<Callable> {
    let callable_id_comps = gen_callable.id().components();
    let resource_id_comps = resource.id().components();
    if &callable_id_comps[0..callable_id_comps.len() - 1] == resource_id_comps {
        let callable_opt = gen_callable.monomorphise(resource.type_map().clone());
        if callable_opt.is_some() {
            return callable_opt;
        }
    }
    for sub_res in resource.type_map().values() {
        let callable_opt = specialize_callable(gen_callable, sub_res);
        if callable_opt.is_some() {
            return callable_opt;
        }
    }
    None
}
