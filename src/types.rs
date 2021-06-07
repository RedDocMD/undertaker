use crate::uses::UsePath;

pub type ResourceID = UsePath;

pub struct GenericResource {
    id: ResourceID,
    type_params: Vec<TypeParam>,
}

pub struct TypeParam {
    name: String,
}

pub struct GenericCallable {
    id: ResourceID,
    type_params: Vec<TypeParam>,
    ctype: CallableType,
    args: Vec<GenericArg>,
    ret: GenericReturn,
}

pub enum CallableType {
    Function,
    Method,
}

pub enum GenericArg {
    Type(TypeParam),
    Res(GenericResource),
}

pub enum GenericReturn {
    Void,
    One(GenericArg),
    Pair(GenericArg, GenericArg),
}
