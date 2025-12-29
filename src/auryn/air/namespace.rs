use crate::{
    auryn::air::data::{AirFunctionId, AirStaticValueId, AirTypedefId},
    utils::{fast_map::FastMap, small_string::SmallString},
};

#[derive(Debug)]
pub struct TypeInfo {
    pub def_id: AirTypedefId,
    pub members: Namespace,
}

/// This type stores the mapping from identifiers to the corresponding air item ids.
/// This is only used during the ast-to-air transformation, but not after the air has been created,
/// because at that point everything is identified using ids.
#[derive(Debug, Default)]
pub struct Namespace {
    pub functions: FastMap<SmallString, AirFunctionId>,
    pub types: FastMap<SmallString, TypeInfo>,
    pub statics: FastMap<SmallString, AirStaticValueId>,
}
