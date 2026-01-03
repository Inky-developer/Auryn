use crate::{
    auryn::air::data::{AirFunctionId, AirStaticValueId, AirTypedefId},
    utils::{fast_map::FastMap, small_string::SmallString},
};

/// This type stores the mapping from identifiers to the corresponding air item ids.
/// This is only used during the ast-to-air transformation, but not after the air has been created,
/// because at that point everything is identified using ids.
#[derive(Debug, Default)]
pub struct Namespace {
    pub types: FastMap<SmallString, AirTypedefId>,
    pub statics: FastMap<SmallString, AirStaticValueId>,
}

impl Namespace {
    pub fn get(&self, ident: &str) -> Option<AirStaticValueId> {
        self.statics.get(ident).copied()
    }

    /// Assumes the value with the given ident to exist and to be a function
    /// and returns the id
    pub fn unwrap_function(&self, ident: &str) -> AirFunctionId {
        AirFunctionId(self.get(ident).expect("ident should be in this namespace"))
    }
}
