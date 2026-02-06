use stdx::{FastMap, SmallString, default};

use crate::auryn::air::{
    data::{AirFunctionId, AirModuleId, AirStaticValueId, TypeAliasId},
    typecheck::{
        type_context::TypeId,
        types::{ExternType, GenericId, ModuleType, StructType},
    },
};

/// Represents a type that was defined by the user.
/// Should just be a pointer to the actual type data or be a structural type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UserDefinedTypeId {
    Extern(TypeId<ExternType>),
    Module(TypeId<ModuleType>),
    Struct(TypeId<StructType>),
    TypeAlias(TypeAliasId),
    Generic(GenericId),
}

/// This type stores the mapping from identifiers to the corresponding air item ids.
/// This is only used during the ast-to-air transformation, but not after the air has been created,
/// because at that point everything is identified using ids.
#[derive(Debug, Clone)]
pub struct Namespace {
    pub types: FastMap<SmallString, UserDefinedTypeId>,
    pub statics: FastMap<SmallString, AirStaticValueId>,
}

impl Namespace {
    pub fn with_modules(modules: impl IntoIterator<Item = (SmallString, AirModuleId)>) -> Self {
        Self {
            statics: default(),
            types: modules
                .into_iter()
                .map(|(name, file_id)| (name, UserDefinedTypeId::Module(file_id.into())))
                .collect(),
        }
    }

    pub fn get(&self, ident: &str) -> Option<AirStaticValueId> {
        self.statics.get(ident).copied()
    }

    /// Assumes the value with the given ident to exist and to be a function
    /// and returns the id
    pub fn unwrap_function(&self, ident: &str) -> AirFunctionId {
        AirFunctionId(self.get(ident).expect("ident should be in this namespace"))
    }
}
