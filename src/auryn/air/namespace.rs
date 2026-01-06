use crate::{
    auryn::air::{
        data::{AirFunctionId, AirStaticValueId},
        typecheck::{
            type_context::TypeId,
            types::{ExternType, FunctionItemType, Type},
        },
    },
    utils::{fast_map::FastMap, small_string::SmallString},
};

/// Represents a type that was defined by the user that later gets registered in the type context, for which we already now the id.
/// The point of this type is to enable recursive type definitions:
/// If an extern type `Foo` is defined, we can already construct a type for it without registering
/// it in the [`TypeContext`] first, by using [`UserDefinedTypeId::to_type`].
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum UserDefinedTypeId {
    Extern(TypeId<ExternType>),
    Function(TypeId<FunctionItemType>),
}

impl UserDefinedTypeId {
    pub fn to_type(self) -> Type {
        match self {
            UserDefinedTypeId::Extern(type_id) => Type::Extern(type_id),
            UserDefinedTypeId::Function(type_id) => Type::FunctionItem(type_id),
        }
    }
}

/// This type stores the mapping from identifiers to the corresponding air item ids.
/// This is only used during the ast-to-air transformation, but not after the air has been created,
/// because at that point everything is identified using ids.
#[derive(Debug, Default)]
pub struct Namespace {
    pub types: FastMap<SmallString, UserDefinedTypeId>,
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
