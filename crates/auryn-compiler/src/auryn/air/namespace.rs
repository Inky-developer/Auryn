use stdx::{FastMap, SmallString, default};

use crate::{
    SyntaxId,
    auryn::{
        air::{
            data::{AirModuleId, AirStaticValueId, TypeAliasId},
            typecheck::{
                type_context::TypeId,
                types::{ExternType, GenericId, ModuleType, StructType},
            },
        },
        syntax_id::{SpanExt, Spanned},
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
    pub types: FastMap<Spanned<SmallString>, UserDefinedTypeId>,
    pub statics: FastMap<Spanned<SmallString>, AirStaticValueId>,
}

impl Namespace {
    pub fn new_with_modules(modules: impl IntoIterator<Item = (SmallString, AirModuleId)>) -> Self {
        Self {
            statics: default(),
            types: modules
                .into_iter()
                .map(|(name, file_id)| {
                    (
                        name.with_span(SyntaxId::new_unset(Some(file_id.0))),
                        UserDefinedTypeId::Module(file_id.0.into()),
                    )
                })
                .collect(),
        }
    }

    pub fn with_generics(&self, generics: impl IntoIterator<Item = Spanned<SmallString>>) -> Self {
        let mut result = self.clone();

        for (index, ident) in generics.into_iter().enumerate() {
            result
                .types
                .insert(ident, UserDefinedTypeId::Generic(GenericId(index)));
        }

        result
    }

    pub fn get(&self, ident: &str) -> Option<AirStaticValueId> {
        self.statics.get(ident).copied()
    }
}
