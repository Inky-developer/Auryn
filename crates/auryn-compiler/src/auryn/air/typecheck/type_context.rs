use std::{
    fmt::Debug,
    hash::Hash,
    marker::PhantomData,
    num::NonZeroU64,
    ops::{Deref, DerefMut},
};

use stdx::default;

use crate::auryn::{
    air::{
        data::{AirFunctionId, AirModuleId},
        typecheck::{
            bounds::{ArrayBound, Bound, StructuralBound},
            type_storage::Types,
            types::{
                ApplicationType, ArrayType, FunctionItemType, GenericType, MetaType, ModuleType,
                NumberLiteralType, StructuralType, Type,
            },
        },
    },
    syntax_id::SyntaxId,
};

#[derive(Debug)]
struct SpecialTypes {
    unit: Type,
}

impl SpecialTypes {
    fn new(storage: &mut Types) -> Self {
        let unit = Type::Structural(storage.add((), StructuralType::UNIT));
        Self { unit }
    }
}

/// Keeps track of additional data associated to a [`Type`].
///
/// The [`TypeContext`] differentiates between two kinds of types:
/// 1. Nominal types: Identified of the syntax id of the identifier in the source code which defined them
/// 2. Structual types: Identified by a id counter, which is mapped to the syntax ids of the [`None`] file.
///
/// Structural types are also deduplicated on insertion: When a structural type is added that already exists,
/// the [`TypeContext`] will return the existing type id, instead of allocating a new one.
///
/// This allows [`Type`]s to be compared directly.
///
/// The [`TypeContext`] can be queryied via [`TypeId`]s and new type data can also be inserted via
/// the various `add_*` methods.
pub struct TypeContext {
    storage: Types,
    special_types: SpecialTypes,
}

impl TypeContext {
    pub fn unit_type(&self) -> Type {
        self.special_types.unit
    }

    pub fn array_bound_of(&mut self, element_bound: Bound) -> Bound {
        Bound::Array(self.add((), ArrayBound { element_bound }))
    }

    pub fn structural_bound_of(&mut self, structural_bound: StructuralBound) -> Bound {
        Bound::Structural(self.add((), structural_bound))
    }

    pub fn number_literal_of(&mut self, value: i128) -> Type {
        Type::NumberLiteral(self.add((), NumberLiteralType { value }))
    }

    pub fn array_of(&mut self, element_type: Type) -> Type {
        Type::Array(self.add((), ArrayType { element_type }))
    }

    pub fn structural_of(&mut self, structural_type: StructuralType) -> Type {
        Type::Structural(self.add((), structural_type))
    }

    pub fn meta_of(&mut self, inner: Type) -> Type {
        Type::Meta(self.add((), MetaType { inner }))
    }

    pub fn generic_of(&mut self, generic: GenericType) -> Type {
        Type::Generic(self.add((), generic))
    }

    pub fn applied_of(&mut self, r#type: Type, arguments: Vec<Type>) -> Type {
        Type::Application(self.add((), ApplicationType { r#type, arguments }))
    }
}

impl Default for TypeContext {
    fn default() -> Self {
        let mut storage = default();
        Self {
            special_types: SpecialTypes::new(&mut storage),
            storage,
        }
    }
}

impl Deref for TypeContext {
    type Target = Types;

    fn deref(&self) -> &Self::Target {
        &self.storage
    }
}

impl DerefMut for TypeContext {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.storage
    }
}

impl Debug for TypeContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TypeContext").finish_non_exhaustive()
    }
}

pub struct TypeId<T>(SyntaxId, PhantomData<T>);

impl<T> TypeId<T> {
    pub(in crate::auryn::air) fn new(syntax_id: SyntaxId) -> Self {
        Self(syntax_id, PhantomData)
    }

    pub fn syntax_id(&self) -> SyntaxId {
        self.0
    }
}

impl<T> Debug for TypeId<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TypeId")
            .field("id", &self.0)
            .field("kind", &std::any::type_name::<T>())
            .finish()
    }
}

impl<T> Clone for TypeId<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for TypeId<T> {}

impl<T> PartialEq for TypeId<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for TypeId<T> {}

impl<T> Hash for TypeId<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl From<AirModuleId> for TypeId<ModuleType> {
    fn from(value: AirModuleId) -> Self {
        TypeId::new(SyntaxId::new(Some(value.0), NonZeroU64::new(1).unwrap()))
    }
}

impl From<AirFunctionId> for TypeId<FunctionItemType> {
    fn from(value: AirFunctionId) -> Self {
        TypeId::new(value.0.0)
    }
}
