use std::{fmt::Debug, hash::Hash, marker::PhantomData, num::NonZeroU64};

use stdx::{BidirectionalMap, FastMap, default};

use crate::auryn::{
    air::{
        data::{AirFunctionId, AirModuleId},
        typecheck::{
            bounds::{ArrayBound, Bound},
            types::{
                ArrayType, ExternType, FunctionItemType, GenericType, IntrinsicType, MetaType,
                ModuleType, NumberLiteralType, StructType, StructuralType, Type, TypeData,
            },
        },
    },
    syntax_id::SyntaxId,
};

pub type TypeMap<T> = FastMap<TypeId<T>, T>;
pub type BidirectionalTypeMap<T> = BidirectionalMap<TypeId<T>, T>;

#[derive(Debug)]
struct SpecialTypes {
    unit: Type,
}

impl Default for SpecialTypes {
    fn default() -> Self {
        Self { unit: Type::Error }
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
#[derive(Debug)]
pub struct TypeContext {
    array_bounds: BidirectionalTypeMap<ArrayBound>,
    number_literals: BidirectionalTypeMap<NumberLiteralType>,
    arrays: BidirectionalTypeMap<ArrayType>,
    metas: BidirectionalTypeMap<MetaType>,
    structurals: BidirectionalTypeMap<StructuralType>,
    structs: TypeMap<StructType>,
    externs: TypeMap<ExternType>,
    modules: TypeMap<ModuleType>,
    function_items: TypeMap<FunctionItemType>,
    intrinsics: BidirectionalTypeMap<IntrinsicType>,
    generics: BidirectionalTypeMap<GenericType>,
    special_types: SpecialTypes,
    next_id: NonZeroU64,
}

impl TypeContext {
    pub fn unit_type(&self) -> Type {
        self.special_types.unit
    }

    pub fn array_bound_of(&mut self, element_bound: Bound) -> Bound {
        Bound::Array(self.add_array_bound(ArrayBound { element_bound }))
    }

    pub fn add_array_bound(&mut self, bound: ArrayBound) -> TypeId<ArrayBound> {
        add_non_unique_type(&mut self.next_id, bound, &mut self.array_bounds)
    }

    pub fn get_array_bound(&self, id: TypeId<ArrayBound>) -> &ArrayBound {
        self.array_bounds.get_by_key(&id).unwrap()
    }

    pub fn number_literal_of(&mut self, value: i128) -> Type {
        Type::NumberLiteral(self.add_number_literal(NumberLiteralType { value }))
    }

    pub fn array_of(&mut self, element_type: Type) -> Type {
        Type::Array(self.add_array(ArrayType { element_type }))
    }

    pub fn structural_of(&mut self, structural_type: StructuralType) -> Type {
        Type::Structural(self.add_structural(structural_type))
    }

    pub fn meta_of(&mut self, inner: Type) -> Type {
        Type::Meta(self.add_meta(MetaType { inner }))
    }

    pub fn generic_of(&mut self, generic: GenericType) -> Type {
        Type::Generic(self.add_generic(generic))
    }

    pub fn add_number_literal(
        &mut self,
        number_literal: NumberLiteralType,
    ) -> TypeId<NumberLiteralType> {
        add_non_unique_type(&mut self.next_id, number_literal, &mut self.number_literals)
    }

    pub fn add_array(&mut self, array: ArrayType) -> TypeId<ArrayType> {
        add_non_unique_type(&mut self.next_id, array, &mut self.arrays)
    }

    pub fn add_structural(&mut self, structural: StructuralType) -> TypeId<StructuralType> {
        add_non_unique_type(&mut self.next_id, structural, &mut self.structurals)
    }

    pub fn add_struct(&mut self, syntax_id: SyntaxId, r#struct: StructType) -> TypeId<StructType> {
        let id = TypeId::new(syntax_id);
        let prev = self.structs.insert(id, r#struct);
        assert!(prev.is_none());
        id
    }

    pub fn add_meta(&mut self, meta: MetaType) -> TypeId<MetaType> {
        add_non_unique_type(&mut self.next_id, meta, &mut self.metas)
    }

    pub fn add_generic(&mut self, generic: GenericType) -> TypeId<GenericType> {
        add_non_unique_type(&mut self.next_id, generic, &mut self.generics)
    }

    pub fn add_intrinsic(&mut self, intrinsic: IntrinsicType) -> TypeId<IntrinsicType> {
        add_non_unique_type(&mut self.next_id, intrinsic, &mut self.intrinsics)
    }

    pub fn add_function_item(
        &mut self,
        syntax_id: SyntaxId,
        function: FunctionItemType,
    ) -> TypeId<FunctionItemType> {
        let id = TypeId::new(syntax_id);
        let prev = self.function_items.insert(id, function);
        assert!(prev.is_none());
        id
    }

    pub fn add_monomorphized_function_item(
        &mut self,
        function: FunctionItemType,
    ) -> TypeId<FunctionItemType> {
        let id = SyntaxId::new(None, self.next_id);
        self.next_id = self.next_id.checked_add(1).unwrap();
        self.add_function_item(id, function)
    }

    pub fn add_extern(&mut self, syntax_id: SyntaxId, r#extern: ExternType) -> TypeId<ExternType> {
        let id = TypeId::new(syntax_id);
        let prev = self.externs.insert(id, r#extern);
        assert!(prev.is_none());
        id
    }

    pub fn add_module(&mut self, module_id: AirModuleId, module: ModuleType) -> TypeId<ModuleType> {
        let id = module_id.into();
        let prev = self.modules.insert(id, module);
        assert!(prev.is_none());
        id
    }

    pub fn get<T: TypeData>(&self, id: TypeId<T>) -> &T {
        T::from_context(id, self)
    }

    pub(super) fn get_number_literal(&self, id: TypeId<NumberLiteralType>) -> &NumberLiteralType {
        self.number_literals.get_by_key(&id).unwrap()
    }
    pub(super) fn get_function_item(&self, id: TypeId<FunctionItemType>) -> &FunctionItemType {
        &self.function_items[&id]
    }
    pub(super) fn get_intrinsic(&self, id: TypeId<IntrinsicType>) -> &IntrinsicType {
        self.intrinsics.get_by_key(&id).unwrap()
    }
    pub(super) fn get_extern(&self, id: TypeId<ExternType>) -> &ExternType {
        &self.externs[&id]
    }
    pub(super) fn get_module(&self, id: TypeId<ModuleType>) -> &ModuleType {
        &self.modules[&id]
    }
    pub(super) fn get_array(&self, id: TypeId<ArrayType>) -> &ArrayType {
        self.arrays.get_by_key(&id).unwrap()
    }
    pub(super) fn get_structural(&self, id: TypeId<StructuralType>) -> &StructuralType {
        self.structurals.get_by_key(&id).unwrap()
    }
    pub(super) fn get_struct(&self, id: TypeId<StructType>) -> &StructType {
        self.structs.get(&id).unwrap()
    }
    pub(super) fn get_meta(&self, id: TypeId<MetaType>) -> &MetaType {
        self.metas.get_by_key(&id).unwrap()
    }
    pub(super) fn get_generic(&self, id: TypeId<GenericType>) -> &GenericType {
        self.generics.get_by_key(&id).unwrap()
    }
}

fn add_non_unique_type<T: Eq + Hash + Clone>(
    next_id: &mut NonZeroU64,
    r#type: T,
    map: &mut BidirectionalTypeMap<T>,
) -> TypeId<T> {
    if let Some(existing_id) = map.get_by_value(&r#type) {
        return *existing_id;
    }

    let id = TypeId::new(SyntaxId::new(None, *next_id));
    *next_id = next_id.checked_add(1).unwrap();
    map.insert(id, r#type);
    id
}

impl Default for TypeContext {
    fn default() -> Self {
        let mut this = Self {
            array_bounds: default(),
            number_literals: default(),
            arrays: default(),
            structurals: default(),
            structs: default(),
            metas: default(),
            externs: default(),
            modules: default(),
            function_items: default(),
            generics: default(),
            intrinsics: default(),
            special_types: default(),
            next_id: NonZeroU64::new(1).unwrap(),
        };

        let unit = this.structural_of(StructuralType::UNIT);
        this.special_types = SpecialTypes { unit };

        this
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
