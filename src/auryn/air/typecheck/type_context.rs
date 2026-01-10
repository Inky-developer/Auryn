use std::{fmt::Debug, hash::Hash, marker::PhantomData};

use crate::{
    auryn::{
        air::{
            data::Intrinsic,
            typecheck::{
                bounds::{ArrayBound, Bound},
                types::{
                    ArrayType, ExternType, FunctionItemType, MetaType, NumberLiteralType, Type,
                },
            },
        },
        syntax_id::SyntaxId,
    },
    utils::{bidirectional_map::BidirectionalMap, default, fast_map::FastMap},
};

pub type TypeMap<T> = FastMap<TypeId<T>, T>;
pub type BidirectionalTypeMap<T> = BidirectionalMap<TypeId<T>, T>;

pub trait FromTypeContext: Sized {
    fn from_context(id: TypeId<Self>, ctx: &TypeContext) -> &Self;
}

#[derive(Debug)]
pub struct TypeContext {
    array_bounds: BidirectionalTypeMap<ArrayBound>,
    number_literals: BidirectionalTypeMap<NumberLiteralType>,
    arrays: BidirectionalTypeMap<ArrayType>,
    metas: BidirectionalTypeMap<MetaType>,
    externs: TypeMap<ExternType>,
    function_items: TypeMap<FunctionItemType>,
}

impl TypeContext {
    pub fn array_bound_of(&mut self, syntax_id: SyntaxId, element_bound: Bound) -> Bound {
        Bound::Array(self.add_array_bound(syntax_id, ArrayBound { element_bound }))
    }

    pub fn add_array_bound(
        &mut self,
        syntax_id: SyntaxId,
        bound: ArrayBound,
    ) -> TypeId<ArrayBound> {
        add_non_unique_type(syntax_id, bound, &mut self.array_bounds)
    }

    pub fn get_array_bound(&self, id: TypeId<ArrayBound>) -> &ArrayBound {
        self.array_bounds.get_by_key(&id).unwrap()
    }

    pub fn number_literal_of(&mut self, syntax_id: SyntaxId, value: i128) -> Type {
        Type::NumberLiteral(self.add_number_literal(syntax_id, NumberLiteralType { value }))
    }

    pub fn array_of(&mut self, syntax_id: SyntaxId, element_type: Type) -> Type {
        Type::Array(self.add_array(syntax_id, ArrayType { element_type }))
    }

    pub fn meta_of(&mut self, syntax_id: SyntaxId, inner: Type) -> Type {
        Type::Meta(self.add_meta(syntax_id, MetaType { inner }))
    }

    pub fn add_number_literal(
        &mut self,
        syntax_id: SyntaxId,
        number_literal: NumberLiteralType,
    ) -> TypeId<NumberLiteralType> {
        add_non_unique_type(syntax_id, number_literal, &mut self.number_literals)
    }

    pub fn add_array(&mut self, syntax_id: SyntaxId, array: ArrayType) -> TypeId<ArrayType> {
        add_non_unique_type(syntax_id, array, &mut self.arrays)
    }

    pub fn add_meta(&mut self, syntax_id: SyntaxId, meta: MetaType) -> TypeId<MetaType> {
        add_non_unique_type(syntax_id, meta, &mut self.metas)
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

    pub fn add_extern(&mut self, syntax_id: SyntaxId, r#extern: ExternType) -> TypeId<ExternType> {
        let id = TypeId::new(syntax_id);
        let prev = self.externs.insert(id, r#extern);
        assert!(prev.is_none());
        id
    }

    pub fn get<T: FromTypeContext>(&self, id: TypeId<T>) -> &T {
        T::from_context(id, self)
    }

    pub(super) fn get_number_literal(&self, id: TypeId<NumberLiteralType>) -> &NumberLiteralType {
        self.number_literals.get_by_key(&id).unwrap()
    }
    pub(super) fn get_function_item(&self, id: TypeId<FunctionItemType>) -> &FunctionItemType {
        &self.function_items[&id]
    }
    pub(super) fn get_extern(&self, id: TypeId<ExternType>) -> &ExternType {
        &self.externs[&id]
    }
    pub(super) fn get_array(&self, id: TypeId<ArrayType>) -> &ArrayType {
        self.arrays.get_by_key(&id).unwrap()
    }
    pub(super) fn get_meta(&self, id: TypeId<MetaType>) -> &MetaType {
        self.metas.get_by_key(&id).unwrap()
    }
}

fn add_non_unique_type<T: Eq + Hash + Clone>(
    syntax_id: SyntaxId,
    r#type: T,
    map: &mut BidirectionalTypeMap<T>,
) -> TypeId<T> {
    if let Some(existing_id) = map.get_by_value(&r#type) {
        return *existing_id;
    }

    let id = TypeId::new(syntax_id);
    map.insert(id, r#type);
    id
}

impl Default for TypeContext {
    fn default() -> Self {
        let mut this = Self {
            array_bounds: default(),
            number_literals: default(),
            arrays: default(),
            metas: default(),
            externs: default(),
            function_items: default(),
        };

        for intrinsic in Intrinsic::ALL {
            this.add_function_item(intrinsic.syntax_id(), intrinsic.function_type());
        }

        this
    }
}

pub struct TypeId<T>(SyntaxId, PhantomData<T>);

impl<T> TypeId<T> {
    pub(in crate::auryn::air) fn new(syntax_id: SyntaxId) -> Self {
        Self(syntax_id, PhantomData)
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
