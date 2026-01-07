use std::{
    fmt::{Debug, Display},
    hash::Hash,
    marker::PhantomData,
    ops::Deref,
};

use crate::{
    auryn::{
        air::{
            data::Intrinsic,
            typecheck::types::{
                ArrayType, ExternType, FunctionItemType, FunctionParameters, MetaType, Type,
            },
        },
        syntax_id::SyntaxId,
    },
    utils::{bidirectional_map::BidirectionalMap, default, fast_map::FastMap},
};

pub type TypeMap<T> = FastMap<TypeId<T>, T>;
pub type BidirectionalTypeMap<T> = BidirectionalMap<TypeId<T>, T>;

#[derive(Debug)]
pub struct TypeContext {
    arrays: BidirectionalTypeMap<ArrayType>,
    metas: BidirectionalTypeMap<MetaType>,
    externs: TypeMap<ExternType>,
    function_items: TypeMap<FunctionItemType>,
}

impl TypeContext {
    pub fn array_of(&mut self, syntax_id: SyntaxId, element_type: Type) -> Type {
        Type::Array(self.add_array(syntax_id, ArrayType { element_type }))
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

    pub fn get_view(&self, r#type: Type) -> TypeView<'_> {
        match r#type {
            Type::Top => TypeView::Top,
            Type::Number => TypeView::Number,
            Type::String => TypeView::String,
            Type::Null => TypeView::Null,
            Type::FunctionItem(type_id) => TypeView::FunctionItem(TypeViewKind {
                id: type_id,
                value: self.get_function_item(type_id),
                ctx: self,
            }),
            Type::Array(type_id) => TypeView::Array(TypeViewKind {
                id: type_id,
                value: self.get_array(type_id),
                ctx: self,
            }),
            Type::Extern(type_id) => TypeView::Extern(TypeViewKind {
                id: type_id,
                value: self.get_extern(type_id),
                ctx: self,
            }),
            Type::Meta(type_id) => TypeView::Meta(TypeViewKind {
                id: type_id,
                value: self.get_meta(type_id),
                ctx: self,
            }),
            Type::Error => TypeView::Error,
        }
    }

    pub fn get_function_item(&self, id: TypeId<FunctionItemType>) -> &FunctionItemType {
        &self.function_items[&id]
    }
    pub fn get_extern(&self, id: TypeId<ExternType>) -> &ExternType {
        &self.externs[&id]
    }
    pub fn get_array(&self, id: TypeId<ArrayType>) -> &ArrayType {
        self.arrays.get_by_key(&id).unwrap()
    }
    pub fn get_meta(&self, id: TypeId<MetaType>) -> &MetaType {
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

#[derive(Clone, Copy)]
pub enum TypeView<'a> {
    Top,
    Number,
    String,
    Null,
    FunctionItem(TypeViewKind<'a, FunctionItemType>),
    Array(TypeViewKind<'a, ArrayType>),
    Extern(TypeViewKind<'a, ExternType>),
    Meta(TypeViewKind<'a, MetaType>),
    Error,
}

impl<'a> TypeView<'a> {
    pub fn get_member(self, ident: &str) -> Option<TypeView<'a>> {
        match self {
            TypeView::Extern(extern_type) => extern_type
                .value
                .get_member(ident)
                .map(|it| it.as_view(extern_type.ctx)),
            _ => None,
        }
    }

    pub fn as_type(self) -> Type {
        match self {
            TypeView::Top => Type::Top,
            TypeView::Number => Type::Number,
            TypeView::String => Type::String,
            TypeView::Null => Type::Null,
            TypeView::FunctionItem(type_view_kind) => Type::FunctionItem(type_view_kind.id),
            TypeView::Array(type_view_kind) => Type::Array(type_view_kind.id),
            TypeView::Extern(type_view_kind) => Type::Extern(type_view_kind.id),
            TypeView::Meta(meta_view_kind) => Type::Meta(meta_view_kind.id),
            TypeView::Error => Type::Error,
        }
    }
}

impl Display for TypeView<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeView::Top => f.write_str("Top"),
            TypeView::Number => f.write_str("Number"),
            TypeView::String => f.write_str("String"),
            TypeView::Null => f.write_str("Null"),
            TypeView::FunctionItem(function_type) => function_type.fmt(f),
            TypeView::Array(array_type) => array_type.fmt(f),
            TypeView::Extern(extern_type) => extern_type.fmt(f),
            TypeView::Meta(meta_type) => meta_type.fmt(f),
            TypeView::Error => f.write_str("<<Error>>"),
        }
    }
}

pub struct TypeViewKind<'a, T> {
    pub id: TypeId<T>,
    pub value: &'a T,
    pub ctx: &'a TypeContext,
}

impl<'a> TypeViewKind<'a, ArrayType> {
    pub fn element(self) -> TypeView<'a> {
        self.element_type.as_view(self.ctx)
    }
}

impl<'a> TypeViewKind<'a, FunctionItemType> {
    pub fn r#return(self) -> TypeView<'a> {
        self.return_type.as_view(self.ctx)
    }
}

impl<'a, T> Clone for TypeViewKind<'a, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, T> Copy for TypeViewKind<'a, T> {}

impl<'a, T> Deref for TypeViewKind<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.value
    }
}

impl<'a> Display for TypeViewKind<'a, FunctionItemType> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("(")?;
        match &self.value.parameters {
            FunctionParameters::Constrained { parameters, .. } => {
                for (index, parameter) in parameters.iter().enumerate() {
                    if index != 0 {
                        f.write_str(",")?;
                    }
                    Display::fmt(&parameter.as_view(self.ctx), f)?;
                }
            }
            FunctionParameters::Unconstrained => {
                f.write_str("...")?;
            }
        }

        f.write_str(" -> ")?;
        Display::fmt(&self.value.return_type.as_view(self.ctx), f)?;

        Ok(())
    }
}

impl<'a> Display for TypeViewKind<'a, ArrayType> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[]{}", self.value.element_type.as_view(self.ctx))
    }
}

impl<'a> Display for TypeViewKind<'a, ExternType> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "extern('{}')", self.value.extern_name)
    }
}

impl<'a> Display for TypeViewKind<'a, MetaType> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "type[{}]", self.value.inner.as_view(self.ctx))
    }
}
