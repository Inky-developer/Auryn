use std::{hash::Hash, num::NonZeroU64};

use stdx::{BidirectionalMap, FastMap, default};

use crate::auryn::{
    air::typecheck::{
        bounds::{ArrayBound, StructuralBound},
        type_context::TypeId,
        types::{
            ArrayType, ExternType, FunctionItemType, GenericType, IntrinsicType, MetaType,
            ModuleType, NumberLiteralType, StructType, StructuralType, TypeData,
        },
    },
    syntax_id::SyntaxId,
};

macro_rules! define_storage {
    (
        $(#[$meta:meta])*
        pub struct $ident:ident {
            $($field:ident: $ty:ty),+ $(,)?
        }
    ) => {
        $(#[$meta])*
        pub struct $ident {
            $(
                $field: <$ty as TypeData>::Storage
            ),*
        }

        $(
            impl StorageItem for $ty {
                fn from_storage(storage: &Types, id: TypeId<Self>) -> &Self {
                    storage.$field.get(id)
                }

                fn add_to_storage(
                    storage: &mut Types,
                    args: <Self::Storage as TypeStorage<Self>>::Args,
                    value: Self,
                ) -> TypeId<Self> {
                    storage.$field.add(args, value)
                }
            }
        )*
    };
}

define_storage! {
    /// The storage for all type and bounds data
    /// Some data will be stored by their syntax definition id as key, while others will be used
    /// themselves as key. The purpose for that is so that a [`Type`] is always only equal to itself.
    #[derive(Default)]
    pub struct Types {
        array_bounds: ArrayBound,
        structural_bounds: StructuralBound,

        number_literals: NumberLiteralType,
        arrays: ArrayType,
        metas: MetaType,
        structurals: StructuralType,
        structs: StructType,
        externs: ExternType,
        modules: ModuleType,
        function_items: FunctionItemType,
        intrinsics: IntrinsicType,
        generics: GenericType,
    }
}

impl Types {
    pub fn get<T: StorageItem>(&self, value: TypeId<T>) -> &T {
        T::from_storage(self, value)
    }

    pub fn add<T: StorageItem>(
        &mut self,
        args: <T::Storage as TypeStorage<T>>::Args,
        value: T,
    ) -> TypeId<T> {
        T::add_to_storage(self, args, value)
    }
}

pub trait StorageItem: TypeData {
    fn from_storage(storage: &Types, id: TypeId<Self>) -> &Self;

    fn add_to_storage(
        storage: &mut Types,
        args: <Self::Storage as TypeStorage<Self>>::Args,
        value: Self,
    ) -> TypeId<Self>;
}

pub trait TypeStorage<T>: Default
where
    Self: Sized,
{
    type Args;

    fn add(&mut self, args: Self::Args, type_data: T) -> TypeId<T>;

    fn get(&self, id: TypeId<T>) -> &T;
}

/// Storage for nominally defined types like modules where each type is associated with its
/// definition syntax id
pub struct NominalStorage<T> {
    data: FastMap<TypeId<T>, T>,
}

impl<T> TypeStorage<T> for NominalStorage<T> {
    type Args = SyntaxId;

    fn add(&mut self, args: Self::Args, type_data: T) -> TypeId<T> {
        let id = TypeId::new(args);
        let prev = self.data.insert(id, type_data);
        assert!(prev.is_none());
        id
    }

    fn get(&self, id: TypeId<T>) -> &T {
        &self.data[&id]
    }
}

impl<T> Default for NominalStorage<T> {
    fn default() -> Self {
        Self { data: default() }
    }
}

/// Storage for structurally defined types like arrays where the id is automatically generated.
/// A new entry for a type is only create if it wasn't inserted before, otherwise the previous id
/// will be re-used
pub struct StructuralStorage<T> {
    data: BidirectionalMap<TypeId<T>, T>,
    next_id: NonZeroU64,
}

impl<T> TypeStorage<T> for StructuralStorage<T>
where
    T: Eq + Hash + Clone,
{
    type Args = ();

    fn add(&mut self, (): Self::Args, type_data: T) -> TypeId<T> {
        if let Some(existing_id) = self.data.get_by_value(&type_data) {
            return *existing_id;
        }

        let id = TypeId::new(SyntaxId::new(None, self.next_id));
        self.next_id = self.next_id.checked_add(1).unwrap();
        self.data.insert(id, type_data);
        id
    }

    fn get(&self, id: TypeId<T>) -> &T {
        self.data
            .get_by_key(&id)
            .expect("Type ids should always be valid")
    }
}

impl<T> Default for StructuralStorage<T> {
    fn default() -> Self {
        Self {
            data: default(),
            next_id: 1.try_into().unwrap(),
        }
    }
}

/// Combination of the other two storage types. Useful for types which can be
/// both nominally defined or structurally defined, depending on the context.
/// For example function items are first nominally assigned, but later during monomorphization
/// they will be duplicated and there won't be an obvious syntax id to use for them, so they get
/// structural storage.
pub struct CombinedStorage<T> {
    nominal_storage: NominalStorage<T>,
    structural_storage: StructuralStorage<T>,
}

impl<T> TypeStorage<T> for CombinedStorage<T>
where
    T: Eq + Hash + Clone,
{
    type Args = Option<SyntaxId>;

    fn add(&mut self, args: Self::Args, type_data: T) -> TypeId<T> {
        match args {
            Some(id) => self.nominal_storage.add(id, type_data),
            None => self.structural_storage.add((), type_data),
        }
    }

    fn get(&self, id: TypeId<T>) -> &T {
        match id.syntax_id().file_id() {
            Some(_) => self.nominal_storage.get(id),
            None => self.structural_storage.get(id),
        }
    }
}

impl<T> Default for CombinedStorage<T> {
    fn default() -> Self {
        Self {
            nominal_storage: default(),
            structural_storage: default(),
        }
    }
}
