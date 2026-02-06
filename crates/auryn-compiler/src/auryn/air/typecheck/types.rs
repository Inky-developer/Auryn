use std::{
    fmt::{Debug, Display},
    ops::{Deref, RangeInclusive},
    str::FromStr,
};

use stdx::{FastMap, FastSet, SmallString};

use crate::auryn::{
    air::{
        data::{ExternFunctionKind, FunctionReference, Intrinsic},
        typecheck::{
            bounds::MaybeBounded,
            type_context::{TypeContext, TypeId},
        },
    },
    syntax_id::SyntaxId,
};

/// Defines the types of this programming language.
/// There are two representations:
///
/// 1. The [`Type`] enum, which is a small copy-able type that is used throughout the compiler
/// 2. The [`TypeView`] type, which is a short-lived enum holding a reference to the [`TypeContext`].
///
/// The [`TypeView`] type makes it more ergonomic to access a type and implements [`Display`].
macro_rules! define_types {
    ($($(#[$meta:meta])* $name:ident $(($data:ident))?),+ $(,)?) => {
        /// The representation of a type of the auryn language
        /// A type can have associated data, which implements the [`FromTypeContext`] trait.
        /// These data can be accessed via the [`TypeContext`] or by creating a view via [`Type::as_view`].
        ///
        /// [`Type`] implements [`Eq`].
        #[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
        pub enum Type {
            $(
                $(#[$meta])*
                $name$((TypeId<$data>))?
            ),*
        }

        impl Type {
            #[allow(non_snake_case)]
            pub fn as_view(self, ty_ctx: &TypeContext) -> TypeView<'_> {
                match self {
                    $(
                        Self::$name$(($data))? => TypeView::$name$((TypeViewKind {
                            id: $data,
                            value: ty_ctx.get($data),
                            ctx: ty_ctx
                        }))?
                    ),*
                }
            }
        }

        /// A view of a type with the ability to access its type data through the type context
        #[derive(Clone, Copy)]
        pub enum TypeView<'a> {
            $(
                $(#[$meta])*
                $name$((TypeViewKind<'a, $data>))?
            ),*
        }

        impl<'a> TypeView<'a> {
            /// Converts this type view back into a type
            #[allow(non_snake_case)]
            pub fn as_type(self) -> Type {
                match self {
                    $(
                        Self::$name$(($data))? => Type::$name$(($data.id))?
                    ),*
                }
            }

            #[allow(non_snake_case)]
            pub fn visit(self, visited_types: &mut FastSet<Type>) {
                match self {
                    $(
                       Self::$name$(($data))? => {
                           #[allow(unused_variables)]
                           let is_new = visited_types.insert(self.as_type());

                           $(
                               if is_new {
                                    $data.visit(&mut |ty| ty.as_view($data.ctx).visit(visited_types));
                               }
                           )?
                       }
                    ),*
                }
            }
        }
    };
}

define_types! {
    /// A 32-bit signed integer
    I32,
    /// A 64-bit signed integer
    I64,
    /// The type of a concrete number
    NumberLiteral(NumberLiteralType),
    Bool,
    String,
    /// A zero-sized type of a concrete function item
    FunctionItem(FunctionItemType),
    /// A function defined & implemented by the compiler
    Intrinsic(IntrinsicType),
    Array(ArrayType),
    Extern(ExternType),
    Structural(StructuralType),
    Struct(StructType),
    Module(ModuleType),
    /// Represents the type of a type, also zero sized, because it is a compile-time only construct.
    Meta(MetaType),
    Generic(GenericType),
    /// Created when an erraneous program is being compiled.
    Error
}

impl Type {
    pub fn as_bounded(self) -> MaybeBounded {
        MaybeBounded::Type(self)
    }

    pub fn int_value_range(self) -> Option<RangeInclusive<i128>> {
        use Type::*;
        match self {
            I32 => Some(i32::MIN as i128..=i32::MAX as i128),
            I64 => Some(i64::MIN as i128..=i64::MAX as i128),
            NumberLiteral(_) | Bool | String | FunctionItem(_) | Intrinsic(_) | Array(_)
            | Extern(_) | Structural(_) | Struct(_) | Module(_) | Meta(_) | Generic(_) | Error => {
                None
            }
        }
    }
}

impl<'a> TypeView<'a> {
    pub fn get_member(self, ident: &str) -> Option<TypeView<'a>> {
        match self {
            TypeView::Extern(extern_type) => extern_type
                .value
                .get_member(ident)
                .map(|it| it.as_view(extern_type.ctx))
                .take_if(|it| !it.is_static_extern_member()),
            TypeView::Structural(structural_type) => structural_type
                .get_member(ident)
                .map(|it| it.as_view(structural_type.ctx)),
            TypeView::Struct(r#struct) => r#struct
                .get_member(ident)
                .map(|it| it.as_view(r#struct.ctx)),
            TypeView::Meta(meta_type) => match meta_type.inner() {
                TypeView::Extern(extern_type) => extern_type
                    .value
                    .get_member(ident)
                    .map(|it| it.as_view(extern_type.ctx))
                    .take_if(|it| it.is_static_extern_member()),
                TypeView::Module(module) => {
                    module.get_member(ident).map(|it| it.as_view(module.ctx))
                }
                _ => None,
            },
            _ => None,
        }
    }

    pub fn is_erroneous(self) -> bool {
        let mut visited_types = FastSet::default();
        self.visit(&mut visited_types);
        visited_types.iter().any(|it| matches!(it, Type::Error))
    }

    /// Returns whether a type is a static extern member.
    /// Right now, every type is considered static except for methods which don't have the static kind
    fn is_static_extern_member(self) -> bool {
        match self {
            TypeView::FunctionItem(function_item) => matches!(
                function_item.reference,
                FunctionReference::Extern {
                    kind: ExternFunctionKind::Static,
                    ..
                }
            ),
            _ => true,
        }
    }
}

pub trait TypeData: Sized {
    fn from_context(id: TypeId<Self>, ctx: &TypeContext) -> &Self;

    fn visit(&self, visitor: &mut impl FnMut(Type));
}

#[derive(Debug, Clone, Eq, PartialEq, Copy, Hash)]
pub struct NumberLiteralType {
    pub value: i128,
}

impl TypeData for NumberLiteralType {
    fn from_context(id: TypeId<Self>, ctx: &TypeContext) -> &Self {
        ctx.get_number_literal(id)
    }

    fn visit(&self, _visitor: &mut impl FnMut(Type)) {
        let Self { value: _ } = self;
    }
}

#[derive(Debug)]
pub struct FunctionItemType {
    pub type_parameters: Vec<GenericType>,
    pub parameters: FunctionParameters,
    pub return_type: Type,
    pub reference: FunctionReference,
}

impl TypeData for FunctionItemType {
    fn from_context(id: TypeId<Self>, ctx: &TypeContext) -> &Self {
        ctx.get_function_item(id)
    }

    fn visit(&self, visitor: &mut impl FnMut(Type)) {
        let Self {
            type_parameters,
            parameters,
            return_type,
            reference: _,
        } = self;
        for type_parameter in type_parameters {
            type_parameter.visit(visitor);
        }
        visitor(*return_type);
        parameters.parameters.iter().copied().for_each(visitor);
    }
}

impl FunctionItemType {
    pub fn parameters(&self) -> &[Type] {
        &self.parameters.parameters
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct IntrinsicType {
    pub intrinsic: Intrinsic,
}

impl TypeData for IntrinsicType {
    fn from_context(id: TypeId<Self>, ctx: &TypeContext) -> &Self {
        ctx.get_intrinsic(id)
    }

    fn visit(&self, _visitor: &mut impl FnMut(Type)) {
        let Self { intrinsic: _ } = self;
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunctionParameters {
    pub parameters: Vec<Type>,
    pub parameters_reference: SyntaxId,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct ArrayType {
    pub element_type: Type,
}

impl TypeData for ArrayType {
    fn from_context(id: TypeId<Self>, ctx: &TypeContext) -> &Self {
        ctx.get_array(id)
    }

    fn visit(&self, visitor: &mut impl FnMut(Type)) {
        let Self { element_type } = self;
        visitor(*element_type);
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ExternType {
    pub extern_name: SmallString,
    pub members: FastMap<SmallString, ExternTypeMember>,
}

impl TypeData for ExternType {
    fn from_context(id: TypeId<Self>, ctx: &TypeContext) -> &Self {
        ctx.get_extern(id)
    }

    fn visit(&self, visitor: &mut impl FnMut(Type)) {
        let Self {
            extern_name: _,
            members,
        } = self;
        for member in members.values() {
            let ExternTypeMember {
                r#type,
                extern_name: _,
            } = member;
            visitor(*r#type);
        }
    }
}

impl ExternType {
    pub fn get_member(&self, member: &str) -> Option<Type> {
        self.members.get(member).map(|it| it.r#type)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ExternTypeMember {
    pub r#type: Type,
    pub extern_name: SmallString,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct StructuralType {
    pub fields: Vec<(SmallString, Type)>,
}

impl TypeData for StructuralType {
    fn from_context(id: TypeId<Self>, ctx: &TypeContext) -> &Self {
        ctx.get_structural(id)
    }

    fn visit(&self, visitor: &mut impl FnMut(Type)) {
        let Self { fields } = self;
        for (_, ty) in fields {
            visitor(*ty);
        }
    }
}

impl StructuralType {
    /// The type to be used for functions that return nothing
    pub const UNIT: Self = StructuralType { fields: Vec::new() };

    pub fn get_member(&self, member: &str) -> Option<Type> {
        self.fields
            .iter()
            .find(|(name, _)| name.as_ref() == member)
            .map(|(_, ty)| *ty)
    }

    pub fn display(&self, ty_ctx: &TypeContext) -> impl Display {
        std::fmt::from_fn(|f| {
            // Currently the unit type is represent using a structural type of zero fields.
            // In the future this formatting can be generalized to all tuple-like structural types
            if self.fields.is_empty() {
                write!(f, "()")
            } else {
                write!(f, "{{ ")?;
                for (index, (ident, ty)) in self.fields.iter().enumerate() {
                    write!(f, "{ident}: {}", ty.as_view(ty_ctx))?;
                    if index + 1 < self.fields.len() {
                        write!(f, ", ")?;
                    }
                }
                write!(f, " }}")
            }
        })
    }
}

#[derive(Debug)]
pub struct StructType {
    pub ident: SmallString,
    pub structural: StructuralType,
}

impl TypeData for StructType {
    fn from_context(id: TypeId<Self>, ctx: &TypeContext) -> &Self {
        ctx.get_struct(id)
    }

    fn visit(&self, visitor: &mut impl FnMut(Type)) {
        let Self {
            ident: _,
            structural,
        } = self;
        structural.visit(visitor);
    }
}

impl StructType {
    pub fn get_member(&self, member: &str) -> Option<Type> {
        self.structural.get_member(member)
    }
}

#[derive(Debug)]
pub struct ModuleType {
    pub name: SmallString,
    pub members: FastMap<SmallString, Type>,
}

impl TypeData for ModuleType {
    fn from_context(id: TypeId<Self>, ctx: &TypeContext) -> &Self {
        ctx.get_module(id)
    }

    fn visit(&self, visitor: &mut impl FnMut(Type)) {
        let Self { name: _, members } = self;
        members.values().copied().for_each(visitor);
    }
}

impl ModuleType {
    pub fn get_member(&self, member: &str) -> Option<Type> {
        self.members.get(member).copied()
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct MetaType {
    pub inner: Type,
}

impl TypeData for MetaType {
    fn from_context(id: TypeId<Self>, ctx: &TypeContext) -> &Self {
        ctx.get_meta(id)
    }

    fn visit(&self, visitor: &mut impl FnMut(Type)) {
        let Self { inner } = self;
        visitor(*inner);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GenericId(pub usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GenericType {
    /// The id of this generic type in its defined scope
    pub id: GenericId,
    pub ident: SmallString,
}

impl TypeData for GenericType {
    fn from_context(id: TypeId<Self>, ctx: &TypeContext) -> &Self {
        ctx.get_generic(id)
    }

    fn visit(&self, _: &mut impl FnMut(Type)) {
        let Self { id: _, ident: _ } = self;
    }
}

impl FromStr for Type {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "I32" => Type::I32,
            "I64" => Type::I64,
            "Bool" => Type::Bool,
            "String" => Type::String,
            _ => return Err(()),
        })
    }
}

/// A reference to the `TypeData` of a type
#[derive(Debug)]
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

impl<'a> TypeViewKind<'a, MetaType> {
    pub fn inner(self) -> TypeView<'a> {
        self.inner.as_view(self.ctx)
    }
}

impl<'a> TypeViewKind<'a, FunctionItemType> {
    pub fn r#return(self) -> TypeView<'a> {
        self.return_type.as_view(self.ctx)
    }
}

impl<'a> TypeViewKind<'a, StructType> {
    pub fn fields(self) -> impl Iterator<Item = (&'a SmallString, TypeView<'a>)> {
        self.value
            .structural
            .fields
            .iter()
            .map(|(ident, ty)| (ident, ty.as_view(self.ctx)))
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
        for (index, parameter) in self.parameters().iter().enumerate() {
            if index != 0 {
                f.write_str(",")?;
            }
            Display::fmt(&parameter.as_view(self.ctx), f)?;
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

impl<'a> Display for TypeViewKind<'a, StructuralType> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.display(self.ctx).fmt(f)
    }
}

impl<'a> Display for TypeViewKind<'a, MetaType> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Type[{}]", self.value.inner.as_view(self.ctx))
    }
}

impl Display for TypeView<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeView::I32 => f.write_str("I32"),
            TypeView::I64 => f.write_str("I64"),
            TypeView::NumberLiteral(data) => write!(f, "{}", data.value.value),
            TypeView::Bool => f.write_str("Bool"),
            TypeView::String => f.write_str("String"),
            TypeView::FunctionItem(function_type) => Display::fmt(&function_type, f),
            TypeView::Intrinsic(intrinsic) => Debug::fmt(intrinsic.value, f),
            TypeView::Array(array_type) => Display::fmt(&array_type, f),
            TypeView::Extern(extern_type) => Display::fmt(&extern_type, f),
            TypeView::Module(module_type) => write!(f, "module {}", module_type.name),
            TypeView::Structural(structural_type) => Display::fmt(&structural_type, f),
            TypeView::Struct(r#struct) => write!(f, "struct {}", r#struct.value.ident),
            TypeView::Meta(meta_type) => Display::fmt(&meta_type, f),
            TypeView::Generic(generic) => write!(f, "{}", generic.value.ident),
            TypeView::Error => f.write_str("<<Error>>"),
        }
    }
}

impl Debug for TypeView<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}
