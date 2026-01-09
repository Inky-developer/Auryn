use std::{
    fmt::Display,
    ops::{Deref, RangeInclusive},
    str::FromStr,
};

use crate::{
    auryn::{
        air::{
            data::{ExternFunctionKind, FunctionReference},
            typecheck::type_context::{TypeContext, TypeId},
        },
        syntax_id::SyntaxId,
    },
    utils::{fast_map::FastMap, small_string::SmallString},
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
        /// A type can have associated data, which implements the [`TypeData`] trait.
        /// These data can be accessed via the [`TypeContext`] or by creating a view via [`Type::as_view`].
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
        }
    };
}

define_types! {
    /// The base type, which every other type is a subtype of
    Top,
    /// Supertype of every number
    Number,
    /// A 32-bit signed integer
    I32,
    /// A 64-bit signed integer
    I64,
    /// The type of a concrete number
    NumberLiteral(NumberLiteralType),
    Bool,
    String,
    /// A type without value. Not to be confused with javas void type
    Unit,
    /// A zero-sized type of a concrete function item
    FunctionItem(FunctionItemType),
    Array(ArrayType),
    Extern(ExternType),
    /// Represents the type of a type, also zero sized, because it is a compile-time only construct.
    Meta(MetaType),
    /// Created when an erraneous program is being compiled.
    Error
}

impl Type {
    pub fn int_value_range(self) -> Option<RangeInclusive<i128>> {
        use Type::*;
        match self {
            I32 => Some(i32::MIN as i128..=i32::MAX as i128),
            I64 => Some(i64::MIN as i128..=i64::MAX as i128),
            Top | Number | NumberLiteral(_) | Bool | String | Unit | FunctionItem(_) | Array(_)
            | Extern(_) | Meta(_) | Error => None,
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
            TypeView::Meta(meta_type) => match meta_type.inner() {
                TypeView::Extern(extern_type) => extern_type
                    .value
                    .get_member(ident)
                    .map(|it| it.as_view(extern_type.ctx))
                    .take_if(|it| it.is_static_extern_member()),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn is_subtype(self, other: TypeView) -> bool {
        use TypeView::*;

        match other {
            Top | Error => true,
            Number => matches!(self, Number | I32 | I64 | NumberLiteral(_)),
            Array(other_data) => {
                if let Array(self_data) = self {
                    self_data.element().is_subtype(other_data.element())
                } else {
                    false
                }
            }
            other => self.as_type() == other.as_type(),
        }
    }

    /// Currently a hack
    /// A type is considered abstract if no value can have its type.
    /// Instead, abstract types are just used as utility types during type checking
    pub fn is_abstract(self) -> bool {
        use TypeView::*;
        match self {
            Top | Number => true,
            Array(array) => array.element().is_abstract(),
            _ => false,
        }
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
}

#[derive(Debug, Clone, Eq, PartialEq, Copy, Hash)]
pub struct NumberLiteralType {
    pub value: i128,
}

impl TypeData for NumberLiteralType {
    fn from_context(id: TypeId<Self>, ctx: &TypeContext) -> &Self {
        ctx.get_number_literal(id)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunctionItemType {
    pub parameters: FunctionParameters,
    pub return_type: Type,
    pub reference: FunctionReference,
}

impl TypeData for FunctionItemType {
    fn from_context(id: TypeId<Self>, ctx: &TypeContext) -> &Self {
        ctx.get_function_item(id)
    }
}

impl FunctionItemType {
    pub fn constrained_parameters(&self) -> &[Type] {
        match &self.parameters {
            FunctionParameters::Unconstrained => {
                panic!("function should have constrained parameters, but they are unconstrained")
            }
            FunctionParameters::Constrained { parameters, .. } => parameters,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum FunctionParameters {
    /// Hack to enable variadic parameters for builtin functions
    Unconstrained,
    Constrained {
        parameters: Vec<Type>,
        parameters_reference: SyntaxId,
    },
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct ArrayType {
    pub element_type: Type,
}

impl TypeData for ArrayType {
    fn from_context(id: TypeId<Self>, ctx: &TypeContext) -> &Self {
        ctx.get_array(id)
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
pub struct MetaType {
    pub inner: Type,
}

impl TypeData for MetaType {
    fn from_context(id: TypeId<Self>, ctx: &TypeContext) -> &Self {
        ctx.get_meta(id)
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

/// A reference to the [`TypeData`] of a type
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
        write!(f, "Type[{}]", self.value.inner.as_view(self.ctx))
    }
}

impl Display for TypeView<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeView::Top => f.write_str("Top"),
            TypeView::Number => f.write_str("Number"),
            TypeView::I32 => f.write_str("I32"),
            TypeView::I64 => f.write_str("I64"),
            TypeView::NumberLiteral(data) => write!(f, "{}", data.value.value),
            TypeView::Bool => f.write_str("Bool"),
            TypeView::String => f.write_str("String"),
            TypeView::Unit => f.write_str("()"),
            TypeView::FunctionItem(function_type) => function_type.fmt(f),
            TypeView::Array(array_type) => array_type.fmt(f),
            TypeView::Extern(extern_type) => extern_type.fmt(f),
            TypeView::Meta(meta_type) => meta_type.fmt(f),
            TypeView::Error => f.write_str("<<Error>>"),
        }
    }
}
