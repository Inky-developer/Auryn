use std::str::FromStr;

use crate::{
    auryn::air::{
        data::FunctionReference,
        type_context::{TypeContext, TypeId, TypeView},
    },
    utils::{fast_map::FastMap, small_string::SmallString},
};

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub enum Type {
    Top,
    Number,
    String,
    Null,
    Function(TypeId<FunctionType>),
    Array(TypeId<ArrayType>),
    Extern(TypeId<ExternType>),
    Error,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunctionType {
    pub parameters: FunctionParameters,
    pub return_type: Type,
    /// TODO: This should not be the function item type, so remove the reference and add a separate type for function items
    pub reference: FunctionReference,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum FunctionParameters {
    /// Hack to enable variadic parameters for builtin functions
    Unconstrained,
    Constrained(Vec<Type>),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct ArrayType {
    pub element_type: Type,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ExternType {
    pub extern_name: SmallString,
    pub members: FastMap<SmallString, ExternTypeMember>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ExternTypeMember {
    pub r#type: Type,
    pub extern_name: SmallString,
}

impl Type {
    pub fn as_view<'a>(&self, ctx: &'a TypeContext) -> TypeView<'a> {
        ctx.get_view(*self)
    }

    pub fn is_subtype(&self, other: &Type) -> bool {
        self == other || matches!(other, Type::Top)
    }
}

impl FunctionType {
    pub fn constrained_parameters(&self) -> &[Type] {
        match &self.parameters {
            FunctionParameters::Unconstrained => {
                panic!("function should have constrained parameters, but they are unconstrained")
            }
            FunctionParameters::Constrained(parameters) => parameters,
        }
    }
}

impl ExternType {
    pub fn get_member(&self, member: &str) -> Option<Type> {
        self.members.get(member).map(|it| it.r#type)
    }
}

impl FromStr for Type {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "Number" => Type::Number,
            "String" => Type::String,
            _ => return Err(()),
        })
    }
}
