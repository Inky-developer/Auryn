use core::fmt;
use std::str::FromStr;

use crate::{
    auryn::syntax_id::SyntaxId,
    utils::{fast_map::FastMap, small_string::SmallString},
};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Top,
    Number,
    String,
    Null,
    Function(Box<FunctionType>),
    Array(Box<Type>),
    Extern(ExternType),
    Error,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunctionType {
    pub parameters: Vec<Type>,
    pub return_type: Type,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ExternType {
    pub extern_name: SmallString,
    pub members: FastMap<SyntaxId, ExternTypeMember>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ExternTypeMember {
    pub r#type: Type,
    pub extern_name: SmallString,
}

impl Type {
    pub fn is_subtype(&self, other: &Type) -> bool {
        self == other || matches!(other, Type::Top)
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

impl fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Top => f.write_str("Top"),
            Type::Number => f.write_str("Number"),
            Type::String => f.write_str("String"),
            Type::Null => f.write_str("Null"),
            Type::Function(function_type) => function_type.fmt(f),
            Type::Array(content_type) => {
                f.write_str("[]")?;
                content_type.fmt(f)
            }
            Type::Extern(extern_type) => extern_type.fmt(f),
            Type::Error => f.write_str("<<Error>>"),
        }
    }
}

impl fmt::Display for FunctionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("(")?;
        for (index, parameter) in self.parameters.iter().enumerate() {
            if index != 0 {
                f.write_str(",")?;
            }
            parameter.fmt(f)?;
        }
        f.write_str(" -> ")?;
        self.return_type.fmt(f)?;

        Ok(())
    }
}

impl fmt::Display for ExternType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "extern(`{}``)", self.extern_name)
    }
}
