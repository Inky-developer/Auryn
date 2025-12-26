use std::fmt::Display;

use crate::{
    auryn::air::types::{FunctionType, Type},
    java::{
        class::{ConstantPoolIndex, PrimitiveType, TypeCategory, VerificationTypeInfo},
        constant_pool_builder::ConstantPoolBuilder,
    },
    utils::small_string::SmallString,
};

/// The representation of a type in the jvm, can be converted into field descriptors and verification type info
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Primitive {
    Integer,
    Array(Box<Primitive>),
    Object(SmallString),
}

impl Primitive {
    pub fn string() -> Self {
        Primitive::Object("java/lang/String".into())
    }

    pub fn to_primitive_type_or_object(
        self,
        pool: &mut ConstantPoolBuilder,
    ) -> Result<PrimitiveType, ConstantPoolIndex> {
        match self {
            Primitive::Integer => Ok(PrimitiveType::Int),
            Primitive::Object(descriptor) => Err(pool.add_class(descriptor)),
            Primitive::Array(inner) => {
                Err(pool.add_class(inner.into_field_descriptor().to_string().into()))
            }
        }
    }

    pub fn into_field_descriptor(self) -> FieldDescriptor {
        match self {
            Primitive::Integer => FieldDescriptor::Integer,
            Primitive::Object(r#type) => FieldDescriptor::Object(r#type),
            Primitive::Array(element_type) => FieldDescriptor::Array {
                dimension_count: 1,
                descriptor: Box::new(element_type.into_field_descriptor()),
            },
        }
    }

    pub fn into_verification_type(self, pool: &mut ConstantPoolBuilder) -> VerificationTypeInfo {
        match self {
            Primitive::Integer => VerificationTypeInfo::Integer,
            Primitive::Array(inner) => VerificationTypeInfo::Object {
                constant_pool_index: pool.add_array_class(inner.into_field_descriptor()),
            },
            Primitive::Object(object) => VerificationTypeInfo::Object {
                constant_pool_index: pool.add_class(object),
            },
        }
    }

    pub fn category(&self) -> TypeCategory {
        match self {
            Primitive::Integer | Primitive::Array(_) | Primitive::Object(_) => TypeCategory::Normal,
        }
    }

    pub fn stack_size(&self) -> u16 {
        self.category().stack_size()
    }
}

/// https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-4.html#jvms-FieldType
#[derive(Debug, Clone)]
pub enum FieldDescriptor {
    Byte,
    Char,
    Double,
    Float,
    Integer,
    Long,
    Object(SmallString),
    Short,
    Boolean,
    Array {
        dimension_count: u8,
        descriptor: Box<FieldDescriptor>,
    },
}

impl FieldDescriptor {
    pub fn string() -> Self {
        FieldDescriptor::Object("java/lang/String".into())
    }

    pub fn print_stream() -> Self {
        FieldDescriptor::Object("java/io/PrintStream".into())
    }

    pub fn into_primitive(self) -> Primitive {
        match self {
            FieldDescriptor::Byte
            | FieldDescriptor::Char
            | FieldDescriptor::Integer
            | FieldDescriptor::Short
            | FieldDescriptor::Boolean => Primitive::Integer,
            FieldDescriptor::Object(name) => Primitive::Object(name),
            FieldDescriptor::Array {
                dimension_count,
                descriptor,
            } => {
                assert_eq!(dimension_count, 1, "Higher dimensions not implemented yet");
                Primitive::Array(Box::new(descriptor.into_primitive()))
            }
            other => todo!("No primitive for {other} yet"),
        }
    }
}

impl Display for FieldDescriptor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FieldDescriptor::Byte => write!(f, "B"),
            FieldDescriptor::Char => write!(f, "C"),
            FieldDescriptor::Double => write!(f, "D"),
            FieldDescriptor::Float => write!(f, "F"),
            FieldDescriptor::Integer => write!(f, "I"),
            FieldDescriptor::Long => write!(f, "J"),
            FieldDescriptor::Object(class_name) => write!(f, "L{class_name};"),
            FieldDescriptor::Short => write!(f, "S"),
            FieldDescriptor::Boolean => write!(f, "Z"),
            FieldDescriptor::Array {
                dimension_count,
                descriptor,
            } => {
                for _ in 0..*dimension_count {
                    write!(f, "[")?;
                }
                descriptor.fmt(f)?;
                Ok(())
            }
        }
    }
}

/// https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-4.html#jvms-ReturnDescriptor
#[derive(Debug, Clone)]
pub enum ReturnDescriptor {
    Value(FieldDescriptor),
    Void,
}

impl ReturnDescriptor {
    pub fn into_primitive(self) -> Option<Primitive> {
        match self {
            ReturnDescriptor::Value(value) => Some(value.into_primitive()),
            ReturnDescriptor::Void => None,
        }
    }
}

impl Display for ReturnDescriptor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReturnDescriptor::Value(field_descriptor) => field_descriptor.fmt(f),
            ReturnDescriptor::Void => f.write_str("V"),
        }
    }
}

impl From<FieldDescriptor> for ReturnDescriptor {
    fn from(value: FieldDescriptor) -> Self {
        ReturnDescriptor::Value(value)
    }
}

#[derive(Debug, Clone)]
pub struct MethodDescriptor {
    pub parameters: Vec<FieldDescriptor>,
    pub return_type: ReturnDescriptor,
}

impl Display for MethodDescriptor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        for arg in &self.parameters {
            arg.fmt(f)?;
        }
        write!(f, ")")?;

        self.return_type.fmt(f)?;
        Ok(())
    }
}

/// Returns the representation of an auryn type for the jvm.
/// Returns [`None`] if the type is not represented at runtime (because it is a compile time construct or zero-sized)
pub fn get_representation(air_type: &Type) -> Option<Primitive> {
    match air_type {
        Type::Number => Some(Primitive::Integer),
        Type::String => Some(Primitive::string()),
        Type::Array(content_type) => {
            let content_repr = get_representation(content_type);
            match content_repr {
                Some(repr) => Some(Primitive::Array(Box::new(repr))),
                None => todo!("Add representation for array of zero-sized types"),
            }
        }
        Type::Null | Type::Function(_) => None,
        Type::Top => todo!("The top type cannot be represented yet"),
        Type::Error => unreachable!("Called with error type"),
    }
}

/// Returns the representation of `air_type` as a jvm return [`ReturnDescriptor`]
pub fn get_return_type_representation(air_type: &Type) -> ReturnDescriptor {
    get_representation(air_type)
        .map(|primitive| primitive.into_field_descriptor().into())
        .unwrap_or(ReturnDescriptor::Void)
}

/// Returns the representation of an auryn type for the jvm
pub fn get_function_representation(ty: &FunctionType) -> MethodDescriptor {
    let parameters = ty
        .parameters
        .iter()
        .flat_map(|it| get_representation(it).map(|it| it.into_field_descriptor()))
        .collect();
    let return_type = get_representation(&ty.return_type).map_or(ReturnDescriptor::Void, |it| {
        it.into_field_descriptor().into()
    });
    MethodDescriptor {
        parameters,
        return_type,
    }
}
