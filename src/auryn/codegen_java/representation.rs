use crate::{
    auryn::air::types::{FunctionType, Type},
    java::function_assembler::{MethodDescriptor, Primitive, ReturnDescriptor},
};

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
