use crate::{
    auryn::air::types::Type,
    java::{constant_pool_builder::ConstantPoolBuilder, function_assembler::Primitive},
};

pub fn translate_type(pool: &mut ConstantPoolBuilder, air_type: &Type) -> Primitive {
    match air_type {
        Type::Number => Primitive::Integer,
        Type::String => Primitive::Object(pool.get_string_index()),
        Type::Function(_) => todo!("Function types cannot be represented yet"),
        Type::Top => todo!("The top type cannot be represented yet"),
        Type::Null => unreachable!("Null type is not represented in java"),
        Type::Error => unreachable!("Called with error type"),
    }
}
