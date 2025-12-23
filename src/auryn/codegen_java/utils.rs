use crate::{
    auryn::air::types::Type,
    java::{constant_pool_builder::ConstantPoolBuilder, function_assembler::Primitive},
};

pub fn translate_type(pool: &mut ConstantPoolBuilder, air_type: &Type) -> Option<Primitive> {
    match air_type {
        Type::Number => Some(Primitive::Integer),
        Type::String => Some(Primitive::Object(pool.get_string_index())),
        Type::Null | Type::Function(_) => None,
        Type::Top => todo!("The top type cannot be represented yet"),
        Type::Error => unreachable!("Called with error type"),
    }
}
