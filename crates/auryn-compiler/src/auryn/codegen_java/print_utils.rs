use crate::{
    auryn::codegen_java::representation::{
        FieldDescriptor, MethodDescriptor, Representation, ReturnDescriptor,
    },
    java::function_assembler::{FunctionAssembler, Instruction},
};

pub fn make_value_printable(
    assembler: &mut FunctionAssembler,
    repr: Option<Representation>,
    load_value: impl FnOnce(&mut FunctionAssembler),
) -> Result<FieldDescriptor, Option<Representation>> {
    match repr {
        Some(repr) if repr.is_printable() => {
            load_value(assembler);
            Ok(repr.into_field_descriptor())
        }
        Some(Representation::Object(_)) => {
            load_value(assembler);
            Ok(FieldDescriptor::object())
        }
        Some(Representation::Array(array)) => {
            load_value(assembler);

            match array.as_ref() {
                Representation::Array(_) => {
                    let descriptor = FieldDescriptor::Array {
                        dimension_count: 1,
                        descriptor: Box::new(FieldDescriptor::object()),
                    };
                    assembler.add(Instruction::InvokeStatic {
                        class_name: "java/util/Arrays".into(),
                        name: "deepToString".into(),
                        method_descriptor: MethodDescriptor {
                            parameters: vec![descriptor],
                            return_type: ReturnDescriptor::Value(FieldDescriptor::string()),
                        },
                    });
                }
                _ => {
                    let descriptor = FieldDescriptor::Array {
                        dimension_count: 1,
                        descriptor: Box::new(
                            array
                                .into_field_descriptor()
                                .into_base_object_or_primitive(),
                        ),
                    };
                    assembler.add(Instruction::InvokeStatic {
                        class_name: "java/util/Arrays".into(),
                        name: "toString".into(),
                        method_descriptor: MethodDescriptor {
                            parameters: vec![descriptor],
                            return_type: ReturnDescriptor::Value(FieldDescriptor::string()),
                        },
                    });
                }
            }
            Ok(FieldDescriptor::string())
        }
        other => Err(other),
    }
}
