use crate::{
    auryn::codegen_java::{
        print_utils::make_value_printable,
        representation::{
            FieldDescriptor, ImplicitArgs, MethodDescriptor, Representation, ReturnDescriptor,
            StructuralRepr,
        },
    },
    java::{
        class::{ClassData, Field, FieldAccessFlags, Method, TypeCategory},
        constant_pool_builder::ConstantPoolBuilder,
        function_assembler::{ConstantValue, FunctionAssembler, Instruction, VariableId},
        source_graph::BlockFinalizer,
    },
    utils::small_string::SmallString,
};

pub(super) fn gen_structural_type_class(info: &StructuralRepr) -> ClassData {
    let mut pool = ConstantPoolBuilder::default();
    let init_method = gen_init_method(info, &mut pool);
    let to_string_method = gen_to_string_method(info, &mut pool);
    let fields = info
        .fields
        .iter()
        .map(|(name, repr)| Field {
            flags: FieldAccessFlags::PUBLIC,
            name_index: pool.add_utf8(name.clone()),
            descriptor_index: pool
                .add_utf8(repr.clone().into_field_descriptor().to_string().into()),
            attributes: Vec::new(),
        })
        .collect();
    ClassData::new(
        info.class_name.clone(),
        pool,
        fields,
        vec![init_method, to_string_method],
    )
}

/// The init method of a structural type consume all its fields as parameters and sets
/// the fields on the class
fn gen_init_method(repr: &StructuralRepr, pool: &mut ConstantPoolBuilder) -> Method {
    let method_name: SmallString = "<init>".into();
    let descriptor = repr.init_descriptor();
    let mut assembler = FunctionAssembler::new(
        pool.add_class(repr.class_name.clone()),
        method_name,
        descriptor.clone(),
        ImplicitArgs::UninitializedThis,
        pool,
    );

    assembler.add(Instruction::Load(VariableId {
        index: 0,
        r#type: Representation::Object(repr.class_name.clone()),
    }));
    if !repr.fields.is_empty() {
        assembler.add(Instruction::Dup(TypeCategory::Normal));
    }
    assembler.add(Instruction::InvokeSpecial {
        class_name: "java/lang/Object".into(),
        name: "<init>".into(),
        method_descriptor: MethodDescriptor::VOID,
    });

    let mut variable_id = 1u16;
    for (index, (name, field)) in repr.fields.iter().cloned().enumerate() {
        let size = field.stack_size();
        let field_descriptor = field.clone().into_field_descriptor();
        if index + 1 != repr.fields.len() {
            assembler.add(Instruction::Dup(TypeCategory::Normal));
        }
        assembler.add_all([
            Instruction::Load(VariableId {
                index: variable_id,
                r#type: field,
            }),
            Instruction::PutField {
                class_name: repr.class_name.clone(),
                name,
                field_descriptor,
            },
        ]);
        variable_id += size;
    }

    assembler.assemble()
}

fn gen_to_string_method(info: &StructuralRepr, pool: &mut ConstantPoolBuilder) -> Method {
    const STRING_BUILDER: &str = "java/lang/StringBuilder";

    fn write_str(assembler: &mut FunctionAssembler, text: &str) {
        assembler.add_all([
            Instruction::LoadConstant {
                value: ConstantValue::String(text.into()),
            },
            Instruction::InvokeVirtual {
                class_name: STRING_BUILDER.into(),
                name: "append".into(),
                method_descriptor: MethodDescriptor {
                    parameters: vec![FieldDescriptor::string()],
                    return_type: ReturnDescriptor::Value(FieldDescriptor::Object(
                        STRING_BUILDER.into(),
                    )),
                },
            },
        ]);
    }

    fn write_field(
        assembler: &mut FunctionAssembler,
        class_name: SmallString,
        class_repr: Option<Representation>,
        field_name: SmallString,
        repr: &Representation,
    ) {
        let field_descriptor = make_value_printable(assembler, Some(repr.clone()), |assembler| {
            assembler.add_all([
                Instruction::Load(VariableId {
                    index: 0,
                    r#type: class_repr.unwrap(),
                }),
                Instruction::GetField {
                    class_name,
                    name: field_name.clone(),
                    field_descriptor: repr.clone().into_field_descriptor(),
                },
            ])
        });
        match field_descriptor {
            Ok(field_descriptor) => assembler.add(Instruction::InvokeVirtual {
                class_name: STRING_BUILDER.into(),
                name: "append".into(),
                method_descriptor: MethodDescriptor {
                    parameters: vec![field_descriptor],
                    return_type: ReturnDescriptor::Value(FieldDescriptor::Object(
                        STRING_BUILDER.into(),
                    )),
                },
            }),
            Err(_) => write_str(assembler, &field_name),
        }
    }

    let method_name: SmallString = "toString".into();
    let descriptor = MethodDescriptor {
        parameters: Vec::new(),
        return_type: ReturnDescriptor::Value(FieldDescriptor::string()),
    };
    let mut assembler = FunctionAssembler::new(
        pool.add_class(info.class_name.clone()),
        method_name,
        descriptor.clone(),
        ImplicitArgs::This,
        pool,
    );

    assembler.add_all([
        Instruction::New(STRING_BUILDER.into()),
        Instruction::Dup(TypeCategory::Normal),
        Instruction::InvokeSpecial {
            class_name: STRING_BUILDER.into(),
            name: "<init>".into(),
            method_descriptor: MethodDescriptor::VOID,
        },
    ]);

    if info.is_named {
        write_str(&mut assembler, &format!("{} {{ ", info.class_name));
    } else {
        write_str(&mut assembler, "{ ");
    }

    for (index, (name, repr)) in info.fields.iter().enumerate() {
        if index != 0 {
            write_str(&mut assembler, &format!(", {name}: "));
        } else {
            write_str(&mut assembler, &format!("{name}: "));
        }

        write_field(
            &mut assembler,
            info.class_name.clone(),
            info.to_representation(),
            name.clone(),
            repr,
        );
    }

    write_str(&mut assembler, " }");
    assembler.add_all([Instruction::InvokeVirtual {
        class_name: STRING_BUILDER.into(),
        name: "toString".into(),
        method_descriptor: MethodDescriptor {
            parameters: Vec::new(),
            return_type: FieldDescriptor::string().into(),
        },
    }]);
    assembler.current_block_mut().finalizer = BlockFinalizer::ReturnObject;
    assembler.assemble()
}
