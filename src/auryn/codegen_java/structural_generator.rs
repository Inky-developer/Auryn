use crate::{
    auryn::codegen_java::representation::{
        ImplicitArgs, MethodDescriptor, Representation, ReturnDescriptor, StructuralRepr,
    },
    java::{
        class::{ClassData, Field, FieldAccessFlags, Method, TypeCategory},
        constant_pool_builder::ConstantPoolBuilder,
        function_assembler::{FunctionAssembler, Instruction, VariableId},
    },
    utils::small_string::SmallString,
};

pub(super) fn gen_structural_type_class(repr: &StructuralRepr) -> ClassData {
    let mut pool = ConstantPoolBuilder::default();
    let init_method = gen_init_method(repr, &mut pool);
    let fields = repr
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
    ClassData::new(repr.class_name.clone(), pool, fields, vec![init_method])
}

/// The init method of a structural type consume all its fields as parameters and sets
/// the fields on the class
fn gen_init_method(repr: &StructuralRepr, pool: &mut ConstantPoolBuilder) -> Method {
    let method_name: SmallString = "<init>".into();
    let descriptor = repr.init_descriptor();
    let mut assembler = FunctionAssembler::new(
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
        method_descriptor: MethodDescriptor {
            parameters: vec![],
            return_type: ReturnDescriptor::Void,
        },
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
