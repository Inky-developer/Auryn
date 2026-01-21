use crate::{
    auryn::{
        air::data::Air,
        codegen_java::{
            class_generator::generate_main_class,
            representation::{
                ImplicitArgs, MethodDescriptor, Representation, RepresentationCtx,
                ReturnDescriptor, StructuralRepr,
            },
        },
    },
    java::{
        class::{ClassData, Field, FieldAccessFlags, Method, TypeCategory},
        constant_pool_builder::ConstantPoolBuilder,
        function_assembler::{FunctionAssembler, Instruction, VariableId},
    },
    utils::{fast_map::FastMap, small_string::SmallString},
};

#[derive(Default)]
pub struct CodegenOutput {
    pub files: FastMap<SmallString, ClassData>,
}

impl std::fmt::Debug for CodegenOutput {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut files = self.files.iter().collect::<Vec<_>>();
        files.sort_by_key(|(name, _)| *name);

        for (name, data) in files {
            writeln!(f, "{name}:")?;
            writeln!(f, "{data:?}")?;
            writeln!(f, "-----\n")?;
        }

        Ok(())
    }
}

pub fn codegen(air: &Air) -> CodegenOutput {
    let mut ctx = RepresentationCtx::default();
    let main_class = generate_main_class(air, &mut ctx);

    let mut output = CodegenOutput::default();
    output.files.insert("Main".into(), main_class);

    for (_type_id, structural_repr) in ctx.structural_types {
        if structural_repr.is_zero_sized() {
            continue;
        }
        let class = gen_structural_type_class(&structural_repr);
        output.files.insert(structural_repr.class_name, class);
    }

    output
}

fn gen_structural_type_class(repr: &StructuralRepr) -> ClassData {
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
