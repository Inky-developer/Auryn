use crate::{
    auryn::{
        air::data::Air,
        codegen_java::{
            class_generator::generate_main_class, representation::RepresentationCtx,
            structural_generator::gen_structural_type_class,
        },
        input_files::InputFiles,
    },
    java::class::ClassData,
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

pub fn codegen(input_files: &InputFiles, air: &Air) -> CodegenOutput {
    let mut ctx = RepresentationCtx::default();
    let main_class = generate_main_class(air, &mut ctx, input_files);

    let mut output = CodegenOutput::default();
    output.files.insert("Main".into(), main_class);

    let structurals = ctx
        .structural_types
        .into_values()
        .chain(ctx.struct_types.into_values());
    for structural_repr in structurals {
        if structural_repr.is_zero_sized {
            continue;
        }
        let class = gen_structural_type_class(&structural_repr);
        output.files.insert(structural_repr.class_name, class);
    }

    output
}
