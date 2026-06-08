use crate::{
    ProjectTree,
    auryn::{
        air::{data::Air, query_air},
        api::AurynError,
        diagnostics::diagnostic::Diagnostics,
        input_files::{InputFileFlags, InputFiles},
    },
};

#[derive(Debug, Default)]
pub struct World {
    pub input_files: InputFiles,
}

impl World {
    pub fn new(
        project_tree: ProjectTree,
        main_file: &str,
        std_library_source: Box<str>,
    ) -> Result<Self, AurynError> {
        let mut input_files = InputFiles::new(project_tree, main_file)?;
        input_files.add("std".into(), std_library_source, InputFileFlags::Privileged);

        Ok(Self { input_files })
    }

    pub fn into_input_files(self) -> InputFiles {
        self.input_files
    }

    pub fn query_air(&mut self) -> (Air, Diagnostics) {
        let mut diagnostics = self
            .input_files
            .iter()
            .flat_map(|(_id, file)| file.syntax_tree().collect_diagnostics())
            .collect::<Vec<_>>();

        let included_modules = self.input_files.iter().map(|(_id, file)| file);

        let (air, air_diagnostics) = query_air(included_modules);
        diagnostics.extend(air_diagnostics.take());
        (air, diagnostics.into_iter().collect())
    }
}
