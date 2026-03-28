use crate::auryn::{
    air::{data::Air, query_air},
    api::AurynError,
    diagnostics::diagnostic::Diagnostics,
    environment::Environment,
    input_files::InputFiles,
};

#[derive(Debug, Default)]
pub struct World {
    pub input_files: InputFiles,
}

impl World {
    pub fn new(environment: &mut impl Environment, main_file: &str) -> Result<Self, AurynError> {
        let input_files = InputFiles::new(environment.load_project(), main_file)?;

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
