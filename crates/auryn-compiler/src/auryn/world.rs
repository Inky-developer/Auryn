use stdx::{FastMap, SmallString};

use crate::auryn::{
    air::{data::Air, query_air},
    api::AurynError,
    diagnostics::diagnostic::Diagnostics,
    environment::Environment,
    file_id::FileId,
    input_files::InputFiles,
};

pub struct World {
    input_files: InputFiles,
    _module_name_to_file_id: FastMap<SmallString, FileId>,
}

impl World {
    pub fn new(environment: &mut impl Environment, main_file: &str) -> Result<Self, AurynError> {
        let input_files = InputFiles::new(environment.load_project(), main_file)?;
        let module_name_to_file_id = input_files
            .iter()
            .map(|(_, file)| (file.name.clone(), file.file_id))
            .collect();

        Ok(Self {
            input_files,
            _module_name_to_file_id: module_name_to_file_id,
        })
    }

    pub fn input_files(&self) -> &InputFiles {
        &self.input_files
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
