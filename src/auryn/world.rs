use crate::{
    auryn::{
        air::{data::Air, query_air},
        ast::query_ast,
        diagnostic::Diagnostics,
        environment::Environment,
        file_id::FileId,
        input_files::{InputFile, InputFiles},
    },
    utils::{fast_map::FastMap, small_string::SmallString},
};

pub struct World {
    input_files: InputFiles,
    module_name_to_file_id: FastMap<SmallString, FileId>,
}

impl World {
    pub fn new(environment: &impl Environment, main_file: &str) -> Self {
        let input_files = InputFiles::new(environment.load_project(), main_file);
        let module_name_to_file_id = input_files
            .iter()
            .map(|(_, file)| (file.name.clone(), file.file_id))
            .collect();

        Self {
            input_files,
            module_name_to_file_id,
        }
    }

    pub fn file_id_for_module(&self, module_name: &str) -> Option<FileId> {
        self.module_name_to_file_id.get(module_name).copied()
    }

    pub fn file(&self, id: FileId) -> &InputFile {
        self.input_files.get(id)
    }

    pub fn input_files(self) -> InputFiles {
        self.input_files
    }

    pub fn query_air(&mut self, file: FileId) -> (Air, Diagnostics) {
        let file = self.file(file);
        let mut diagnostics = file.syntax_tree().collect_diagnostics();

        let included_modules = self.input_files.iter().map(|(_id, file)| file);

        let ast = query_ast(file.syntax_tree());
        let (air, air_diagnostics) = query_air(ast, included_modules);
        diagnostics.extend(air_diagnostics.take());
        (air, diagnostics.into_iter().collect())
    }
}
