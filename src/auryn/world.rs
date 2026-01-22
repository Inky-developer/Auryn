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
    _module_name_to_file_id: FastMap<SmallString, FileId>,
}

impl World {
    pub fn new(environment: &impl Environment, main_file: &str) -> Self {
        let input_files = InputFiles::new(environment.load_project(), main_file);
        let _module_name_to_file_id = input_files
            .iter()
            .map(|(_, file)| (file.name.clone(), file.file_id))
            .collect();

        Self {
            input_files,
            _module_name_to_file_id,
        }
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

        let ast = query_ast(file.syntax_tree()).unwrap();
        let air = query_air(ast);
        diagnostics.extend(air.diagnostics.take());
        (air.air, diagnostics.into_iter().collect())
    }
}
