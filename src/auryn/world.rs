use crate::{
    auryn::{
        air::{data::Air, query_air},
        ast::query_ast,
        diagnostic::Diagnostics,
        environment::Environment,
        file_id::FileId,
        input_files::{InputFile, InputFiles},
    },
    utils::{default, fast_map::FastMap, small_string::SmallString},
};

pub struct World {
    environment: Box<dyn Environment>,
    input_files: InputFiles,
    module_name_to_file_id: FastMap<SmallString, FileId>,
}

impl World {
    pub fn new(environment: Box<dyn Environment>) -> Self {
        Self {
            environment,
            input_files: default(),
            module_name_to_file_id: default(),
        }
    }

    pub fn file_id_for_module(&mut self, path: &str) -> Option<FileId> {
        if !self.module_name_to_file_id.contains_key(path) {
            let contents = self.environment.load_module(path)?;
            let file_id = self.input_files.add(path.into(), contents);
            self.module_name_to_file_id.insert(path.into(), file_id);
        }
        self.module_name_to_file_id.get(path).copied()
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
