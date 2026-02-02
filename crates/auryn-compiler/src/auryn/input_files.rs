use std::{cell::OnceCell, num::NonZeroU16};

use stdx::{FastMap, SmallString, default};

use crate::auryn::{
    api::AurynError,
    diagnostics::diagnostic_display::ComputedSpan,
    environment::ProjectTree,
    file_id::FileId,
    parser::{Parser, ParserOutput},
    syntax_id::SyntaxId,
    syntax_tree::SyntaxTree,
};

#[derive(Debug)]
pub struct InputFile {
    pub name: SmallString,
    pub source: Box<str>,
    pub file_id: FileId,
    pub parser_output: OnceCell<ParserOutput>,
}

impl InputFile {
    pub fn new(file_id: FileId, name: SmallString, source: Box<str>) -> Self {
        Self {
            file_id,
            name,
            source,
            parser_output: OnceCell::new(),
        }
    }

    pub fn syntax_tree(&self) -> &SyntaxTree {
        &self
            .parser_output
            .get_or_init(|| Parser::new(self.file_id, &self.source).parse())
            .syntax_tree
    }

    pub fn compute_span(&self, syntax_id: SyntaxId) -> ComputedSpan {
        self.syntax_tree().get_span(syntax_id)
    }
}

#[derive(Debug)]
pub struct InputFiles {
    data: FastMap<FileId, InputFile>,
    file_id_counter: NonZeroU16,
}

impl InputFiles {
    pub fn new(mut project_tree: ProjectTree, main_file: &str) -> Result<Self, AurynError> {
        let mut this = Self::default();

        let (main_name, main_contents) = project_tree
            .source_files
            .remove_entry(main_file)
            .ok_or_else(|| AurynError::MainFileDoesNotExist(main_file.to_string()))?;
        this.add(main_name, main_contents);

        for (name, contents) in project_tree.source_files {
            this.add(name, contents);
        }

        Ok(this)
    }

    pub fn add(&mut self, name: SmallString, source: Box<str>) -> FileId {
        let file_id = FileId(self.file_id_counter);
        self.data
            .insert(file_id, InputFile::new(file_id, name, source));
        self.file_id_counter = self.file_id_counter.checked_add(1).unwrap();
        file_id
    }

    pub fn get(&self, file_id: FileId) -> &InputFile {
        &self.data[&file_id]
    }

    pub fn iter(&self) -> impl Iterator<Item = (FileId, &InputFile)> + Clone {
        self.data.iter().map(|(file_id, file)| (*file_id, file))
    }

    pub fn compute_span(&self, syntax_id: SyntaxId) -> ComputedSpan {
        let file_id = syntax_id.file_id().unwrap();
        self.data[&file_id].compute_span(syntax_id)
    }
}

impl Default for InputFiles {
    fn default() -> Self {
        Self {
            data: default(),
            file_id_counter: 1.try_into().unwrap(),
        }
    }
}
