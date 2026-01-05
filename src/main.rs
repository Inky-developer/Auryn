use std::{fs::OpenOptions, io::Write};

use auryn::{
    auryn::{
        air::query_air,
        ast::query_ast,
        codegen_java::class_generator::generate_class,
        diagnostic::{DiagnosticKind, Diagnostics, InputFile, InputFiles},
        file_id::FileId,
        parser::Parser,
    },
    java::class::ClassData,
};

fn main() -> std::io::Result<()> {
    let mut args = std::env::args();
    args.next().unwrap();
    if let Some(filename) = args.next() {
        let input = std::fs::read_to_string(filename)?;
        if let Some(class) = get_class(&input) {
            run(class);
        }
    } else {
        repl();
    }

    Ok(())
}

fn repl() {
    let mut input = String::new();
    loop {
        input.clear();
        read_user_input(&mut input);
        let input = format!("fn main() {{ {input} }}");
        if let Some(class) = get_class(&input) {
            run(class);
        }
    }
}

fn run(class: ClassData) {
    let mut f = OpenOptions::new()
        .create(true)
        .truncate(true)
        .write(true)
        .open("Main.class")
        .unwrap();
    class.serialize(&mut f).unwrap();

    let mut handle = std::process::Command::new("java")
        .arg("Main")
        .spawn()
        .unwrap();
    handle.wait().unwrap();
}

fn get_class(input: &str) -> Option<ClassData> {
    let result = Parser::new(FileId::MAIN_FILE, input).parse();
    let mut diagnostics = result
        .syntax_tree
        .as_ref()
        .map(|it| it.collect_diagnostics())
        .unwrap_or_default();

    let syntax_tree = result.syntax_tree.unwrap();
    // println!("{}", syntax_tree.display(input));

    let ast = query_ast(&syntax_tree).unwrap();
    let air = query_air(ast);
    diagnostics.extend(air.diagnostics.take());
    if !diagnostics.is_empty() {
        let should_abort = diagnostics
            .iter()
            .any(|it| matches!(it.kind, DiagnosticKind::Error(_)));
        let mut input_files = InputFiles::default();
        input_files.add(
            FileId::MAIN_FILE,
            InputFile {
                name: "main".into(),
                source: input.into(),
                syntax_tree,
            },
        );
        let diagnostics: Diagnostics = diagnostics.into_iter().collect();
        diagnostics.display(&input_files).eprint();

        if should_abort {
            return None;
        }
    }
    Some(generate_class(&air.air))
}

fn read_user_input(buf: &mut String) {
    print_prompt();
    for line in std::io::stdin().lines() {
        let line = line.unwrap();
        if line.is_empty() {
            break;
        }
        buf.push_str(&line);
        buf.push('\n');

        print_prompt();
    }
}

fn print_prompt() {
    print!("> ");
    std::io::stdout().flush().unwrap();
}
