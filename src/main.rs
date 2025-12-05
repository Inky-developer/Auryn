use std::{fs::OpenOptions, io::Write};

use auryn::{
    auryn::{ast::query_ast, codegen_java::query_class, parser::Parser},
    java::class::ClassData,
};

fn main() {
    repl();
}

fn repl() {
    let mut input = String::new();
    let stdin = std::io::stdin();
    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();
        stdin.read_line(&mut input).unwrap();
        let class = get_class(&input);
        input.clear();

        let mut f = OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open("Helloworld.class")
            .unwrap();
        class.serialize(&mut f).unwrap();

        let mut handle = std::process::Command::new("java")
            .arg("Helloworld")
            .spawn()
            .unwrap();
        handle.wait().unwrap();

        std::fs::remove_file("Helloworld.class").unwrap();
    }
}

fn get_class(input: &str) -> ClassData {
    let result = Parser::new(input).parse();
    if !result.diagnostics.is_empty() {
        println!("Warn: {:?}", result.diagnostics);
    }
    let ast = query_ast(result.syntax_tree.as_ref().unwrap());
    let class = query_class("Helloworld".to_string(), &ast).unwrap();
    class
}
