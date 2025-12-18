use std::{fs::OpenOptions, io::Write};

use auryn::{
    auryn::{ast::query_ast, codegen_java::query_class, parser::Parser},
    java::class::ClassData,
};

fn main() -> std::io::Result<()> {
    let mut args = std::env::args();
    args.next().unwrap();
    if let Some(filename) = args.next() {
        let input = std::fs::read_to_string(filename)?;
        let class = get_class(&input);
        run(class);
    } else {
        repl();
    }

    Ok(())
}

fn repl() {
    let mut input = String::new();
    loop {
        read_user_input(&mut input);
        let class = get_class(&input);
        input.clear();

        run(class);
    }
}

fn run(class: ClassData) {
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
}

fn get_class(input: &str) -> ClassData {
    let result = Parser::new(input).parse();
    // dbg!(&result.syntax_tree);
    let diagnostics = result
        .syntax_tree
        .as_ref()
        .map(|it| it.collect_diagnostics())
        .unwrap_or_default();
    if !diagnostics.is_empty() {
        println!("Warn: {diagnostics:?}");
    }
    let ast = query_ast(result.syntax_tree.as_ref().unwrap());

    query_class("Helloworld".to_string(), &ast).unwrap()
}

fn read_user_input(buf: &mut String) {
    print_prompt();
    for line in std::io::stdin().lines() {
        let line = line.unwrap();
        if line.is_empty() {
            break;
        }
        buf.push_str(&line);

        print_prompt();
    }
}

fn print_prompt() {
    print!("> ");
    std::io::stdout().flush().unwrap();
}
