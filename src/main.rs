use std::fs::OpenOptions;

use auryn::auryn::{ast::query_ast, codegen_java::query_class, parser::Parser};

fn main() {
    let input = "1 + 2 * 3";
    let result = Parser::new(input).parse();
    println!("{}", result.syntax_tree.as_ref().unwrap().display(input));
    let ast = query_ast(result.syntax_tree.as_ref().unwrap());
    let class = query_class("Helloworld".to_string(), &ast).unwrap();

    dbg!(&class);

    let mut f = OpenOptions::new()
        .create(true)
        .truncate(true)
        .write(true)
        .open("Helloworld.class")
        .unwrap();
    class.serialize(&mut f).unwrap();
}
