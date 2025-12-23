use crate::auryn::{
    air::{
        ast_transformer::{AirOutput, transform_ast},
        typecheck::typecheck_air,
    },
    ast::ast_node,
};

pub mod ast_transformer;
pub mod data;
pub mod typecheck;
pub mod types;

pub fn query_air(ast: ast_node::Root) -> AirOutput {
    let mut output = transform_ast(ast);

    let typecheck_diagnostics = typecheck_air(&mut output.air);
    output.diagnostics.extend(typecheck_diagnostics);

    output
}

#[cfg(test)]
mod tests {
    use crate::auryn::{
        air::{ast_transformer::AirOutput, query_air},
        ast::query_ast2,
        file_id::FileId,
        parser::Parser,
    };

    fn compile_wrapped(input: &str) -> AirOutput {
        let wrapped_input = format!("fn main() {{ {input} }}");
        compile(&wrapped_input)
    }

    fn compile(input: &str) -> AirOutput {
        let output = Parser::new(FileId::MAIN_FILE, input).parse();
        let diagnostics = output
            .syntax_tree
            .as_ref()
            .map(|it| it.collect_diagnostics())
            .unwrap_or_default();
        if !diagnostics.is_empty() {
            panic!("Could not parse input: {diagnostics:?}");
        }

        let tree = output.syntax_tree.unwrap();
        println!("{}", tree.display(input));
        let ast = query_ast2(&tree).unwrap();
        query_air(ast)
    }

    #[test]
    fn it_works() {
        insta::assert_debug_snapshot!(compile_wrapped("print(1)"));
    }

    #[test]
    fn test_function_call() {
        insta::assert_debug_snapshot!(compile(
            "fn main() { foo(1) }\nfn foo(bar: Number) { print(bar) }"
        ));
    }

    #[test]
    fn invalid_assignment() {
        insta::assert_debug_snapshot!(compile_wrapped("let a = print(1)\nprint(a + 1)"));
    }
}
