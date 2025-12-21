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
        parser::Parser,
    };

    fn compile(input: &str) -> AirOutput {
        let output = Parser::new(input).parse();
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
        insta::assert_debug_snapshot!(compile("print(1)"));
    }

    #[test]
    fn invalid_assignment() {
        insta::assert_debug_snapshot!(compile("let a = print(1)\nprint(a + 1)"));
    }
}
