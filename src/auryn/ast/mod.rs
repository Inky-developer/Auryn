pub mod ast_parser;

use crate::auryn::{
    ast::ast_parser::{FromSyntaxNode, NodeOrError, Root},
    syntax_tree::SyntaxTree,
};

pub fn query_ast(syntax_tree: &SyntaxTree) -> NodeOrError<Root> {
    let &SyntaxTree { root_node } = &syntax_tree;

    <NodeOrError<Root>>::from_syntax_node(root_node)
}

#[cfg(test)]
mod tests {
    use crate::auryn::{
        ast::{NodeOrError, Root, query_ast},
        parser::Parser,
    };

    fn parse(input: &str) -> NodeOrError<Root> {
        query_ast(&Parser::new(input).parse().syntax_tree.unwrap())
    }

    #[test]
    fn test_expression() {
        insta::assert_debug_snapshot!(parse("1+2"));
        insta::assert_debug_snapshot!(parse("1 + 2 * 3"));
        insta::assert_debug_snapshot!(parse("1 * 2 + 3"));
    }

    #[test]
    fn test_function_call() {
        insta::assert_debug_snapshot!(parse("print(1)"));
    }

    #[test]
    fn test_expressions() {
        insta::assert_debug_snapshot!(parse("1 + (2 + 3)"));
    }
}
