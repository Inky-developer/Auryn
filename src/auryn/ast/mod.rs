pub mod ast_node;

use crate::auryn::syntax_tree::SyntaxTree;

pub fn query_ast(syntax_tree: &SyntaxTree) -> ast_node::Root<'_> {
    ast_node::Root::new(&syntax_tree.root_node).expect("Parser should always emit a root node")
}
