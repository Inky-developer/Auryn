pub mod ast_node;

use crate::auryn::{
    ast::ast_node::{AstError, AstResult},
    syntax_tree::SyntaxTree,
};

pub fn query_ast(syntax_tree: &SyntaxTree) -> AstResult<ast_node::Root<'_>> {
    ast_node::Root::new(&syntax_tree.root_node).ok_or(AstError)
}
