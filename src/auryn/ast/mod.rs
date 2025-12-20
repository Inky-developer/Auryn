pub mod ast_node;

use crate::auryn::ast::ast_node::{AstError, AstResult};
use crate::auryn::syntax_tree::SyntaxTree;

pub fn query_ast2(syntax_tree: &SyntaxTree) -> AstResult<ast_node::Root<'_>> {
    ast_node::Root::new(&syntax_tree.root_node).ok_or(AstError)
}
