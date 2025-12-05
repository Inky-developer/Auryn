use crate::auryn::{
    BinaryOperatorToken,
    syntax_tree::{SyntaxNode, SyntaxNodeKind, SyntaxTree},
};

#[derive(Debug)]
pub struct AstError;

pub type NodeOrError<T> = Result<Node<T>, AstError>;

trait FromSyntaxNode
where
    Self: Sized,
{
    fn get_kind(node: &SyntaxNode) -> Result<Self, AstError>;

    fn from_syntax_node(node: &SyntaxNode) -> NodeOrError<Self> {
        let result = Self::get_kind(node)?;
        Ok(Node {
            kind: result,
            len: node.len + node.trailing_whitespace,
        })
    }
}

#[derive(Debug)]
pub struct Node<T> {
    pub len: u32,
    pub kind: T,
}

pub type NodeRef<T> = Box<NodeOrError<T>>;

#[derive(Debug)]
pub struct Root {
    pub expression: NodeRef<Expression>,
}

#[derive(Debug)]
pub enum Expression {
    Value(NodeRef<Value>),
    BinaryOperation {
        lhs: NodeRef<Expression>,
        operator: NodeRef<BinaryOperatorToken>,
        rhs: NodeRef<Expression>,
    },
}

impl FromSyntaxNode for Expression {
    fn get_kind(node: &SyntaxNode) -> Result<Self, AstError> {
        if node.kind != SyntaxNodeKind::Expression {
            return Err(AstError);
        }

        match node.children.len() {
            1 => {
                let (value,) = node.map_children()?;
                Ok(Expression::Value(NodeRef::new(value)))
            }
            3 => {
                let (lhs, op, rhs) = node.map_children()?;
                Ok(Expression::BinaryOperation {
                    lhs: NodeRef::new(lhs),
                    operator: NodeRef::new(op),
                    rhs: NodeRef::new(rhs),
                })
            }
            _ => Err(AstError),
        }
    }
}

#[derive(Debug)]
pub enum Value {
    Int(i32),
}

impl FromSyntaxNode for Value {
    fn get_kind(node: &SyntaxNode) -> Result<Self, AstError> {
        let SyntaxNodeKind::Number(number) = node.kind else {
            return Err(AstError);
        };
        if !node.children.is_empty() {
            return Err(AstError);
        }

        Ok(Value::Int(number))
    }
}

impl FromSyntaxNode for BinaryOperatorToken {
    fn get_kind(node: &SyntaxNode) -> Result<Self, AstError> {
        let SyntaxNodeKind::BinaryOperator(op) = node.kind else {
            return Err(AstError);
        };
        if !node.children.is_empty() {
            return Err(AstError);
        }
        Ok(op)
    }
}

pub fn query_ast(syntax_tree: &SyntaxTree) -> NodeOrError<Root> {
    let &SyntaxTree {
        root_node,
        leading_whitespace,
    } = &syntax_tree;
    let len = root_node.len + leading_whitespace;

    let (expression,) = root_node.map_children()?;
    NodeOrError::Ok(Node {
        kind: Root {
            expression: NodeRef::new(expression),
        },
        len,
    })
}

impl SyntaxNode {
    fn map_children<T: FromSyntaxNodeChildren>(&self) -> Result<T, AstError> {
        T::from_syntax_node_children(&self.children)
    }
}

trait FromSyntaxNodeChildren
where
    Self: Sized,
{
    fn from_syntax_node_children(children: &[SyntaxNode]) -> Result<Self, AstError>;
}

impl<A> FromSyntaxNodeChildren for (NodeOrError<A>,)
where
    A: FromSyntaxNode,
    Self: Sized,
{
    fn from_syntax_node_children(children: &[SyntaxNode]) -> Result<Self, AstError> {
        let [first] = children else {
            return Err(AstError);
        };

        let first = A::from_syntax_node(first);
        Ok((first,))
    }
}
impl<A, B> FromSyntaxNodeChildren for (NodeOrError<A>, NodeOrError<B>)
where
    A: FromSyntaxNode,
    B: FromSyntaxNode,
    Self: Sized,
{
    fn from_syntax_node_children(children: &[SyntaxNode]) -> Result<Self, AstError> {
        let [first, second] = children else {
            return Err(AstError);
        };

        let first = A::from_syntax_node(first);
        let second = B::from_syntax_node(second);
        Ok((first, second))
    }
}
impl<A, B, C> FromSyntaxNodeChildren for (NodeOrError<A>, NodeOrError<B>, NodeOrError<C>)
where
    A: FromSyntaxNode,
    B: FromSyntaxNode,
    C: FromSyntaxNode,
    Self: Sized,
{
    fn from_syntax_node_children(children: &[SyntaxNode]) -> Result<Self, AstError> {
        let [first, second, third] = children else {
            return Err(AstError);
        };

        let first = A::from_syntax_node(first);
        let second = B::from_syntax_node(second);
        let third = C::from_syntax_node(third);
        Ok((first, second, third))
    }
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
}
