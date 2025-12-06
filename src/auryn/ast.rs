use crate::auryn::{
    Span,
    syntax_tree::{SyntaxItem, SyntaxNode, SyntaxNodeKind, SyntaxTree},
    tokenizer::BinaryOperatorToken,
};

#[derive(Debug, Clone, Copy)]
pub enum AstError {
    TooFewChildren,
    UnexpectedNode,
    TooManyChildren,
    InvalidChildren,
}

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
            span: node.span,
        })
    }
}

#[derive(Debug)]
pub struct Node<T> {
    pub span: Span,
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
            return Err(AstError::UnexpectedNode);
        }

        let num_children = node.node_children().count();

        match num_children {
            1 => {
                let (value,) = node.map_child_nodes()?;
                Ok(Expression::Value(NodeRef::new(value)))
            }
            3 => {
                let (lhs, op, rhs) = node.map_child_nodes()?;
                Ok(Expression::BinaryOperation {
                    lhs: NodeRef::new(lhs),
                    operator: NodeRef::new(op),
                    rhs: NodeRef::new(rhs),
                })
            }
            _ => Err(AstError::UnexpectedNode),
        }
    }
}

#[derive(Debug)]
pub enum Value {
    Int(i32),
    FunctionCall(NodeRef<FunctionCall>),
    Expression(NodeRef<Expression>),
}

impl FromSyntaxNode for Value {
    fn get_kind(node: &SyntaxNode) -> Result<Self, AstError> {
        match &node.kind {
            SyntaxNodeKind::Number(num) => Ok(Value::Int(*num)),
            SyntaxNodeKind::Parenthesis => {
                let (expression,) = node.map_child_nodes()?;
                Ok(Value::Expression(NodeRef::new(expression)))
            }
            SyntaxNodeKind::FunctionCall { .. } => Ok(Value::FunctionCall(NodeRef::new(
                FunctionCall::from_syntax_node(node),
            ))),
            _ => Err(AstError::UnexpectedNode),
        }
    }
}

#[derive(Debug)]
pub struct FunctionCall {
    pub ident: String,
    pub arguments: Vec<NodeOrError<Expression>>,
}

impl FromSyntaxNode for FunctionCall {
    fn get_kind(node: &SyntaxNode) -> Result<Self, AstError> {
        let SyntaxNodeKind::FunctionCall(ident) = &node.kind else {
            return Err(AstError::UnexpectedNode);
        };
        let ident = ident.clone();

        let arguments = node
            .node_children()
            .map(|child| Expression::from_syntax_node(child))
            .collect();
        Ok(FunctionCall { ident, arguments })
    }
}

impl FromSyntaxNode for BinaryOperatorToken {
    fn get_kind(node: &SyntaxNode) -> Result<Self, AstError> {
        let SyntaxNodeKind::BinaryOperator(op) = node.kind else {
            return Err(AstError::UnexpectedNode);
        };
        Ok(op)
    }
}

pub fn query_ast(syntax_tree: &SyntaxTree) -> NodeOrError<Root> {
    let &SyntaxTree { root_node } = &syntax_tree;

    let span = root_node.span;

    let (expression,) = root_node.map_child_nodes()?;
    NodeOrError::Ok(Node {
        kind: Root {
            expression: NodeRef::new(expression),
        },
        span,
    })
}

impl SyntaxNode {
    fn map_child_nodes<T: FromSyntaxNodes>(&self) -> Result<T, AstError> {
        T::from_syntax_nodes(self.children.iter().filter_map(SyntaxItem::as_node))
    }
}

trait FromSyntaxNodes
where
    Self: Sized,
{
    fn from_syntax_nodes<'a>(
        children: impl Iterator<Item = &'a SyntaxNode>,
    ) -> Result<Self, AstError>;
}

impl<A> FromSyntaxNodes for (NodeOrError<A>,)
where
    A: FromSyntaxNode,
    Self: Sized,
{
    fn from_syntax_nodes<'a>(
        mut children: impl Iterator<Item = &'a SyntaxNode>,
    ) -> Result<Self, AstError> {
        let first = children.next().ok_or(AstError::TooFewChildren)?;
        let first = A::from_syntax_node(first);
        if !children.next().is_none() {
            return Err(AstError::TooManyChildren);
        }
        Ok((first,))
    }
}
impl<A, B> FromSyntaxNodes for (NodeOrError<A>, NodeOrError<B>)
where
    A: FromSyntaxNode,
    B: FromSyntaxNode,
    Self: Sized,
{
    fn from_syntax_nodes<'a>(
        mut children: impl Iterator<Item = &'a SyntaxNode>,
    ) -> Result<Self, AstError> {
        let first = A::from_syntax_node(children.next().ok_or(AstError::TooFewChildren)?);
        let second = B::from_syntax_node(children.next().ok_or(AstError::TooFewChildren)?);
        if !children.next().is_none() {
            return Err(AstError::TooManyChildren);
        }
        Ok((first, second))
    }
}
impl<A, B, C> FromSyntaxNodes for (NodeOrError<A>, NodeOrError<B>, NodeOrError<C>)
where
    A: FromSyntaxNode,
    B: FromSyntaxNode,
    C: FromSyntaxNode,
    Self: Sized,
{
    fn from_syntax_nodes<'a>(
        mut children: impl Iterator<Item = &'a SyntaxNode>,
    ) -> Result<Self, AstError> {
        let first = A::from_syntax_node(children.next().ok_or(AstError::TooFewChildren)?);
        let second = B::from_syntax_node(children.next().ok_or(AstError::TooFewChildren)?);
        let third = C::from_syntax_node(children.next().ok_or(AstError::TooFewChildren)?);
        if !children.next().is_none() {
            return Err(AstError::TooManyChildren);
        }
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

    #[test]
    fn test_function_call() {
        insta::assert_debug_snapshot!(parse("print(1)"));
    }
}
