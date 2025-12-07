use crate::auryn::{
    Span,
    syntax_tree::{SyntaxItem, SyntaxNode, SyntaxNodeKind},
    tokenizer::BinaryOperatorToken,
};

#[derive(Debug, Clone)]
pub enum AstError {
    UnexpectedNode {
        expected: &'static str,
        got: SyntaxNodeKind,
    },
    ExpectedBinaryOperator,
    TooFewChildren,
    TooManyChildren {
        context: &'static str,
    },
}

#[derive(Debug)]
pub struct Node<T> {
    pub span: Span,
    pub kind: T,
}

pub type NodeRef<T> = Box<NodeOrError<T>>;

pub type NodeOrError<T> = Result<Node<T>, AstError>;

pub trait FromSyntaxNode
where
    Self: Sized,
{
    fn from_syntax_node(node: &SyntaxNode) -> Self;
}

impl FromSyntaxNode for Result<Node<BinaryOperatorToken>, AstError> {
    fn from_syntax_node(node: &SyntaxNode) -> Self {
        let SyntaxNodeKind::BinaryOperator(op) = node.kind else {
            return Err(AstError::ExpectedBinaryOperator);
        };
        Ok(Node {
            kind: op,
            span: node.span,
        })
    }
}

impl<T> FromSyntaxNode for Result<Box<T>, AstError>
where
    T: FromSyntaxNode,
{
    fn from_syntax_node(node: &SyntaxNode) -> Self {
        let value = T::from_syntax_node(node);
        Ok(Box::new(value))
    }
}

impl<T> FromSyntaxNode for Result<Vec<T>, AstError>
where
    T: FromSyntaxNode,
{
    fn from_syntax_node(node: &SyntaxNode) -> Self {
        let items = node.node_children().map(T::from_syntax_node).collect();
        Ok(items)
    }
}

pub trait ToAstNode {
    type Target;

    fn to_ast_node(&self) -> Result<&Self::Target, AstError>;
}

impl<T> ToAstNode for NodeRef<T>
where
    T: ToAstNode,
{
    type Target = T::Target;

    fn to_ast_node(&self) -> Result<&Self::Target, AstError> {
        self.as_ref()
            .as_ref()
            .map_err(Clone::clone)?
            .kind
            .to_ast_node()
    }
}

impl<T> ToAstNode for Vec<NodeOrError<T>> {
    type Target = Vec<NodeOrError<T>>;

    fn to_ast_node(&self) -> Result<&Self::Target, AstError> {
        Ok(self)
    }
}

impl ToAstNode for BinaryOperatorToken {
    type Target = BinaryOperatorToken;

    fn to_ast_node(&self) -> Result<&Self::Target, AstError> {
        Ok(self)
    }
}

macro_rules! create_ast_parser {
    (pub struct $ident:ident @ $source_type:path{$($source_var:ident: $syntax_type:ty),*} {$(pub $var_name:ident: $typ:ty),* $(,)?} $($rest:tt)*) => {
        #[derive(Debug)]
        pub struct $ident {
            $(pub $source_var: $syntax_type,)*
            $(pub $var_name: $typ,)*
        }

        impl ToAstNode for $ident {
            type Target = $ident;

            fn to_ast_node(&self) -> Result<&Self::Target, AstError> {
                Ok(self)
            }
        }

        impl $ident {
            $(
                pub fn $var_name(&self) -> Result<&<$typ as ToAstNode>::Target, AstError>  where $typ: ToAstNode{
                    self.$var_name.to_ast_node()
                }
            )*
        }

        impl FromSyntaxNode for NodeOrError<$ident> {
            #[allow(unused_parens, unused_mut, unused_variables)]
            fn from_syntax_node(node: &SyntaxNode) -> Self {
                let ($($source_var),*) = match &node.kind {
                    $source_type{$($source_var),*} => ($($source_var.clone()),*),
                    other => return Err(AstError::UnexpectedNode { expected: stringify!($source_type), got: other.clone() }),
                };

                let mut children = node.children.iter().filter_map(SyntaxItem::as_node);
                $(
                    let $var_name = <Result<$typ, AstError>>::from_syntax_node(children.next().ok_or(AstError::TooFewChildren)?)?;
                )*

                let kind = $ident {
                    $($source_var,)*
                    $($var_name,)*
                };
                Ok(Node { kind, span: node.span })
            }
        }

        create_ast_parser! { $($rest)* }
    };

    (pub enum $ident:ident @ $source_type:path {$($var_name:ident($typ:ty: $pat:pat)),* $(,)?} $($rest:tt)*) => {
        #[derive(Debug)]
        pub enum $ident {
            $($var_name($typ)),*
        }

        impl ToAstNode for $ident {
            type Target = $ident;

            fn to_ast_node(&self) -> Result<&Self::Target, AstError> {
                Ok(self)
            }
        }

        impl FromSyntaxNode for NodeOrError<$ident> {
            #[allow(non_snake_case)]
            fn from_syntax_node(node: &SyntaxNode) -> Self {
                if node.kind != $source_type {
                    return Err(AstError::UnexpectedNode { expected: stringify!($source_type), got: node.kind.clone() });
                }

                let mut children = node.children.iter().filter_map(SyntaxItem::as_node);
                let first_child = children.next().ok_or(AstError::TooFewChildren)?;
                if children.next().is_some() {
                    return Err(AstError::TooManyChildren { context: stringify!($ident) });
                }

                let variant = match &first_child.kind {
                    $(
                        $pat => $ident::$var_name(<Result<$typ, AstError>>::from_syntax_node(first_child)?),
                    )*
                    other => return Err(AstError::UnexpectedNode {expected: concat!("One of ", $(stringify!($pat), ", "),*), got: other.clone()})
                };

                Ok(Node { kind: variant, span: node.span })
            }
        }

        create_ast_parser! { $($rest)* }
    };

    () => {};
}

create_ast_parser! {
    pub struct Root @ SyntaxNodeKind::Root{} {
        pub block: NodeRef<Block>,
    }

    pub struct Block @ SyntaxNodeKind::Block{} {
        pub statements: Vec<NodeOrError<Statement>>,
    }

    pub enum Statement @ SyntaxNodeKind::Statement {
        Assignement(NodeRef<Assignment>: SyntaxNodeKind::Assignment {..}),
        Expression(NodeRef<Expression>: SyntaxNodeKind::Expression),
    }

    pub struct Assignment @ SyntaxNodeKind::Assignment { ident: String } {
        pub expression: NodeRef<Expression>
    }

    pub enum Expression @ SyntaxNodeKind::Expression {
        Value(NodeRef<Value>: SyntaxNodeKind::Value),
        BinaryOperation(NodeRef<BinaryOperation>: SyntaxNodeKind::BinaryOperation),
    }

    pub struct BinaryOperation @ SyntaxNodeKind::BinaryOperation{} {
        pub lhs: NodeRef<Expression>,
        pub operator: NodeRef<BinaryOperatorToken>,
        pub rhs: NodeRef<Expression>,
    }

    pub enum Value @ SyntaxNodeKind::Value {
        Number(NodeRef<Number>: SyntaxNodeKind::Number {..}),
        Ident(NodeRef<Ident>: SyntaxNodeKind::Ident {..} ),
        FunctionCall(NodeRef<FunctionCall>: SyntaxNodeKind::FunctionCall {..}),
        Parenthesis(NodeRef<Parenthesis>: SyntaxNodeKind::Parenthesis),
    }

    pub struct Number @ SyntaxNodeKind::Number { value: i32 } {}

    pub struct Ident @ SyntaxNodeKind::Ident { ident: String } {}

    pub struct FunctionCall @ SyntaxNodeKind::FunctionCall { ident: String } {
        pub arguments: Vec<NodeOrError<Expression>>
    }

    pub struct Parenthesis @ SyntaxNodeKind::Parenthesis{} {
        pub expression: NodeRef<Expression>,
    }
}
