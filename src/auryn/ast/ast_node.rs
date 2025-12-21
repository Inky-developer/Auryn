use crate::auryn::{
    syntax_id::SyntaxId,
    syntax_tree::{SyntaxNode, SyntaxNodeKind, SyntaxToken},
    tokenizer::{BinaryOperatorToken, TokenKind},
};

#[derive(Debug, Clone, Copy)]
pub struct Node<'a> {
    pub syntax_node: &'a SyntaxNode,
}

#[derive(Debug)]
pub struct AstError;

pub type AstResult<T> = Result<T, AstError>;

impl<'a> Node<'a> {
    fn nodes(&self) -> impl Iterator<Item = &'a SyntaxNode> {
        self.syntax_node
            .children
            .iter()
            .filter_map(|item| item.as_node())
    }

    fn tokens(&self) -> impl Iterator<Item = &'a SyntaxToken> {
        self.syntax_node
            .children
            .iter()
            .filter_map(|item| item.as_token())
    }

    fn get_token_by_kind(&self, kind: TokenKind) -> Option<&'a SyntaxToken> {
        self.tokens().find(|token| token.kind == kind)
    }
}

macro_rules! ast_node {
    (pub struct $name:ident = $syntax_node_kind:path as { $($rest:tt)* }) => {
        #[derive(Debug, Clone, Copy)]
        pub struct $name<'a>(#[allow(unused)] Node<'a>);

        impl<'a> $name<'a> {
            pub(super) fn new(syntax_node: &'a SyntaxNode) -> Option<Self> {
                if !matches!(syntax_node.kind, $syntax_node_kind) {
                    return None;
                }
                Some(Self(Node { syntax_node } ))
            }

            pub fn id(&self) -> SyntaxId {
                self.0.syntax_node.id
            }

            gen_ast_node_inner!{impl $($rest)*}
        }
    };

    (pub enum $name:ident = $syntax_node_kind:path as $(|)? $($syntax_node_ty:path as $node_ty:ident)|*) => {
        #[derive(Debug, Clone, Copy)]
        pub enum $name<'a> {
            $(
                $node_ty($node_ty<'a>)
            ),*
        }

        impl<'a> $name<'a> {
            pub(super) fn new(syntax_node: &'a SyntaxNode) -> Option<Self> {
                if !matches!(syntax_node.kind, $syntax_node_kind) {
                    return None;
                }
                let first_child = syntax_node.node_children().next()?;
                match first_child.kind {$(
                        $syntax_node_ty => <$node_ty>::new(first_child).map(Self::$node_ty),
                    )*
                    _ => None
                }
            }

            pub fn id(&self) -> SyntaxId {
                match self {
                    $(
                        Self::$node_ty(value) => value.id(),
                    )*
                }
            }
        }
    };
}

macro_rules! gen_ast_node_inner {
    (impl token $ident:ident: $kind:path, $($rest:tt)*) => {
        pub fn $ident(&self) -> AstResult<&SyntaxToken> {
            self.0.get_token_by_kind($kind).ok_or(AstError)
        }

        gen_ast_node_inner!{impl $($rest)*}
    };

    (impl $ident:ident: $kind:tt, $($rest:tt)*) => {
        pub fn $ident(&self) -> AstResult<$kind<'a>> {
            self.0.nodes().find_map(<$kind>::new).ok_or(AstError)
        }

        gen_ast_node_inner!{impl $($rest)*}
    };

    (impl) => {};

    (impl ...$children_name:ident: $children_type:tt ) => {
        pub fn $children_name(&self) -> impl Iterator<Item = $children_type<'a>> {
            self.0.nodes().filter_map(<$children_type>::new)
        }
    };
}

ast_node! {
    pub struct Root = SyntaxNodeKind::Root as { block: Block, }
}

ast_node! {
    pub struct Block = SyntaxNodeKind::Block as { ...statements: Statement }
}

ast_node! {
    pub enum Statement = SyntaxNodeKind::Statement as
        | SyntaxNodeKind::Assignment as Assignment
        | SyntaxNodeKind::IfStatement as IfStatement
        | SyntaxNodeKind::Loop as LoopStatement
        | SyntaxNodeKind::Break as BreakStatement
        | SyntaxNodeKind::VariableUpdate as VariableUpdate
        | SyntaxNodeKind::Expression as Expression
}

ast_node! {
    pub struct Assignment = SyntaxNodeKind::Assignment as { token ident: TokenKind::Identifier, expression: Expression, }
}

ast_node! {
    pub struct IfStatement = SyntaxNodeKind::IfStatement as { expression: Expression, block: Block, }
}

ast_node! {
    pub struct LoopStatement = SyntaxNodeKind::Loop as { block: Block, }
}

ast_node! {
    pub struct BreakStatement = SyntaxNodeKind::Break as {}
}

ast_node! {
    pub struct VariableUpdate = SyntaxNodeKind::VariableUpdate as { token ident: TokenKind::Identifier, expression: Expression, }
}

ast_node! {
    pub enum Expression = SyntaxNodeKind::Expression as
        | SyntaxNodeKind::Value as Value
        | SyntaxNodeKind::BinaryOperation as BinaryOperation
}

ast_node! {
    pub struct BinaryOperation = SyntaxNodeKind::BinaryOperation as { ...expressions: Expression }
}

// Manually implementing this since the macro cannot handle this node
impl<'a> BinaryOperation<'a> {
    pub fn binary_operator(&self) -> AstResult<BinaryOperatorToken> {
        self.0
            .tokens()
            .find_map(|it| it.kind.to_binary_operator())
            .ok_or(AstError)
    }

    pub fn lhs(&self) -> AstResult<Expression<'a>> {
        self.expressions().nth(0).ok_or(AstError)
    }

    pub fn rhs(&self) -> AstResult<Expression<'a>> {
        self.expressions().nth(1).ok_or(AstError)
    }
}

ast_node! {
    pub enum Value = SyntaxNodeKind::Value as
        | SyntaxNodeKind::Number as Number
        | SyntaxNodeKind::Ident as Ident
        | SyntaxNodeKind::FunctionCall as FunctionCall
        | SyntaxNodeKind::Parenthesis as Parenthesis
}

ast_node! {
    pub struct Number = SyntaxNodeKind::Number as { token value: TokenKind::Number, }
}

ast_node! {
    pub struct Ident = SyntaxNodeKind::Ident as { token ident: TokenKind::Identifier, }
}

ast_node! {
    pub struct FunctionCall = SyntaxNodeKind::FunctionCall as { token ident: TokenKind::Identifier, argument_list: ArgumentList, }
}

ast_node! {
    pub struct ArgumentList = SyntaxNodeKind::ArgumentList as { ...arguments: Expression }
}

ast_node! {
    pub struct Parenthesis = SyntaxNodeKind::Parenthesis as { expression: Expression, }
}
