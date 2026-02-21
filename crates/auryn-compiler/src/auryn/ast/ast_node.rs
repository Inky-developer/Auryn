use crate::auryn::{
    syntax_id::SyntaxId,
    syntax_tree::{SyntaxNode, SyntaxNodeKind, SyntaxToken},
    tokenizer::{BinaryOperatorToken, TokenKind, UpdateOperatorToken},
};

#[derive(Debug, Clone, Copy)]
pub struct Node<'a> {
    pub syntax_node: &'a SyntaxNode,
}

#[derive(Debug)]
pub struct AstError;

pub type AstResult<T> = Result<T, AstError>;

impl<'a> Node<'a> {
    fn nodes(self) -> impl Iterator<Item = &'a SyntaxNode> {
        self.syntax_node
            .children
            .iter()
            .filter_map(|item| item.as_node())
    }

    fn tokens(self) -> impl Iterator<Item = &'a SyntaxToken> {
        self.syntax_node
            .children
            .iter()
            .filter_map(|item| item.as_token())
    }

    fn get_token_by_kind(self, kind: TokenKind) -> Option<&'a SyntaxToken> {
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

            #[allow(dead_code)]
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

            #[allow(dead_code)]
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
        pub fn $ident(self) -> AstResult<&'a SyntaxToken> {
            self.0.get_token_by_kind($kind).ok_or(AstError)
        }

        gen_ast_node_inner!{impl $($rest)*}
    };

    (impl $ident:ident: $kind:tt, $($rest:tt)*) => {
        pub fn $ident(self) -> AstResult<$kind<'a>> {
            self.0.nodes().find_map(<$kind>::new).ok_or(AstError)
        }

        gen_ast_node_inner!{impl $($rest)*}
    };

    (impl) => {};

    (impl ...$children_name:ident: $children_type:tt ) => {
        pub fn $children_name(self) -> impl Iterator<Item = $children_type<'a>> {
            self.0.nodes().filter_map(<$children_type>::new)
        }
    };
}

ast_node! {
    pub struct Root = SyntaxNodeKind::Root as { file: File, }
}

ast_node! {
    pub struct File = SyntaxNodeKind::File as { ...items: Item }
}

ast_node! {
    pub enum Item = SyntaxNodeKind::Item as
        | SyntaxNodeKind::ExternBlock as ExternBlock
        | SyntaxNodeKind::FunctionDefinition as FunctionDefinition
        | SyntaxNodeKind::TypeAlias as TypeAlias
        | SyntaxNodeKind::Struct as Struct
}

ast_node! {
    pub struct ExternBlock = SyntaxNodeKind::ExternBlock as { token extern_target: TokenKind::StringLiteral, ...items: ExternBlockItem }
}

ast_node! {
    pub struct ExternBlockItem = SyntaxNodeKind::ExternBlockItem as { metadata: ExternBlockItemMetadata, kind: ExternBlockItemKind, }
}

ast_node! {
    pub struct ExternBlockItemMetadata = SyntaxNodeKind::ItemMetadata as { token value: TokenKind::StringLiteral, }
}

ast_node! {
    pub enum ExternBlockItemKind = SyntaxNodeKind::ExternBlockItemKind as
        | SyntaxNodeKind::ExternType as ExternType
}

ast_node! {
    pub struct ExternType = SyntaxNodeKind::ExternType as { token ident: TokenKind::Identifier, body: ExternTypeBody, }
}

ast_node! {
    pub struct ExternTypeBody = SyntaxNodeKind::ExternTypeBody as { ...items: ExternTypeBodyItem }
}

ast_node! {
    pub struct ExternTypeBodyItem = SyntaxNodeKind::ExternTypeBodyItem as { metadata: ExternBlockItemMetadata, kind: ExternTypeBodyItemKind, }
}

ast_node! {
    pub enum ExternTypeBodyItemKind = SyntaxNodeKind::ExternTypeBodyItemKind as
        | SyntaxNodeKind::ExternTypeStaticLet as ExternTypeStaticLet
        | SyntaxNodeKind::ExternTypeFunction as ExternTypeFunction
}

ast_node! {
    pub struct ExternTypeStaticLet = SyntaxNodeKind::ExternTypeStaticLet as { token ident: TokenKind::Identifier, r#type: Type, }
}

ast_node! {
    pub struct ExternTypeFunction = SyntaxNodeKind::ExternTypeFunction as { token ident: TokenKind::Identifier, parameters: ParameterList, return_type: ReturnType, }
}

impl ExternTypeFunction<'_> {
    pub fn is_static(&self) -> bool {
        self.0
            .tokens()
            .any(|it| it.kind == TokenKind::KeywordStatic)
    }
}

ast_node! {
    pub struct FunctionDefinition = SyntaxNodeKind::FunctionDefinition as {
        token ident: TokenKind::Identifier,
        maybe_generic_parameter_list: GenericParameterList,
        parameter_list: ParameterList,
        return_type: ReturnType,
        block: Block,
    }
}

ast_node! {
    pub struct GenericParameterList = SyntaxNodeKind::GenericParameterList as { ...parameters: GenericParameter }
}

ast_node! {
    pub struct GenericParameter = SyntaxNodeKind::GenericParameterDefinition as { token ident: TokenKind::Identifier, }
}

ast_node! {
    pub struct ParameterList = SyntaxNodeKind::ParameterList as { ...parameters: Parameter }
}

ast_node! {
    pub struct Parameter = SyntaxNodeKind::ParameterDefinition as { token ident: TokenKind::Identifier, r#type: Type, }
}

ast_node! {
    pub struct ReturnType = SyntaxNodeKind::ReturnType as { r#type: Type, }
}

ast_node! {
    pub struct TypeAlias = SyntaxNodeKind::TypeAlias as { token ident: TokenKind::Identifier, r#type: Type, }
}

ast_node! {
    pub struct Struct = SyntaxNodeKind::Struct as { token ident: TokenKind::Identifier, maybe_generics: GenericParameterList, body: StructBody, }
}

ast_node! {
    pub struct StructBody = SyntaxNodeKind::StructBody as { ...fields: StructuralTypeField }
}

ast_node! {
    pub enum Type = SyntaxNodeKind::Type as
        | SyntaxNodeKind::StructuralType as StructuralType
        | SyntaxNodeKind::ArrayType as ArrayType
        | SyntaxNodeKind::UnitType as UnitType
        | SyntaxNodeKind::TypeRef as TypeRef
}

ast_node! {
    pub struct ArrayType = SyntaxNodeKind::ArrayType as { r#type: Type, }
}

ast_node! {
    pub struct StructuralType = SyntaxNodeKind::StructuralType as { ...fields: StructuralTypeField }
}

ast_node! {
    pub struct StructuralTypeField = SyntaxNodeKind::StructuralTypeField as { token ident: TokenKind::Identifier, r#type: Type, }
}

ast_node! {
    pub struct UnitType = SyntaxNodeKind::UnitType as {}
}

ast_node! {
    pub struct TypeRef = SyntaxNodeKind::TypeRef as { token ident: TokenKind::Identifier, maybe_generic_args: TypeArguments, }
}

ast_node! {
    pub struct TypeArguments = SyntaxNodeKind::TypeArguments as { ...types: Type }
}

ast_node! {
    pub struct Block = SyntaxNodeKind::Block as { ...statements: Statement }
}

ast_node! {
    pub enum Statement = SyntaxNodeKind::Statement as
        | SyntaxNodeKind::Assignment as Assignment
        | SyntaxNodeKind::IfStatement as IfStatement
        | SyntaxNodeKind::Loop as LoopStatement
        | SyntaxNodeKind::WhileLoop as WhileStatement
        | SyntaxNodeKind::Break as BreakStatement
        | SyntaxNodeKind::Continue as ContinueStatement
        | SyntaxNodeKind::Return as ReturnStatement
        | SyntaxNodeKind::VariableUpdate as VariableUpdate
        | SyntaxNodeKind::Expression as Expression
}

ast_node! {
    pub struct Assignment = SyntaxNodeKind::Assignment as { token ident: TokenKind::Identifier, r#type: Type, expression: Expression, }
}

ast_node! {
    pub struct IfStatement = SyntaxNodeKind::IfStatement as { expression: Expression, block: Block, r#else: IfStatementElse, }
}

ast_node! {
    pub enum IfStatementElse = SyntaxNodeKind::IfStatementElse as
        | SyntaxNodeKind::Statement as Statement
        | SyntaxNodeKind::Block as Block
}

ast_node! {
    pub struct LoopStatement = SyntaxNodeKind::Loop as { block: Block, }
}

ast_node! {
    pub struct WhileStatement = SyntaxNodeKind::WhileLoop as { expression: Expression, block: Block, }
}

ast_node! {
    pub struct BreakStatement = SyntaxNodeKind::Break as {}
}

ast_node! {
    pub struct ContinueStatement = SyntaxNodeKind::Continue as {}
}

ast_node! {
    pub struct ReturnStatement = SyntaxNodeKind::Return as { expression: Expression, }
}

ast_node! {
    pub struct VariableUpdate = SyntaxNodeKind::VariableUpdate as { path: Path, expression: Expression, }
}

impl VariableUpdate<'_> {
    pub fn assignment_token(self) -> AstResult<UpdateOperatorToken> {
        self.0
            .tokens()
            .find_map(|it| it.kind.to_assignment_operator())
            .ok_or(AstError)
    }
}

ast_node! {
    pub struct Path = SyntaxNodeKind::Path as { expression: Expression, }
}

ast_node! {
    pub enum Expression = SyntaxNodeKind::Expression as
        | SyntaxNodeKind::Value as Value
        | SyntaxNodeKind::PostfixOperation as PostfixOperation
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

    pub fn lhs(&'a self) -> AstResult<Expression<'a>> {
        self.expressions().nth(0).ok_or(AstError)
    }

    pub fn rhs(&'a self) -> AstResult<Expression<'a>> {
        self.expressions().nth(1).ok_or(AstError)
    }
}

ast_node! {
    pub struct PostfixOperation = SyntaxNodeKind::PostfixOperation as { value: ValueOrPostfix, operator: PostfixOperator, }
}

#[derive(Debug, Clone, Copy)]
pub enum ValueOrPostfix<'a> {
    Value(Value<'a>),
    Postfix(PostfixOperation<'a>),
}

impl<'a> ValueOrPostfix<'a> {
    pub fn new(node: &'a SyntaxNode) -> Option<Self> {
        match node.kind {
            SyntaxNodeKind::Value => Value::new(node).map(Self::Value),
            SyntaxNodeKind::PostfixOperation => PostfixOperation::new(node).map(Self::Postfix),
            _ => None,
        }
    }

    #[expect(dead_code)]
    pub fn id(&self) -> SyntaxId {
        match self {
            ValueOrPostfix::Value(value) => value.id(),
            ValueOrPostfix::Postfix(postfix_operation) => postfix_operation.id(),
        }
    }
}

ast_node! {
    pub enum PostfixOperator = SyntaxNodeKind::PostfixOperator as
        | SyntaxNodeKind::ArgumentList as ArgumentList
        | SyntaxNodeKind::Accessor as Accessor
}

ast_node! {
    pub struct ArgumentList = SyntaxNodeKind::ArgumentList as { ...arguments: Expression }
}

ast_node! {
    pub struct Accessor = SyntaxNodeKind::Accessor as { token ident: TokenKind::Identifier, }
}

ast_node! {
    pub enum Value = SyntaxNodeKind::Value as
        | SyntaxNodeKind::NumberLiteral as NumberLiteral
        | SyntaxNodeKind::StringLiteral as StringLiteral
        | SyntaxNodeKind::PrefixNot as PrefixNot
        | SyntaxNodeKind::BooleanLiteral as BooleanLiteral
        | SyntaxNodeKind::Ident as Ident
        | SyntaxNodeKind::Parenthesis as Parenthesis
        | SyntaxNodeKind::StructLiteral as StructLiteral
}

ast_node! {
    pub struct NumberLiteral = SyntaxNodeKind::NumberLiteral as { token value: TokenKind::NumberLiteral, }
}

ast_node! {
    pub struct StringLiteral = SyntaxNodeKind::StringLiteral as { token value: TokenKind::StringLiteral, }
}

ast_node! {
    pub struct PrefixNot = SyntaxNodeKind::PrefixNot as { value: ValueOrPostfix, }
}

ast_node! {
    pub struct BooleanLiteral = SyntaxNodeKind::BooleanLiteral as { }
}

impl BooleanLiteral<'_> {
    pub fn value(self) -> bool {
        self.0.tokens().any(|it| it.kind == TokenKind::KeywordTrue)
    }
}

ast_node! {
    pub struct Ident = SyntaxNodeKind::Ident as { token ident: TokenKind::Identifier, }
}

ast_node! {
    pub struct Parenthesis = SyntaxNodeKind::Parenthesis as { expression: Expression, }
}

ast_node! {
    pub struct StructLiteral = SyntaxNodeKind::StructLiteral as { ...fields: StructLiteralField }
}

impl<'a> StructLiteral<'a> {
    pub fn ident(self) -> Option<&'a SyntaxToken> {
        self.0.tokens().find(|it| it.kind == TokenKind::Identifier)
    }
}

ast_node! {
    pub struct StructLiteralField = SyntaxNodeKind::StructLiteralField as { token ident: TokenKind::Identifier, value: Expression, }
}
