use crate::{
    auryn::{air::types::Type, syntax_id::SyntaxId, tokenizer::BinaryOperatorToken},
    utils::{fast_map::FastMap, small_string::SmallString},
};

#[derive(Debug)]
pub struct Air {
    pub functions: FastMap<AirFunctionId, AirFunction>,
}

impl Air {
    pub fn main_function(&self) -> (AirFunctionId, &AirFunction) {
        let mut main_functions = self
            .functions
            .iter()
            .filter(|(_, it)| it.ident.as_ref() == "main");
        let (id, function) = main_functions
            .next()
            .expect("There should be a main function");
        assert!(main_functions.next().is_none());
        (*id, function)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct AirFunctionId(pub SyntaxId);

#[derive(Debug)]
pub struct AirFunction {
    pub r#type: AirType,
    pub declared_parameters: Vec<SmallString>,
    pub ident: SmallString,
    pub blocks: FastMap<AirBlockId, AirBlock>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct AirBlockId(pub(super) usize);

impl AirBlockId {
    pub const ROOT: Self = Self(0);
}

#[derive(Debug)]
pub struct AirBlock {
    pub nodes: Vec<AirNode>,
    pub finalizer: AirBlockFinalizer,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct AirValueId(pub(super) usize);

#[derive(Debug)]
pub struct AirNode {
    pub id: SyntaxId,
    pub kind: AirNodeKind,
}

#[derive(Debug)]
pub enum AirNodeKind {
    Assignment(Assignment),
    Expression(Box<AirExpression>),
}

#[derive(Debug)]
pub struct Assignment {
    pub target: AirValueId,
    pub expression: Box<AirExpression>,
}

#[derive(Debug, Clone)]
pub enum AirType {
    /// Represent a type that has been resolved
    Computed(Type),
    /// Represents a type that was explicitly specified in the source ode
    Explicit(SmallString),
    /// Represents a type was left implicit in the code
    Inferred,
}

impl AirType {
    pub fn computed(&self) -> &Type {
        match self {
            AirType::Computed(computed) => computed,
            other => panic!("Expected computed type, got {other:?}"),
        }
    }
}

#[must_use]
#[derive(Debug)]
pub struct AirExpression {
    pub id: SyntaxId,
    pub r#type: AirType,
    pub kind: AirExpressionKind,
}

impl AirExpression {
    pub const fn new(id: SyntaxId, kind: AirExpressionKind) -> Self {
        Self {
            id,
            kind,
            r#type: AirType::Inferred,
        }
    }

    pub const fn error(id: SyntaxId) -> Self {
        Self::new(id, AirExpressionKind::Error)
    }
}

#[derive(Debug)]
pub enum AirExpressionKind {
    Constant(AirConstant),
    BinaryOperator(BinaryOperation),
    Variable(AirValueId),
    IntrinsicCall(IntrinsicCall),
    Error,
}

#[derive(Debug)]
pub struct BinaryOperation {
    pub lhs: Box<AirExpression>,
    pub rhs: Box<AirExpression>,
    pub operator: BinaryOperatorToken,
}

#[derive(Debug)]
pub struct LoadConstant {
    pub constant: AirConstant,
}

#[derive(Debug)]
pub enum AirConstant {
    Number(i32),
    String(SmallString),
}

#[derive(Debug)]
pub struct IntrinsicCall {
    pub intrinsic: Intrinsic,
    pub arguments: Vec<AirExpression>,
}

#[derive(Debug)]
pub enum Intrinsic {
    Print,
}

impl Intrinsic {
    pub fn signature(&self) -> (&'static [Type], Type) {
        match self {
            Intrinsic::Print => (&[Type::Top], Type::Null),
        }
    }
}

#[derive(Debug)]
pub enum AirBlockFinalizer {
    Return,
    Goto(AirBlockId),
    Branch {
        value: Box<AirExpression>,
        pos_block: AirBlockId,
        neg_block: AirBlockId,
    },
}
