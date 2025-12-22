use crate::{
    auryn::{air::types::Type, syntax_id::SyntaxId, tokenizer::BinaryOperatorToken},
    utils::{fast_map::FastMap, small_string::SmallString},
};

#[derive(Debug)]
pub struct Air {
    pub blocks: FastMap<AirBlockId, AirBlock>,
}

impl Air {
    pub fn root_block(&self) -> &AirBlock {
        &self.blocks[&AirBlockId::ROOT]
    }
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
    Computed(Type),
    Inferred,
}

impl AirType {
    pub fn computed(&self) -> &Type {
        match self {
            AirType::Computed(computed) => computed,
            AirType::Inferred => panic!("Expected computed type, got inferred"),
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
