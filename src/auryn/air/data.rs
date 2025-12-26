use std::str::FromStr;

use crate::{
    auryn::{
        air::types::{FunctionType, Type},
        syntax_id::SyntaxId,
        tokenizer::BinaryOperatorToken,
    },
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
    pub unresolved_type: UnresolvedType,
    pub ident: SmallString,
    pub blocks: FastMap<AirBlockId, AirBlock>,
}

impl AirFunction {
    pub fn computed_type(&self) -> &FunctionType {
        let AirType::Computed(Type::Function(function_type)) = &self.r#type else {
            unreachable!("Function type should be computed at this point");
        };
        function_type
    }

    pub fn unresolved_type(&self) -> (&[UnresolvedType], Option<&UnresolvedType>) {
        let UnresolvedType::Function {
            parameters,
            return_type,
        } = &self.unresolved_type
        else {
            unreachable!("Should be a function type");
        };
        (parameters, return_type.as_deref())
    }

    /// Returns value ids for the arguments.
    /// The value ids increment for each argument.
    pub fn argument_ids(&self) -> impl Iterator<Item = AirValueId> {
        self.unresolved_type()
            .0
            .iter()
            .enumerate()
            .map(|(index, _)| AirValueId(index))
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

/// Represents a type that was written by the user but not resolved yet.
#[derive(Debug, Clone)]
pub enum UnresolvedType {
    Ident(SyntaxId, SmallString),
    Array(SyntaxId, Box<UnresolvedType>),
    Function {
        parameters: Vec<UnresolvedType>,
        return_type: Option<Box<UnresolvedType>>,
    },
}

#[derive(Debug, Clone)]
pub enum AirType {
    Inferred,
    Unresolved(UnresolvedType),
    Computed(Type),
}

impl AirType {
    pub fn computed(&self) -> &Type {
        match self {
            AirType::Computed(inner) => inner,
            _ => unreachable!("Type should be computed at this point"),
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
    Call(Call),
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
pub struct Call {
    pub function: AirFunctionId,
    pub arguments: Vec<AirExpression>,
}

#[derive(Debug)]
pub struct IntrinsicCall {
    pub intrinsic: Intrinsic,
    pub arguments: Vec<AirExpression>,
}

#[derive(Debug)]
pub enum Intrinsic {
    Print,
    ArrayOf,
    ArrayOfZeros,
    ArrayGet,
    ArraySet,
    ArrayLen,
}

impl FromStr for Intrinsic {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "print" => Intrinsic::Print,
            "arrayOf" => Intrinsic::ArrayOf,
            "arrayOfZeros" => Intrinsic::ArrayOfZeros,
            "arrayGet" => Intrinsic::ArrayGet,
            "arraySet" => Intrinsic::ArraySet,
            "arrayLen" => Intrinsic::ArrayLen,
            _ => return Err(()),
        })
    }
}

#[derive(Debug)]
pub enum AirBlockFinalizer {
    Return(ReturnValue),
    Goto(AirBlockId),
    Branch {
        value: Box<AirExpression>,
        pos_block: AirBlockId,
        neg_block: AirBlockId,
    },
}

#[derive(Debug)]
pub enum ReturnValue {
    Expression(Box<AirExpression>),
    Null(SyntaxId),
}

impl ReturnValue {
    pub fn expression(&self) -> Option<&AirExpression> {
        match self {
            ReturnValue::Expression(expression) => Some(expression),
            _ => None,
        }
    }
}
