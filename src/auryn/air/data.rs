use std::{fmt::Debug, num::NonZeroU64, str::FromStr};

use crate::{
    auryn::{
        air::{
            namespace::UserDefinedTypeId,
            typecheck::{
                type_context::{TypeContext, TypeId},
                types::{FunctionItemType, FunctionParameters, Type, TypeView, TypeViewKind},
            },
        },
        syntax_id::SyntaxId,
        tokenizer::BinaryOperatorToken,
    },
    utils::{fast_map::FastMap, small_string::SmallString},
};

#[derive(Default)]
pub struct Air {
    pub functions: FastMap<AirFunctionId, AirFunction>,
    pub types: FastMap<UserDefinedTypeId, AirType>,
    pub ty_ctx: TypeContext,
    pub statics: FastMap<AirStaticValueId, AirStaticValue>,
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

impl Debug for Air {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Air")
            .field("functions", &self.functions)
            .finish_non_exhaustive()
    }
}

#[derive(Debug, Clone)]
pub enum AirStaticValue {
    Function(AirFunctionId),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct AirStaticValueId(pub SyntaxId);

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct AirFunctionId(pub AirStaticValueId);

#[derive(Debug)]
pub struct AirFunction {
    pub r#type: AirType,
    pub unresolved_type: UnresolvedType,
    pub ident: SmallString,
    pub blocks: FastMap<AirBlockId, AirBlock>,
}

impl AirFunction {
    pub fn computed_type(&self) -> TypeId<FunctionItemType> {
        let AirType::Computed(Type::FunctionItem(function_type)) = self.r#type else {
            unreachable!("Function type should be computed at this point");
        };
        function_type
    }

    pub fn unresolved_type(
        &self,
    ) -> (
        SyntaxId,
        &[UnresolvedType],
        Option<&UnresolvedType>,
        &FunctionReference,
    ) {
        let UnresolvedType::Function {
            parameters_reference,
            parameters,
            return_type,
            reference,
        } = &self.unresolved_type
        else {
            unreachable!("Should be a function type");
        };
        (
            *parameters_reference,
            parameters,
            return_type.as_deref(),
            reference,
        )
    }

    /// Returns value ids for the arguments.
    /// The value ids increment for each argument.
    pub fn argument_ids(&self) -> impl Iterator<Item = AirLocalValueId> {
        self.unresolved_type()
            .1
            .iter()
            .enumerate()
            .map(|(index, _)| AirLocalValueId(index))
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
pub enum AirValueId {
    Local(AirLocalValueId),
    Global(AirStaticValueId),
    Intrinsic(Intrinsic),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct AirLocalValueId(pub(super) usize);

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
    pub target: AirLocalValueId,
    pub expected_type: Option<UnresolvedType>,
    pub expression: Box<AirExpression>,
}

#[derive(Debug, Clone)]
pub enum UnresolvedExternMember {
    StaticLet {
        r#type: UnresolvedType,
        extern_name: SmallString,
    },
    Function {
        unresolved_type: UnresolvedType,
        ident: SmallString,
        extern_name: SmallString,
    },
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum FunctionReference {
    Intrinsic(Intrinsic),
    UserDefined(AirFunctionId),
    Extern {
        parent: Type,
        kind: ExternFunctionKind,
        extern_name: SmallString,
        syntax_id: SyntaxId,
    },
}

impl FunctionReference {
    pub fn syntax_id(&self) -> SyntaxId {
        match self {
            FunctionReference::UserDefined(air_function_id) => air_function_id.0.0,
            FunctionReference::Extern { syntax_id, .. } => *syntax_id,
            other => panic!("{other:?} has no syntax id"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExternFunctionKind {
    Static,
    Method,
}

/// Represents a type that was written by the user but not resolved yet.
#[derive(Debug, Clone)]
pub enum UnresolvedType {
    /// A type that was defined by the user, identified by its `syntax_id`
    DefinedType(UserDefinedTypeId),
    /// A type not defined by the user (so probably built-in, like `String`)
    Ident(SyntaxId, SmallString),
    Array(SyntaxId, Box<UnresolvedType>),
    Unit,
    /// A function type
    Function {
        parameters_reference: SyntaxId,
        parameters: Vec<UnresolvedType>,
        return_type: Option<Box<UnresolvedType>>,
        reference: FunctionReference,
    },
    Extern {
        id: SyntaxId,
        extern_name: SmallString,
        members: FastMap<SmallString, UnresolvedExternMember>,
    },
}

#[derive(Debug, Clone)]
pub enum AirType {
    Inferred,
    Unresolved(UnresolvedType),
    Computed(Type),
}

impl AirType {
    pub fn as_view<'a>(&self, ty_ctx: &'a TypeContext) -> TypeView<'a> {
        self.computed().as_view(ty_ctx)
    }

    pub fn computed(&self) -> Type {
        match self {
            AirType::Computed(inner) => *inner,
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
    /// Loads a type as a value
    Type(Type),
    Accessor(Accessor),
    Call(Call),
    Error,
}

#[derive(Debug)]
pub struct BinaryOperation {
    pub lhs: Box<AirExpression>,
    pub rhs: Box<AirExpression>,
    pub operator: BinaryOperatorToken,
}

#[derive(Debug)]
pub struct Accessor {
    pub value: Box<AirExpression>,
    pub ident: SmallString,
    pub ident_id: SyntaxId,
}

#[derive(Debug)]
pub struct LoadConstant {
    pub constant: AirConstant,
}

#[derive(Debug)]
pub enum AirConstant {
    Number(i128),
    Boolean(bool),
    String(SmallString),
}

#[derive(Debug)]
pub struct Call {
    pub function: Box<AirExpression>,
    pub arguments: Vec<AirExpression>,
}

impl Call {
    /// Should be called from codegen
    pub fn function_type<'a>(&self, ty_ctx: &'a TypeContext) -> TypeViewKind<'a, FunctionItemType> {
        let computed_type = self.function.r#type.computed().as_view(ty_ctx);
        let TypeView::FunctionItem(function_type) = computed_type else {
            unreachable!("Should generate call only with function type");
        };
        function_type
    }
}

#[repr(u8)]
#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
pub enum Intrinsic {
    Print,
    UnsafeTransmute,
    ArrayOf,
    ArrayOfZeros,
    ArrayGet,
    ArraySet,
    ArrayLen,
}

impl Intrinsic {
    pub const ALL: &[Self] = &[
        Self::Print,
        Self::UnsafeTransmute,
        Self::ArrayOf,
        Self::ArrayOfZeros,
        Self::ArrayGet,
        Self::ArraySet,
        Self::ArrayLen,
    ];

    /// The id for an intrinsic is the syntax id without file.
    pub fn syntax_id(&self) -> SyntaxId {
        SyntaxId::new(None, NonZeroU64::new(*self as u64 + 1).unwrap())
    }

    pub fn r#type(&self) -> Type {
        Type::FunctionItem(TypeId::new(self.syntax_id()))
    }

    pub fn function_type(&self) -> FunctionItemType {
        FunctionItemType {
            parameters: FunctionParameters::Unconstrained,
            return_type: Type::Top,
            reference: FunctionReference::Intrinsic(*self),
        }
    }
}

impl FromStr for Intrinsic {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "print" => Intrinsic::Print,
            "unsafeTransmute" => Intrinsic::UnsafeTransmute,
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
