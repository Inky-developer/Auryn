use std::{fmt::Debug, str::FromStr};

use crate::{
    auryn::{
        air::{
            namespace::UserDefinedTypeId,
            typecheck::{
                type_context::{TypeContext, TypeId},
                types::{FunctionItemType, IntrinsicType, Type, TypeView, TypeViewKind},
            },
            unresolved_type::UnresolvedType,
        },
        file_id::FileId,
        syntax_id::SyntaxId,
        tokenizer::BinaryOperatorToken,
    },
    utils::{fast_map::FastMap, small_string::SmallString},
};

#[derive(Debug, Default)]
pub struct Globals {
    pub functions: FastMap<AirFunctionId, AirFunction>,
    pub types: FastMap<UserDefinedTypeId, AirType>,
    pub type_aliases: FastMap<TypeAliasId, AirType>,
    pub statics: FastMap<AirStaticValueId, AirStaticValue>,
}

impl Globals {
    pub fn merge(&mut self, other: Self) {
        let Globals {
            functions,
            types,
            type_aliases,
            statics,
        } = other;
        self.functions.extend(functions);
        self.types.extend(types);
        self.type_aliases.extend(type_aliases);
        self.statics.extend(statics);
    }
}
pub struct Air {
    pub globals: Globals,
    pub ty_ctx: TypeContext,
}

impl Air {
    pub fn main_function(&self) -> (AirFunctionId, &AirFunction) {
        let mut main_functions = self
            .globals
            .functions
            .iter()
            .filter(|(id, _)| id.0.0.file_id() == Some(FileId::MAIN_FILE))
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
            .field("functions", &self.globals.functions)
            .finish_non_exhaustive()
    }
}

#[derive(Debug, Clone)]
pub enum AirStaticValue {
    Function(AirFunctionId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeAliasId(pub SyntaxId);

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct AirModuleId(pub FileId);

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

#[derive(Debug)]
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

#[derive(Debug)]
pub enum FunctionReference {
    UserDefined(AirFunctionId),
    Extern {
        parent: Box<AirType>,
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
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExternFunctionKind {
    Static,
    Method,
}

#[derive(Debug)]
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
    Type(AirType),
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
    StructLiteral(Vec<(SmallString, AirExpression)>),
}

#[derive(Debug)]
pub struct Call {
    pub function: Box<AirExpression>,
    pub arguments: Vec<AirExpression>,
}

pub enum CallKind<'a> {
    FunctionItem(TypeViewKind<'a, FunctionItemType>),
    Intrinsic(TypeViewKind<'a, IntrinsicType>),
}

impl Call {
    /// Should be called from codegen
    pub fn function_type<'a>(&self, ty_ctx: &'a TypeContext) -> CallKind<'a> {
        let computed_type = self.function.r#type.computed().as_view(ty_ctx);
        match computed_type {
            TypeView::FunctionItem(function_type) => CallKind::FunctionItem(function_type),
            TypeView::Intrinsic(intrinsic_type) => CallKind::Intrinsic(intrinsic_type),
            other => panic!("invalid function type {other}"),
        }
    }
}

#[repr(u8)]
#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
pub enum Intrinsic {
    Print,
    UnsafeTransmute,
    Cast,
    ArrayOf,
    ArrayOfZeros,
    ArrayGet,
    ArraySet,
    ArrayLen,
}

impl Intrinsic {
    pub fn r#type(self, ty_ctx: &mut TypeContext) -> Type {
        Type::Intrinsic(ty_ctx.add_intrinsic(IntrinsicType { intrinsic: self }))
    }
}

impl FromStr for Intrinsic {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "print" => Intrinsic::Print,
            "unsafeTransmute" => Intrinsic::UnsafeTransmute,
            "cast" => Intrinsic::Cast,
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
